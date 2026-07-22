library(mice)
library(purrr)
library(broom)
library(dplyr)
library(tibble)
library(progressr)

# =============================================================================
# 0. CONFIGURATION INITIALE
# =============================================================================
imp <- readRDS("donnees/df_impute_surv.rds")
dep_var <- "resultat_candida_def"

# "iep" n'est plus un effet aléatoire : avec un seul épisode par cluster, la
# corrélation intra-cluster qui justifiait un modèle mixte n'existe plus. On
# garde uniquement cet identifiant pour l'exclure des variables candidates
# (ce n'est pas un prédicteur).
id_var <- "iep"

n_boot <- 200 # Nombre de bootstraps par dataset imputé
alpha_final <- 0.15 # Seuil de significativité pour la sélection finale

n_imputations <- imp$m
candidate_vars <- setdiff(names(imp$data), c(dep_var, id_var))

# Active/désactive la parallélisation des bootstraps (recommandé : le coût total
# est de l'ordre de n_imputations * n_boot * longueur(candidate_vars) ajustements glm).
use_parallel <- FALSE
n_cores <- max(1, parallel::detectCores() - 1)

if (use_parallel) {
  library(future)
  library(future.apply)
  plan(multisession, workers = n_cores)
}

# Barre de progression : fonctionne aussi bien en séquentiel qu'en parallèle
# (future_lapply) grâce à progressr.
handlers(global = TRUE)
handlers(handler_txtprogressbar(width = 60))

# =============================================================================
# 1. BOOTSTRAP STRATIFIÉ SUR L'ISSUE + SÉLECTION FORWARD (AIC)
# =============================================================================
# Puisqu'il n'y a plus de structure de cluster (un seul épisode par iep), le
# bootstrap rééchantillonne directement les LIGNES, stratifié sur dep_var :
# on tire avec remise, séparément parmi les lignes à 0 et parmi les lignes à
# 1, en gardant exactement le même effectif dans chaque strate que dans les
# données originales.
#   => taille d'échantillon ET proportion de resultat_candida_def = 0/1
#      exactement identiques à chaque bootstrap (contrairement à
#      l'approximation "au niveau cluster" nécessaire avec un modèle mixte).
#
# Le critère d'arrêt de la sélection forward compare l'AIC du meilleur ajout
# candidat à l'AIC du modèle courant : une variable n'est ajoutée que si elle
# améliore réellement l'AIC (et non simplement si un modèle converge).

fit_glm_safe <- function(formula, data) {
  tryCatch(
    suppressWarnings(
      glm(formula, data = data, family = binomial())
    ),
    error = function(e) NULL
  )
}

stratified_bootstrap_sample <- function(data, dep_var, dep_levels) {
  # Utilise les deux modalités réellement observées (dep_levels), quel que
  # soit leur codage (0/1, 1/2, facteur texte...), plutôt que de supposer
  # "0"/"1" en dur. C'est indispensable pour repérer immédiatement un
  # problème de codage plutôt que de produire silencieusement des
  # échantillons vides.
  outcome <- as.character(data[[dep_var]])
  idx_a <- which(outcome == dep_levels[1])
  idx_b <- which(outcome == dep_levels[2])

  boot_idx <- c(
    sample(idx_a, length(idx_a), replace = TRUE),
    sample(idx_b, length(idx_b), replace = TRUE)
  )

  data[boot_idx, , drop = FALSE]
}

# Vérifie que dep_var est bien binaire (exactement 2 modalités non manquantes)
# dans un dataset donné, et retourne ces modalités. Arrête le script avec un
# message explicite plutôt que de laisser le bootstrap produire des
# échantillons vides ou dégénérés en silence.
validate_binary_outcome <- function(data, dep_var) {
  if (!(dep_var %in% names(data))) {
    stop(sprintf(
      "La variable dep_var = '%s' n'existe pas dans le dataset imputé (noms disponibles : %s).",
      dep_var,
      paste(names(data), collapse = ", ")
    ))
  }

  outcome <- as.character(data[[dep_var]])
  n_na <- sum(is.na(outcome))
  observed_levels <- sort(unique(outcome[!is.na(outcome)]))

  if (n_na > 0) {
    warning(sprintf(
      "%d valeur(s) manquante(s) dans %s après imputation : vérifier le dataset.",
      n_na,
      dep_var
    ))
  }

  if (length(observed_levels) != 2) {
    stop(sprintf(
      paste(
        "dep_var = '%s' n'a pas exactement 2 modalités non manquantes",
        "(modalités observées : %s). Le bootstrap stratifié et glm(family = binomial)",
        "nécessitent une issue binaire à 2 niveaux. Vérifiez le codage de cette variable",
        "dans df_impute_surv.rds (ex. 0/1 attendu, mais peut-être codée en 1/2, en texte,",
        "ou avec un nom de colonne différent)."
      ),
      dep_var,
      paste(observed_levels, collapse = ", ")
    ))
  }

  observed_levels
}

run_one_bootstrap <- function(data, vars, dep_levels) {
  boot_data <- stratified_bootstrap_sample(data, dep_var, dep_levels)

  null_formula <- as.formula(paste(dep_var, "~ 1"))
  null_model <- fit_glm_safe(null_formula, boot_data)
  if (is.null(null_model)) {
    return(list(variables = character(0), p_values = setNames(numeric(0), character(0))))
  }
  current_aic <- AIC(null_model)

  current_vars <- character(0)
  remaining_vars <- vars

  while (length(remaining_vars) > 0) {
    best_aic <- Inf
    best_var <- NULL

    for (var in remaining_vars) {
      test_vars <- c(current_vars, var)
      test_formula <- as.formula(paste(dep_var, "~", paste(test_vars, collapse = " + ")))
      test_model <- fit_glm_safe(test_formula, boot_data)
      if (!is.null(test_model) && AIC(test_model) < best_aic) {
        best_aic <- AIC(test_model)
        best_var <- var
      }
    }

    # On n'accepte l'ajout que s'il améliore l'AIC du modèle courant.
    if (!is.null(best_var) && best_aic < current_aic) {
      current_vars <- c(current_vars, best_var)
      remaining_vars <- setdiff(remaining_vars, best_var)
      current_aic <- best_aic
    } else {
      break
    }
  }

  if (length(current_vars) == 0) {
    return(list(variables = character(0), p_values = setNames(numeric(0), character(0))))
  }

  final_formula <- as.formula(paste(dep_var, "~", paste(current_vars, collapse = " + ")))
  final_model <- fit_glm_safe(final_formula, boot_data)
  if (is.null(final_model)) {
    return(list(variables = current_vars, p_values = setNames(numeric(0), character(0))))
  }

  p_values <- tidy(final_model) %>%
    filter(term %in% current_vars) %>%
    select(term, p.value) %>%
    deframe()

  list(variables = current_vars, p_values = p_values)
}

# --- Pré-calcul : jeux de données imputés -----------------------------------
# Fait une seule fois (pas à chaque bootstrap ni à chaque itération), pour
# éviter de rappeler complete(imp, i) inutilement.

imputed_datasets <- lapply(seq_len(n_imputations), function(i) complete(imp, i))

# Validation immédiate du codage de dep_var (une fois par dataset imputé) :
# arrête le script tout de suite avec un message clair si dep_var n'est pas
# binaire ou n'existe pas sous ce nom, plutôt que de laisser tourner
# n_imputations * n_boot bootstraps qui échoueront tous silencieusement.
dep_levels_by_imputation <- lapply(imputed_datasets, function(d) {
  validate_binary_outcome(d, dep_var)
})

cat(
  "\n[Info] Modalités observées de '",
  dep_var,
  "' (imputation 1) : ",
  paste(dep_levels_by_imputation[[1]], collapse = " / "),
  "\n",
  "[Info] Effectifs correspondants (imputation 1) :\n",
  sep = ""
)
print(table(imputed_datasets[[1]][[dep_var]], useNA = "ifany"))
cat("\n")

# --- Boucle principale : toutes les combinaisons (imputation, bootstrap) ---
# On aplatit directement l'espace des itérations en n_imputations * n_boot
# tâches indépendantes, ce qui simplifie à la fois la parallélisation et le
# suivi de progression global (au lieu d'une boucle imbriquée imputation ->
# bootstrap).

total_iterations <- n_imputations * n_boot

run_all_bootstraps <- function() {
  p <- progressor(steps = total_iterations)

  worker <- function(idx) {
    im_index <- ((idx - 1) %/% n_boot) + 1
    res <- run_one_bootstrap(
      imputed_datasets[[im_index]],
      candidate_vars,
      dep_levels_by_imputation[[im_index]]
    )
    p(sprintf("imputation %d/%d", im_index, n_imputations))
    res
  }

  if (use_parallel) {
    future_lapply(seq_len(total_iterations), worker, future.seed = TRUE)
  } else {
    lapply(seq_len(total_iterations), worker)
  }
}

set.seed(123)
all_boot_results <- with_progress(run_all_bootstraps())

# =============================================================================
# 2. SÉLECTION FINALE : VARIABLES SIGNIFICATIVES (p < alpha_final) DANS >= 50%
#    DES n_boot * n_imputations MODÈLES
# =============================================================================
total_models <- length(all_boot_results) # = n_imputations * n_boot

count_variable <- function(var) {
  selected <- vapply(all_boot_results, function(b) var %in% b$variables, logical(1))
  significant <- vapply(
    all_boot_results,
    function(b) {
      if (!(var %in% b$variables)) {
        return(FALSE)
      }
      p <- b$p_values[var]
      !is.na(p) && p < alpha_final
    },
    logical(1)
  )
  c(selected_count = sum(selected), significant_count = sum(significant))
}

counts_matrix <- t(sapply(candidate_vars, count_variable))

final_selection <- data.frame(
  variable = candidate_vars,
  selected_count = counts_matrix[, "selected_count"],
  significant_count = counts_matrix[, "significant_count"],
  proportion_selected = counts_matrix[, "selected_count"] / total_models,
  proportion_significant = counts_matrix[, "significant_count"] / total_models,
  row.names = NULL
)

print(final_selection)

final_vars <- final_selection %>%
  filter(proportion_significant >= 0.2) %>%
  pull(variable)

cat(
  "\nVariables sélectionnées dans le modèle final (p <",
  alpha_final,
  "dans >= 50% des",
  total_models,
  "modèles bootstrap) :\n",
  paste(final_vars, collapse = ", "),
  "\n\n"
)

if (length(final_vars) == 0) {
  stop(
    "Aucune variable n'a été sélectionnée. Vérifie tes seuils, le nombre de bootstraps, ou tes données."
  )
}

# =============================================================================
# 3. MODÈLE FINAL + COMBINAISON AVEC LES RÈGLES DE RUBIN
# =============================================================================
final_formula <- as.formula(paste(dep_var, "~", paste(final_vars, collapse = " + ")))

fits <- lapply(seq_len(n_imputations), function(i) {
  fit_glm_safe(final_formula, imputed_datasets[[i]])
})

if (any(vapply(fits, is.null, logical(1)))) {
  warning(
    "Le modèle final n'a pas convergé sur au moins un dataset imputé : vérifier les résultats avant de pooler."
  )
  fits <- fits[!vapply(fits, is.null, logical(1))]
}

pooled_results <- pool(fits)

# summary() sur l'objet "pool" donne directement term / estimate / std.error /
# statistic / df / p.value déjà combinés selon les règles de Rubin.
pooled_summary <- summary(pooled_results, conf.int = TRUE)
print(pooled_summary)
saveRDS(pooled_summary, file = "pooled_summary_selection.rds")

# =============================================================================
# 4. (OPTIONNEL) PERFORMANCE DU MODÈLE
# =============================================================================
# Attention : l'AUC ci-dessous est calculée sur les mêmes données qui ont servi
# à la sélection des variables (étapes 1 à 2). Il s'agit donc d'une AUC
# "apparente", optimiste (double dipping). Pour une estimation honnête de la
# performance, il faudrait soit une validation externe, soit une correction de
# l'optimisme par bootstrap (ex. méthode de Harrell), ce qui dépasse le cadre
# de ce script.

if (requireNamespace("pROC", quietly = TRUE)) {
  library(pROC)
  auc_values <- sapply(seq_along(fits), function(i) {
    data_imputed <- imputed_datasets[[i]]
    pred_prob <- predict(fits[[i]], type = "response", newdata = data_imputed)
    as.numeric(auc(roc(data_imputed[[dep_var]], pred_prob, quiet = TRUE)))
  })
  cat(
    "\nAUC apparente moyenne (sur les datasets imputés, optimiste - cf. note ci-dessus) :",
    round(mean(auc_values, na.rm = TRUE), 3),
    "\n"
  )
}

if (use_parallel) {
  plan(sequential)
}
