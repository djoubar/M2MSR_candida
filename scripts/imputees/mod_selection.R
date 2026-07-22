library(lme4)
library(mice)
library(purrr)
library(broom.mixed)
library(dplyr)
library(tibble)
library(progressr)

# =============================================================================
# 0. CONFIGURATION INITIALE
# =============================================================================
imp <- readRDS("donnees/df_impute_surv.rds")
dep_var <- "resultat_candida_def"
random_effect <- "iep"

n_boot <- 200 # Nombre de bootstraps par dataset imputé
alpha_final <- 0.05 # Seuil de significativité pour la sélection finale

# Règle de résumé de l'issue au niveau cluster, utilisée pour stratifier le
# bootstrap. resultat_candida_def N'EST PAS constant au sein d'un même iep
# (confirmé), donc il faut choisir comment définir "l'issue du cluster" :
#   - "any_positive" : le cluster est positif s'il contient au moins un 1
#                       (ex : Candida détecté au moins une fois pendant le
#                       séjour). C'est en général la définition cliniquement
#                       la plus naturelle pour ce type d'issue.
#   - "majority"      : le cluster prend la valeur la plus fréquente parmi
#                       ses observations (égalité tranchée en faveur de 1).
# À confirmer/ajuster selon le sens clinique de vos données.
cluster_outcome_rule <- "any_positive" # ou "majority"

n_imputations <- imp$m
candidate_vars <- setdiff(names(imp$data), c(dep_var, random_effect))

# Active/désactive la parallélisation des bootstraps (recommandé : le coût total
# est de l'ordre de n_imputations * n_boot * longueur(candidate_vars) ajustements glmer).
use_parallel <- TRUE
# Correction : sur la plupart des machines, detectCores() - 10 est négatif ou
# nul, ce qui forçait n_cores à 1 (donc aucune parallélisation réelle, avec en
# plus l'overhead de sérialisation de future). On garde ici un cœur de libre
# pour le système, ce qui est le compromis standard.
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
# 1. BOOTSTRAP DE CLUSTERS STRATIFIÉ SUR L'ISSUE + SÉLECTION FORWARD (AIC)
# =============================================================================
# Le bootstrap rééchantillonne les clusters (et non les lignes individuelles),
# pour préserver la structure de corrélation intra-cluster propre aux modèles
# mixtes. De plus, il est stratifié sur l'issue résumée au niveau cluster
# (cf. cluster_outcome_rule ci-dessus) :
#   - le nombre de clusters tirés dans chaque strate (0 / 1) est fixé et égal
#     au nombre de clusters de cette strate dans les données originales
#     => taille (en nombre de clusters) et proportion 0/1 (au niveau cluster)
#     identiques à chaque bootstrap.
# Note : le nombre total de LIGNES peut encore varier légèrement d'un
# bootstrap à l'autre, car les clusters n'ont pas tous la même taille. Fixer
# exactement le nombre de lignes nécessiterait un bootstrap au niveau
# individuel, ce qui casserait la structure de cluster : c'est le compromis
# standard pour les modèles mixtes.
#
# Le critère d'arrêt de la sélection forward compare l'AIC du meilleur ajout
# candidat à l'AIC du modèle courant : une variable n'est ajoutée que si elle
# améliore réellement l'AIC (et non simplement si un modèle converge).

fit_glmer_safe <- function(formula, data) {
  tryCatch(
    suppressWarnings(
      glmer(formula, data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
    ),
    error = function(e) NULL
  )
}

# Résume l'issue au niveau cluster selon la règle choisie. Calculé une seule
# fois par dataset imputé (pas à chaque bootstrap), puisque la composition
# des clusters ne change pas entre bootstraps d'une même imputation.
get_cluster_outcome <- function(data, cluster_var, dep_var, rule) {
  data %>%
    mutate(.dep_num = as.numeric(as.character(.data[[dep_var]]))) %>%
    group_by(.data[[cluster_var]]) %>%
    summarise(
      n_obs = n(),
      n_positive = sum(.dep_num == 1, na.rm = TRUE),
      outcome = if (rule == "any_positive") {
        as.integer(n_positive > 0)
      } else {
        tab <- table(.dep_num)
        as.integer(names(tab)[which.max(tab)])
      },
      .groups = "drop"
    ) %>%
    rename(cluster_id = 1)
}

cluster_bootstrap_sample <- function(data, cluster_var, cluster_outcome) {
  ids_0 <- cluster_outcome$cluster_id[cluster_outcome$outcome == 0]
  ids_1 <- cluster_outcome$cluster_id[cluster_outcome$outcome == 1]

  boot_ids <- c(
    if (length(ids_0) > 0) sample(ids_0, length(ids_0), replace = TRUE) else character(0),
    if (length(ids_1) > 0) sample(ids_1, length(ids_1), replace = TRUE) else character(0)
  )

  do.call(
    rbind,
    lapply(seq_along(boot_ids), function(i) {
      d <- data[data[[cluster_var]] == boot_ids[i], ]
      # Suffixe pour que chaque tirage du même cluster soit traité comme une
      # unité distincte par glmer (sinon les doublons seraient fusionnés en un
      # seul niveau d'effet aléatoire).
      d[[cluster_var]] <- paste0(d[[cluster_var]], "_b", i)
      d
    })
  )
}

run_one_bootstrap <- function(data, vars, cluster_outcome) {
  boot_data <- cluster_bootstrap_sample(data, random_effect, cluster_outcome)

  null_formula <- as.formula(paste(dep_var, "~ 1 + (1 |", random_effect, ")"))
  null_model <- fit_glmer_safe(null_formula, boot_data)
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
      test_formula <- as.formula(paste(
        dep_var,
        "~",
        paste(test_vars, collapse = " + "),
        "+ (1 |",
        random_effect,
        ")"
      ))
      test_model <- fit_glmer_safe(test_formula, boot_data)
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

  final_formula <- as.formula(paste(
    dep_var,
    "~",
    paste(current_vars, collapse = " + "),
    "+ (1 |",
    random_effect,
    ")"
  ))
  final_model <- fit_glmer_safe(final_formula, boot_data)
  if (is.null(final_model)) {
    return(list(variables = current_vars, p_values = setNames(numeric(0), character(0))))
  }

  p_values <- tidy(final_model, conf.int = FALSE, effects = "fixed") %>%
    filter(term %in% current_vars) %>%
    select(term, p.value) %>%
    deframe()

  list(variables = current_vars, p_values = p_values)
}

# --- Pré-calcul : jeux de données imputés + résumé d'issue par cluster ------
# Fait une seule fois (pas à chaque bootstrap ni à chaque itération), pour
# éviter de rappeler complete(imp, i) inutilement et pour pouvoir afficher un
# résumé des clusters mixtes avant de lancer les n_imputations * n_boot
# ajustements.

imputed_datasets <- lapply(seq_len(n_imputations), function(i) complete(imp, i))

cluster_outcomes <- lapply(imputed_datasets, function(d) {
  get_cluster_outcome(d, random_effect, dep_var, cluster_outcome_rule)
})

# Résumé informatif sur les clusters mixtes (imprimé une fois, sur la 1ère
# imputation ; la composition des clusters ne dépend pas de l'imputation sauf
# si dep_var lui-même a des valeurs imputées).
mixed_summary <- imputed_datasets[[1]] %>%
  mutate(.dep_num = as.numeric(as.character(.data[[dep_var]]))) %>%
  group_by(.data[[random_effect]]) %>%
  summarise(n_distinct_outcome = n_distinct(.dep_num), .groups = "drop")

n_mixed <- sum(mixed_summary$n_distinct_outcome > 1)
cat(
  "\n[Info] Règle de stratification cluster : '",
  cluster_outcome_rule,
  "'\n",
  "[Info] Nombre de clusters (",
  random_effect,
  ") avec issue non constante : ",
  n_mixed,
  " / ",
  nrow(mixed_summary),
  "\n",
  "[Info] Répartition des clusters obtenue avec cette règle (imputation 1) :\n",
  sep = ""
)
print(table(outcome = cluster_outcomes[[1]]$outcome))
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
      cluster_outcomes[[im_index]]
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
# 2. SÉLECTION FINALE : VARIABLES SIGNIFICATIVES (p < alpha_final) DANS >= 60%
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
  filter(proportion_significant >= 0.6) %>%
  pull(variable)

cat(
  "\nVariables sélectionnées dans le modèle final (p <",
  alpha_final,
  "dans >= 60% des",
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
final_formula <- as.formula(paste(
  dep_var,
  "~",
  paste(final_vars, collapse = " + "),
  "+ (1 |",
  random_effect,
  ")"
))

fits <- lapply(seq_len(n_imputations), function(i) {
  fit_glmer_safe(final_formula, imputed_datasets[[i]])
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
