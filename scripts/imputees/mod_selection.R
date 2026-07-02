library(lme4)
library(mice)
library(purrr)
library(broom.mixed)
library(dplyr)
library(tibble)

# =============================================================================
# 0. CONFIGURATION INITIALE
# =============================================================================
# imp <- readRDS("donnees/df_impute.rds")
dep_var <- "resultat_candida_def"
random_effect <- "iep"

n_boot <- 200 # Nombre de bootstraps par dataset imputé
alpha_final <- 0.05 # Seuil de significativité pour la sélection finale
alpha_univariate <- 0.15 # Seuil pour la sélection univariée

# Proportion d'imputations dans lesquelles une variable doit être significative
# en univarié pour être retenue comme candidate. 0 < seuil <= 1.
# (1/n_imputations équivaut à la règle "au moins une fois" de la version initiale ;
#  0.5 est plus conservateur et recommandé.)
univariate_selection_threshold <- 0.5

n_imputations <- imp$m
candidate_vars <- setdiff(names(imp$data), c(dep_var, random_effect))

# Active/désactive la parallélisation des bootstraps (recommandé : le coût total
# est de l'ordre de n_imputations * n_boot * longueur(candidate_vars) ajustements glmer).
use_parallel <- FALSE
n_cores <- max(1, parallel::detectCores() - 1)

if (use_parallel) {
  library(future)
  library(future.apply)
  plan(multisession, workers = n_cores)
}

# =============================================================================
# 1. SÉLECTION UNIVARIÉE (test du rapport de vraisemblance) SUR CHAQUE IMPUTATION
# =============================================================================
# Important : pour une variable catégorielle à >2 niveaux, glmer/tidy renvoie un
# terme par niveau (ex. "varNiveau2", "varNiveau3"), jamais "var" tel quel.
# Comparer un test de Wald niveau par niveau au nom de la variable d'origine
# (comme dans la version initiale) échoue silencieusement pour ces variables :
# leur p-value est NA et elles ne sont jamais retenues. On utilise donc un test
# du rapport de vraisemblance (LRT) entre le modèle avec et sans la variable,
# ce qui donne un effet global correct quel que soit le nombre de niveaux.

test_univariate_single <- function(data, var) {
  if (is.factor(data[[var]]) && nlevels(droplevels(data[[var]])) < 2) {
    return(NA_real_)
  }

  f1 <- as.formula(paste(dep_var, "~", var, "+ (1 |", random_effect, ")"))
  f0 <- as.formula(paste(dep_var, "~ 1 + (1 |", random_effect, ")"))

  fit <- function(f) {
    tryCatch(
      suppressWarnings(
        glmer(f, data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
      ),
      error = function(e) NULL
    )
  }

  m1 <- fit(f1)
  m0 <- fit(f0)
  if (is.null(m1) || is.null(m0)) {
    return(NA_real_)
  }

  lrt <- tryCatch(anova(m0, m1), error = function(e) NULL)
  if (is.null(lrt) || nrow(lrt) < 2) {
    return(NA_real_)
  }

  lrt$`Pr(>Chisq)`[2]
}

univariate_results <- map_df(
  1:n_imputations,
  ~ {
    data_imputed <- complete(imp, .x)
    map_dfr(
      candidate_vars,
      function(v) {
        p_val <- test_univariate_single(data_imputed, v)
        data.frame(imputation = .x, variable = v, p_value = p_val)
      }
    ) %>%
      mutate(selected = !is.na(p_value) & p_value < alpha_univariate)
  }
)

univariate_summary <- univariate_results %>%
  group_by(variable) %>%
  summarise(
    n_tested = sum(!is.na(p_value)),
    n_selected = sum(selected, na.rm = TRUE),
    prop_selected = n_selected / n_imputations,
    .groups = "drop"
  ) %>%
  filter(prop_selected >= univariate_selection_threshold)

selected_univariate <- univariate_summary$variable

cat(
  "Variables sélectionnées en univarié (p <",
  alpha_univariate,
  "dans >=",
  round(univariate_selection_threshold * 100),
  "% des",
  n_imputations,
  "datasets imputés) :\n",
  paste(selected_univariate, collapse = ", "),
  "\n\n"
)

if (length(selected_univariate) == 0) {
  stop("Aucune variable n'a passé l'étape univariée. Vérifie le seuil ou les données.")
}

# =============================================================================
# 2. BOOTSTRAP (n_boot réplications) + SÉLECTION FORWARD (AIC) PAR IMPUTATION
# =============================================================================
# Deux corrections importantes par rapport à la version initiale :
#  (a) Le critère d'arrêt de la sélection forward comparait seulement les AIC
#      des variables candidates entre elles, sans jamais les comparer à l'AIC
#      du modèle courant. Une variable était donc TOUJOURS ajoutée tant qu'au
#      moins un modèle convergeait : ce n'était plus une sélection, mais un
#      simple classement de toutes les variables. Ici, on n'ajoute une variable
#      que si elle améliore réellement l'AIC du modèle courant.
#  (b) Le bootstrap rééchantillonnait les lignes individuellement, ce qui casse
#      la structure de cluster induite par l'effet aléatoire (un même cluster
#      pouvant se retrouver fragmenté ou avec une taille très différente d'un
#      bootstrap à l'autre). On rééchantillonne maintenant les clusters
#      eux-mêmes (bootstrap par cluster), pratique standard pour les modèles
#      mixtes.

fit_glmer_safe <- function(formula, data) {
  tryCatch(
    suppressWarnings(
      glmer(formula, data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
    ),
    error = function(e) NULL
  )
}

cluster_bootstrap_sample <- function(data, cluster_var) {
  ids <- unique(data[[cluster_var]])
  boot_ids <- sample(ids, length(ids), replace = TRUE)
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

run_one_bootstrap <- function(data, vars) {
  boot_data <- cluster_bootstrap_sample(data, random_effect)

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

bootstrap_forward_selection <- function(data, vars) {
  if (length(vars) == 0) {
    return(list())
  }

  if (use_parallel) {
    future_lapply(1:n_boot, function(i) run_one_bootstrap(data, vars), future.seed = TRUE)
  } else {
    lapply(1:n_boot, function(i) run_one_bootstrap(data, vars))
  }
}

set.seed(123)
bootstrap_all_imputations <- map(
  1:n_imputations,
  ~ {
    data_imputed <- complete(imp, .x)
    bootstrap_forward_selection(data_imputed, selected_univariate)
  }
)

# =============================================================================
# 3. SÉLECTION FINALE : VARIABLES SIGNIFICATIVES (p < alpha_final) DANS >= 60%
#    DES n_boot * n_imputations MODÈLES
# =============================================================================
# Correction critique par rapport à la version initiale : il fallait garder un
# résultat PAR bootstrap (et non tout aplatir avec unlist/do.call(c, ...)), sinon
# il est impossible de relier une p-value à "ce" bootstrap précis, et le
# dénominateur (n_imputations seulement, au lieu de n_boot * n_imputations) ne
# correspondait plus au numérateur. On aplatit ici la liste de listes en une
# liste plate de longueur n_imputations * n_boot, chaque élément représentant
# un modèle bootstrap individuel.

all_boot_results <- unlist(bootstrap_all_imputations, recursive = FALSE)
total_models <- length(all_boot_results) # = n_imputations * n_boot (modèles ayant convergé ou non)

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

counts_matrix <- t(sapply(selected_univariate, count_variable))

final_selection <- data.frame(
  variable = selected_univariate,
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
# 4. MODÈLE FINAL + COMBINAISON AVEC LES RÈGLES DE RUBIN
# =============================================================================
final_formula <- as.formula(paste(
  dep_var,
  "~",
  paste(final_vars, collapse = " + "),
  "+ (1 |",
  random_effect,
  ")"
))

fits <- lapply(1:n_imputations, function(i) {
  data_imputed <- complete(imp, i)
  fit_glmer_safe(final_formula, data_imputed)
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
# (as.data.frame() sur l'objet pool, utilisé dans la version initiale, ne
# renvoie pas ce tableau.)
pooled_summary <- summary(pooled_results, conf.int = TRUE)
print(pooled_summary)
saveRDS(pooled_summary, file = "pooled_summary_selection.rds")

# =============================================================================
# 5. (OPTIONNEL) PERFORMANCE DU MODÈLE
# =============================================================================
# Attention : l'AUC ci-dessous est calculée sur les mêmes données qui ont servi
# à la sélection des variables (étapes 1 à 3). Il s'agit donc d'une AUC
# "apparente", optimiste (double dipping). Pour une estimation honnête de la
# performance, il faudrait soit une validation externe, soit une correction de
# l'optimisme par bootstrap (ex. méthode de Harrell), ce qui dépasse le cadre
# de ce script.

if (requireNamespace("pROC", quietly = TRUE)) {
  library(pROC)
  auc_values <- sapply(seq_along(fits), function(i) {
    data_imputed <- complete(imp, i)
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
