library(mice)
library(purrr)
library(broom)
library(dplyr)
library(tibble)

# =============================================================================
# 0. CONFIGURATION INITIALE
# =============================================================================
imp <- readRDS("donnees/df_impute_surv.rds")
dep_var <- "resultat_candida_def"
exclude_vars <- "iep"
n_boot <- 200
alpha_final <- 0.15
n_imputations <- imp$m
candidate_vars <- setdiff(names(imp$data), c(dep_var, exclude_vars))

# Parrélélisation (optionnelle)
use_parallel <- TRUE
n_cores <- max(1, parallel::detectCores() - 1)

if (use_parallel) {
  library(future)
  library(future.apply)
  plan(multisession, workers = n_cores)
}

# =============================================================================
# 1. BOOTSTRAP + SÉLECTION FORWARD (AIC)
# =============================================================================
fit_glm_safe <- function(formula, data) {
  tryCatch(
    suppressWarnings(
      glm(formula, data = data, family = binomial)
    ),
    error = function(e) NULL
  )
}

row_bootstrap_sample <- function(data) {
  idx <- sample(seq_len(nrow(data)), nrow(data), replace = TRUE)
  data[idx, , drop = FALSE]
}

run_one_bootstrap <- function(data, vars) {
  boot_data <- row_bootstrap_sample(data)

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
      test_formula <- as.formula(paste(
        dep_var,
        "~",
        paste(test_vars, collapse = " + ")
      ))
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

  final_formula <- as.formula(paste(
    dep_var,
    "~",
    paste(current_vars, collapse = " + ")
  ))
  final_model <- fit_glm_safe(final_formula, boot_data)
  if (is.null(final_model)) {
    return(list(variables = current_vars, p_values = setNames(numeric(0), character(0))))
  }

  p_values <- tidy(final_model, conf.int = FALSE) %>%
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
    bootstrap_forward_selection(data_imputed, candidate_vars)
  }
)

# =============================================================================
# 2. SÉLECTION FINALE : VARIABLES SIGNIFICATIVES (p < alpha_final) DANS >= 60%
#    DES n_boot * n_imputations MODÈLES
# =============================================================================
all_boot_results <- unlist(bootstrap_all_imputations, recursive = FALSE)
total_models <- length(all_boot_results)

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
  paste(final_vars, collapse = " + ")
))

fits <- lapply(1:n_imputations, function(i) {
  data_imputed <- complete(imp, i)
  fit_glm_safe(final_formula, data_imputed)
})

if (any(vapply(fits, is.null, logical(1)))) {
  warning(
    "Le modèle final n'a pas convergé sur au moins un dataset imputé : vérifier les résultats avant de pooler."
  )
  fits <- fits[!vapply(fits, is.null, logical(1))]
}

pooled_results <- pool(fits)


pooled_summary <- summary(pooled_results, conf.int = TRUE)
print(pooled_summary)
saveRDS(pooled_summary, file = "pooled_summary_selection.rds")

# =============================================================================
# 4. (OPTIONNEL) PERFORMANCE DU MODÈLE
# =============================================================================
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
