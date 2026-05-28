# stepwise

# --- 1. Packages nécessaires ---
library(lme4) # Pour glmer (régression logistique mixte)
library(mice) # Pour gérer les objets mira/mids
library(dplyr) # Pour manipuler les données

# --- 2. Préparation ---
# Supposons que votre objet mira s'appelle 'imp'
# Si c'est un objet 'mira', extrayez les données imputées :
if (class(imp) == "mira") {
  impsets <- lapply(1:imp$m, function(i) complete(imp, i))
} else if (class(imp) == "mids") {
  impsets <- lapply(1:imp$m, function(i) complete(imp, i))
} else {
  stop("L'objet doit être de classe 'mira' ou 'mids' (package mice).")
}

# Liste des variables candidates (toutes les colonnes sauf la cible et l'effet aléatoire)
all_vars <- names(impsets[[1]])
candidates <- setdiff(all_vars, c("resultat_candida_def", "iep"))

# --- 3. Fonctions de sélection forward/backward ---
# Sélection FORWARD
forward_selection <- function(data) {
  # Modèle de base (intercept + effet aléatoire)
  base_formula <- as.formula("I(resultat_candida_def == 'positif') ~ 1 + (1 | iep)")
  base_model <- tryCatch(
    glmer(
      base_formula,
      data = data,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")
    ),
    error = function(e) NULL
  )
  if (is.null(base_model)) {
    stop("Échec du modèle de base.")
  }

  current_formula <- base_formula
  current_aic <- AIC(base_model)
  selected_vars <- character(0)
  remaining_vars <- candidates

  # Ajout séquentiel des variables
  while (length(remaining_vars) > 0) {
    aic_values <- numeric(length(remaining_vars))
    for (i in seq_along(remaining_vars)) {
      var <- remaining_vars[i]
      test_formula <- update(current_formula, paste0("~ . + ", var))
      test_model <- tryCatch(
        glmer(
          test_formula,
          data = data,
          family = binomial(link = "logit"),
          control = glmerControl(optimizer = "bobyqa")
        ),
        error = function(e) NULL
      )
      aic_values[i] <- if (!is.null(test_model)) AIC(test_model) else Inf
    }

    best_var <- remaining_vars[which.min(aic_values)]
    if (min(aic_values) < current_aic) {
      current_formula <- update(current_formula, paste0("~ . + ", best_var))
      current_model <- glmer(
        current_formula,
        data = data,
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa")
      )
      current_aic <- AIC(current_model)
      selected_vars <- c(selected_vars, best_var)
      remaining_vars <- setdiff(remaining_vars, best_var)
    } else {
      break
    }
  }
  return(list(
    formula = current_formula,
    model = current_model,
    aic = current_aic,
    vars = selected_vars
  ))
}

# Sélection BACKWARD
backward_selection <- function(data) {
  # Modèle complet (toutes les variables)
  full_formula <- as.formula(paste(
    "I(resultat_candida_def == 'positif') ~",
    paste(candidates, collapse = " + "),
    "+ (1 | iep)"
  ))
  full_model <- tryCatch(
    glmer(
      full_formula,
      data = data,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")
    ),
    error = function(e) NULL
  )
  if (is.null(full_model)) {
    warning("Modèle complet échoué. Passage à la sélection forward.")
    return(forward_selection(data))
  }

  current_formula <- full_formula
  current_model <- full_model
  current_aic <- AIC(current_model)
  selected_vars <- candidates

  # Retrait séquentiel des variables
  while (length(selected_vars) > 0) {
    aic_values <- numeric(length(selected_vars))
    for (i in seq_along(selected_vars)) {
      var <- selected_vars[i]
      test_formula <- update(current_formula, paste0("~ . - ", var))
      test_model <- tryCatch(
        glmer(
          test_formula,
          data = data,
          family = binomial(link = "logit"),
          control = glmerControl(optimizer = "bobyqa")
        ),
        error = function(e) NULL
      )
      aic_values[i] <- if (!is.null(test_model)) AIC(test_model) else Inf
    }

    best_var <- selected_vars[which.min(aic_values)]
    if (min(aic_values) < current_aic) {
      current_formula <- update(current_formula, paste0("~ . - ", best_var))
      current_model <- glmer(
        current_formula,
        data = data,
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa")
      )
      current_aic <- AIC(current_model)
      selected_vars <- setdiff(selected_vars, best_var)
    } else {
      break
    }
  }
  return(list(
    formula = current_formula,
    model = current_model,
    aic = current_aic,
    vars = selected_vars
  ))
}

# --- 4. Application aux 50 imputations ---
# Forward
forward_results <- lapply(impsets, function(data) {
  tryCatch(forward_selection(data), error = function(e) {
    warning(paste("Échec forward :", e$message))
    return(NULL)
  })
})

# Backward
backward_results <- lapply(impsets, function(data) {
  tryCatch(backward_selection(data), error = function(e) {
    warning(paste("Échec backward :", e$message))
    return(NULL)
  })
})

# --- 5. Résultats ---
# Fréquence des variables sélectionnées (forward)
forward_vars <- unlist(lapply(forward_results, function(x) {
  if (!is.null(x)) x$vars else character(0)
}))
forward_freq <- table(forward_vars)
print("🔹 Variables sélectionnées en FORWARD (fréquence sur 50 imputations) :")
print(sort(forward_freq, decreasing = TRUE))

# Fréquence des variables sélectionnées (backward)
backward_vars <- unlist(lapply(backward_results, function(x) {
  if (!is.null(x)) x$vars else character(0)
}))
backward_freq <- table(backward_vars)
print("🔹 Variables sélectionnées en BACKWARD (fréquence sur 50 imputations) :")
print(sort(backward_freq, decreasing = TRUE))

# --- 6. Modèle final (exemple : variables dans >50% des cas) ---
final_vars_forward <- names(forward_freq)[forward_freq > 25] # >50% des 50 imputations
final_formula_forward <- as.formula(paste(
  "I(resultat_candida_def == 'positif') ~",
  paste(final_vars_forward, collapse = " + "),
  "+ (1 | iep)"
))

# Pooling des coefficients (si vous voulez un modèle final)
if (length(final_vars_forward) > 0) {
  final_models <- lapply(impsets, function(data) {
    glmer(
      final_formula_forward,
      data = data,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")
    )
  })
  library(mice)
  pooled_results <- pool(final_models)
  print("🔹 Résultats poolés du modèle final (forward) :")
  summary(pooled_results)
}
