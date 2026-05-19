################################################################################
#                                                                              #
#                            M2MSR_TBL4_R_LOG_MIXTE                            #
#                                                                              #
################################################################################
# !!! STEPWISE
df_stepwise <- df_base |>
  mutate(
    hospit_atb_duree_72 = as.factor(ifelse(hospit_atb_duree > 3, "Oui", "Non")),
    hospit_parenterale = as.factor(ifelse(hospit_parenterale_duree > 0, "Oui", "Non"))
  ) |>
  select(
    iep,
    resultat_candida_def,
    demo_centre,
    hc_transfu,
    demo_atcd_diabete,
    hc_dialyse,
    hc_choc,
    hc_catheter_majeur,
    hospit_parenterale,
    hospit_chirurgie_abdominale,
    hospit_atb_duree_72,
    hc_delai,
    hc_vi_cat,
    demo_atcd_hemato,
  ) |>
  na.omit()

mod_intercept <- glmer(
  resultat_candida_def ~ 1 + (1 | iep),
  data = df_stepwise,
  family = binomial
)

mod_full <- glmer(
  resultat_candida_def ~ . + (1 | iep),
  data = df_stepwise,
  family = "binomial",
  na.action = na.fail
)

predictors <- setdiff(names(df_stepwise), c("resultat_candida_def", "iep"))

##################################################################################################
# Forward
# forward_selection <- function(data, random_effect = "(1 | iep)") {
#   # Modèle de base (intercept + effet aléatoire)
#   base_formula <- as.formula(paste("resultat_candida_def ~ 1 +", random_effect))
#   best_model <- glmer(base_formula, data = data, family = binomial)
#   best_aic <- AIC(best_model)
#   selected_vars <- character(0)
#   remaining_vars <- predictors

#   cat("=== FORWARD SELECTION ===\n")
#   cat("AIC initial (intercept only):", best_aic, "\n\n")

#   while (length(remaining_vars) > 0) {
#     aic_improvements <- numeric(length(remaining_vars))
#     for (i in seq_along(remaining_vars)) {
#       var <- remaining_vars[i]
#       current_formula <- as.formula(
#         paste(
#           "resultat_candida_def ~",
#           paste(c(selected_vars, var), collapse = " + "),
#           "+",
#           random_effect
#         )
#       )
#       temp_model <- tryCatch(
#         glmer(current_formula, data = data, family = binomial),
#         error = function(e) NULL
#       )
#       if (!is.null(temp_model)) {
#         aic_improvements[i] <- AIC(temp_model)
#       } else {
#         aic_improvements[i] <- Inf
#       }
#     }

#     # Trouver la variable qui améliore le plus l'AIC
#     best_improvement <- which.min(aic_improvements)
#     best_var <- remaining_vars[best_improvement]
#     best_aic_new <- aic_improvements[best_improvement]

#     if (best_aic_new < best_aic) {
#       selected_vars <- c(selected_vars, best_var)
#       remaining_vars <- remaining_vars[-best_improvement]
#       best_aic <- best_aic_new
#       best_formula <- as.formula(
#         paste("resultat_candida_def ~", paste(selected_vars, collapse = " + "), "+", random_effect)
#       )
#       best_model <- glmer(best_formula, data = data, family = binomial)
#       cat("Ajout de", best_var, "| AIC:", round(best_aic, 2), "\n")
#     } else {
#       break
#     }
#   }

#   cat("\nModèle final (Forward) :\n")
#   print(summary(best_model))
#   return(best_model)
# }

# Backward
backward_selection <- function(data, random_effect = "(1 | iep)") {
  # Modèle complet avec contrôle de convergence
  full_formula <- as.formula(
    paste("resultat_candida_def ~", paste(predictors, collapse = " + "), "+", random_effect)
  )
  best_model <- glmer(
    full_formula,
    data = data,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)) # <-- Ajout ici
  )
  best_aic <- AIC(best_model)
  selected_vars <- predictors

  cat("\n=== BACKWARD SELECTION ===\n")
  cat("AIC initial (modèle complet):", best_aic, "\n\n")

  while (length(selected_vars) > 0) {
    aic_improvements <- numeric(length(selected_vars))
    for (i in seq_along(selected_vars)) {
      var <- selected_vars[i]
      current_vars <- selected_vars[-i]
      if (length(current_vars) == 0) {
        current_formula <- as.formula(paste("resultat_candida_def ~ 1 +", random_effect))
      } else {
        current_formula <- as.formula(
          paste("resultat_candida_def ~", paste(current_vars, collapse = " + "), "+", random_effect)
        )
      }
      temp_model <- tryCatch(
        {
          glmer(
            current_formula,
            data = data,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)) # <-- Ajout ici
          )
        },
        error = function(e) NULL
      )
      if (!is.null(temp_model)) {
        aic_improvements[i] <- AIC(temp_model)
      } else {
        aic_improvements[i] <- Inf
      }
    }
    # ... (le reste de la fonction reste identique)
  }
  cat("\nModèle final (Backward) :\n")
  print(summary(best_model))
  return(best_model)
}

# Exec
# model_forward <- forward_selection(df_stepwise)
model_backward <- backward_selection(df_stepwise)

# --- 5. Comparaison des modèles finaux ---
cat("\n=== COMPARAISON DES MODÈLES ===\n")
cat("AIC Forward :", AIC(mod_forward), "\n")
cat("AIC Backward :", AIC(model_backward), "\n")


# AUC + Courbe calibration
mod_forward <- readRDS("models/mod_brutes_forward.rds")
tbl_regression(mod_forward, exponentiate = TRUE)
# AUC
pred_cond <- predict(mod_forward, type = "response", re.form = NULL)
auc_cond <- roc(df_stepwise$resultat_candida_def, pred_cond)
auc_cond$auc
g1_auc_meta_fwd <- plot(auc_cond, main = "Courbe ROC (prédictions conditionnelles)")
# Courbe calibration
dd <- datadist(df_base)
options(datadist = "dd")
df_stepwise$pred_cond <- pred_cond
df_stepwisea$hc_delai <- as.numeric(df_stepwise$hc_delai, units = "days")
calibrate_glmm <- function(
  mod_forward,
  df_stepwise,
  pred_col = "pred_cond",
  y_col = "resultat_candida_def"
) {
  # Créer un objet lrm (nécessaire pour calibrate)
  fit <- lrm(
    as.formula(paste(
      y_col,
      "~",
      paste(setdiff(names(df_stepwise), c(y_col, pred_col, "iep")), collapse = "+")
    )),
    data = df_stepwise,
    x = TRUE,
    y = TRUE
  )
  cal <- calibrate(fit, B = 100, method = "boot")
  plot(cal)
  return(cal)
}

# Exécuter
calib_forward <- calibrate_glmm(mod_forward, df_stepwise)
