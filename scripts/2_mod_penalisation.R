# ==================================================================================================
#                                 MODELISATION PAR PENALISATION
# ==================================================================================================

library(tidyverse)
library(glmnet)
library(caret)
library(mice)

# --------------------------------------------------------------------------------------------------
# Paramètres
# --------------------------------------------------------------------------------------------------

variable_a_predire <- "resultat_candida_def"
variables_predictives <- setdiff(
  names(df_train),
  variable_a_predire
)

# Matrices x/y pour train et test (mêmes colonnes !)
.x_train <- model.matrix(~ . - 1, df_train[, variables_predictives])
.x_test <- model.matrix(~ . - 1, df_test[, variables_predictives])
.y_train <- df_train[[variable_a_predire]]
.y_test <- df_test[[variable_a_predire]]

# Poids de classe (déséquilibre 376 vs 29 000)
.w_train <- ifelse(
  .y_train == levels(.y_train)[2],
  sum(.y_train == levels(.y_train)[1]) / sum(.y_train == levels(.y_train)[2]),
  1
)

# --------------------------------------------------------------------------------------------------
# Modèle Lasso
# --------------------------------------------------------------------------------------------------
cv_lasso <- cv.glmnet(
  .x_train,
  .y_train,
  family = "binomial",
  alpha = 1,
  weights = .w_train,
  type.measure = "auc",
  nfolds = 5
)

cl <- coef(cv_lasso, s = "lambda.1se")
vars_lasso <- setdiff(rownames(cl)[as.vector(cl != 0)], "(Intercept)")


# --------------------------------------------------------------------------------------------------
# Modèle Ridge
# --------------------------------------------------------------------------------------------------
cv_ridge <- cv.glmnet(
  .x_train[, vars_lasso],
  .y_train,
  family = "binomial",
  alpha = 0,
  weights = .w_train,
  type.measure = "auc",
  nfolds = 5
)

pred_lasso <- predict(cv_lasso, newx = .x_test, s = "lambda.1se", type = "response")
pred_ridge <- predict(cv_ridge, newx = .x_test[, vars_lasso], s = "lambda.min", type = "response")


#

####################################################################################################
#                                        METRICS                                                   #
####################################################################################################

library(tidyverse)
library(pROC)
library(caret)
library(MLmetrics)

# model <- pred_ridge # modifier pred_proba si pred_lasso ou pred_ridge
df_test |>
  na.omit(df_test)

# --------------------------------------------------------------------------------------------------
# Probabilités prédites & seuil
# --------------------------------------------------------------------------------------------------
# ! si rigdge/lasso, utiliser :
# pred_proba <- pred_lasso
pred_proba <- pred_ridge
# sinon :
# pred_proba <- predict(model, newdata = df_test, type = "prob")[, "Positive"]

pred_reelle <- df_test$resultat_candida_def

# --------------------------------------------------------------------------------------------------
# Courbe ROC
# --------------------------------------------------------------------------------------------------
# Courbe ROC
roc_obj <- roc(
  response = pred_reelle,
  predictor = pred_proba
)

# ROC_df
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

# AUC (Area Under the Curve)
auc_value <- auc(roc_obj)
auc_ci <- ci.auc(roc_obj, method = "bootstrap", boot.n = 2000)
print(paste("AUC :", round(auc_value, 4)))
print(paste("IC 95% :", round(auc_ci[1], 4), "-", round(auc_ci[3], 4)))

# ROC_plot
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "black", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  annotate(
    "text",
    x = 0.5,
    y = 0.25,
    label = paste0("AUC = ", round(auc_value, 3)),
    color = "black",
    size = 5
  ) +
  labs(title = "Courbe ROC", x = "1 - Spécificité", y = "Sensibilité") +
  theme_classic()

# --------------------------------------------------------------------------------------------------
# Mesures principales
# --------------------------------------------------------------------------------------------------
best_seuil <- coords(
  roc_obj,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)
print(best_seuil)

# --------------------------------------------------------------------------------------------------
# Matrice de confusion
# --------------------------------------------------------------------------------------------------
pred_class <- ifelse(pred_proba > best_seuil$threshold, "Positive", "Négative") |>
  as_factor()

mtx_confusion <- confusionMatrix(
  pred_class, # Prédictions
  pred_reelle # Vraies valeurs
)


# Précision, rappel, F1-score
print(paste("Accuracy :", Accuracy(pred_class, pred_reelle)))
print(paste("Precision :", Precision(pred_class, pred_reelle)))
print(paste("Recall :", Recall(pred_class, pred_reelle)))
print(paste("F1-Score :", F1_Score(pred_class, pred_reelle)))
