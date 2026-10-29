####################################################################################################
#                                        METRICS                                                   #
####################################################################################################

library(tidyverse)
library(pROC)
library(caret)
library(MLmetrics)

model <- model.rf # modifier pred_proba si pred_lasso ou pred_ridge
df_test |>
  na.omit(df_test)

# --------------------------------------------------------------------------------------------------
# Probabilités prédites & seuil
# --------------------------------------------------------------------------------------------------
# ! si rigdge/lasso, utiliser :
# pred_proba <- pred_lasso
# pred_proba <- pred_ridge
# sinon :
pred_proba <- predict(model, newdata = df_test, type = "prob")[, "Positive"]

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
