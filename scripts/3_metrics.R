####################################################################################################
#                                        METRICS                                                   #
####################################################################################################

library(MLmetrics)
library(pROC)
library(caret)

model <- model.rf

df_test <- df_test |>
  na.omit()
# --------------------------------------------------------------------------------------------------
# Probabilités prédites & seuil
# --------------------------------------------------------------------------------------------------

pred_proba <- predict(model, newdata = df_test)
pred_class <- ifelse(pred_proba > 0.03, 1, 0)

# --------------------------------------------------------------------------------------------------
# Matrice de confusion
# --------------------------------------------------------------------------------------------------
pred_reelle <- df_test$resultat_candida_def

mtx_confusion <- confusionMatrix(
  factor(pred_proba), # Prédictions
  factor(pred_reelle) # Vraies valeurs
)

# --------------------------------------------------------------------------------------------------
# Courbe ROC + AUC
# --------------------------------------------------------------------------------------------------
# Courbe ROC

# AUC (Area Under the Curve)
auc_value <- auc(pred_reelle, pred_proba)
print(paste("AUC :", auc_value))

# --------------------------------------------------------------------------------------------------
# Mesures principales
# --------------------------------------------------------------------------------------------------
# Précision, rappel, F1-score
print(paste("Accuracy :", Accuracy(pred_class, y)))
print(paste("Precision :", Precision(pred_class, y)))
print(paste("Recall :", Recall(pred_class, y)))
print(paste("F1-Score :", F1_Score(pred_class, y)))
