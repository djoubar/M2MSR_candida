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
# insérer le dataframe
df <- readRDS("donnees/df_impute.rds")
df <- complete(df, 1)

# insérer la variable à prédire
variable_a_predire <- "resultat_candida_def"

# insérer les variables prédictives
variables_predictives <- setdiff(names(df), c("id_hemoc", "iep", "groupehc", variable_a_predire))

# ! valeurs manquantes = modèle infaisable
df <- na.omit(df[, c(variable_a_predire, variables_predictives)])

x <- model.matrix(~ . - 1, data = df[, variables_predictives])
y <- df[[variable_a_predire]]

# --------------------------------------------------------------------------------------------------
# Modèle Ridge
# --------------------------------------------------------------------------------------------------
cv_model <- cv.glmnet(
  x = x, # Matrice des variables explicatives (standardisées)
  y = y, # Variable cible (binaire)
  family = "binomial", # Régression logistique
  alpha = 0, # Ridge (L2) => alpha = 0 // Lasso => alpha = 1
  nfolds = 5, # Nombre de folds pour la validation croisée
  type.measure = "class" # Métrique : erreur de classification (ou "auc" pour l'AUC)
)

# Afficher le lambda optimal (paramètre de régularisation)
print(paste("Lambda optimal :", cv_model$lambda.min))

# Afficher les coefficients du modèle avec le lambda optimal
coef(cv_model, s = "lambda.min")

# Prédire les probabilités
pred_proba <- predict(cv_model, newx = x, type = "response", s = "lambda.min")

# Convertir en classe (0 ou 1) avec un seuil de 0.5
pred_class <- ifelse(pred_proba > 0.03, 1, 0)

# Évaluer avec une matrice de confusion
confusionMatrix(
  factor(pred_class), # Prédictions
  factor(y) # Vraies valeurs
)
# AUC (Area Under the Curve)
auc_value <- auc(y, pred_proba)
print(paste("AUC :", auc_value))

# Précision, rappel, F1-score
library(MLmetrics)
print(paste("Accuracy :", Accuracy(pred_class, y)))
print(paste("Precision :", Precision(pred_class, y)))
print(paste("Recall :", Recall(pred_class, y)))
print(paste("F1-Score :", F1_Score(pred_class, y)))

# Tracer les coefficients
plot(cv_model, xvar = "lambda", main = "Coefficients en fonction de lambda (Ridge)")

# Tracer l'erreur en fonction de lambda
plot(cv_model, main = "Erreur de validation croisée")
# --------------------------------------------------------------------------------------------------
# Modèle Lasso
# --------------------------------------------------------------------------------------------------
xcol <- df[, variables_predictives]
mod_pen <- glmnet(x, y, family = "binomial", data = df, standardize = TRUE)
print(mod_pen)
plot(mod_pen, xvar = "lambda", label = TRUE)
exp(coef(mod_pen, s = 0.002))

# --------------------------------------------------------------------------------------------------
# Lambda optimal
# --------------------------------------------------------------------------------------------------
cv.mod_pen <- cv.glmnet(x, y, family = "binomial", data = df, standardize = TRUE)
cv.mod_pen
plot(cv.mod_pen)
coef(cv.mod_pen, s = "lambda.min") # minimum mean cross-validated error
coef(cv.mod_pen, s = "lambda.1se") # most regularized model such that the cross-validated error is within one standard error of the minimum.
