# ==================================================================================================
#                                 MODELISATION PAR PENALISATION
# ==================================================================================================
# Lasso (sélection des variables) puis Ridge (réestimation sur les variables retenues par le Lasso).
# Sorties : models/pred_lasso.rds, models/pred_ridge.rds, models/mod_penalisation.rds

library(tidyverse)
library(glmnet)

if (!exists("df_train")) source("scripts/1_splits.R")
source("scripts/_ml_utils.R")

# --------------------------------------------------------------------------------------------------
# Paramètres
# --------------------------------------------------------------------------------------------------
# Matrices x/y pour train et test (mêmes colonnes !)
.x_train <- model.matrix(~ . - 1, df_train[, variables_predictives])
.x_test <- model.matrix(~ . - 1, df_test[, variables_predictives])
stopifnot(identical(colnames(.x_train), colnames(.x_test)))
.y_train <- df_train[[variable_a_predire]]

# Poids de classe (déséquilibre 376 vs 29 000)
.w_train <- ifelse(
  .y_train == niv[2],
  sum(.y_train == niv[1]) / sum(.y_train == niv[2]),
  1
)

# Plis de validation croisée par patient
.foldid <- creer_plis_patient(df_train, k = 5, graine = graine)

# --------------------------------------------------------------------------------------------------
# Modèle Lasso
# --------------------------------------------------------------------------------------------------
set.seed(graine)
cv_lasso <- cv.glmnet(
  .x_train,
  .y_train,
  family = "binomial",
  alpha = 1,
  weights = .w_train,
  type.measure = "auc",
  foldid = .foldid
)

cl <- coef(cv_lasso, s = "lambda.1se")
vars_lasso <- setdiff(rownames(cl)[as.vector(cl != 0)], "(Intercept)")
stopifnot(length(vars_lasso) >= 2)

# --------------------------------------------------------------------------------------------------
# Modèle Ridge
# --------------------------------------------------------------------------------------------------
set.seed(graine)
cv_ridge <- cv.glmnet(
  .x_train[, vars_lasso, drop = FALSE],
  .y_train,
  family = "binomial",
  alpha = 0,
  weights = .w_train,
  type.measure = "auc",
  foldid = .foldid
)

# --------------------------------------------------------------------------------------------------
# Prédictions sur le test
# --------------------------------------------------------------------------------------------------
pred_lasso <- predict(cv_lasso, newx = .x_test, s = "lambda.1se", type = "response")
pred_ridge <- predict(
  cv_ridge,
  newx = .x_test[, vars_lasso, drop = FALSE],
  s = "lambda.min",
  type = "response"
)

sauver_predictions("lasso", pred_lasso)
sauver_predictions("ridge", pred_ridge)
saveRDS(
  list(cv_lasso = cv_lasso, cv_ridge = cv_ridge, vars_lasso = vars_lasso),
  file.path(dossier_modeles, "mod_penalisation.rds")
)
