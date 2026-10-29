library(glmnet)
library(pROC)

set.seed(42)

# Matrices x/y pour train et test (mêmes colonnes !)
x_train <- model.matrix(~ . - 1, df_train[, variables_predictives])
x_test <- model.matrix(~ . - 1, df_test[, variables_predictives])
y_train <- df_train[[variable_a_predire]]
y_test <- df_test[[variable_a_predire]]

# Poids de classe (déséquilibre 376 vs 29 000)
w_train <- ifelse(
  y_train == levels(y_train)[2],
  sum(y_train == levels(y_train)[1]) / sum(y_train == levels(y_train)[2]),
  1
)

# --- 1. Lasso : sélection de variables par CV ---
cv_lasso <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 1,
  weights = w_train,
  type.measure = "auc",
  nfolds = 5
)

cl <- coef(cv_lasso, s = "lambda.1se")
vars_lasso <- setdiff(rownames(cl)[as.vector(cl != 0)], "(Intercept)")

# --- 2. Ridge : estimation sur les variables retenues ---
cv_ridge <- cv.glmnet(
  x_train[, vars_lasso],
  y_train,
  family = "binomial",
  alpha = 0,
  weights = w_train,
  type.measure = "auc",
  nfolds = 5
)

# --- 3. Évaluation sur le test (modèle "final" = le fit complet de la CV) ---
pred_proba <- predict(cv_lasso, newx = x_test, s = "lambda.1se", type = "response")
pred_ridge <- predict(cv_ridge, newx = x_test[, vars_lasso], s = "lambda.min", type = "response")

roc_l <- roc(y_test, as.vector(pred_lasso))
auc(roc_l)
roc_r <- roc(y_test, as.vector(pred_ridge))
auc(roc_r)

# --- 4. Seuil de décision métier (exemple : taux de faux négatifs ≤ 50 %) ---
coords(roc_r, x = 0.5, input = "sensitivity", ret = c("threshold"))$threshold
