# ==================================================================================================
#                      MODELISATION PAR PENALISATION SUR DONNEES IMPUTEES (MICE)
# ==================================================================================================

source("scripts/_setup.R")

library(tidyverse)
library(glmnet)
library(caret)
library(mice)
library(pROC)

imp <- readRDS("donnees/df_impute.rds")

# --------------------------------------------------------------------------------------------------
# Paramètres
# --------------------------------------------------------------------------------------------------
variable_a_predire <- "resultat_candida_def"
variables_exclues  <- c()        # identifiants, dates... présents dans imp$data mais pas prédicteurs
graine             <- 123

# Vérifications : bon objet, mêmes patients, même ordre

m <- imp$m                       # 20

# --------------------------------------------------------------------------------------------------
# 1. Issue observée et découpage train / test par patient
# --------------------------------------------------------------------------------------------------
y_all <- factor(imp$data[[variable_a_predire]])   # issue OBSERVÉE, jamais l'issue imputée
niv   <- levels(y_all)                            # niv[2] = événement modélisé
n     <- length(y_all)

patients_ok <- which(!is.na(y_all))               # patients sans issue exclus

set.seed(graine)
idx_train <- patients_ok[createDataPartition(y_all[patients_ok], p = 0.75, list = FALSE)]
is_train  <- seq_len(n) %in% idx_train
is_test   <- !is_train & !is.na(y_all)

# --------------------------------------------------------------------------------------------------
# 2. Les 20 jeux empilés
# --------------------------------------------------------------------------------------------------
long <- complete(imp, action = "long")
long$.id <- rep(seq_len(n), m)                    # n° de patient (plus sûr que les rownames)
long <- long[!is.na(y_all[long$.id]), ]           # on garde les patients avec issue observée

variables_predictives <- setdiff(names(imp$data), c(variable_a_predire, variables_exclues))

X <- model.matrix(~ . - 1, long[, variables_predictives])
stopifnot(nrow(X) == nrow(long))                  # aucun NA résiduel après imputation
y <- y_all[long$.id]

tr <- is_train[long$.id]
te <- is_test[long$.id]

# --------------------------------------------------------------------------------------------------
# 3. Poids (déséquilibre des classes, calculé sur le train) x 1/m
# --------------------------------------------------------------------------------------------------
ratio <- sum(y_all[is_train] == niv[1]) / sum(y_all[is_train] == niv[2])
w     <- ifelse(y == niv[2], ratio, 1) / m

# --------------------------------------------------------------------------------------------------
# 4. Plis de validation croisée par patient, stratifiés sur l'issue
# --------------------------------------------------------------------------------------------------
set.seed(graine)
fold_patient <- createFolds(y_all[idx_train], k = 5, list = FALSE)
foldid       <- fold_patient[match(long$.id[tr], idx_train)]

# --------------------------------------------------------------------------------------------------
# 5. Lasso (sélection) puis Ridge (réestimation)
# --------------------------------------------------------------------------------------------------
cv_lasso <- cv.glmnet(
  X[tr, ], y[tr],
  family = "binomial", alpha = 1,
  weights = w[tr], foldid = foldid, type.measure = "auc"
)

cl <- coef(cv_lasso, s = "lambda.1se")
vars_lasso <- setdiff(rownames(cl)[as.vector(cl != 0)], "(Intercept)")
stopifnot(length(vars_lasso) >= 2)

cv_ridge <- cv.glmnet(
  X[tr, vars_lasso, drop = FALSE], y[tr],
  family = "binomial", alpha = 0,
  weights = w[tr], foldid = foldid, type.measure = "auc"
)

# --------------------------------------------------------------------------------------------------
# 6. Prédictions sur le test : moyenne des probabilités sur les 20 imputations
# --------------------------------------------------------------------------------------------------
id_te <- long$.id[te]

p_lasso <- as.vector(predict(cv_lasso, newx = X[te, ], s = "lambda.1se", type = "response"))
p_ridge <- as.vector(predict(cv_ridge, newx = X[te, vars_lasso, drop = FALSE],
                             s = "lambda.min", type = "response"))

pred_lasso <- tapply(p_lasso, id_te, mean)
pred_ridge <- tapply(p_ridge, id_te, mean)
y_test     <- y_all[as.integer(names(pred_lasso))]

roc_lasso <- roc(y_test, pred_lasso, levels = niv, direction = "<")
roc_ridge <- roc(y_test, pred_ridge, levels = niv, direction = "<")
auc(roc_lasso); auc(roc_ridge)
roc.test(roc_lasso, roc_ridge)
