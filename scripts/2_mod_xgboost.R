# ==================================================================================================
#                                           XG_BOOST
# ==================================================================================================
# Sorties : models/pred_xgboost.rds, models/mod_xgboost.rds

library(tidyverse)
library(xgboost)

if (!exists("df_train")) source("scripts/1_splits.R")
source("scripts/_ml_utils.R")

# --------------------------------------------------------------------------------------------------
# Paramètres
# --------------------------------------------------------------------------------------------------
x_tr <- model.matrix(~ . - 1, df_train[, variables_predictives])
x_te <- model.matrix(~ . - 1, df_test[, variables_predictives])
stopifnot(identical(colnames(x_tr), colnames(x_te)))
y_tr_num <- as.integer(df_train[[variable_a_predire]] == niv[2]) # 0/1 numérique
y_te_num <- as.integer(df_test[[variable_a_predire]] == niv[2])

dtrain <- xgb.DMatrix(data = x_tr, label = y_tr_num)
dtest <- xgb.DMatrix(data = x_te, label = y_te_num)

n_pos <- sum(y_tr_num == 1)
n_neg <- sum(y_tr_num == 0)
spw <- n_neg / n_pos # ≈ 77 dans votre cas

# Plis de validation croisée par patient (liste des indices de validation de chaque pli)
.foldid <- creer_plis_patient(df_train, k = 5, graine = graine)
.folds <- split(seq_along(.foldid), .foldid)

params_base <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.05,
  max_depth = 5,
  min_child_weight = 10,
  subsample = 0.8,
  colsample_bytree = 0.7,
  scale_pos_weight = spw
)

# --------------------------------------------------------------------------------------------------
# Tuning (grille + validation croisée avec arrêt précoce)
# --------------------------------------------------------------------------------------------------
grille <- expand.grid(
  max_depth = c(3, 5, 7),
  min_child_weight = c(1, 10, 30),
  eta = c(0.02, 0.05, 0.1)
)

resultats <- do.call(
  rbind,
  lapply(seq_len(nrow(grille)), function(i) {
    p <- modifyList(params_base, as.list(grille[i, ]))
    set.seed(graine)
    cv <- xgb.cv(
      p,
      data = dtrain,
      nrounds = 2000,
      folds = .folds,
      early_stopping_rounds = 50, # s'arrête quand l'AUC de validation stagne
      verbose = 0
    )
    # best_iteration : à la racine (xgboost < 3) ou dans early_stop (xgboost >= 3)
    best_iter <- cv$best_iteration %||% cv$early_stop$best_iteration
    data.frame(grille[i, ], auc = max(cv$evaluation_log$test_auc_mean), nrounds = best_iter)
  })
)
best <- resultats[which.max(resultats$auc), ]
print(best)

params_final <- modifyList(params_base, as.list(best[, c("max_depth", "min_child_weight", "eta")]))

# --------------------------------------------------------------------------------------------------
# Modèle final
# --------------------------------------------------------------------------------------------------
set.seed(graine)
mod_xgb <- xgb.train(
  params = params_final,
  data = dtrain,
  nrounds = best$nrounds,
  verbose = 0
)

# Prédictions sur le test — même jeu de test que RF et glmnet !
pred_xgb <- predict(mod_xgb, dtest)

importance_xgb <- xgb.importance(model = mod_xgb)

sauver_predictions("xgboost", pred_xgb)
saveRDS(
  list(model = mod_xgb, tuning = resultats, importance = importance_xgb),
  file.path(dossier_modeles, "mod_xgboost.rds")
)
