# ==================================================================================================
#                                           XG_BOOST
# ==================================================================================================

library(xgboost)

# Paramètres

x_tr <- model.matrix(~ . - 1, df_tr[, variables_predictives])
x_te <- model.matrix(~ . - 1, df_te[, variables_predictives])
y_tr_num <- as.integer(y_tr == levels(y_tr)[2]) # 0/1 numérique
y_te_num <- as.integer(y_te == levels(y_te)[2])

dtrain <- xgb.DMatrix(data = x_tr, label = y_tr_num)
dtest <- xgb.DMatrix(data = x_te, label = y_te_num)

n_pos <- sum(y_tr_num == 1)
n_neg <- sum(y_tr_num == 0)
spw <- n_neg / n_pos # ≈ 77 dans votre cas

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

grille <- expand.grid(
  max_depth = c(3, 5, 7),
  min_child_weight = c(1, 10, 30),
  eta = c(0.02, 0.05, 0.1)
)

resultats <- do.call(
  rbind,
  lapply(seq_len(nrow(grille)), function(i) {
    p <- modifyList(params_base, as.list(grille[i, ]))
    cv <- xgb.cv(
      p,
      data = dtrain,
      nrounds = 2000,
      nfold = 5,
      early_stopping_rounds = 50, # s'arrête quand l'AUC de validation stagne
      verbose = 0
    )
    data.frame(grille[i, ], auc = max(cv$evaluation_log$test_auc_mean), nrounds = cv$best_iteration)
  })
)
best <- resultats[which.max(resultats$auc), ]

params_final <- modifyList(params_base, as.list(best[, c("max_depth", "min_child_weight", "eta")]))
params_final$eta <- best$eta

set.seed(42)
mod_xgb <- xgb.train(
  params = params_final,
  data = dtrain,
  nrounds = best$nrounds,
  verbose = 1
)

# Prédictions et AUC sur le test — même jeu de test que RF et glmnet !
prob_te <- predict(mod_xgb, dtest)
roc_xgb <- roc(y_te_num, prob_te)
auc(roc_xgb)

importance <- xgb.importance(model = mod_xgb)
xgb.plot.importance(importance, top_n = 20)
xgb.plot.tree(model = mod_xgb) # inspection d'un arbre
