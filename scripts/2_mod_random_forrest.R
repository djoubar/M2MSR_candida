#===================================================================================================
#
#                                       RANDOM FORREST
#
#===================================================================================================
# Sorties : models/pred_rf.rds, models/mod_rf.rds

library(tidyverse)
library(randomForest)

if (!exists("df_train")) source("scripts/1_splits.R")
source("scripts/_ml_utils.R")

# --------------------------------------------------------------------------------------------------
# Paramètres
# --------------------------------------------------------------------------------------------------
.formule <- reformulate(variables_predictives, response = variable_a_predire)
.y_train <- df_train[[variable_a_predire]]

# Sous-échantillonnage équilibré : chaque arbre est construit sur autant de négatifs que de
# positifs (« balanced random forest »), adapté au déséquilibre 376 vs 29 000
.n_pos <- sum(.y_train == niv[2])
.sampsize <- c(.n_pos, .n_pos)

# --------------------------------------------------------------------------------------------------
# Tuning de mtry sur l'AUC out-of-bag
# --------------------------------------------------------------------------------------------------
mtries <- c(2, 4, 6, 9, 13, 18)
mtries <- mtries[mtries <= length(variables_predictives)]
tune_rf <- tibble(
  mtry = mtries,
  auc_oob = map_dbl(mtries, function(m) {
    set.seed(graine)
    mod <- randomForest(
      .formule,
      data = df_train,
      ntree = 500,
      mtry = m,
      strata = .y_train,
      sampsize = .sampsize
    )
    as.numeric(pROC::auc(pROC::roc(.y_train, mod$votes[, niv[2]], quiet = TRUE)))
  })
)
print(tune_rf)
mtry_opt <- tune_rf$mtry[which.max(tune_rf$auc_oob)]

# --------------------------------------------------------------------------------------------------
# Modèle final
# --------------------------------------------------------------------------------------------------
set.seed(graine)
model.rf <- randomForest(
  .formule,
  data = df_train,
  ntree = 2000,
  mtry = mtry_opt,
  importance = TRUE,
  strata = .y_train,
  sampsize = .sampsize
)

pred_rf <- predict(model.rf, newdata = df_test, type = "prob")[, niv[2]]

sauver_predictions("rf", pred_rf)
saveRDS(list(model = model.rf, tune = tune_rf), file.path(dossier_modeles, "mod_rf.rds"))
