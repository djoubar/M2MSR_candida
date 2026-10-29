#===================================================================================================
#
#                                       RANDOM FORREST
#
#===================================================================================================

library(randomForest)
library(caret)

df_train <- df_train |>
  na.omit()

var_predire <- "resultat_candida_def"

# --------------------------------------------------------------------------------------------------
# Modèle
# --------------------------------------------------------------------------------------------------

model.rf <- randomForest(
  var_predire ~ .,
  data = df_train,
  ntree = 2000,
  importance = TRUE,
  strata = var_predire,
  mtry = 4
  # ,nodesize = 50
)

# --------------------------------------------------------------------------------------------------
# Tuning
# --------------------------------------------------------------------------------------------------

tune_rf <- tuneRF(
  df_train[, -which(names(df_train) == var_predire)],
  df_train$resultat_candida,
  mtryStart = 4,
  ntreeTry = 500,
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE,
  plot = TRUE
)

# --------------------------------------------------------------------------------------------------
# Tuning 2 le retour
# --------------------------------------------------------------------------------------------------

mtries <- c(1, 2, 3, 4, 6, 9, 13, 18)
aucs <- sapply(mtries, function(m) {
  mod <- randomForest(
    var_predire ~ .,
    data = df_train,
    ntree = 500,
    mtry = m,
    strata = "resultat_candida"
  )
  pROC::auc(pROC::roc(df_train$resultat_candida_def, mod$votes[, "Positive"]))
})
data.frame(mtry = mtries, AUC = aucs)
