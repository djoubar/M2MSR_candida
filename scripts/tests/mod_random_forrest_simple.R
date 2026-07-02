#===================================================================================================
#
#                                       RANDOM FORREST
#
#===================================================================================================
library(randomForest)
library(rsample)

source("scripts/survie/_setup_survie.R")
set.seed(123)
df_rf <- df_base |>
  arrange(iep, date_hemoc) |>
  distinct(iep, .keep_all = TRUE) |>
  na.omit()


#===================================================================================================
#
#===================================================================================================

split <- initial_split(df_rf, prop = 0.2, strata = resultat_candida_def)
df_20 <- training(split)
df_80 <- testing(split)

model.rf <- randomForest(resultat_candida_def ~ ., data = df_80, ntree = 1000, importance = TRUE)
plotRF <- varImpPlot(model.rf)
candida_pred <- predict(model.rf, df_80)
df_80$candida_pred <- candida_pred
matrix <- table(df_80$resultat_candida_def, candida_pred)
precision <- sum(diag(matrix) / sum(matrix))
#
# rf.candida <- table(data$resultat_candida_def,
#                     predict(model.rf, newdata = data))
# rf.candida
# probabilites <- predict(model.rf, newdata = data, type = "prob")[,1]
#
# roc_curve <- roc(data$resultat_candida_def, probabilites)
# auc_value <- auc(roc_curve)
# plot(roc_curve)

split <- initial_split(df_rf, prop = 0.8, strata = resultat_candida_def)
df_80 <- training(split)
df_20 <- testing(split)
