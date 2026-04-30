###### RF
set.seed(69)
library(randomForest)

.df_rf <- df_base

# ! que
.df_rf <- .df_rf |>
  arrange(iep, date_hemoc) |>
  distinct(iep, .keep_all = TRUE) |>
  select(
    resultat_candida_def,
    demo_centre,
    demo_age,
    demo_sexe,
    demo_atcd_diabete,
    demo_atcd_pancreatite,
    demo_atcd_tumeur,
    adm_pancreatite_aigue,
    adm_poids,
    adm_temp_min,
    adm_temp_max,
    # adm_vi_cat,
    adm_dialyse,
    adm_cgr,
    adm_pfc,
    adm_cp,
    # hc_vi_cat,
    hc_dialyse,
    hc_kta,
    hc_vvc,
    hc_ktd,
    hc_ecmo,
    hc_cgr,
    hc_pfc,
    hc_cp,
    hospit_vi_duree,
    hospit_parenterale_duree,
    hospit_vvc_duree,
    hospit_kta_duree,
    hospit_ktd_duree,
    hospit_ecmo_duree,
    hospit_atb_duree,
    hospit_ctc_duree,
    hospit_cgr,
    hospit_pfc,
    hospit_cp,
    hospit_fibro
  ) |>
  na.omit()

split <- initial_split(.df_rf, prop = 0.2, strata = resultat_candida_def)
df_20 <- training(split)
df_80 <- testing(split)

model.rf <- randomForest(resultat_candida_def ~ ., data = df_20, ntree = 1000, importance = TRUE)
plotRF <- varImpPlot(model.rf)
candida_pred = predict(model.rf, df_80)
df_80$candida_pred = candida_pred
matrix = table(df_80$resultat_candida_def, candida_pred)
precision = sum(diag(matrix) / sum(matrix))
#
# rf.candida <- table(data$resultat_candida_def,
#                     predict(model.rf, newdata = data))
# rf.candida
# probabilites <- predict(model.rf, newdata = data, type = "prob")[,1]
#
# roc_curve <- roc(data$resultat_candida_def, probabilites)
# auc_value <- auc(roc_curve)
# plot(roc_curve)
