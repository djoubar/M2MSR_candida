################################################################################
#                                                                              #
#                                  M2MSR_MICE                                  #
#                                                                              #
################################################################################
# source("scripts/brutes/_setup.R")
# df_imp <- df_base |>
#   select(
#     -c(
#       date_adm_hospit,
#       date_adm_rea,
#       date_hemoc,
#       date_deces,
#       date_sortie_rea,
#       date_candidemie,
#       all_of(starts_with("adm_sofa")),
#       all_of(starts_with("hc_sofa")),
#       adm_neutro_min,
#       adm_lympho_min,
#       hc_lactates_moy,
#       adm_lactates_moy,
#       adm_diurese_tot,
#       hc_diurese_tot,
#       hc_neutro_min,
#       hc_lympho_min
#     )
#   )

# imp <- mice(
#   df_imp,
#   m = 50,
#   defaultMethod = c("lasso.norm", "logreg"),
#   maxit = 30
# )
# saveRDS(imp, file = "donnees/df_impute.rds")

#   # Ajuster le modèle sur ce jeu
#   model_i <- glmer(
#     resultat_candida_def ~ demo_centre +
#       hc_transfu +
#       demo_atcd_diabete +
#       hc_dialyse +
#       hc_choc +
#       hc_catheter_majeur +
#       hospit_parenterale_duree +
#       hospit_chirurgie_abdominale +
#       hospit_atb_duree +
#       hc_delai +
#       hc_vi_cat +
#       demo_atcd_hemato +
#       (1 | iep),
#     data = df_imputed_i,
#     family = binomial,
#     control = glmerControl(optimizer = "bobyqa")
#   )

#   # Prédire sur le jeu de test
#   predict(model_i, newdata = df_base, type = "response", re.form = NA)
# })

# # Pooler les prédictions (moyenne sur les 50 imputations)
# pooled_predictions <- rowMeans(do.call(cbind, predictions))

# roc_obj <- roc(
#   response = df_base$resultat_candida_def,  # ✅ Utiliser test_data
#   predictor = pooled_predictions
# )
# auc_value <- auc(roc_obj)

# # Tracer la courbe ROC
# plot(roc_obj, main = paste("Courbe ROC (AUC =", round(auc_value, 3), ")"))
# legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), bty = "n")

# # ✅ Si vous voulez calibrer le modèle poolé (coefficients moyens)
# cal <- calibrate(pooled_fit, method = "boot", B = 100)
# plot(cal, main = "Courbe de calibration (modèle poolé)")
