################################################################################
#                                                                              #
#                                  M2MSR_MICE                                  #
#                                                                              #
################################################################################
library(tidyverse)
library(readxl)
library(questionr)
library(gtsummary)
library(flextable)
library(patchwork)
library(labelled)
library(gt)
library(lme4)
library(glmnet)
library(pacman)
library(parameters)
library(see)
library(geepack)
library(Hmisc)
library(mice)
library(rsample)
library(hebstr)
library(MuMIn)
library(rms)
library(DT)
library(pROC)

df_imp <- df_base |>
  select(
    -c(date_adm_hospit, date_adm_rea, date_hemoc, date_sortie_rea),
    -all_of(starts_with("adm_sofa")),
    -all_of(starts_with("hc_sofa")),
    -adm_diurese_norm,
    -adm_diurese_tot,
    -adm_lactates_max,
    -adm_lactates_moy,
    -adm_neutro_min,
    -adm_lympho_min,
    -hc_diurese_tot,
    -hc_diurese_norm,
    -hc_pfio2_min,
    -hc_creat_max,
    -hc_lactates_max,
    -hc_lactates_moy,
    -hc_neutro_min,
    -hc_lympho_min,
    -hc_glucanes_max,
    -hc_mannanes_max
  )

# imp <- mice(
#   df_imp,
#   m = 50,
#   method = "rf",
#   maxit = 50,
#   seed = 123
# )
# saveRDS(imp, file = "donnees/df_impute.rds")

# # pooler les résultats
# # ✅ Supposons que test_data est votre jeu de test (sans NA)
# # Si vous n'avez pas de jeu de test, utilisez une validation croisée.

# # Extraire chaque imputation et prédire sur test_data
# predictions <- lapply(1:50, function(i) {
#   # Extraire le i-ème jeu imputé complet
#   df_imputed_i <- complete(imp, i)

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
