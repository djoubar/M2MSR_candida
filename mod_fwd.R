library(leaps)
df_stepwise <- complete(imp, 1)
model_full <- glmer(
  resultat_candida_def ~ demo_type_rea +
    demo_atcd_hemato +
    adm_igs2 +
    adm_choc +
    adm_diurese_norm +
    adm_creat_max +
    adm_lactates_max +
    adm_pfio2_min +
    adm_transfu +
    hc_choc +
    hc_diurese_norm +
    hc_creat_max +
    hc_lactates_max +
    hc_leuco_min +
    hc_vi_cat +
    hc_dialyse +
    hc_transfu +
    hc_amines +
    hospit_parenterale_duree +
    hospit_ctc_duree +
    hospit_atb_duree +
    (1 | iep),
  data = df_stepwise,
  family = "binomial"
)

candidate_models <- dredge(
  model_full,
  subset = . ~ demo_type_rea +
    demo_atcd_hemato +
    adm_igs2 +
    adm_choc +
    adm_diurese_norm +
    adm_creat_max +
    adm_lactates_max +
    adm_pfio2_min +
    adm_transfu +
    hc_choc +
    hc_diurese_norm +
    hc_creat_max +
    hc_lactates_max +
    hc_leuco_min +
    hc_vi_cat +
    hc_dialyse +
    hc_transfu +
    hc_amines +
    hospit_parenterale_duree +
    hospit_ctc_duree +
    hospit_atb_duree +
    (1 | iep)
)
best_model <- get.models(candidate_models, subset = 1)[[1]]
saveRDS(best_model, "mod_bckwd.rds")
