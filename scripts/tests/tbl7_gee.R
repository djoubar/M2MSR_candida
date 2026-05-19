rm (list = ls())
source("scripts/_setup.R")
library(gee)

data <- data |>
  arrange(iep, date_hemoc) |>
  distinct(iep, .keep_all = TRUE) |>
  select(iep, resultat_candida_def, demo_centre, demo_age, demo_sexe, demo_atcd_diabete,
         demo_atcd_pancreatite, demo_atcd_tumeur, adm_pancreatite_aigue,
         adm_poids, adm_temp_min, adm_temp_max,
         adm_vi_cat, adm_dialyse, adm_cgr, adm_pfc, adm_cp,
         hc_vi_cat,hc_dialyse,hc_kta,hc_vvc,hc_ktd,hc_ecmo,hc_cgr,hc_pfc,hc_cp,
         hospit_vi_duree, hospit_parenterale_duree, hospit_vvc_duree, hospit_kta_duree,
         hospit_ktd_duree,hospit_ecmo_duree,hospit_atb_duree,hospit_ctc_duree,
         hospit_cgr,hospit_pfc,hospit_cp,hospit_fibro) |>
  na.omit()

mod.gee <- gee(data = data,
               formula = resultat_candida_def ~ demo_centre+ demo_age+ demo_sexe+ demo_atcd_diabete,
               id = iep,
               corstr="exchangeable",
               family = "binomial")
summary(mod.gee)
