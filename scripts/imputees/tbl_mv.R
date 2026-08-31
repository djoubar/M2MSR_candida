library(mice)
library(gtsummary)

imp <- readRDS("donnees/df_impute_surv.rds")

fit_multi <- with(
  imp,
  glm(
    resultat_candida_def ~
      hc_vi_cat +
      hc_transfu +
      hc_dialyse +
      adm_igs2 +
      hospit_ctc_duree +
      hc_catheter_majeur +
      hospit_cgr +
      hospit_chirurgie_abdominale,
    family = binomial(link = "logit")
  )
)

tbl_multi <- tbl_regression(
  fit_multi,
  exponentiate = TRUE,
  pvalue_fun = ~ style_pvalue(.x, digits = 3),
  label = list(
    hc_vi_cat = "Ventilation mécanique invasive dans les 48h précédant l'hémoculture",
    hc_transfu = "Transfusion (CGR ou PFC ou CP) dans les 48h précédant l'hémoculture",
    hc_dialyse = "Epuration extra-rénale dans les 48h précédant l'hémoculture",
    hc_catheter_majeur = "Cathéter Veineux Central dans les 48h précédant l'hémoculture",
    hospit_chirurgie_abdominale = "Chirurgie abdominale",
    hospit_cgr = "Nombre de CGR administrés",
    adm_igs2 = "Score IGS 2 à l'admission",
    hospit_ctc_duree = "Durée de traitement par corticoïdes (en jours)"
  )
) |>
  modify_header(estimate = "**OR ajusté**") %>%
  bold_p(t = 0.05)
