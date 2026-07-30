imp <- readRDS("donnees/df_impute_surv.rds")

names(imp$data)

fit_multi <- with(
  imp,
  glm(
    resultat_candida_def ~
      hc_vi_cat +
      hc_transfu +
      hc_dialyse +
      hc_kta +
      adm_igs2 +
      hospit_ctc_duree +
      hc_catheter_majeur +
      hospit_cgr +
      hospit_chirurgie_abdominale,
    ,
    family = binomial(link = "logit")
  )
)

tbl_multi <- tbl_regression(
  fit_multi,
  exponentiate = TRUE,
  pvalue_fun = ~ style_pvalue(.x, digits = 3)
) %>%
  modify_header(estimate = "**OR ajusté**") %>%
  bold_p(t = 0.05)

print(tbl_multi)
