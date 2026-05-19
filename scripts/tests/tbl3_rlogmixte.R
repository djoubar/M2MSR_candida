################################################################################
#                                                                              #
#                            M2MSR_TBL3_R_LOG_MIXTE                            #
#                                                                              #
################################################################################
.df_tbl3 <- df_base |>
  select(
    resultat_candida_def,
    iep,
    demo_centre,
    demo_age,
    demo_sexe,
  )

tbl3m_uv <-
  tbl_uvregression(
    data = .df_tbl3,
    method = glmer,
    y = resultat_candida_def,
    formula = "{y} ~ {x} + (1|iep)",
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

mod.m <- glmer(
  data = .df_tbl3,
  formula = resultat_candida_def ~ demo_centre +
    demo_atcd_hemato +
    adm_transfu +
    hc_transfu +
    demo_atcd_diabete +
    hc_catheter_majeur +
    # hc_vi_cat
    hospit_parenterale_duree +
    hc_dialyse +
    hc_neutro_min +
    hospit_vvc_duree +
    adm_poids +
    adm_temp_min +
    adm_choc +
    hc_choc +
    hc_delai +
    hospit_chirurgie_majeure +
    hospit_chirurgie_abdominale +
    (1 | iep),
  family = "binomial"
)
summary(mod.m)
tbl3m_mv <- tbl_regression(mod.m, exponentiate = TRUE)

tbl3 <- tbl_merge(
  tbls = list(tbl3m_uv, tbl3m_mv),
  tab_spanner = c("Univariée", "Multivariée")
)

pacman::p_load(easystats)
plot_rlog_mixte <- mod.m %>%
  model_parameters(exponentiate = TRUE) %>%
  plot()
