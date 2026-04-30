################################################################################
#                                                                              #
#                            M2MSR_TBL4_R_LOG_MIXTE                            #
#                                                                              #
################################################################################

.df_tbl4 <- df_base |>
  select(
    resultat_candida_def,
    iep,
    demo_centre,
    hc_glucanes_max,
    demo_atcd_hemato,
    adm_transfu,
    hc_transfu,
    demo_atcd_diabete,
    hc_mannanes_max,
    hc_catheter_majeur,
    # hc_vi_cat,
    hospit_parenterale_duree,
    hc_dialyse,
    hc_neutro_min,
    demo_atcd_diabete,
    hospit_vvc_duree,
    adm_poids,
    adm_temp_min,
    adm_choc,
    hc_choc,
    hospit_chirurgie_majeure,
    hospit_chirurgie_abdominale
  )

tbl4m_uv <-
  tbl_uvregression(
    data = .df_tbl4,
    method = glmer,
    y = resultat_candida_def,
    formula = "{y} ~ {x} + (1|iep) + (1|demo_centre)",
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

mod.m <- glmer(
  data = .df_tbl4,
  y = resultat_candida_def,
  formula = "{y} ~ {x} + (1|iep) + (1|demo_centre)",
  family = "binomial"
)
summary(mod.m)
tbl4m_mv <- tbl_regression(mod.m, exponentiate = TRUE)

tbl4 <- tbl_merge(
  tbls = list(tbl4m_uv, tbl4m_mv),
  tab_spanner = c("Univariée", "Multivariée")
)

tbl4 |>
  as_gt() |>
  gtsave(filename = "figures/tbl4.html")

pacman::p_load(easystats)
plot_rlog_mixte <- mod.m %>%
  model_parameters(exponentiate = TRUE) %>%
  plot()
