################################################################################
#                                                                              #
#                            M2MSR_TBL3_R_LOG_SIMPLE                           #
#                                                                              #
################################################################################
.df_tbl3 <- df_base

.df_tbl3 <- .df_tbl3 |>
  select(
    iep,
    resultat_candida_def,
    hc_glucanes_max,
    hc_mannanes_max,
    hc_transfu,
    hc_vvc,
    # hc_vi_cat,
    hc_dialyse,
    hc_neutro_min,
    demo_atcd_diabete,
    hc_choc,
    hospit_chirurgie_majeure,
    hospit_chirurgie_abdominale
  )

.df_tbl3_xiep <- .df_tbl3 |>
  select(-iep)

tbl3_uv <-
  tbl_uvregression(
    data = .df_tbl3_xiep,
    method = glm,
    y = resultat_candida_def,
    formula = "{y} ~ {x}",
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

mod.nm_mv <- glm(
  data = .df_tbl3,
  family = "binomial",
  formula = resultat_candida_def ~ hc_glucanes_max +
    hc_mannanes_max +
    hc_cgr +
    hc_pfc +
    hc_cp +
    hc_vvc +
    # hc_vi_cat +
    hc_dialyse +
    hc_neutro_min +
    demo_atcd_diabete +
    hospit_fibro +
    hc_noradre
)


tbl3_mv <- tbl_regression(mod.nm_mv, exponentiate = TRUE)
mod.nm_mv |>
  model_parameters(exponentiate = TRUE) |>
  plot()

tbl3 <- tbl_merge(
  tbls = list(tbl3_uv, tbl3_mv),
  tab_spanner = c("Univariée", "Multivariée")
)

pacman::p_load(easystats)
plot_rlog <- mod.nm_mv %>%
  model_parameters(exponentiate = TRUE) %>%
  plot()
