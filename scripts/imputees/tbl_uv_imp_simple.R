imp <- read_rds("donnees/df_impute.rds")
# source("scripts/brutes/_setup.R")
df_base <- complete(imp, 1)
# names(df_base)
# str(df_base)

df_base <- df_base |>
  # select(-c(id_hemoc, groupehc)) |>
  mutate(
    duree_hospit = as.numeric(duree_hospit, units = "hours"),
    hc_delai = as.numeric(hc_delai, units = "hours")
  ) |>
  select(
    -c(
      id_hemoc,
      -groupehc,
      adm_albu_20,
      adm_albu_4,
      adm_bicar,
      adm_gelo,
      adm_nacl,
      adm_plasmalyte,
      adm_ringer,
      adm_adre,
      adm_dobu,
      adm_isoprenaline,
      adm_noradre,
      adm_terlipressine,
      hc_albu_20,
      hc_albu_4,
      hc_bicar,
      hc_gelo,
      hc_nacl,
      hc_plasmalyte,
      hc_ringer,
      hc_voluven,
      hc_adre,
      hc_dobu,
      hc_isoprenaline,
      hc_noradre,
      hc_terlipressine
    )
  )

tbl_uv <- tbl_uvregression(
  data = df_base,
  y = resultat_candida_def,
  method = glmer,
  formula = "{y} ~ {x} + (1|iep)",
  method.args = list(family = binomial),
  exponentiate = TRUE
) |>
  as_gt() |>
  gtsave("tbl_uv_imput_simp.docx")

# tbl_uv <- tbl_uvregression(
#   data = df_base,
#   y = resultat_candida_def[1:3],
#   method = glmer,
#   formula = "{y} ~ {x} + (1|iep)",
#   method.args = list(family = binomial),
#   exponentiate = TRUE
# ) |>
#   modify_table_body(
#     ~ p.adjust(.x, method = "fdr"),
#     columns = p.value
#   ) |>
#   as_gt()
# gtsave("tbl_uv_imput_simp_fdr.docx")
