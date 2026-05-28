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
