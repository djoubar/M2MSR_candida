# TESTS GEE
source("scripts/_setup.R")

data <- data |> 
  select(iep, groupehc, resultat_candida_def, hc_glucanes_max, hc_mannanes_max, hc_cgr, 
         hc_pfc, hc_cp, hc_vvc, hc_vi_cat, hc_dialyse, 
         demo_atcd_diabete, hospit_fibro, hc_noradre) |> 
  mutate(groupehc = factor(groupehc))

data_long <- data |>
  mutate(across(-c(iep, resultat_candida_def))) |> 
  pivot_longer(
    cols = -iep,
    names_to =c("variable", "groupehc"),
    names_sep = "_",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from = variable,
    valeurs_from = value,
    id_cols = c(iep, groupe_hc)
  )

mod.gee <- geeglm(
  data = data,
  resultat_candida_def ~ hc_glucanes_max+ hc_mannanes_max+ hc_cgr+ 
    hc_pfc+ hc_cp+ hc_vvc+ hc_vi_cat+ hc_dialyse+ hc_neutro_min+ 
    demo_atcd_diabete+ hospit_fibro+ hc_noradre,
  id = iep, 
  family = "binomial",
  corstr = "ar1"
)

summary(mod.gee)

table(data$resultat_candida_def)
