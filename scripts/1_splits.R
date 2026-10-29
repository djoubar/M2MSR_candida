library(tidyverse)
library(mice)
library(rsample)

.df_tot <- readRDS("donnees/df_impute_ml.rds")
df <- complete(.df_tot, 1) |>
  select(-c(demo_atcd_diabete, demo_atcd_tumeur, iep)) |>
  mutate(
    resultat_candida_def = as.factor(resultat_candida_def)
  )

split <- initial_split(df, prop = 0.3, strata = "resultat_candida_def")
df_train <- training(split)
df_test <- testing(split)
