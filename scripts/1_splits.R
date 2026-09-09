library(tidyverse)
library(mice)
library(rsample)

.df_tot <- readRDS("donnees/df_impute.rds")
df <- complete(.df_tot, 1)

split <- initial_split(df, prop = 0.2, strata = "resultat_candida_def")
df_train <- training(split)
df_test <- testing(split)
