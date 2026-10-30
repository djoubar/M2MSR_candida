# ==================================================================================================
#                                   DECOUPAGE APPRENTISSAGE / TEST
# ==================================================================================================
# Le découpage est fait PAR PATIENT (iep) : tous les épisodes d'un même patient sont soit en
# apprentissage, soit en test. Il est enregistré dans models/split.rds pour que tous les modèles
# soient évalués sur le même jeu de test. Mettre refaire_split <- TRUE pour le régénérer.

library(tidyverse)
library(mice)
library(rsample)

variable_a_predire <- "resultat_candida_def"
prop_train <- 0.7
graine <- 42
fichier_split <- "models/split.rds"
if (!exists("refaire_split")) refaire_split <- FALSE

# Variables jamais utilisées comme prédicteurs (identifiants, dates, informations postérieures à
# l'hémoculture). any_of() : ignorées si déjà absentes du jeu imputé.
variables_exclues <- c(
  "iep", "n_sejour", "groupehc", "date_adm_hospit", "date_adm_rea", "date_hemoc",
  "date_sortie_rea", "date_deces", "date_candidemie", "deces_rea", "duree_hospit"
)

if (file.exists(fichier_split) && !refaire_split) {
  .split <- readRDS(fichier_split)
  df_train <- .split$df_train
  df_test <- .split$df_test
} else {
  .df_tot <- readRDS("donnees/df_impute_ml.rds")
  df <- complete(.df_tot, 1) |>
    select(-c(demo_atcd_diabete, demo_atcd_tumeur)) |>
    select(-any_of(setdiff(variables_exclues, "iep"))) |>
    mutate(
      resultat_candida_def = as.factor(resultat_candida_def),
      across(where(is.character), as.factor),
      across(where(\(x) inherits(x, "difftime")), as.numeric)
    )

  # Strate au niveau patient : au moins une candidémie au cours du suivi
  .strate <- df |>
    group_by(iep) |>
    mutate(.patient_candida = any(resultat_candida_def == levels(resultat_candida_def)[2])) |>
    ungroup()

  set.seed(graine)
  split <- group_initial_split(.strate, group = iep, prop = prop_train, strata = .patient_candida)
  df_train <- training(split) |> select(-.patient_candida)
  df_test <- testing(split) |> select(-.patient_candida)

  dir.create(dirname(fichier_split), showWarnings = FALSE)
  saveRDS(list(df_train = df_train, df_test = df_test), fichier_split)
}

niv <- levels(df_train[[variable_a_predire]]) # niv[2] = événement modélisé ("Positive")
variables_predictives <- setdiff(names(df_train), c(variable_a_predire, variables_exclues))

message(sprintf(
  "[split] train : %d épisodes (%d candidémies) | test : %d épisodes (%d candidémies)",
  nrow(df_train), sum(df_train[[variable_a_predire]] == niv[2]),
  nrow(df_test), sum(df_test[[variable_a_predire]] == niv[2])
))
