# ==================================================================================================
#                      SCORE (REGRESSION LOGISTIQUE MIXTE) — MODELE DE REFERENCE
# ==================================================================================================
# Applique au jeu de test l'équation du score présenté dans le mémoire (effets fixes du modèle
# logistique mixte), pour le comparer aux modèles d'apprentissage sur les mêmes épisodes.
# ! Le score a été développé sur l'ensemble de la cohorte (test inclus) : son AUC sur df_test est
# ! donc optimiste par rapport à celle des autres modèles, entraînés sur df_train uniquement.
# Sortie : models/pred_score.rds

library(tidyverse)

if (!exists("df_train")) source("scripts/1_splits.R")
source("scripts/_ml_utils.R")

.vars_score <- c(
  "hc_vi_cat", "hc_transfu", "hc_dialyse", "hospit_ctc_duree", "adm_igs2", "hc_vvc",
  "hospit_cgr", "hospit_chirurgie_abdominale"
)
.manquantes <- setdiff(.vars_score, names(df_test))
if (length(.manquantes) > 0) {
  stop("Variables du score absentes du jeu de test : ", paste(.manquantes, collapse = ", "))
}

pred_score <- df_test |>
  mutate(
    s = -4.99 +
      0.80 * (hc_vi_cat == "Oui") +
      0.88 * (hc_transfu == "Oui") +
      0.56 * (hc_dialyse == "Oui") +
      0.009 * hospit_ctc_duree +
      0.009 * adm_igs2 +
      0.62 * (hc_vvc == "Oui") +
      0.019 * hospit_cgr +
      0.35 * (hospit_chirurgie_abdominale == "Oui"),
    proba = plogis(s)
  ) |>
  pull(proba)

sauver_predictions("score", pred_score)
