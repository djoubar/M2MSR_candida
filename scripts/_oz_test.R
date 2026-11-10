# ==================================================================================================
#                     SCORE D'OSTROSKY-ZEICHNER SUR LES PATIENTS DU JEU DE TEST
# ==================================================================================================
# Le score (règle binaire) est calculé dans scripts/_oz.R à partir de variables absentes du jeu
# imputé (os_atb, os_cvc, os_eer). Le découpage étant fait par patient, on restreint ici les
# épisodes aux patients (iep) du jeu de test pour une comparaison avec les autres modèles.
# Objets créés :
#   - df_oz_test : épisodes du jeu de test avec score_oz renseigné
#   - pred_oz    : prédictions au format de sauver_predictions(), ajoutées aux modèles par 3_metrics.R

if (!exists("df_test")) source("scripts/1_splits.R")
if (!exists("df_base") || !"score_oz" %in% names(df_base)) source("scripts/_oz.R")

df_oz_test <- df_base |>
  arrange(iep, groupehc, date_hemoc) |>
  group_by(iep, groupehc) |>
  slice(1) |> # un épisode par groupe d'hémocultures, comme dans _setup.R
  ungroup() |>
  filter(iep %in% df_test$iep, !is.na(score_oz), !is.na(resultat_candida_def))

.niv_test <- levels(df_test[[variable_a_predire]])

pred_oz <- list(
  id = "oz",
  proba = as.numeric(df_oz_test$score_oz == "OUI"),
  # recodage par position (Négative, Positive) vers les niveaux du jeu de test
  obs = factor(.niv_test[as.integer(df_oz_test$resultat_candida_def)], levels = .niv_test),
  date = Sys.time()
)

message(sprintf(
  "[oz] %d épisodes de test (%d candidémies) ; %d épisodes du jeu de test initial",
  length(pred_oz$obs), sum(pred_oz$obs == .niv_test[2]), nrow(df_test)
))
