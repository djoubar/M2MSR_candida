# ==================================================================================================
#                          FONCTIONS COMMUNES AUX MODELES D'APPRENTISSAGE
# ==================================================================================================
# Chaque script 2_mod_*.R enregistre ses probabilités prédites sur df_test dans
# models/pred_<id>.rds via sauver_predictions(). Les scripts 3_metrics.R et 4_roc.R relisent
# ces fichiers : aucun modèle n'est ré-entraîné au rendu du Quarto.

library(tidyverse)
library(pROC)

dossier_modeles <- "models"
dossier_figures <- "figures"

# Ordre et couleur fixes par modèle (la couleur suit le modèle, quel que soit le nombre affiché)
modeles_ref <- tribble(
  ~id      , ~label                          , ~couleur  , ~trait    ,
  "score"  , "Score (régression logistique)" , "#2a78d6" , "solid"   ,
  "lasso"  , "Lasso"                         , "#eb6834" , "dashed"  ,
  "ridge"  , "Ridge (variables Lasso)"       , "#1baf7a" , "dotdash" ,
  "rf"     , "Random Forest"                 , "#eda100" , "longdash",
  "xgboost", "XGBoost"                       , "#e87ba4" , "twodash"
)

# --------------------------------------------------------------------------------------------------
# Plis de validation croisée par patient (un patient n'est jamais à la fois en apprentissage et en
# validation), stratifiés sur la survenue d'au moins une candidémie chez le patient
# --------------------------------------------------------------------------------------------------
creer_plis_patient <- function(data, k = 5, graine = 42) {
  patients <- data |>
    group_by(iep) |>
    summarise(evt = any(.data[[variable_a_predire]] == niv[2]), .groups = "drop")

  set.seed(graine)
  patients <- patients |>
    group_by(evt) |>
    mutate(pli = sample(rep_len(seq_len(k), n()))) |>
    ungroup()

  patients$pli[match(data$iep, patients$iep)]
}

# --------------------------------------------------------------------------------------------------
# Enregistrement / chargement des prédictions sur le jeu de test
# --------------------------------------------------------------------------------------------------
sauver_predictions <- function(id, proba, obs = df_test[[variable_a_predire]]) {
  stopifnot(id %in% modeles_ref$id, length(proba) == length(obs))
  dir.create(dossier_modeles, showWarnings = FALSE)
  saveRDS(
    list(id = id, proba = as.vector(proba), obs = obs, date = Sys.time()),
    file.path(dossier_modeles, paste0("pred_", id, ".rds"))
  )
  message(sprintf("[%s] prédictions enregistrées (n = %d)", id, length(proba)))
}

charger_predictions <- function() {
  fichiers <- file.path(dossier_modeles, paste0("pred_", modeles_ref$id, ".rds"))
  fichiers <- fichiers[file.exists(fichiers)]
  if (length(fichiers) == 0) {
    stop("Aucune prédiction dans ", dossier_modeles, "/ : lancer d'abord scripts/0_run_ml.R")
  }
  preds <- map(fichiers, readRDS)
  names(preds) <- map_chr(preds, "id")

  # Tous les modèles doivent avoir été évalués sur le même jeu de test
  n_test <- map_int(preds, \(p) length(p$obs))
  if (n_distinct(n_test) > 1 || !all(map_lgl(preds, \(p) identical(p$obs, preds[[1]]$obs)))) {
    stop("Les modèles n'ont pas été évalués sur le même jeu de test : relancer scripts/0_run_ml.R")
  }
  preds
}

# --------------------------------------------------------------------------------------------------
# Métriques de performance d'un modèle
# --------------------------------------------------------------------------------------------------
roc_modele <- function(p) {
  roc(p$obs, p$proba, levels = levels(p$obs), direction = "<", quiet = TRUE)
}

metriques_modele <- function(roc_obj) {
  auc_ci <- ci.auc(roc_obj, method = "delong")
  youden <- coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"),
    transpose = FALSE
  )[1, ]

  tibble(
    auc = auc_ci[2],
    auc_inf = auc_ci[1],
    auc_sup = auc_ci[3],
    seuil = youden$threshold,
    sensibilite = youden$sensitivity,
    specificite = youden$specificity,
    vpp = youden$ppv,
    vpn = youden$npv,
    f1 = 2 * youden$ppv * youden$sensitivity / (youden$ppv + youden$sensitivity)
  )
}
