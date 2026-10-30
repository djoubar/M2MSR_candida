# ==================================================================================================
#                          PIPELINE D'APPRENTISSAGE : ENTRAINEMENT + EVALUATION
# ==================================================================================================
# A lancer depuis la racine du projet (une seule fois, ou après modification d'un modèle).
# Le Quarto ne fait ensuite que relire les résultats enregistrés dans models/ et figures/.
#
# Pour ne ré-entraîner qu'un modèle : source() le script 2_mod_*.R correspondant, puis 3 et 4.
# Pour regénérer le découpage train/test : refaire_split <- TRUE avant de sourcer 1_splits.R
# (tous les modèles doivent alors être ré-entraînés).

source("scripts/1_splits.R")

source("scripts/2_mod_score.R")
source("scripts/2_mod_penalisation.R")
source("scripts/2_mod_random_forrest.R")
source("scripts/2_mod_xgboost.R")

source("scripts/3_metrics.R")
source("scripts/4_roc.R")

gt_perf
gt_delong
plot_roc
