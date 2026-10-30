####################################################################################################
#                                        METRICS                                                   #
####################################################################################################
# Performances de tous les modèles enregistrés dans models/ sur le même jeu de test.
# Objets créés :
#   - rocs      : liste nommée des objets pROC (réutilisée par 4_roc.R)
#   - tbl_perf  : tibble des métriques (valeurs numériques)
#   - gt_perf   : tableau gt mis en forme pour le Quarto

library(tidyverse)
library(pROC)
library(gt)

source("scripts/_ml_utils.R")

preds <- charger_predictions()
rocs <- map(preds, roc_modele)

# --------------------------------------------------------------------------------------------------
# Tableau des métriques (seuil de Youden pour sensibilité, spécificité, VPP, VPN, F1)
# --------------------------------------------------------------------------------------------------
tbl_perf <- imap(rocs, \(r, id) metriques_modele(r) |> mutate(id = id, .before = 1)) |>
  list_rbind() |>
  left_join(select(modeles_ref, id, label), by = "id") |>
  relocate(label, .after = id)

print(tbl_perf, width = Inf)

# --------------------------------------------------------------------------------------------------
# Mise en forme
# --------------------------------------------------------------------------------------------------
.pct <- \(x) format(round(100 * x, 1), nsmall = 1, decimal.mark = ",")
.dec <- \(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",")

gt_perf <- tbl_perf |>
  transmute(
    Modèle = label,
    `AUC (IC 95 %)` = sprintf("%s (%s – %s)", .dec(auc), .dec(auc_inf), .dec(auc_sup)),
    `Seuil` = .pct(seuil),
    `Sensibilité (%)` = .pct(sensibilite),
    `Spécificité (%)` = .pct(specificite),
    `VPP (%)` = .pct(vpp),
    `VPN (%)` = .pct(vpn),
    `F1` = .dec(f1)
  ) |>
  gt() |>
  tab_footnote(
    "Seuil, sensibilité, spécificité, VPP, VPN et F1 au seuil maximisant l'indice de Youden ; IC de l'AUC par la méthode de DeLong."
  )
