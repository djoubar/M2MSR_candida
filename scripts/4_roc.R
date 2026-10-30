####################################################################################################
#                                  COURBES ROC COMPAREES                                           #
####################################################################################################
# Superpose les courbes ROC de tous les modèles évalués sur le même jeu de test et compare leurs AUC
# (test de DeLong apparié) au modèle de référence.
# Objets créés :
#   - plot_roc    : ggplot des courbes ROC (enregistré dans figures/roc_comparaison.png)
#   - tbl_delong  : tibble des comparaisons d'AUC
#   - gt_delong   : tableau gt mis en forme pour le Quarto

library(tidyverse)
library(pROC)
library(gt)

if (!exists("rocs") || !exists("tbl_perf")) source("scripts/3_metrics.R")

# Référence : le score s'il a été calculé, sinon le modèle de meilleure AUC
id_ref <- if ("score" %in% names(rocs)) "score" else tbl_perf$id[which.max(tbl_perf$auc)]

# --------------------------------------------------------------------------------------------------
# Données des courbes
# --------------------------------------------------------------------------------------------------
.legende <- tbl_perf |>
  mutate(
    legende = sprintf(
      "%s : AUC %s (%s – %s)",
      label,
      format(round(auc, 2), nsmall = 2, decimal.mark = ","),
      format(round(auc_inf, 2), nsmall = 2, decimal.mark = ","),
      format(round(auc_sup, 2), nsmall = 2, decimal.mark = ",")
    )
  ) |>
  left_join(select(modeles_ref, id, couleur, trait), by = "id")

roc_df <- imap(rocs, \(r, id) {
  tibble(id = id, fpr = 1 - r$specificities, tpr = r$sensitivities) |>
    arrange(fpr, tpr)
}) |>
  list_rbind() |>
  mutate(id = factor(id, levels = .legende$id))

# --------------------------------------------------------------------------------------------------
# Graphique
# --------------------------------------------------------------------------------------------------
plot_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, colour = id, linetype = id)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_path(linewidth = 0.8) +
  scale_colour_manual(
    values = set_names(.legende$couleur, .legende$id),
    labels = set_names(.legende$legende, .legende$id),
    name = NULL
  ) +
  scale_linetype_manual(
    values = set_names(.legende$trait, .legende$id),
    labels = set_names(.legende$legende, .legende$id),
    name = NULL
  ) +
  scale_x_continuous(expand = expansion(0.01), labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(expand = expansion(0.01), labels = scales::label_number(decimal.mark = ",")) +
  coord_equal() +
  labs(x = "1 - Spécificité", y = "Sensibilité") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.key.width = unit(1.6, "lines"),
    legend.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.3)
  )

dir.create(dossier_figures, showWarnings = FALSE)
ggsave(
  file.path(dossier_figures, "roc_comparaison.png"),
  plot_roc,
  width = 7,
  height = 7,
  dpi = 300,
  bg = "white"
)

# --------------------------------------------------------------------------------------------------
# Comparaison des AUC au modèle de référence (DeLong, apparié : mêmes épisodes de test)
# --------------------------------------------------------------------------------------------------
tbl_delong <- setdiff(names(rocs), id_ref) |>
  map(\(id) {
    t <- roc.test(rocs[[id_ref]], rocs[[id]], method = "delong", paired = TRUE)
    tibble(
      id = id,
      diff_auc = as.numeric(auc(rocs[[id]]) - auc(rocs[[id_ref]])),
      p = t$p.value
    )
  }) |>
  list_rbind() |>
  left_join(select(modeles_ref, id, label), by = "id")

gt_delong <- tbl_delong |>
  transmute(
    Modèle = label,
    `Différence d'AUC` = format(round(diff_auc, 3), nsmall = 3, decimal.mark = ","),
    `p (DeLong)` = format.pval(p, digits = 2, eps = 0.001, decimal.mark = ",")
  ) |>
  gt() |>
  tab_footnote(paste0(
    "Différence d'AUC par rapport au modèle de référence : ",
    modeles_ref$label[modeles_ref$id == id_ref],
    "."
  ))

plot_roc
