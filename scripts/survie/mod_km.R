#===================================================================================================
#
#                                        SURVIE_KM
#
#===================================================================================================
library(tidyverse)
library(survival)
library(ggsurvfit)
library(survminer)

if (!exists("df_base")) {
  source("scripts/brutes/_setup.R")
}

df_cox <- df_base |>
  arrange(iep, date_hemoc) |>
  distinct(iep, .keep_all = TRUE) |>
  group_by(iep) |>
  mutate(
    outcome_km = ifelse(deces_rea == "Oui", 1, 0),
    temps = min(
      as.numeric(coalesce(date_deces, date_sortie_rea) - date_adm_rea),
      na.rm = TRUE
    )
  ) |>
  ungroup()

#===================================================================================================
#                                  COURBE KAPLAN MEIER + LOG RANK
#================================================================s===================================
fit <- survfit(Surv(temps, outcome_km) ~ resultat_candida_def, data = df_cox)

fig_km <-
  ggsurvplot(
    fit,
    palette = c("#2B56A8", "#E97027"),
    conf.int = TRUE,
    risk.table = TRUE,
    pval = TRUE,
    risk.table.col = "strata",
    ncensor.plot = TRUE,
    ncensor.plot.height = 0.25,
    legend = "none",
    risk.table.legend = TRUE,
    legend.labs = c("Absence de Candidémie", "Candidémie"),
    ggtheme = theme_classic(),
    xlim = c(0, 60),
    break.time.by = 10,
    # conf.int.style = "step",
    surv.median.line = "hv",
    xlab = "Temps (jours)",
    ylab = "Probabilité de survie",
    title = "Courbe de survie par groupe",
    legend.title = "Groupe",
    risk.table.title = "Nombre à risque",
    pval.coord = c(5, 0.2),
    pval.size = 4,
    ncensor.plot.title = "Nombre de censures"
  )

fig_km$plot <- fig_km$plot +
  annotate(
    "text",
    x = 55,
    y = 0.36,
    label = "Candidémie",
    color = "#E97027",
    size = 2,
    fontface = "bold",
    hjust = 0
  ) +
  annotate(
    "text",
    x = 55,
    y = 0.55,
    label = "Absence de Candidémie",
    color = "#2B56A8",
    size = 2,
    fontface = "bold",
    hjust = 0
  )
