#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-regroupement
#| fig-cap: "Regroupement des hémocultures par épisode de suspicion d'infection"
#| lightbox: true
#| fig-align: center
knitr::include_graphics("figures/regroupement.png")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-flowchart
#| fig-cap: "Flow chart"
#| lightbox: true
#| fig-align: center
knitr::include_graphics("figures/flow_chart.png")
#
#
#
#
#
#
#
#
#
#| label: tbl-demographique
#| message: false
#| warning: false
#| code-fold: true
#| tbl-cap: "Description démographique de la population, données clinico-biologiques à l'admission en soins intensifs/réanimation"
#| lightbox: true
source("scripts/brutes/_setup.R")
source("scripts/brutes/tbl1_demo.R")

tbl1_gt <- tbl1 |>
  as_gt()
tbl1_gt
#
#
#
#
#
#
#


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-NA
#| fig-cap: "Données manquantes"
#| lightbox: true
source("scripts/supplementary/fig_NA.R")
figNA
#
#
#
#
#
#
#
#
#
#| label: tbl-bv
#| tbl-cap: "Analyse bivariée de la survenue d'une candidémie"
#| lightbox: true
source("scripts/brutes/tbl2_bv.R")

tbl2
#
#
#
#
#
#
#
#| label: stepwise_imp_1_plot
#| fig-cap: "Forrest plots, AUC & Courbes de calibration des modélisations stepwise forward & backward après imputation simple"
#| lightbox: true
#| fig-subcap:
#| - "Forrest Plot de la régression forward"
#| - "Forrest Plot de la régression backward"
#| - "ROC curve de la régression forward"
#| - "ROC curve de la régression backward"
#| - "Courbe de calibration de la régression forward"
#| - "Courbe de calibration de la régression backward"
#| layout-ncol: 2
#| fig-align: center
knitr::include_graphics("figures/FP_fwd.png")
knitr::include_graphics("figures/FP_bwd.png")
knitr::include_graphics("figures/ROC_fwd.png")
knitr::include_graphics("figures/ROC_bwd.png")
knitr::include_graphics("figures/CC_fwd.png")
knitr::include_graphics("figures/CC_bwd.png")
#
#
#
#
#
#| label: kaplan-meier
#| fig-cap: "Courbe Kaplan Meier de la mortalité en réanimation selon la présence ou non d'une candidémie"
#| lightbox: true
#| fig-align: center

source("scripts/survie/mod_cox.R")

fig_km
#
#
#
#
#
#| label: fine-gray
#| fig-cap: "Modèle FG"
#| lightbox: true
#| layout-ncol: 2

source("scripts/survie/mod_fine_gray.R")

fig_fp_fg
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
