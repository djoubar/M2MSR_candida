# ==============================================================================
#
#                            MODELE FINE GRAY
#
#===============================================================================
library(tidyverse)
library(riskRegression)
library(gtsummary)
library(survival)
library(cmprsk)
library(tidycmprsk)
library(broom)
library(gt)
library(ggsurvfit)
library(prodlim)

set.seed(142)

if (!exists("df_base")) {
  source("scripts/brutes/_setup.R")
}

if (!exists("df_fg")) {
  source("scripts/survie/_setup_survie.R")
}

# ==============================================================================
#                           MODELISATION UNIVARIEE
# ==============================================================================
# covariables <- setdiff(names(df_fg), c("iep", "temps", "outcome"))

# models_uni <- lapply(covariables, function(x) {
#   tryCatch(
#     tidycmprsk::crr(
#       data = df_fg,
#       Surv(temps, outcome),
#       covariables = x
#     ),
#     error = function(e) NULL
#   )
# }) %>%
#   compact()

# tbl_uni <- tbl_uvregression(
#   y = Surv(temps, outcome),
#   data = df_fg,
#   method = "crr",
#   exponentiate = TRUE
# ) %>%
#   add_n(location = "level") %>%
#   bold_labels()
# tbl_uni |>
#   as_gt() |>
#   gtsave("tbl_fg_uv.docx")

# ------------------------------------------------------------------------------
#                              SELECTION STEPWISE
# ------------------------------------------------------------------------------

# df_fg$outcome_cat <- as.factor(df_fg$outcome_cat)
# df_long <- finegray(Surv(temps, outcome_cat) ~ ., data = df_fg)
# variables_candidates <- setdiff(
#   names(df_fg),
#   c(
#     "iep",
#     "outcome",
#     "temps",
#     "demo_uf",
#     "demo_type_rea",
#     "outcome_cat",
#     "resultat_candida_def",
#     "outcome_cox",
#   )
# )

# # Formule pour le modèle complet
# formule_complete <- as.formula(paste(
#   "Surv(fgstart, fgstop, fgstatus) ~",
#   paste(variables_candidates, collapse = " + ")
# ))

# # 1. Modèle minimal (Intercept seul)
# model_base <- coxph(Surv(fgstart, fgstop, fgstatus) ~ 1, data = df_long, weights = fgwt) # 'id' est créé automatiquement par finegray()

# # 2. Modèle complet (Toutes les variables)
# model_complet <- coxph(formule_complete, data = df_long, weights = fgwt)
# # Sélection forward basée sur l'AIC
# model_final <- step(
#   model_base,
#   scope = list(lower = model_base, upper = model_complet),
#   direction = "forward"
# )

# # Afficher le résumé du modèle final sélectionné
# summary(model_final)

# ------------------------------------------------------------------------------
#                           MODELE MULTIVARIE
# ------------------------------------------------------------------------------
model_fg <- crr(
  Surv(temps, outcome_cat) ~
    hc_vi_cat +
    hc_cgr +
    hc_cp +
    hc_dialyse +
    hc_amines +
    hc_vvc +
    hospit_chirurgie_majeure +
    hospit_ctc_duree +
    hospit_immunosup_duree +
    demo_age +
    hc_amines +
    hc_hypothermie +
    hc_fievre +
    hospit_parenterale_duree +
    demo_type_rea,
  data = df_fg
)

# fig_cuminc_fg <- ggcuminc(
#   model_fg,
#   pval = TRUE,
#   conf.int = TRUE,
#   xlab = "Temps (jours)",
#   ylab = "Incidence cumulée",
#   ggtheme = theme_classical(),
#   palette = c("#E7B800", "#2E9FDF", "#FC4E07"),
#   risk.table = TRUE
# ) +
#   labs(title = "Courbes d'incidence cumulée par outcome")
# # saveRDS(model_fg, "models/model_fg.rds")

# tbl_fg <- model_fg %>%
#   gtsummary::tbl_regression(
#     exponentiate = TRUE,
#     label = list(
#       outcome = "Candidémie vs Décès",
#       demo_age = "Âge",
#       demo_sexe = "Sexe",
#       nb_hemocultures = "Nombre d'hémocultures"
#       # Ajoute les labels pour les autres variables si besoin
#     )
#   ) %>%
#   add_n(location = "level")

# labels <- c(
#   adm_igs2 = "Score IGS2 à l'admission",
#   # demo_type_rea = "Réanimation médicale",
#   # demo_atcd_diabete = "Antécédent de diabète",
#   hc_dialyse = "Dialyse pendant l'hémoculture",
#   hc_cgr = "CGR avant l'hémoculture",
#   hc_vi_cat = "Ventilation invasive à l'hémoculture",
#   hospit_parenterale_duree = "Durée de nutrition parentérale (jours)",
#   hospit_vvc_duree = "Durée de ventilation mécanique (jours)",
#   hospit_atb_duree = "Durée d'antibiothérapie (jours)",
#   # hc_deficit_lympho = "Déficit lymphocytaire",
#   hc_deficit_neutro = "Déficit en neutrophiles",
#   hospit_chirurgie_abdominale = "Chirurgie abdominale"
#   # hospit_chirurgie_majeure = "Chirurgie majeure"
# )

tidy_model <- tidy(
  model_fg,
  conf.int = TRUE,
  exponentiate = TRUE,
  term = fct_reorder(term, estimate, .desc = FALSE)
)


# Forest plot (identique à l'exemple précédent)
fig_fp_fg <- ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Adjusted Subdistribution Hazard Ratio (aSHR)", y = "Covariable") +
  theme_classic()

saveRDS(fig_fp_fg, file = "models/fp_fg.rds")
# ------------------------------------------------------------------------------
#                     ESTIMATION AUC POUR PREDICTION CANDIDEMIE
# ------------------------------------------------------------------------------
cause_interet <- levels(df_fg$outcome_cat)[
  grepl("candid", levels(df_fg$outcome_cat), ignore.case = TRUE)
]
times_interest <- c(28, 60, 90)

model_fg_rr <- FGR(
  Hist(temps, outcome_cat) ~
    hc_vi_cat +
    hc_cgr +
    hc_cp +
    hc_dialyse +
    hc_amines +
    hc_vvc +
    hospit_chirurgie_majeure +
    hospit_ctc_duree +
    hospit_immunosup_duree +
    demo_age +
    hc_amines +
    hc_hypothermie +
    hc_fievre +
    hospit_parenterale_duree +
    demo_type_rea,
  data = df_fg,
  cause = cause_interet
)

auc_results <- Score(
  list("Fine-Gray" = model_fg_rr),
  formula = Hist(temps, outcome_cat) ~ 1,
  data = df_fg,
  cause = cause_interet,
  times = times_interest,
  metrics = c("auc", "brier"),
  plots = "calibration",
  B = 100,
  split.method = "bootcv",
  seed = 123
)

summary(auc_results)

calibration_plot <- function(model, data, time_point, cause, n_groups = 10) {
  preds <- predictRisk(model, newdata = data, times = time_point, cause = cause)

  data$pred <- as.vector(preds)
  data$decile <- dplyr::ntile(data$pred, n_groups)

  obs_list <- lapply(1:n_groups, function(g) {
    sub <- data[data$decile == g, ]
    fit2 <- tidycmprsk::cuminc(Surv(temps, outcome_cat) ~ 1, data = sub)

    tbl <- broom::tidy(fit2) %>%
      dplyr::filter(outcome == cause, time <= time_point) %>%
      dplyr::slice_tail(n = 1)

    data.frame(
      decile = g,
      pred_mean = mean(sub$pred),
      obs = ifelse(nrow(tbl) == 0, NA, tbl$estimate)
    )
  })

  cal_df <- do.call(rbind, obs_list)

  ggplot(cal_df, aes(x = pred_mean, y = obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3, color = "#2C7BB6") +
    geom_smooth(method = "loess", se = TRUE, color = "#D7191C", fill = "#fdae61", alpha = 0.2) +
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    labs(
      title = paste0("Courbe de calibration - J", time_point),
      subtitle = paste0("Modele de Fine & Gray - ", cause),
      x = "Risque predit (incidence cumulee)",
      y = "Incidence cumulee observee (Aalen-Johansen)",
      caption = paste0(n_groups, " groupes de risque")
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

calibration_plot(model_fg_rr, df_fg, time_point = 28, cause = cause_interet)
calibration_plot(model_fg_rr, df_fg, time_point = 60, cause = cause_interet)

auc_results$AUC$score %>%
  select(model, times, AUC, lower, upper) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  knitr::kable(caption = "AUC time-dependent - Modele Fine & Gray")
