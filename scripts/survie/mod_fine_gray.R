# ==============================================================================
#
#                            MODELE FINE GRAY
#
#===============================================================================
library(tidyverse)
library(riskRegression)
# library(prodlim)
library(survival)
library(tidycmprsk)
library(broom)
set.seed(142)
source("scripts/survie/_setup_survie.R")

# ------------------------------------------------------------------------------
#                           MODELISATION UNIVARIEE
# ------------------------------------------------------------------------------
covariables <- setdiff(names(df_fg), c("iep", "temps", "outcome"))

models_uni <- lapply(covariables, function(x) {
  tryCatch(
    tidycmprsk::crr(
      data = df_fg,
      Surv(temps, outcome),
      covariables = x
    ),
    error = function(e) NULL
  )
}) %>%
  compact()

tbl_uni <- tbl_uvregression(
  y = Surv(temps, outcome),
  data = df_fg,
  method = "crr",
  exponentiate = TRUE
) %>%
  add_n(location = "level") %>%
  bold_labels()
tbl_uni |>
  as_gt() |>
  gtsave("tbl_fg_uv.docx")

# ------------------------------------------------------------------------------
#                              SELECTION STEPWISE
# ------------------------------------------------------------------------------
# library(survival)
# df_fg$outcome_cat <- as.factor(df_fg$outcome_cat)
# df_long <- finegray(Surv(temps, outcome_cat) ~ ., data = df_fg)
# # Liste des variables prédictives candidates (à adapter selon votre dataset)
# # Exemple : c("age", "sexe", "bmi", "traitement")
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
#     "hc_glucanes_max",
#     "hc_mannanes_max"
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
    demo_atcd_diabete +
    demo_atcd_hemato +
    demo_type_rea +
    adm_igs2 +
    # adm_dialyse +
    hc_temp_max +
    hc_leuco_min +
    hc_choc +
    # hc_creat_max +
    # hc_uree_max +
    hc_dialyse +
    hc_vi_cat +
    hc_catheter_majeur +
    hc_transfu +
    hospit_parenterale_duree +
    hospit_ctc_duree +
    # hospit_vi_duree +
    # hospit_fibro +
    hospit_cgr,
  data = df_fg
)

fig_cuminc_fg <- ggcuminc(
  model_fg,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Temps (jours)",
  ylab = "Incidence cumulée",
  ggtheme = theme_classical(),
  palette = c("#E7B800", "#2E9FDF", "#FC4E07"),
  risk.table = TRUE
) +
  labs(title = "Courbes d'incidence cumulée par outcome")
# saveRDS(model_fg, "models/model_fg.rds")

tbl_fg <- model_fg %>%
  gtsummary::tbl_regression(
    exponentiate = TRUE,
    label = list(
      outcome = "Candidémie vs Décès",
      demo_age = "Âge",
      demo_sexe = "Sexe",
      nb_hemocultures = "Nombre d'hémocultures"
      # Ajoute les labels pour les autres variables si besoin
    )
  ) %>%
  add_n(location = "level")

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
) |>
  mutate(term = factor(term, labels = labels))

# Forest plot (identique à l'exemple précédent)
fig_fp_fg <- ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Adjusted Hazard Ratio (aHR)", y = "Covariable") +
  theme_classic()

# ------------------------------------------------------------------------------
#                     ESTIMATION AUC POUR PREDICTION CANDIDEMIE
# ------------------------------------------------------------------------------
cause_interet <- levels(df_fg$outcome_cat)[
  grepl("candid", levels(df_fg$outcome_cat), ignore.case = TRUE)
]
times_interest <- c(28, 60, 90)

model_fg_rr <- FGR(
  Hist(temps, outcome_cat) ~ demo_age +
    demo_atcd_hemato +
    adm_choc +
    adm_igs2 +
    adm_diurese_norm +
    adm_lactates_max +
    adm_transfu +
    adm_amines +
    hc_choc +
    demo_atcd_diabete +
    hc_dialyse +
    hc_transfu +
    hc_vi_cat +
    hc_catheter_majeur +
    hospit_parenterale_duree +
    hospit_vvc_duree +
    hospit_neutropen_duree +
    hospit_lymphopenie_duree +
    hospit_atb_duree +
    hc_deficit_lympho +
    hc_deficit_neutro +
    hospit_chirurgie_majeure,
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
