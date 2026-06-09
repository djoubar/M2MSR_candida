source("scripts/survie/_setup_survie.R")

#===============================================================================
#                                MODELE_UV
#===============================================================================

# Sélection des covariables (toutes sauf temps et outcome)
# covariates <- setdiff(names(df_fg), c("iep", "temps", "outcome"))

# models_uni <- lapply(covariates, function(x) {
#   tryCatch(
#     tidycmprsk::crr(
#       data = df_fg,
#       Surv(temps, outcome),
#       covariates = x
#     ),
#     error = function(e) NULL
#   )
# }) %>%
#   compact() # Supprime les NULL

# # Tableau univarié
# tbl_uni <- tbl_uvregression(
#   y = Surv(temps, outcome),
#   data = df_fg,
#   method = "crr", # Méthode pour Fine-Gray
#   exponentiate = TRUE
# ) %>%
#   add_n(location = "level") %>%
#   bold_labels()
# tbl_uni |>
#   as_gt() |>
#   gtsave("tbl_fg_uv.docx")

#==============================================================================#
#                                 MODELE_MV                                    #
#==============================================================================#

model_fg <- crr(
  Surv(temps, outcome_cat) ~
    demo_age +
    hc_dialyse +
    hc_vi_cat +
    hc_catheter_majeur +
    hc_amines +
    # hc_deficit_neutro +
    hospit_parenterale_duree +
    hospit_ctc_duree +
    hospit_fibro +
    # hospit_immunosup_duree +
    # hospit_neutrophi_duree +
    # hospit_chirurgie_abdominale +
    hospit_cgr,
  data = df_fg
)

# fig_cuminc_fg <- ggcuminc(
#   model_fg,
#   # Affiche toutes les courbes
#   pval = TRUE, # Test de comparaison
#   conf.int = TRUE, # Intervalles de confiance
#   xlab = "Temps (jours)", # Personnalisation
#   ylab = "Incidence cumulée",
#   ggtheme = theme_minimal(), # Thème ggplot2
#   palette = c("#E7B800", "#2E9FDF", "#FC4E07"), # Couleurs pour 3 outcomes
#   risk.table = TRUE # Tableau de risque
# ) +
#   labs(title = "Courbes d'incidence cumulée par outcome")

# saveRDS(model_fg, "models/model_fg.rds")

# Tableau des résultats (exponentié = HR)
# tbl <- model_fg %>%
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

# 1. Créer un vecteur de labels
labels <- c(
  adm_igs2 = "Score IGS2 à l'admission",
  # demo_type_rea = "Réanimation médicale",
  # demo_atcd_diabete = "Antécédent de diabète",
  hc_dialyse = "Dialyse pendant l'hémoculture",
  hc_cgr = "CGR avant l'hémoculture",
  hc_vi_cat = "Ventilation invasive à l'hémoculture",
  hospit_parenterale_duree = "Durée de nutrition parentérale (jours)",
  hospit_vvc_duree = "Durée de ventilation mécanique (jours)",
  hospit_atb_duree = "Durée d'antibiothérapie (jours)",
  # hc_deficit_lympho = "Déficit lymphocytaire",
  hc_deficit_neutro = "Déficit en neutrophiles",
  hospit_chirurgie_abdominale = "Chirurgie abdominale"
  # hospit_chirurgie_majeure = "Chirurgie majeure"
)

# 2. Appliquer les labels dans tidy_model
tidy_model <- tidy(
  model_fg,
  conf.int = TRUE,
  exponentiate = TRUE,
  term = fct_reorder(term, estimate, .desc = FALSE)
)
# %>%
# mutate(term = factor(term, labels = labels))

# Forest plot (identique à l'exemple précédent)
fig_fp_fg <- ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Adjusted Hazard Ratio (aHR)", y = "Covariable") +
  theme_classic()

# #==================================================================================================
# times_interest <- c(28, 60, 90)

# model_fg_rr <- FGR(
#   Surv(temps, outcome_cat) ~ demo_age +
#     demo_type_rea +
#     adm_lactates_max +
#     hc_vi_cat +
#     hc_transfu +
#     hc_uree_max +
#     hc_dialyse +
#     hc_amines +
#     hc_choc +
#     hc_diurese_norm +
#     hc_vvc +
#     hc_cp +
#     hc_delai +
#     hospit_ctc_duree +
#     hospit_immunosup_duree +
#     hospit_cgr +
#     hospit_parenterale_duree +
#     hospit_chirurgie_majeure +
#     hospit_vvc_duree,
#   data = df_fg,
#   cause = "Candidémie"
# )

# # model_fg_rr <- FGR(
# #   Hist(temps, outcome_cat) ~ demo_age +
# #     demo_atcd_hemato +
# #     adm_choc +
# #     adm_igs2 +
# #     adm_diurese_norm +
# #     adm_lactates_max +
# #     adm_transfu +
# #     adm_amines +
# #     hc_choc +
# #     demo_atcd_diabete +
# #     hc_dialyse +
# #     hc_transfu +
# #     hc_vi_cat +
# #     hc_catheter_majeur +
# #     hc_transfu +
# #     hospit_parenterale_duree +
# #     hospit_vvc_duree +
# #     hospit_neutropen_duree +
# #     hospit_lymphopenie_duree +
# #     hospit_atb_duree +
# #     hc_deficit_lympho +
# #     hc_deficit_neutro +
# #     hospit_chirurgie_majeure,
# #   data = df_fg,
# #   cause = "Candidémie"
# # )

# # ── AUC via Score() ───────────────────────────────────────────────────────────
# set.seed(123)

# auc_results <- Score(
#   list("Fine-Gray" = model_fg_rr),
#   formula = Hist(temps, outcome_cat) ~ 1,
#   data = df_fg,
#   cause = "Candidémie",
#   times = times_interest,
#   metrics = c("auc", "brier"), # Brier score en bonus
#   plots = "calibration", # prépare les données pour la calibration
#   B = 100, # bootstrap pour les IC (augmenter à 200+ en prod)
#   split.method = "bootcv", # validation croisée bootstrap
#   seed = 123
# )

# # Afficher les résultats

# # ── Option A : via plotCalibration() de riskRegression (rapide) ──────────────
# plotCalibration(
#   auc_results,
#   times = 28, # choisir UN temps à la fois
#   cens.method = "jackknife",
#   method = "nne", # nearest-neighbor estimation
#   round = FALSE
# )

# # ── Option B : calibration manuelle avec ggplot2 (plus de contrôle) ──────────

# calibration_plot <- function(model, data, time_point, n_groups = 10) {
#   # Prédictions de l'incidence cumulée au temps t
#   preds <- predictRisk(model, newdata = data, times = time_point, cause = 1)

#   # Découper en déciles
#   data$pred <- as.vector(preds)
#   data$decile <- ntile(data$pred, n_groups)

#   # Incidence cumulée observée par groupe via Aalen-Johansen
#   obs_list <- lapply(1:n_groups, function(g) {
#     sub <- data[data$decile == g, ]
#     fit <- survfit(Surv(temps, outcome_cat) ~ 1, data = sub)
#     # Pour risques compétitifs, utiliser tidycmprsk::cuminc()
#     fit2 <- cuminc(Surv(temps, outcome_cat) ~ 1, data = sub)

#     # Extraire la probabilité au temps_point
#     tbl <- tidy(fit2) %>%
#       filter(outcome == "1", time <= time_point) %>% # cause 1
#       slice_tail(n = 1)

#     data.frame(
#       decile = g,
#       pred_mean = mean(sub$pred),
#       obs = ifelse(nrow(tbl) == 0, 0, tbl$estimate)
#     )
#   })

#   cal_df <- do.call(rbind, obs_list)

#   # ── Graphique ────────────────────────────────────────────────────────────────
#   ggplot(cal_df, aes(x = pred_mean, y = obs)) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
#     geom_point(size = 3, color = "#2C7BB6") +
#     geom_smooth(method = "loess", se = TRUE, color = "#D7191C", fill = "#fdae61", alpha = 0.2) +
#     scale_x_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
#     scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
#     labs(
#       title = paste0("Courbe de calibration — J", time_point),
#       subtitle = "Modèle de Fine & Gray — Candidémie",
#       x = "Risque prédit (incidence cumulée)",
#       y = "Incidence cumulée observée (Aalen-Johansen)",
#       caption = paste0(n_groups, " groupes de risque")
#     ) +
#     theme_minimal(base_size = 13) +
#     theme(plot.title = element_text(face = "bold"))
# }

# # Appel pour J28 et J60
# calibration_plot(model_fg_rr, df_fg, time_point = 28)
# calibration_plot(model_fg_rr, df_fg, time_point = 60)

# # AUC + Brier Score pour chaque temps d'intérêt
# summary(auc_results, what = c("AUC", "Brier"))

# # Format publication-ready
# auc_results$AUC$score %>%
#   select(model, times, AUC, lower, upper) %>%
#   mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
#   knitr::kable(caption = "AUC time-dependent — Modèle Fine & Gray")
