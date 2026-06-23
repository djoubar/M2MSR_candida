# ==============================================================================
#
#                            MODELE DE COX
#
# ==============================================================================
library(tidyverse)
library(survival)
library(broom)
library(gtsummary)
library(timeROC) # Pour l'AUC time-dependent
library(rms) # Pour la calibration (calibrate())
library(ggplot2)
library(gt)

set.seed(142)

if (!exists("df_base")) {
  source("scripts/brutes/_setup.R")
}


# ==============================================================================
# 1. MODELISATION UNIVARIEE
# ==============================================================================
covariables <- setdiff(names(df_fg), c("iep", "temps", "outcome", "outcome_cat"))

# Modèles univariés (1 variable à la fois)
models_uni_cox <- lapply(covariables, function(x) {
  tryCatch(
    coxph(
      Surv(temps, outcome_cox) ~ get(x), # 'outcome' doit être binaire (0/1)
      data = df_fg
    ),
    error = function(e) NULL
  )
}) %>%
  compact()

# Tableau des résultats univariés
tbl_uni_cox <- tbl_uvregression(
  y = Surv(temps, outcome_cox),
  data = df_fg,
  method = "coxph",
  exponentiate = TRUE
) %>%
  add_n(location = "level") %>%
  bold_labels()

tbl_uni_cox %>%
  as_gt() %>%
  gtsave("tbl_cox_uv.docx")

# ==============================================================================
# 2. SELECTION STEPWISE (Forward/Backward basée sur l'AIC)
# ==============================================================================
# Variables candidates (exclure les variables non pertinentes)
variables_candidates <- setdiff(
  names(df_fg),
  c(
    "iep",
    "temps",
    "outcome",
    "outcome_cat",
    "outcome_cox",
    "demo_uf",
    "demo_type_rea",
    "resultat_candida_def"
  )
)

# Formule complète
formule_complete <- as.formula(paste(
  "Surv(temps, outcome_cox) ~",
  paste(variables_candidates, collapse = " + ")
))

# Modèle de base (intercept seul)
model_base_cox <- coxph(Surv(temps, outcome_cox) ~ 1, data = df_fg)

# Modèle complet
model_complet_cox <- coxph(formule_complete, data = df_fg)

# Sélection forward
model_final_cox <- step(
  model_base_cox,
  scope = list(lower = model_base_cox, upper = model_complet_cox),
  direction = "forward"
)

summary(model_final_cox)

# ==============================================================================
# 3. MODELE MULTIVARIE FINAL (à adapter selon la sélection)
# ==============================================================================
model_cox <- coxph(
  Surv(temps, outcome_cox) ~
    demo_atcd_diabete +
    demo_atcd_hemato +
    demo_type_rea +
    adm_igs2 +
    hc_temp_max +
    hc_leuco_min +
    hc_choc +
    hc_dialyse +
    hc_vi_cat +
    hc_catheter_majeur +
    hc_transfu +
    hospit_parenterale_duree +
    hospit_ctc_duree +
    hospit_cgr,
  data = df_fg
)

# Tableau des résultats multivariés
tbl_cox <- model_cox %>%
  tbl_regression(
    exponentiate = TRUE,
    # label = list(
    #   outcome_cox = "Événement (ex: Décès)",
    #   demo_atcd_diabete = "Antécédent de diabète",
    #   demo_atcd_hemato = "Antécédent de mhm",
    #   demo_type_rea = "Type de réanimation",
    #   adm_igs2 = "IGS2",
    #   hc_temp_max = "Temp max",
    #   hc_leuco_min = "Leuco min",
    #   hc_choc = "Choc à l'hc",
    #   hc_dialyse = "Dialyse à l'hc",
    #   hc_vi_cat = "VI à l'hc",
    #   hc_catheter_majeur = "KT",
    #   hc_transfu = "Transfusion à l'hc",
    #   hospit_parenterale_duree = "Durée parentérale",
    #   hospit_ctc_duree = "Durée CTC",
    #   hospit_cgr = "Nombre de CGR",
    # )
  ) %>%
  add_n(location = "level")

tbl_cox %>%
  as_gt() %>%
  gtsave("tbl_cox_multivarie.docx")

# Forest plot
tidy_model_cox <- tidy(
  model_cox,
  conf.int = TRUE,
  exponentiate = TRUE,
  term = fct_reorder(term, estimate, .desc = FALSE)
)

labels <- c(
  adm_igs2 = "Score IGS2 à l'admission",
  hc_dialyse = "Dialyse pendant l'hémoculture",
  hc_vi_cat = "Ventilation invasive",
  hospit_parenterale_duree = "Durée nutrition parentérale (jours)"
)

tidy_model_cox <- tidy_model_cox %>%
  mutate(term = factor(term, labels = labels))

fig_fp_cox <- ggplot(tidy_model_cox, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    x = "Hazard Ratio ajusté (aHR)",
    y = "Covariable",
    title = "Forest Plot - Modèle de Cox"
  ) +
  theme_classic()

# ------------------------------------------------------------------------------
# 4. ESTIMATION AUC TIME-DEPENDENT (avec timeROC)
# ------------------------------------------------------------------------------
# Prédictions du risque à différents temps
times_interest <- c(28, 60, 90)

# Fonction pour calculer l'AUC à un temps donné
get_auc_timeROC <- function(model, data, time) {
  # Calcul des scores de risque (linéaire prédit)
  risk_scores <- predict(model, newdata = data, type = "risk")

  # Création de l'objet timeROC
  roc_obj <- timeROC(
    T = data$temps, # Temps de suivi
    E = data$outcome_cox, # Événement (0/1)
    X = risk_scores, # Scores prédits
    timepoint = time # Temps d'intérêt
  )
  auc(roc_obj) # Retourne l'AUC à ce temps
}

# Calcul de l'AUC pour chaque temps
auc_results_cox <- map_df(
  times_interest,
  ~ {
    auc_val <- get_auc_timeROC(model_cox, df_fg, .x)
    tibble(
      time = .x,
      AUC = auc_val$AUC,
      lower = auc_val$AUC.lower,
      upper = auc_val$AUC.upper
    )
  }
)

# Affichage
auc_results_cox %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  knitr::kable(caption = "AUC time-dependent - Modèle de Cox")

# ------------------------------------------------------------------------------
# 5. COURBE DE CALIBRATION (avec rms::calibrate)
# ------------------------------------------------------------------------------
# Préparation des données pour rms
dd_cox <- datadist(df_fg)
options(datadist = "dd_cox")

# Ajustement du modèle avec rms (pour utiliser calibrate())
fit_cox_rms <- cph(
  Surv(temps, outcome_cox) ~
    demo_atcd_diabete +
    demo_atcd_hemato +
    demo_type_rea +
    adm_igs2 +
    hc_temp_max +
    hc_leuco_min +
    hc_choc +
    hc_dialyse +
    hc_vi_cat +
    hc_catheter_majeur +
    hc_transfu +
    hospit_parenterale_duree +
    hospit_ctc_duree +
    hospit_cgr,
  data = df_fg,
  x = TRUE,
  y = TRUE
)

# Calibration à 28, 60 et 90 jours
cal_cox_28 <- calibrate(fit_cox_rms, B = 100, u = 28, cmethod = "Kaplan-Meier")
cal_cox_60 <- calibrate(fit_cox_rms, B = 100, u = 60, cmethod = "Kaplan-Meier")
cal_cox_90 <- calibrate(fit_cox_rms, B = 100, u = 90, cmethod = "Kaplan-Meier")

# Plot des courbes de calibration
plot(
  cal_cox_28,
  main = "Courbe de calibration - J28",
  xlab = "Risque prédit",
  ylab = "Survie observée"
)
plot(
  cal_cox_60,
  main = "Courbe de calibration - J60",
  xlab = "Risque prédit",
  ylab = "Survie observée"
)
plot(
  cal_cox_90,
  main = "Courbe de calibration - J90",
  xlab = "Risque prédit",
  ylab = "Survie observée"
)

# Version ggplot (optionnelle)
ggcal_cox <- function(cal_obj, time_point) {
  cal_df <- data.frame(
    predicted = cal_obj$predicted,
    observed = cal_obj$observed,
    lower = cal_obj$lower,
    upper = cal_obj$upper
  ) %>%
    na.omit()

  ggplot(cal_df, aes(x = predicted, y = observed)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(color = "#2C7BB6", size = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, color = "#2C7BB6") +
    geom_smooth(method = "loess", se = TRUE, color = "#D7191C", fill = "#fdae61", alpha = 0.2) +
    labs(
      title = paste0("Courbe de calibration - J", time_point),
      x = "Risque prédit (1 - Survie)",
      y = "Survie observée (Kaplan-Meier)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

ggcal_cox(cal_cox_28, 28)
ggcal_cox(cal_cox_60, 60)
ggcal_cox(cal_cox_90, 90)
