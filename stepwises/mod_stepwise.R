library(tidyverse)
library(gtsummary)
library(pROC)
library(lme4)
library(rms)
library(mice)
library(broom)
library(boot)
library(car)
library(CalibrationCurves)

imp <- read_rds("donnees/df_impute.rds")
if (!exists("df_base")) {
  source("scripts/brutes/_setup.R")
}

#===============================================================================
#                              CREATION DES MODELES
#===============================================================================
df_stepwise <- complete(imp, 11) %>%
  left_join(
    df_base %>%
      select(id_hemoc, date_adm_hospit, date_adm_rea, date_hemoc, date_sortie_rea, date_deces),
    by = "id_hemoc"
  ) %>%
  group_by(iep) %>%
  summarise(
    demo_centre = first(demo_centre),
    demo_type_rea = first(demo_type_rea),
    demo_age = first(demo_age),
    demo_sexe = first(demo_sexe),
    demo_atcd_hemato = first(demo_atcd_hemato),
    demo_atcd_diabete = first(demo_atcd_diabete),
    demo_atcd_pancreatite = first(demo_atcd_pancreatite),
    demo_atcd_tumeur = first(demo_atcd_tumeur),
    demo_atcd_transplantation = first(demo_atcd_transplantation),
    adm_choc = first(adm_choc),
    adm_igs2 = first(adm_igs2),
    adm_pancreatite_aigue = first(adm_pancreatite_aigue),
    adm_poids = first(adm_poids),
    adm_hypothermie = first(adm_hypothermie),
    adm_fievre = first(adm_fievre),
    adm_diurese_norm = first(adm_diurese_norm),
    adm_creat_max = first(adm_creat_max),
    adm_uree_max = first(adm_uree_max),
    adm_lactates_max = first(adm_lactates_max),
    adm_vi_cat = first(adm_vi_cat),
    adm_dialyse = first(adm_dialyse),
    adm_cgr = first(adm_cgr),
    adm_pfc = first(adm_pfc),
    adm_cp = first(adm_cp),
    adm_transfu = first(adm_transfu),
    adm_amines = first(adm_amines),
    hc_dialyse = last(hc_dialyse),
    hc_kta = last(hc_kta),
    hc_vvc = last(hc_vvc),
    hc_ktd = last(hc_ktd),
    hc_ecmo = last(hc_ecmo),
    hc_catheter_majeur = last(hc_catheter_majeur),
    hc_cgr = last(hc_cgr),
    hc_pfc = last(hc_pfc),
    hc_cp = last(hc_cp),
    hc_transfu = last(hc_transfu),
    hc_amines = last(hc_amines),
    hc_hypothermie = last(hc_hypothermie),
    hc_fievre = last(hc_fievre),
    hc_antifongique = last(hc_antifongique),
    hc_creat_max = last(hc_creat_max),
    hc_uree_max = last(hc_uree_max),
    hc_vi_cat = last(hc_vi_cat),
    hospit_vi_duree = last(hospit_vi_duree),
    hospit_parenterale_duree = last(hospit_parenterale_duree),
    hospit_vvc_duree = last(hospit_vvc_duree),
    hospit_kta_duree = last(hospit_kta_duree),
    hospit_ktd_duree = last(hospit_ktd_duree),
    hospit_ecmo_duree = last(hospit_ecmo_duree),
    hospit_atb_duree = last(hospit_atb_duree),
    hospit_ctc_duree = last(hospit_ctc_duree),
    hospit_immunosup_duree = last(hospit_immunosup_duree),
    hospit_neutropen_duree = last(hospit_neutropen_duree),
    hospit_neutrophi_duree = last(hospit_neutrophi_duree),
    hospit_cgr = last(hospit_cgr),
    hospit_pfc = last(hospit_pfc),
    hospit_cp = last(hospit_cp),
    hospit_fibro = last(hospit_fibro),
    hospit_chirurgie_majeure = last(hospit_chirurgie_majeure),
    hospit_chirurgie_abdominale = last(hospit_chirurgie_abdominale),
    hospit_chirurgie_hepatobiliaire = last(hospit_chirurgie_hepatobiliaire),
    hospit_chirurgie_susmesocolique = last(hospit_chirurgie_susmesocolique),
    nb_hemocultures = n(),
    resultat_candida_def = last(resultat_candida_def)
  ) %>%
  ungroup()

mod_intercept <- glm(resultat_candida_def ~ 1, data = df_stepwise, family = "binomial")
mod_tot <- glm(resultat_candida_def ~ ., data = df_stepwise, family = "binomial")

forward <- step(mod_intercept, direction = "forward", scope = formula(mod_tot))
backward <- step(mod_tot, direction = "backward", scope = formula(mod_tot))
both <- step(mod_intercept, direction = "both", scope = formula(mod_tot))

tbl_regression(forward, exponentiate = TRUE)
tbl_regression(backward, exponentiate = TRUE)
tbl_regression(both, exponentiate = TRUE)

saveRDS(forward, "models/stepwises/mod_imp11_fwd.RDS")
saveRDS(backward, "models/stepwises/mod_imp11_bwd.RDS")
saveRDS(both, "models/stepwises/mod_imp11_both.RDS")

# ==============================================================================
# Tidy models
# ==============================================================================

tidy_model_fwd <- tidy(forward, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = factor(
      term,
      levels = c(
        "hc_vi_catOui",
        "hc_transfuOui",
        "hospit_cgr",
        "hc_dialyseOui",
        "hc_vvcOui",
        "hospit_chirurgie_majeureOui",
        "hospit_ctc_duree",
        "hospit_immunosup_duree",
        "demo_centreSLG",
        "adm_lactates_max",
        "hc_cpOui",
        "iep",
        "hc_uree_max",
        "demo_age",
        "adm_hypothermieOui",
        "hc_aminesOui",
        "adm_diurese_norm",
        "hospit_chirurgie_abdominaleOui",
        "hospit_chirurgie_susmesocoliqueOui"
      ),
      # labels = list(hc_vi_catOui ~ "Ventilation invasive à l'hémoculture")
    ),
    term = fct_reorder(term, estimate, .desc = FALSE),
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )

tidy_model_bwd <- tidy(backward, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = factor(
      term,
      levels = c(
        "iep",
        "demo_centreSLG",
        "demo_age",
        "adm_hypothermieOui",
        "adm_diurese_norm",
        "adm_creat_max",
        "adm_uree_max",
        "adm_lactates_max",
        "hc_dialyseOui",
        "hc_vvcOui",
        "hc_cgrOui",
        "hc_cpOui",
        "hc_aminesOui",
        "hc_hypothermieOui",
        "hc_fievreOui",
        "hc_uree_max",
        "hc_vi_catOui",
        "hospit_parenterale_duree",
        "hospit_ctc_duree",
        "hospit_immunosup_duree",
        "hospit_cgr",
        "hospit_chirurgie_majeureOui",
        "hospit_chirurgie_abdominaleOui",
        "hospit_chirurgie_susmesocoliqueOui"
      ),
      # labels = list(hc_vi_catOui ~ "Ventilation invasive à l'hémoculture")
    ),
    term = fct_reorder(term, estimate, .desc = FALSE),
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )

# ==============================================================================
# Forrest plots
# ==============================================================================

fp_fwd <-
  ggplot(tidy_model_fwd, aes(x = OR, y = term)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Odds Ratio",
    y = "Variable",
    title = "Forest Plot - Régression Logistique"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, hjust = 0))


fp_bwd <-
  ggplot(tidy_model_bwd, aes(x = OR, y = term)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Odds Ratio",
    y = "Variable",
    title = "Forest Plot - Régression Logistique"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, hjust = 0))

#==============================================================================
# Predictions, ROC, Courbe calibration FORWARD
#==============================================================================
df_stepwise$score <- predict(forward, type = "response")
df_stepwise$risque <- ifelse(
  df_stepwise$score < 0.01,
  "Faible",
  ifelse(df_stepwise$score <= 0.2, "Modéré", "Élevé")
)
table(df_stepwise$risque)
B <- 1000
boot_auc_fun <- function(data, indices) {
  boot_data <- data[indices, ]
  roc_boot <- roc(response = boot_data$resultat_candida_def, predictor = boot_data$score)
  return(auc(roc_boot))
}

# Exécute le bootstrap
boot_results_fwd <- boot(
  data = df_stepwise,
  statistic = boot_auc_fun,
  R = B,
  strata = df_stepwise$resultat_candida_def # Stratifie sur resultat_candida_def pour équilibrer les classes
)

# AUC moyenne et IC 95% (méthode BCa)
auc_fwd_mean <- mean(boot_results_fwd$t)
auc_fwd_ci <- boot.ci(boot_results_fwd, type = "bca")$bca[4:5] # [1] = inf, [2] = sup

roc_obj_fwd <- roc(response = df_stepwise$resultat_candida_def, predictor = df_stepwise$score)

roc_fwd <- ggroc(roc_obj_fwd, colour = "black", size = 0.5) +
  ggtitle(paste("Modèle Forward - AUC =", round(auc(roc_obj_fwd), 3))) +
  theme_minimal()

# Courbe ROC originale + annotation de l'IC bootstrap
roc_fwd <- ggroc(roc_obj_fwd, colour = "black", size = 0.5) +
  geom_abline(intercept = 1, slope = -1, linetype = "dashed", color = "red") +
  ggtitle(paste(
    "Modèle Forward - AUC moyen =",
    round(auc_fwd_mean, 3),
    "\nIC 95% (bootstrap) : [",
    round(auc_fwd_ci[1], 3),
    "; ",
    round(auc_fwd_ci[2], 3),
    "]"
  )) +
  theme_minimal()
# Fin essai boot

pred_probs <- predict(forward, type = "response")

dd <- datadist(df_stepwise)
options(datadist = "dd")

fit_lrm <- rms::lrm(
  resultat_candida_def ~
    hc_vi_cat +
    hc_transfu +
    hospit_cgr +
    hc_dialyse +
    hc_vvc +
    hospit_chirurgie_majeure +
    hospit_ctc_duree +
    hospit_immunosup_duree +
    demo_centre +
    adm_lactates_max +
    hc_cp +
    # iep +
    hc_uree_max +
    demo_age +
    adm_hypothermie +
    hc_amines +
    adm_diurese_norm +
    hospit_chirurgie_abdominale +
    hospit_chirurgie_susmesocolique,
  data = df_stepwise,
  x = TRUE,
  y = TRUE
)

# 3. Calcule la calibration
cal_fwd <- calibrate(fit_lrm, method = "boot", b = 200)
cc_fwd <- val.prob.ci.2(
  p = pred_fwd,
  y = obs_fwd,
  g = 10, # nombre de groupes (deciles)
  pl = TRUE,
  smooth = "loess",
  CL.smooth = TRUE, # IC autour du lissage
  logistic.cal = TRUE # droite de calibration logistique
)

# 4. Sauvegarde
ggsave(filename = "figures/FP_fwd11.png", plot = fp_fwd)
ggsave(filename = "figures/ROC_fwd11.png", plot = roc_fwd)
ggsave(
  filename = "figures/CC_fwd11.png",
  plot = cc_fwd$ggPlot
)


#==============================================================================
# Predictions, ROC, Courbe calibration BACKWARD
#==============================================================================

df_stepwise$score <- predict(backward, type = "response")
df_stepwise$risque <- ifelse(
  df_stepwise$score < 0.01,
  "Faible",
  ifelse(df_stepwise$score <= 0.2, "Modéré", "Élevé")
)

table(df_stepwise$risque)

roc_obj_bwd <- roc(response = df_stepwise$resultat_candida_def, predictor = df_stepwise$score)

roc_bwd <- ggroc(roc_obj_bwd, colour = "black", size = 0.5) +
  ggtitle(paste("Modèle Backward - AUC =", round(auc(roc_obj_bwd), 3))) +
  theme_minimal()

pred_probs <- predict(backward, type = "response")

dd <- datadist(df_stepwise)
options(datadist = "dd")

fit_lrm <- rms::lrm(
  resultat_candida_def ~
    # iep +
    demo_centre +
    demo_age +
    adm_hypothermie +
    adm_diurese_norm +
    adm_creat_max +
    adm_uree_max +
    adm_lactates_max +
    hc_dialyse +
    hc_vvc +
    hc_cgr +
    hc_cp +
    hc_amines +
    hc_hypothermie +
    hc_fievre +
    hc_uree_max +
    hc_vi_cat +
    hospit_parenterale_duree +
    hospit_ctc_duree +
    hospit_immunosup_duree +
    hospit_cgr +
    hospit_chirurgie_majeure +
    hospit_chirurgie_abdominale +
    hospit_chirurgie_susmesocolique,
  data = df_stepwise,
  x = TRUE,
  y = TRUE
)

# 3. Calcule la calibration
cal_bwd <- calibrate(fit_lrm, method = "boot", B = 100)
cc_bwd <- val.prob.ci.2(
  p = pred_bwd,
  y = obs_bwd,
  g = 10,
  pl = TRUE,
  smooth = "loess",
  CL.smooth = TRUE,
  logistic.cal = TRUE
)

ggsave(
  filename = "figures/CC_bwd0.png",
  plot = cc_bwd$ggPlot,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(filename = "figures/FP_bwd0.png", plot = fp_bwd)
ggsave(filename = "figures/ROC_bwd0.png", plot = roc_bwd)
ggsave(filename = "figures/CC_bwd0.png", plot = cc_bwd)
