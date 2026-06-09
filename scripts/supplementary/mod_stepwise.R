imp <- read_rds("donnees/df_impute.rds")
source("scripts/brutes/_setup.R")

#===============================================================================
#                              CREATION DES MODELES
#===============================================================================

df_stepwise <- complete(imp, 1) %>%
  left_join(
    df_base %>%
      select(
        id_hemoc,
        date_adm_hospit,
        date_adm_rea,
        date_hemoc,
        date_sortie_rea,
        date_deces
      ),
    by = "id_hemoc"
  ) %>%
  group_by(iep) %>%
  summarise(
    demo_centre = first(demo_centre),
    demo_type_rea = first(demo_type_rea),
    demo_uf = first(demo_uf),
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
    adm_temp_min = first(adm_temp_min),
    adm_temp_max = first(adm_temp_max),
    adm_diurese_norm = first(adm_diurese_norm),
    adm_creat_max = first(adm_creat_max),
    adm_uree_max = first(adm_uree_max),
    adm_pfio2_min = first(adm_pfio2_min),
    adm_lactates_max = first(adm_lactates_max),
    adm_leuco_min = first(adm_leuco_min),
    adm_vi_cat = first(adm_vi_cat),
    adm_dialyse = first(adm_dialyse),
    adm_cgr = first(adm_cgr),
    adm_pfc = first(adm_pfc),
    adm_cp = first(adm_cp),
    adm_transfu = first(adm_transfu),
    adm_amines = first(adm_amines),

    # --- Covariables DYNAMIQUES (hc_*) : agrégation ---
    hc_choc = last(hc_choc),
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

    # Continues : pire valeur (max/min) ou moyenne
    hc_delai = as.numeric(last(hc_delai)), # Délai jusqu'à la 1ère hémoculture
    hc_temp_min = last(hc_temp_min),
    hc_temp_max = last(hc_temp_max),
    hc_diurese_norm = last(hc_diurese_norm),
    hc_pfio2_min = last(hc_pfio2_min),
    hc_creat_max = last(hc_creat_max),
    hc_uree_max = last(hc_uree_max),
    hc_lactates_max = last(hc_lactates_max),
    hc_leuco_min = last(hc_leuco_min),
    hc_glucanes_max = last(hc_glucanes_max),
    hc_mannanes_max = last(hc_mannanes_max),
    hc_vi_cat = last(hc_vi_cat),

    # --- Covariables HOSPIT (durées) : première valeur ---
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
    hospit_lymphopenie_duree = last(hospit_lymphopenie_duree),
    hospit_cgr = last(hospit_cgr),
    hospit_pfc = last(hospit_pfc),
    hospit_cp = last(hospit_cp),
    hospit_fibro = last(hospit_fibro),
    hospit_chirurgie_majeure = last(hospit_chirurgie_majeure),
    hospit_chirurgie_abdominale = last(hospit_chirurgie_abdominale),
    nb_hemocultures = n(),
    resultat_candida_def = last(resultat_candida_def)
  ) %>%
  ungroup() |>
  select(-c(demo_uf, demo_centre, hc_glucanes_max, hc_mannanes_max))

mod_intercept <- glm(
  resultat_candida_def ~ 1,
  data = df_stepwise,
  family = "binomial"
)

mod_tot <- glm(
  resultat_candida_def ~ .,
  data = df_stepwise,
  family = "binomial"
)

forward <- step(mod_intercept, direction = 'forward', scope = formula(mod_tot))
# tbl_regression(forward, exponentiate = TRUE)
saveRDS(forward, "models/mod_imp1_fwd.RDS")
backward <- step(mod_tot, direction = 'backward', scope = formula(mod_tot))
# tbl_regression(backward, exponentiate = TRUE)
saveRDS(backward, "models/mod_imp1_bwd.RDS")
both <- step(mod_intercept, direction = 'both', scope = formula(mod_tot))
tbl_regression(both, exponentiate = TRUE)
saveRDS(both, "models/mod_imp1_both.RDS")
#===============================================================================
#                               FORREST PLOTS
#===============================================================================
forward <- readRDS("models/mod_imp1_fwd.RDS")
backward <- readRDS("models/mod_imp1_bwd.RDS")

tidy_model_fwd <- tidy(forward, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = factor(
      term,
      levels = c(
        "hc_vi_catOui",
        "hc_glucanes_maxPositif",
        "hc_mannanes_maxPositif",
        "hc_transfu1",
        "hc_chocOui",
        "hospit_cgr",
        "temps",
        "hc_delai",
        "hospit_immunosup_duree",
        "hospit_vvc_duree",
        "hospit_ctc_duree",
        "demo_type_reaMedicale",
        "hc_diurese_norm",
        "demo_age",
        "adm_temp_min",
        "hospit_neutrophi_duree",
        "hospit_chirurgie_majeureOui",
        "hc_aminesOui",
        "hc_catheter_majeur1",
        "hospit_parenterale_duree",
        "hospit_vi_duree",
        "adm_vi_catOui",
        "hc_temp_min",
        "adm_uree_max",
        "adm_creat_max",
        "adm_pfio2_min",
        "hc_pfio2_min",
        "nb_hemocultures",
        "hc_dialyseOui"
      ),
      # labels = list(hc_vi_catOui ~ "Ventilation invasive à l'hémoculture")
    ),
    term = fct_reorder(term, estimate, .desc = FALSE),
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )

fp_fwd <- ggplot(tidy_model_fwd, aes(x = OR, y = term)) +
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

tidy_model_bwd <- tidy(backward, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = factor(
      term,
      levels = c(
        "temps",
        "demo_type_rea",
        "demo_age",
        "adm_temp_min",
        "adm_creat_max",
        "adm_uree_max",
        "adm_pfio2_min",
        "adm_vi_cat",
        "hc_choc",
        "hc_catheter_majeur",
        "hc_cgr",
        "hc_cp",
        "hc_amines",
        "hc_delai",
        "hc_temp_min",
        "hc_diurese_norm",
        "hc_pfio2_min",
        "hc_leuco_min",
        "hc_glucanes_max",
        "hc_mannanes_max",
        "hc_vi_cat",
        "hospit_vi_duree",
        "hospit_parenterale_duree",
        "hospit_vvc_duree",
        "hospit_ctc_duree",
        "hospit_immunosup_duree",
        "hospit_neutrophi_duree",
        "hospit_cgr",
        "hospit_chirurgie_majeure",
        "nb_hemocultures"
      ),
      # labels = list(hc_vi_catOui ~ "Ventilation invasive à l'hémoculture")
    ),
    term = fct_reorder(term, estimate, .desc = FALSE),
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )

fp_bwd <- ggplot(tidy_model_bwd, aes(x = OR, y = term)) +
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
#                          PREDICTIONS / ROC / CC
#==============================================================================
# FWD

df_stepwise$score <- predict(forward, type = "response")
# ajouter re.form = NA pour mm
df_stepwise$risque <- ifelse(
  df_stepwise$score < 0.01,
  "Faible",
  ifelse(df_stepwise$score <= 0.15, "Modéré", "Élevé")
)

# table(df_stepwise$risque)
# tapply(df_stepwise$score, df_stepwise$risque, mean) # Moyennes par groupe

roc_obj_fwd <- roc(response = df_stepwise$resultat_candida_def, predictor = df_stepwise$score)

roc_fwd <- ggroc(roc_obj_fwd, colour = "black", size = 0.5) +
  ggtitle(paste("Modèle Forward - AUC =", round(auc(roc_obj_fwd), 3))) +
  theme_minimal()

pred_probs <- predict(forward, type = "response")

dd <- datadist(df_stepwise)
options(datadist = "dd")

fit_lrm <- lrm(
  resultat_candida_def ~ hc_vi_cat +
    hc_glucanes_max +
    hc_mannanes_max +
    hc_transfu +
    hc_choc +
    hospit_cgr +
    hospit_immunosup_duree +
    hospit_vvc_duree +
    hospit_ctc_duree +
    demo_type_rea +
    hc_diurese_norm +
    demo_age +
    adm_temp_min +
    hospit_neutrophi_duree +
    hospit_chirurgie_majeure +
    hc_amines +
    hc_catheter_majeur +
    hospit_parenterale_duree +
    hospit_vi_duree +
    adm_vi_cat +
    hc_temp_min +
    adm_uree_max +
    adm_creat_max +
    adm_pfio2_min +
    hc_pfio2_min +
    nb_hemocultures +
    hc_dialyse,
  data = df_stepwise,
  x = TRUE,
  y = TRUE
)

# 3. Calcule la calibration
cal_fwd <- calibrate(fit_lrm, method = "boot", b = 200)
cc_fwd <- plot(cal)


# BWD
df_stepwise$score <- predict(backward, type = "response")
# ajouter re.form = NA pour mm
df_stepwise$risque <- ifelse(
  df_stepwise$score < 0.01,
  "Faible",
  ifelse(df_stepwise$score <= 0.15, "Modéré", "Élevé")
)

# table(df_stepwise$risque)
# tapply(df_stepwise$score, df_stepwise$risque, mean) # Moyennes par groupe

roc_obj_bwd <- roc(response = df_stepwise$resultat_candida_def, predictor = df_stepwise$score)

roc_bwd <- ggroc(roc_obj_bwd, colour = "black", size = 0.5) +
  ggtitle(paste("Modèle Backward - AUC =", round(auc(roc_obj_bwd), 3))) +
  theme_minimal()

pred_probs <- predict(backward, type = "response")

dd <- datadist(df_stepwise)
options(datadist = "dd")

fit_lrm <- lrm(
  resultat_candida_def ~
    # temps +
    demo_type_rea +
    demo_age +
    adm_temp_min +
    adm_creat_max +
    adm_uree_max +
    adm_pfio2_min +
    adm_vi_cat +
    hc_choc +
    hc_catheter_majeur +
    hc_cgr +
    hc_cp +
    hc_amines +
    hc_delai +
    hc_temp_min +
    hc_diurese_norm +
    hc_pfio2_min +
    hc_leuco_min +
    hc_glucanes_max +
    hc_mannanes_max +
    hc_vi_cat +
    hospit_vi_duree +
    hospit_parenterale_duree +
    hospit_vvc_duree +
    hospit_ctc_duree +
    hospit_immunosup_duree +
    hospit_neutrophi_duree +
    hospit_cgr +
    hospit_chirurgie_majeure +
    nb_hemocultures,
  data = df_stepwise,
  x = TRUE,
  y = TRUE
)

# 3. Calcule la calibration
cal_bwd <- calibrate(fit_lrm, method = "boot", B = 100)
cc_bwd <- plot(cal)

ggsave(filename = "figures/FP_fwd.png", plot = fp_fwd)
ggsave(filename = "figures/FP_bwd.png", plot = fp_bwd)
ggsave(filename = "figures/ROC_fwd.png", plot = roc_fwd)
ggsave(filename = "figures/ROC_bwd.png", plot = roc_bwd)
save(path = "figures/CC_fwd.png", plot = cc_fwd)
save(filename = "figures/CC_bwd.png", plot = cc_bwd)
