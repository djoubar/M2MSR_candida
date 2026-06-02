imp <- read_rds("donnees/df_impute.rds")
source("scripts/brutes/_setup.R")

################################################################################
#                                    SETUP                                     #
################################################################################
df_fg <- complete(imp, 1) %>%
  left_join(
    df_base %>%
      select(
        id_hemoc,
        date_adm_hospit,
        date_adm_rea,
        date_hemoc,
        date_sortie_rea,
        date_deces,
        date_candidemie
      ),
    by = "id_hemoc"
  ) %>%
  group_by(iep) %>%
  summarise(
    temps = min(
      as.numeric(coalesce(date_candidemie, date_deces, date_sortie_rea) - date_adm_rea),
      na.rm = TRUE
    ),
    outcome = case_when(
      any(!is.na(date_candidemie) & (is.na(date_deces) | date_candidemie < date_deces)) ~ 1,
      any(!is.na(date_deces) & (is.na(date_candidemie) | date_deces < date_candidemie)) ~ 2,
      TRUE ~ 0
    ),
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
    hc_delai = last(hc_delai), # Délai jusqu'à la 1ère hémoculture
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
    nb_hemocultures = n()
  ) %>%
  ungroup() %>%
  # Convertir outcome en facteur
  mutate(
    outcome = factor(outcome, levels = c(0, 1, 2), labels = c("Sortie", "Candidémie", "Décès"))
  )

################################################################################
#                                MODELE_UV                                     #
################################################################################

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
  Surv(temps, outcome) ~ demo_centre +
    demo_age +
    demo_atcd_hemato +
    adm_igs2 +
    adm_creat_max +
    adm_pfio2_min +
    adm_lactates_max +
    adm_transfu +
    hc_amines +
    hc_creat_max +
    hc_lactates_max +
    hc_vi_cat +
    hc_catheter_majeur +
    hc_transfu +
    hospit_parenterale_duree +
    hospit_atb_duree +
    hospit_ctc_duree +
    hospit_neutropen_duree +
    hospit_chirurgie_majeure,
  data = df_fg
)

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

# Extrais les résultats sous forme de tibble
tidy_model <- tidy(model_fg, conf.int = TRUE)

# Forest plot (identique à l'exemple précédent)
ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()

################################################################################
#                                     CIF                                      #
################################################################################
# CIF globales (sans stratification)
fit_cif <- cuminc(Surv(temps, outcome) ~ 1, data = df_fg)
ggcuminc(fit_cif, outcome = c("Sortie", "Décès", "Candidémie")) +
  add_confidence_interval() +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  labs(
    title = "Incidence cumulée : Candidémie (rouge) vs Décès (bleu)",
    x = "Temps (jours)",
    y = "Probabilité cumulée"
  ) +
  theme_minimal()

################################################################################
#                             VERIF_COLINEARITE                                #
################################################################################
vif(lm(temps ~ ., data = df_fg)) # Attention : VIF > 5-10 = problème
