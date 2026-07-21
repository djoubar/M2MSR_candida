# ==============================================================================
#
#                             SETUP / DATA MANAGEMENT
#
# ==============================================================================
library(tidyverse)
library(labelled)
library(gtsummary)
library(patchwork)
library(readxl)

df_base <- read_xlsx("~/M2MSR/donnees/20260619_extraction_fusionnee.xlsx") |>
  set_names(tolower) |>
  mutate(demo_sexe = as.factor(sexe_y)) |>
  dplyr::select(
    -age,
    -sexe_x,
    -atcd,
    -atcd_pancreatite_aigue_ctx,
    -atcd_diabete_ctx,
    -atcd_tumeur_solide_ctx,
    -sexe_y,
    -motif_adm,
    -adm_pancreatite_aigue_ctx,
    -poids,
    -libelle_uf_x
  ) |>
  mutate(across(where(is.numeric), ~ round(., 2)))

# ==============================================================================
#                                  RENOMMER
# ==============================================================================
df_base <- df_base |>
  rename(
    # dates et informations de base
    n_sejour = cisencounterid,
    date_adm_hospit = admission_hopital,
    date_adm_rea = admission_rea,
    date_sortie_rea = sortie_rea,
    # informations démographiques/atcd
    demo_centre = base,
    demo_uf = libelle_uf_y,
    demo_age = age_entree,
    demo_atcd_hemato = atcd_maladie_hemato_maligne,
    demo_atcd_diabete = atcd_diabete,
    demo_atcd_pancreatite = atcd_pancreatite_aigue,
    demo_atcd_tumeur = atcd_tumeur_solide,
    demo_atcd_transplantation = transplantation_organes_solides_pmsi,
    # variables à l'admission
    ## gravité
    adm_igs2 = igs2,
    adm_sofa_respi = sofa_respiratoire_ap_adm,
    adm_sofa_coag = sofa_coagulation_ap_adm,
    adm_sofa_hepatique = sofa_hepatique_ap_adm,
    adm_sofa_neuro = sofa_neurologique_ap_adm,
    adm_sofa_cardio = sofa_cardiovasculaire_ap_adm,
    adm_sofa_renal = sofa_renal_ap_adm,
    adm_sofa_tot = sofa_total_ap_adm,
    ## cliniques
    adm_poids = poids_admission,
    adm_hypothermie = min_temperature_24h_apres_adm,
    adm_fievre = max_temperature_24h_apres_adm,
    adm_diurese_tot = total_vol_urines_24h_apres_adm,
    adm_diurese_norm = total_vol_urines_norm_24h_apres_adm,
    ## biologiques
    adm_creat_max = max_creatinine_24h_apres_adm,
    adm_uree_max = max_uree_24h_apres_adm,
    adm_pfio2_min = min_rapport_pao2_fio2_24h_apres_adm,
    adm_lactates_max = max_lactates_24h_apres_adm,
    adm_lactates_moy = mean_lactates_24h_apres_adm,
    adm_neutro_min = min_neutrophiles_24h_apres_adm,
    adm_lympho_min = min_lymphocytes_24h_apres_adm,
    adm_leuco_min = min_leucocytes_24h_apres_adm,
    ## thérapeutiques
    adm_vi_cat = ventilation_mecanique_invasive_24h_apres_adm,
    adm_dialyse = dialyse_24h_apres_adm,
    adm_cgr = cgr_déleucocyté_ap_adm,
    adm_pfc = pfc_ap_adm,
    adm_cp = plaquettes_ap_adm,
    adm_albu_20 = `albumine_20%_ap_adm`,
    adm_albu_4 = `albumine_4%_ap_adm`,
    adm_bicar = `bicarbonate_1.4%_ap_adm`,
    adm_gelo = gélofusine_ap_adm,
    adm_nacl = `nacl_0.9%_ap_adm`,
    adm_plasmalyte = plasmalyte_ap_adm,
    adm_ringer = `ringer_(lactate)_ap_adm`,
    adm_adre = adrénaline_24h_ap_adm,
    adm_dobu = dobutamine_24h_ap_adm,
    adm_isoprenaline = isoprénaline_24h_ap_adm,
    adm_noradre = noradrénaline_24h_ap_adm,
    adm_terlipressine = terlipressine_24h_ap_adm,
    # variables à l'hémoculture
    ## gravité
    hc_sofa_respi = sofa_respiratoire_av_hemoc,
    hc_sofa_coag = sofa_coagulation_av_hemoc,
    hc_sofa_hepatique = sofa_hepatique_av_hemoc,
    hc_sofa_neuro = sofa_neurologique_av_hemoc,
    hc_sofa_cardio = sofa_cardiovasculaire_av_hemoc,
    hc_sofa_renal = sofa_renal_av_hemoc,
    hc_sofa_tot = sofa_total_av_hemoc,
    ## cliniques
    hc_hypothermie = min_temperature_24h_avant_hemoc,
    hc_fievre = max_temperature_24h_avant_hemoc,
    hc_diurese_tot = total_vol_urines_24h_avant_hemoc,
    hc_diurese_norm = total_vol_urines_norm_24h_avant_hemoc,
    ## biologiques
    hc_pfio2_min = min_rapport_pao2_fio2_24h_avant_hemoc,
    hc_creat_max = max_creatinine_24h_avant_hemoc,
    hc_uree_max = max_uree_24h_avant_hemoc,
    hc_lactates_max = max_lactates_24h_avant_hemoc,
    hc_lactates_moy = mean_lactate_24h_avant_hemoc,
    hc_neutro_min = min_neutrophiles_24h_avant_hemoc,
    hc_lympho_min = min_lymphocytes_24h_avant_hemoc,
    hc_glucanes_max = max_biomarqueurs_glucane_avant_hemoc,
    hc_mannanes_max = max_biomarqueurs_mannane_avant_hemoc,
    hc_leuco_min = min_leucocytes_24h_avant_hemoc,
    ## thérapeutiques
    hc_vi_cat = ventilation_mecanique_invasive_24h_avant_hemoc,
    hc_dialyse = dialyse_24h_avant_hemoc,
    hc_kta = catheter_arteriel_central_24h_avant_hemoc,
    hc_vvc = catheter_veineux_24h_avant_hemoc,
    hc_ktd = catheter_dialyse_24h_avant_hemoc,
    hc_ecmo = catheter_ecmo_central_24h_avant_hemoc,
    hc_cgr = cgr_déleucocyté_av_hemoc,
    hc_pfc = pfc_av_hemoc,
    hc_cp = plaquettes_av_hemoc,
    hc_albu_20 = `albumine_20%_av_hemoc`,
    hc_albu_4 = `albumine_4%_av_hemoc`,
    hc_bicar = `bicarbonate_1.4%_av_hemoc`,
    hc_gelo = gélofusine_av_hemoc,
    hc_nacl = `nacl_0.9%_av_hemoc`,
    hc_plasmalyte = plasmalyte_av_hemoc,
    hc_ringer = `ringer_(lactate)_av_hemoc`,
    hc_voluven = voluven_av_hemoc,
    hc_adre = adrénaline_24h_av_hemoc,
    hc_dobu = dobutamine_24h_av_hemoc,
    hc_isoprenaline = isoprénaline_24h_av_hemoc,
    hc_noradre = noradrénaline_24h_av_hemoc,
    hc_terlipressine = terlipressine_24h_av_hemoc,
    hc_antifongique = tt_antifongique_48h_av_hemoc,
    # variables expo hospit
    hospit_vi_duree = duree_ventilation_mecanique_invasive_en_heures_avant_hemoc,
    hospit_parenterale_duree = duree_nutrition_parenterale_avant_hemoc,
    hospit_kta_duree = duree_kt_arteriel_central_en_jours_avant_hemoc,
    hospit_vvc_duree = duree_kt_veineux_en_jours_avant_hemoc,
    hospit_ktd_duree = duree_kt_dialyse_en_jours_avant_hemoc,
    hospit_ecmo_duree = duree_kt_ecmo_en_jours_avant_hemoc,
    hospit_atb_duree = duree_exposition_antibiotiques,
    hospit_ctc_duree = duree_exposition_corticoides,
    hospit_immunosup_duree = duree_exposition_immunosuppresseurs,
    hospit_neutropen_duree = duree_neutropenie,
    hospit_neutrophi_duree = duree_neutrophilie,
    hospit_lymphopenie_duree = duree_lymphopenie,
    hospit_cgr = nb_culots_globulaires,
    hospit_pfc = nb_poches_plasma,
    hospit_cp = nb_culots_plaquettaires,
    hospit_fibro = fibroscopie_digestive,
    hospit_chirurgie_majeure = chirurgie_majeure_ccam,
    hospit_chirurgie_abdominale = chirurgie_abdominale_ccam,
    hospit_chirurgie_susmesocolique = use_chirurgie_sus_mesocolique,
    hospit_chirurgie_hepatobiliaire = use_chir_hepatobiliaire,
  )


# ==============================================================================
#                                   REFORMATER
# ==============================================================================
df_base <- df_base |>
  mutate(
    hc_glucanes_max_num = as.numeric(gsub("[<>]", "", hc_glucanes_max)),
    hc_glucanes_max = case_when(
      hc_glucanes_max_num < 80 ~ "0",
      hc_glucanes_max_num >= 80 ~ "1",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-hc_glucanes_max_num) |>
  mutate(
    hc_mannanes_max_num = as.numeric(gsub("[<>]", "", hc_mannanes_max)),
    hc_mannanes_max = case_when(
      hc_mannanes_max_num <= 20 ~ "0",
      hc_mannanes_max_num > 20 ~ "1",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-hc_mannanes_max_num) |>
  mutate(
    # demographiques
    demo_sexe = as.factor(demo_sexe),
    demo_centre = as.factor(demo_centre),
    demo_sexe = factor(demo_sexe, levels = c("F", "M"), labels = c("Féminin", "Masculin")),
    demo_atcd_tumeur = factor(demo_atcd_tumeur, levels = c("0", "1"), labels = c("Non", "Oui")),
    demo_atcd_hemato = factor(demo_atcd_hemato, levels = c("0", "1"), labels = c("Non", "Oui")),
    demo_atcd_diabete = factor(demo_atcd_diabete, levels = c("0", "1"), labels = c("Non", "Oui")),
    deces_rea = factor(deces_rea, levels = c("0", "1"), labels = c("Non", "Oui")),
    demo_atcd_transplantation = factor(
      demo_atcd_transplantation,
      levels = c("0", "1"),
      labels = c("Non", "Oui")
    ),
    demo_atcd_pancreatite = factor(
      demo_atcd_pancreatite,
      levels = c("0", "1"),
      labels = c("Non", "Oui")
    ),
    demo_type_rea = ifelse(
      demo_uf %in% c("UNITE A", "UNITE B", "UNITE C", "UNITE D", "UNITE E", "UNITE F", "UNITE G"),
      "Medicale",
      "Chirurgicale"
    ),
    # admission
    adm_vi_cat = factor(adm_vi_cat, levels = c("non", "oui"), labels = c("Non", "Oui")),
    adm_dialyse = factor(adm_dialyse, levels = c("non", "oui"), labels = c("Non", "Oui")),
    adm_pancreatite_aigue = factor(
      adm_pancreatite_aigue,
      levels = c("0", "1"),
      labels = c("Non", "Oui")
    ),
    adm_dialyse = as.factor(adm_dialyse),
    adm_amines = as.factor(ifelse(
      adm_adre > 0 | adm_noradre > 0 | adm_dobu > 0 | adm_isoprenaline > 0 | adm_terlipressine > 0,
      "Oui",
      "Non"
    )),
    adm_hypothermie = as.factor(ifelse(
      adm_hypothermie < 36,
      "Oui",
      "Non"
    )),
    adm_fievre = as.factor(ifelse(
      adm_fievre > 38.3,
      "Oui",
      "Non"
    )),
    adm_pfio2_min = (case_when(
      adm_pfio2_min < 100 ~ "< 100",
      adm_pfio2_min >= 100 & adm_pfio2_min < 200 ~ "100-200",
      adm_pfio2_min >= 200 & adm_pfio2_min <= 300 ~ "200-300",
      adm_pfio2_min > 300 ~ "> 300",
      TRUE ~ NA_character_
    )),
    adm_sofa_respi = as.factor(adm_sofa_respi),
    adm_sofa_coag = as.factor(adm_sofa_coag),
    adm_sofa_hepatique = as.factor(adm_sofa_hepatique),
    adm_sofa_neuro = as.factor(adm_sofa_neuro),
    adm_sofa_cardio = as.factor(adm_sofa_cardio),
    adm_sofa_renal = as.factor(adm_sofa_renal),
    adm_cgr = factor(adm_cgr, levels = c("0", "1"), labels = c("Non", "Oui")),
    adm_pfc = factor(adm_pfc, levels = c("0", "1"), labels = c("Non", "Oui")),
    adm_cp = factor(adm_cp, levels = c("0", "1"), labels = c("Non", "Oui")),
    adm_choc = as.factor(ifelse(
      adm_amines == "Oui" | adm_sofa_cardio == "4" | adm_lactates_max > 2 | adm_sofa_cardio == "3",
      "Oui",
      "Non"
    )),
    adm_albu_20 = as.factor(ifelse(
      adm_albu_20 > 0,
      "Oui",
      "Non"
    )),
    adm_albu_4 = as.factor(ifelse(
      adm_albu_4 > 0,
      "Oui",
      "Non"
    )),
    adm_ringer = as.factor(ifelse(
      adm_ringer > 0,
      "Oui",
      "Non"
    )),
    adm_plasmalyte = as.factor(ifelse(
      adm_plasmalyte > 0,
      "Oui",
      "Non"
    )),
    adm_gelo = as.factor(ifelse(
      adm_gelo > 0,
      "Oui",
      "Non"
    )),
    adm_bicar = as.factor(ifelse(
      adm_bicar > 0,
      "Oui",
      "Non"
    )),
    adm_nacl = as.factor(ifelse(
      adm_nacl > 0,
      "Oui",
      "Non"
    )),
    adm_adre = as.factor(ifelse(
      adm_adre > 0,
      "Oui",
      "Non"
    )),
    adm_noradre = as.factor(ifelse(
      adm_noradre > 0,
      "Oui",
      "Non"
    )),
    adm_dobu = as.factor(ifelse(
      adm_dobu > 0,
      "Oui",
      "Non"
    )),
    adm_terlipressine = as.factor(ifelse(
      adm_terlipressine > 0,
      "Oui",
      "Non"
    )),
    adm_isoprenaline = as.factor(ifelse(
      adm_isoprenaline > 0,
      "Oui",
      "Non"
    )),
    # hemoculture
    hc_hypothermie = as.factor(ifelse(
      hc_hypothermie < 36,
      "Oui",
      "Non"
    )),
    hc_fievre = as.factor(ifelse(
      hc_fievre > 38.3,
      "Oui",
      "Non"
    )),
    hc_sofa_respi = as.factor(hc_sofa_respi),
    hc_sofa_coag = as.factor(hc_sofa_coag),
    hc_sofa_hepatique = as.factor(hc_sofa_hepatique),
    hc_sofa_neuro = as.factor(hc_sofa_neuro),
    hc_sofa_cardio = as.factor(hc_sofa_cardio),
    hc_sofa_renal = as.factor(hc_sofa_renal),
    hc_pfio2_min = (case_when(
      hc_pfio2_min < 100 ~ "< 100",
      hc_pfio2_min >= 100 & adm_pfio2_min < 200 ~ "100-200",
      hc_pfio2_min >= 200 & adm_pfio2_min <= 300 ~ "200-300",
      hc_pfio2_min > 300 ~ "> 300",
      TRUE ~ NA_character_
    )),
    hc_cgr = factor(hc_cgr, levels = c("0", "1"), labels = c("Non", "Oui")),
    hc_pfc = factor(hc_pfc, levels = c("0", "1"), labels = c("Non", "Oui")),
    hc_cp = factor(hc_cp, levels = c("0", "1"), labels = c("Non", "Oui")),
    hc_delai = date_hemoc - date_adm_rea,
    duree_hospit = date_sortie_rea - date_adm_rea,
    hc_amines = as.factor(ifelse(
      hc_adre > 0 | hc_noradre > 0 | hc_dobu > 0 | hc_isoprenaline > 0 | hc_terlipressine > 0,
      "Oui",
      "Non"
    )),
    hc_choc = as.factor(ifelse(
      hc_amines == "Oui" | hc_sofa_cardio == "4" | hc_lactates_max > 2 | hc_sofa_cardio == "3",
      "Oui",
      "Non"
    )),
    hc_albu_20 = as.factor(ifelse(
      hc_albu_20 > 0,
      "Oui",
      "Non"
    )),
    hc_albu_4 = as.factor(ifelse(
      hc_albu_4 > 0,
      "Oui",
      "Non"
    )),
    hc_bicar = as.factor(ifelse(
      hc_bicar > 0,
      "Oui",
      "Non"
    )),
    hc_ringer = as.factor(ifelse(
      hc_ringer > 0,
      "Oui",
      "Non"
    )),
    hc_plasmalyte = as.factor(ifelse(
      hc_plasmalyte > 0,
      "Oui",
      "Non"
    )),
    hc_gelo = as.factor(ifelse(
      hc_gelo > 0,
      "Oui",
      "Non"
    )),
    hc_nacl = as.factor(ifelse(
      hc_nacl > 0,
      "Oui",
      "Non"
    )),
    hc_adre = as.factor(ifelse(
      hc_adre > 0,
      "Oui",
      "Non"
    )),
    hc_noradre = as.factor(ifelse(
      hc_noradre > 0,
      "Oui",
      "Non"
    )),
    hc_dobu = as.factor(ifelse(
      hc_dobu > 0,
      "Oui",
      "Non"
    )),
    hc_terlipressine = as.factor(ifelse(
      hc_terlipressine > 0,
      "Oui",
      "Non"
    )),
    hc_isoprenaline = as.factor(ifelse(
      hc_isoprenaline > 0,
      "Oui",
      "Non"
    )),
    hc_antifongique = as.factor(ifelse(
      hc_antifongique > 0,
      "Oui",
      "Non"
    )),
    hc_vi_cat = factor(hc_vi_cat, levels = c("non", "oui"), labels = c("Non", "Oui")),
    hc_kta = factor(hc_kta, levels = c("non", "oui"), labels = c("Non", "Oui")),
    hc_vvc = factor(hc_vvc, levels = c("non", "oui"), labels = c("Non", "Oui")),
    hc_ktd = factor(hc_ktd, levels = c("non", "oui"), labels = c("Non", "Oui")),
    hc_dialyse = factor(hc_dialyse, levels = c("non", "oui"), labels = c("Non", "Oui")),
    hc_ecmo = factor(hc_ecmo, levels = c("non", "oui"), labels = c("Non", "Oui")),
    hc_glucanes_max = factor(
      hc_glucanes_max,
      levels = c("0", "1"),
      labels = c("Négatif", "Positif")
    ),
    hc_mannanes_max = factor(
      hc_mannanes_max,
      levels = c("0", "1"),
      labels = c("Négatif", "Positif")
    ),
    # hospitalisation
    hospit_fibro = factor(hospit_fibro, levels = c("0", "1"), labels = c("Non", "Oui")),
    hospit_chirurgie_majeure = factor(
      hospit_chirurgie_majeure,
      levels = c("0", "1"),
      labels = c("Non", "Oui")
    ),
    hospit_chirurgie_abdominale = factor(
      hospit_chirurgie_abdominale,
      levels = c("0", "1"),
      labels = c("Non", "Oui")
    ),
    hospit_chirurgie_susmesocolique = factor(
      hospit_chirurgie_susmesocolique,
      levels = c("0", "1"),
      labels = c("Non", "Oui")
    ),
    hospit_chirurgie_hepatobiliaire = factor(
      hospit_chirurgie_hepatobiliaire,
      levels = c("0", "1"),
      labels = c("Non", "Oui")
    ),
    adm_transfu = as.factor(ifelse(
      adm_cgr == "Oui" | adm_pfc == "Oui" | adm_cp == "Oui",
      "Oui",
      "Non"
    )),
    hc_transfu = as.factor(ifelse(
      hc_cgr == "Oui" | hc_pfc == "Oui" | hc_cp == "Oui",
      "Oui",
      "Non"
    )),
    hospit_parenterale_cat = as.factor(ifelse(
      hospit_parenterale_duree > 0,
      "1",
      "0"
    )),
    hc_catheter_majeur = as.factor(ifelse(
      hc_vvc == "Oui" | hc_ktd == "Oui" | hc_ecmo == "Oui",
      "1",
      "0"
    ))
  )

# ==============================================================================
#              REGROUPEMENT DES HEMOCULTURES + SUPPRESSION CONTROLES
# ==============================================================================

df_base <- df_base |>
  # 1. Regroupement par épisode de sepsis
  group_by(n_sejour) |>
  arrange(n_sejour, date_hemoc) |>
  mutate(
    diff = as.numeric(difftime(date_hemoc, lag(date_hemoc), units = "hours"), na.rm = TRUE),
    groupehc = cumsum(ifelse(is.na(diff) | diff > 24, 1, 0))
  ) |>
  ungroup() |>
  # 2. Marquer les groupes avec candidémie
  group_by(n_sejour, groupehc) |>
  mutate(resultat_candida_def = any(resultat_candida == "positif", na.rm = TRUE)) |>
  ungroup() |>
  # 3. Supprimer les groupes QUI SUIVENT un groupe positif
  group_by(n_sejour) |>
  arrange(n_sejour, groupehc) |>
  mutate(
    suppress_after = cummax(ifelse(lag(resultat_candida_def, default = FALSE), 1, 0))
  ) |>
  ungroup() |>
  filter(suppress_after == 0)

# ==============================================================================
#                            COMPLETER MANUELLEMENT
# ==============================================================================

# compléter sexe_féminin
df_base <- df_base |>
  mutate(
    demo_sexe = case_when(
      iep %in% c("16123204", "22517597", "14926498") ~ "Féminin",
      TRUE ~ demo_sexe
    )
  )
# compléter sexe masculin
df_base <- df_base |>
  mutate(
    demo_sexe = case_when(
      iep %in%
        c(
          "23175309",
          "22842308",
          "22513578",
          "22470648",
          "22445608",
          "16005104",
          "14814310"
        ) ~ "Masculin",
      TRUE ~ demo_sexe
    )
  )

# compléter atcd_diabete
df_base <- df_base |>
  mutate(
    demo_atcd_diabete = case_when(
      iep %in%
        c(
          "19627822"
        ) ~ "Oui",
      TRUE ~ demo_atcd_diabete
    )
  )

# compléter atcd_transplant
df_base <- df_base |>
  mutate(
    demo_atcd_transplantation = case_when(
      iep %in%
        c(
          "19627822"
        ) ~ "Oui",
      TRUE ~ demo_atcd_transplantation
    )
  )

# compléter atcd tumeur
df_base <- df_base |>
  mutate(
    demo_atcd_tumeur = case_when(
      iep %in%
        c(
          "13017041"
        ) ~ "Oui",
      TRUE ~ demo_atcd_tumeur
    )
  )

df_base <- df_base |>
  mutate(across(
    starts_with("demo_atcd"),
    ~ fct_explicit_na(., na_level = "Non")
  )) |>
  mutate(across(
    starts_with("hospit_chirurgie"),
    ~ fct_explicit_na(., na_level = "Non")
  ))

# ==============================================================================
#                                  LABELS
# ==============================================================================
var_label(df_base) <- list(
  demo_centre = "Centre",
  demo_atcd_hemato = "Antécédent de maladie hématologique maligne",
  demo_age = "Age",
  demo_sexe = "Sexe",
  demo_atcd_tumeur = "Antécédent de tumeur solide",
  demo_atcd_pancreatite = "Antécédent de pancréatite aigue",
  demo_atcd_diabete = "Antécédent de diabète",

  adm_igs2 = "IGS 2 à l'admission",
  adm_pancreatite_aigue = "Admission pour pancréatite aigue",
  adm_poids = "Poids à l'admission",
  adm_vi_cat = "Ventilation invasive à l'admission",
  adm_pfio2_min = "PaO2/FiO2 minimal à l'admission",
  adm_creat_max = "Créatinémie maximale (en mg/L) à l'admission",
  adm_uree_max = "Urée maximale (en g/L) à l'admission",
  adm_dialyse = "Dialyse à l'admission",
  adm_hypothermie = "Hypothermie à l'admission",
  adm_fievre = "Fièvre à l'admission",
  adm_diurese_tot = "Diurèse totale à l'admission",
  adm_diurese_norm = "Diurèse normalisée",
  adm_lactates_max = "Lactatémie maximale (en mmol/L) à l'admission",
  adm_lactates_moy = "Lactatémie moyenne (en mmol/L) à l'admission",
  adm_neutro_min = "Neutrophiles (en G/L) à l'admission",
  adm_lympho_min = "Lymphocytes (en G/L) à l'admission",
  adm_cgr = "Culot Globulaires à l'admission",
  adm_pfc = "Plasma frais congelé à l'admission",
  adm_cp = "Culot plaquettaire à l'admission",

  hc_sofa_respi = "SOFA respiratoire à l'hémoculture",
  hc_sofa_coag = "SOFA coagulation à l'hémoculture",
  hc_sofa_hepatique = "SOFA hépatique à l'hémoculture",
  hc_sofa_neuro = "SOFA neurologique à l'hémoculture",
  hc_sofa_cardio = "SOFA cardiologique à l'hémoculture",
  hc_sofa_renal = "SOFA rénal à l'hémoculture",
  hc_sofa_tot = "SOFA total à l'hémoculture",
  hc_hypothermie = "Hypothermie Hc",
  hc_fievre = "Fièvre Hc",
  hc_diurese_tot = "Diurèse totale à l'hémoculture",
  hc_diurese_norm = "Diurèse normalisée à l'hémoculture",
  hc_pfio2_min = "PaO2/FiO2 à l'hémoculture",
  hc_creat_max = "Créatinémie à l'hémoculture",
  hc_uree_max = "Urémie à l'hémoculture",
  hc_lactates_max = "Lactatémie à l'hémoculture",
  hc_lactates_moy = "Lactatémie moy à l'hémoculture",
  hc_neutro_min = "Neutropénie min à l'hémoculture",
  hc_lympho_min = "Lymphopénie à l'hémoculture",
  hc_glucanes_max = "Glucanes à l'hémoculture",
  hc_mannanes_max = "Mannanes à l'hémoculture",
  hc_vi_cat = "Ventilation invasive à l'hémoculture",
  hc_dialyse = "Dialyse à l'hémoculture",
  hc_kta = "KTA à l'hémoculture",
  hc_vvc = "VVC à l'hémoculture",
  hc_ktd = "Cathéter de dialyse à l'hémoculture",
  hc_ecmo = "ECMO à l'hémoculture",
  hc_cgr = "CGR à l'hémoculture",
  hc_pfc = "PFC à l'hémoculture",
  hc_cp = "CP à l'hémoculture",
  hc_albu_20 = "Albu 20% à l'hémoculture",
  hc_albu_4 = "Albu 4% à l'hémoculture",
  hc_bicar = "Bicarbonates à l'hémoculture",
  hc_gelo = "Gélo à l'hémoculture",
  hc_nacl = "NaCl à l'hémoculture",
  hc_plasmalyte = "Plasmalyte à l'hémoculture",
  hc_ringer = "RL à l'hémoculture",
  hc_voluven = "Voluven à l'hémoculture",
  hc_adre = "Adrénaline à l'hémoculture",
  hc_dobu = "Dobutamine à l'hémoculture",
  hc_isoprenaline = "Isopréaline à l'hémoculture",
  hc_noradre = "Noradrénaline à l'hémoculture",
  hc_terlipressine = "Terlipressine à l'hémoculture",

  hospit_vi_duree = "Durée de ventilation invasive à l'hémoculture",
  hospit_parenterale_duree = "Durée de nutrition parentérale à l'hémoculture",
  hospit_vvc_duree = "Durée de VVC à l'hémoculture",
  hospit_kta_duree = "Durée de KTA à l'hémoculture",
  hospit_ktd_duree = "Durée de KTD à l'hémoculture",
  hospit_ecmo_duree = "Durée ECMO à l'hémoculture",
  hospit_atb_duree = "Durée ATBT à l'hémoculture",
  hospit_ctc_duree = "Durée CTC à l'hémoculture",
  hospit_immunosup_duree = "Durée immunosuppression à l'hémoculture",
  hospit_neutropen_duree = "Durée neutropénie à l'hémoculture",
  hospit_neutrophi_duree = "Durée neutrophilie à l'hémoculture",
  hospit_lymphopenie_duree = "Durée lymphopénie à l'hémoculture",
  hospit_cgr = "Nombres CGR avant l'hémoculture",
  hospit_pfc = "Nombres PFC avant l'hémoculture",
  hospit_cp = "Nombres CP avant l'hémoculture",
  hospit_fibro = "Fibroscopie pendant l'hospitalisation",
  hospit_chirurgie_abdominale = "Chirurgie abdominale pendant l'hospitalisation",
  hospit_chirurgie_majeure = "Chirurgie majeure pendant l'hospitalisation"
)

# ==============================================================================
#                                   SELECTION
# ==============================================================================

df_base <- df_base %>%
  dplyr::select(
    # dates et informations de base
    id_hemoc,
    iep,
    resultat_candida_def,
    # resultat_candida,
    groupehc,
    # n_prvl,
    date_adm_hospit,
    date_adm_rea,
    date_hemoc,
    date_sortie_rea,
    duree_hospit,
    date_deces,
    deces_rea,
    # informations démographiques/atcd
    demo_centre,
    demo_type_rea,
    demo_uf,
    demo_age,
    demo_sexe,
    demo_atcd_hemato,
    demo_atcd_diabete,
    demo_atcd_pancreatite,
    demo_atcd_tumeur,
    demo_atcd_transplantation,
    # variables à l'admission
    adm_choc,
    ## gravité
    adm_igs2,
    adm_sofa_tot,
    adm_sofa_respi,
    adm_sofa_coag,
    adm_sofa_hepatique,
    adm_sofa_neuro,
    adm_sofa_cardio,
    adm_sofa_renal,
    adm_pancreatite_aigue,
    ## cliniques
    adm_poids,
    adm_hypothermie,
    adm_fievre,
    adm_diurese_tot,
    adm_diurese_norm,
    ## biologiques
    adm_creat_max,
    adm_uree_max,
    adm_pfio2_min,
    adm_lactates_max,
    adm_lactates_moy,
    adm_leuco_min,
    adm_neutro_min,
    adm_lympho_min,
    ## thérapeutiques
    adm_vi_cat,
    adm_dialyse,
    adm_cgr,
    adm_pfc,
    adm_cp,
    adm_transfu,
    # adm_albu_20,
    # adm_albu_4,
    # adm_bicar,
    # adm_gelo,
    # adm_nacl,
    # adm_plasmalyte,
    # adm_ringer,
    adm_amines,
    # adm_adre,
    # adm_dobu,
    # adm_isoprenaline,
    # adm_noradre,
    # adm_terlipressine,

    # variables à l'hémoculture
    hc_delai,
    hc_choc,
    ## gravité
    hc_sofa_respi,
    hc_sofa_coag,
    hc_sofa_hepatique,
    hc_sofa_neuro,
    hc_sofa_cardio,
    hc_sofa_renal,
    hc_sofa_tot,
    ## cliniques
    hc_hypothermie,
    hc_fievre,
    hc_diurese_tot,
    hc_diurese_norm,
    ## biologiques
    hc_pfio2_min,
    hc_creat_max,
    hc_uree_max,
    hc_lactates_max,
    # hc_lactates_moy,
    hc_neutro_min,
    hc_leuco_min,
    hc_lympho_min,
    hc_glucanes_max,
    hc_mannanes_max,
    ## thérapeutiques
    hc_vi_cat,
    hc_dialyse,
    hc_kta,
    hc_vvc,
    hc_ktd,
    hc_ecmo,
    hc_catheter_majeur,
    hc_cgr,
    hc_pfc,
    hc_cp,
    hc_transfu,
    # hc_albu_20,
    # hc_albu_4,
    # hc_bicar,
    # hc_gelo,
    # hc_nacl,
    # hc_plasmalyte,
    # hc_ringer,
    # hc_voluven,
    hc_amines,
    # hc_adre,
    # hc_dobu,
    # hc_isoprenaline,
    # hc_noradre,
    # hc_terlipressine,
    hc_antifongique,
    # variables expo hospit
    hospit_vi_duree,
    hospit_parenterale_duree,
    hospit_vvc_duree,
    hospit_kta_duree,
    hospit_ktd_duree,
    hospit_ecmo_duree,
    hospit_atb_duree,
    hospit_ctc_duree,
    hospit_immunosup_duree,
    hospit_neutropen_duree,
    hospit_neutrophi_duree,
    hospit_lymphopenie_duree,
    hospit_cgr,
    hospit_pfc,
    hospit_cp,
    hospit_fibro,
    hospit_chirurgie_majeure,
    hospit_chirurgie_abdominale,
    hospit_chirurgie_susmesocolique,
    hospit_chirurgie_hepatobiliaire
  ) |>
  arrange(iep, groupehc) |>
  group_by(iep, groupehc) |>
  slice(1) |>
  ungroup() |>
  mutate(
    resultat_candida_def = factor(
      resultat_candida_def,
      levels = c(0, 1),
      labels = c("Négative", "Positive")
    ),
    date_candidemie = as.POSIXct(ifelse(
      resultat_candida_def == "Positive",
      date_hemoc,
      NA
    ))
  )

df_base <- df_base |>
  mutate(
    demo_sexe = factor(
      demo_sexe,
      levels = c("Féminin", "Masculin"),
      labels = c("Féminin", "Masculin")
    ),
    adm_pfio2_min = factor(
      adm_pfio2_min,
      levels = c("< 100", "100-200", "200-300", "> 300"),
      labels = c("< 100", "100-200", "200-300", "> 300")
    ),
    hc_pfio2_min = factor(
      hc_pfio2_min,
      levels = c("< 100", "100-200", "200-300", "> 300"),
      labels = c("< 100", "100-200", "200-300", "> 300")
    ),
    hc_deficit_neutro = ifelse(hospit_neutropen_duree > 0 | hospit_ctc_duree > 0, "1", "0"),
    hc_deficit_lympho = ifelse(hospit_lymphopenie_duree > 0 | hospit_immunosup_duree > 0, "1", "0")
  )
