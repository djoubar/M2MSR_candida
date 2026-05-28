################################################################################
#                                                                              #
#                            M2MSR_TBL2_DEMOGRAPHIQUE                          #
#                                                                              #
################################################################################
.df_tbl2 <- df_base

theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
set_flextable_defaults(
  font.family = "Serif",
  font.size = 12,
  padding = 2,
  border.color = "#CCCCCC",
  line_spacing = 1.3,
  line_width = 2
)

tbl2 <-
  tbl_summary(
    data = .df_tbl2,
    by = "resultat_candida_def",
    missing = "no",
    label = list(
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
      adm_temp_min = "Température minimale (en °C) à l'admission",
      adm_temp_max = "Température maximale (en °C) à l'admission",
      adm_diurese_tot = "Diurèse totale à l'admission",
      adm_diurese_norm = "Diurèse normalisée",
      adm_lactates_max = "Lactatémie maximale (en mmol/L) à l'admission",
      adm_lactates_moy = "Lactatémie moyenne (en mmol/L) à l'admission",
      adm_neutro_min = "Neutrophiles (en G/L) à l'admission",
      adm_lympho_min = "Lymphocytes (en G/L) à l'admission",
      adm_cgr = "Culot Globulaires à l'admission",
      adm_pfc = "Plasma frais congelé à l'admission",
      adm_cp = "Culot plaquettaire à l'admission",
      adm_transfu = "Transfusion à l'admission",
      adm_amines = "Amines à l'admission",
      hc_delai = "Délai entre admission et prélèvements de l'hémoculture",
      hc_choc = "Choc à l'hémoculture",
      hc_sfu = "Transfusion à l'hémoculture",
      hc_amines = "Amines à l'hémoculture",
      hc_temp_min = "Température minimale à l'hémoculture",
      hc_temp_max = "Température maximale à l'hémoculture",
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
      hc_vi_cat = "Ventialtion invasive à l'hémoculture",
      hc_dialyse = "Dialyse à l'hémoculture",
      hc_kta = "KTA à l'hémoculture",
      hc_vvc = "VVC à l'hémoculture",
      hc_ktd = "Cathéter de dialyse à l'hémoculture",
      hc_ecmo = "ECMO à l'hémoculture",
      hc_cgr = "CGR à l'hémoculture",
      hc_pfc = "PFC à l'hémoculture",
      hc_cp = "CP à l'hémoculture",

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
      hospit_lymphopenie_duree = "Dureé lymphopénie à l'hémoculture",
      hospit_cgr = "Nombres CGR avant l'hémoculture",
      hospit_pfc = "Nombres PFC avant l'hémoculture",
      hospit_cp = "Nombres CP avant l'hémoculture",
      hospit_fibro = "Fibroscopie pendant l'hospitalisation"
    ),
    include = c(
      # informations démographiques/atcd
      demo_centre,
      demo_age,
      demo_sexe,
      demo_atcd_hemato,
      demo_atcd_diabete,
      demo_atcd_pancreatite,
      demo_atcd_tumeur,
      # variables à l'admission
      ## gravité
      adm_igs2,
      adm_pancreatite_aigue,
      adm_choc,
      ## cliniques
      adm_poids,
      adm_temp_min,
      adm_temp_max,
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
      adm_amines,
      # variables à l'hémoculture
      ## gravité
      hc_delai,
      hc_choc,
      ## cliniques
      hc_temp_min,
      hc_temp_max,
      hc_diurese_tot,
      hc_diurese_norm,
      ## biologiques
      hc_pfio2_min,
      hc_creat_max,
      hc_uree_max,
      hc_lactates_max,
      hc_lactates_moy,
      hc_leuco_min,
      hc_neutro_min,
      hc_lympho_min,
      hc_glucanes_max,
      hc_mannanes_max,
      ## therapeutiques
      hc_vi_cat,
      hc_dialyse,
      hc_kta,
      hc_vvc,
      hc_ktd,
      hc_ecmo,
      hc_cgr,
      hc_pfc,
      hc_cp,
      hc_transfu,
      hc_amines,
      # variables hospitalisation
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
      hospit_chirurgie_abdominale
    )
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_p() |>
  add_q(method = "bonferroni")
