################################################################################
#                                                                              #
#                            M2MSR_TBL2_DEMOGRAPHIQUE                          #
#                                                                              #
################################################################################
.df_tbl2 <- df_base |>
  select(
    -demo_uf,
    -demo_centre,
    -starts_with("adm_sofa"),
    -adm_plasmalyte,
    -adm_ringer,
    -adm_gelo,
    -adm_adre,
    -adm_albu_20,
    -adm_albu_4,
    -adm_bicar,
    -adm_nacl,
    -adm_dobu,
    -adm_isoprenaline,
    -adm_terlipressine,
    -adm_noradre,
    -starts_with("hc_sofa"),
    -hc_plasmalyte,
    -hc_ringer,
    -hc_gelo,
    -hc_adre,
    -hc_albu_20,
    -hc_albu_4,
    -hc_bicar,
    -hc_dobu,
    -hc_isoprenaline,
    hc_nacl,
    -hc_voluven,
    -hc_noradre,
    -hc_terlipressine
  )

tbl2_demo <-
  tbl_summary(
    data = .df_tbl2,
    by = "resultat_candida_def",
    missing = "no",
    statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
    label = list(
      demo_atcd_hemato = "Antécédent de maladie hématologique maligne",
      demo_type_rea = "Hospitalisation en réanimation médicale",
      demo_age = "Age",
      demo_sexe = "Sexe",
      demo_atcd_tumeur = "Antécédent de tumeur solide",
      demo_atcd_pancreatite = "Antécédent de pancréatite aigue",
      demo_atcd_diabete = "Antécédent de diabète",
      demo_atcd_transplantation = "Antécédent de transplantation d'organe solide"
    ),
    include = c(all_of(starts_with("demo"))),
    type = list(
      c(
        demo_sexe,
        demo_atcd_hemato,
        demo_atcd_diabete,
        demo_atcd_pancreatite,
        demo_atcd_tumeur,
        demo_atcd_transplantation
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        demo_atcd_hemato,
        demo_atcd_diabete,
        demo_atcd_pancreatite,
        demo_atcd_tumeur,
        demo_atcd_transplantation
      ) ~ "Oui",
      demo_type_rea ~ "Medicale",
      demo_sexe ~ "Masculin"
    )
  ) |>
  italicize_levels() |>
  add_p() |>
  add_q(method = "fdr")

tbl2_adm <-
  tbl_summary(
    data = .df_tbl2,
    by = "resultat_candida_def",
    missing = "no",
    statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
    label = list(
      adm_igs2 = "IGS 2",
      adm_choc = "Etat de choc",
      adm_pancreatite_aigue = "Pancréatite aigue",
      adm_poids = "Poids",
      adm_vi_cat = "Ventilation mécanique invasive",
      adm_pfio2_min = "PaO2/FiO2 minimal",
      adm_creat_max = "Créatinémie maximale (en mg/L)",
      adm_uree_max = "Urée maximale (en g/L)",
      adm_dialyse = "Dialyse",
      adm_temp_min = "Température minimale (en °C)",
      adm_temp_max = "Température maximale (en °C)",
      adm_diurese_tot = "Diurèse totale",
      adm_diurese_norm = "Diurèse normalisée",
      adm_lactates_max = "Lactatémie maximale (en mmol/L)",
      adm_lactates_moy = "Lactatémie moyenne (en mmol/L) ",
      adm_leuco_min = "Leucocytes (en G/L)",
      adm_neutro_min = "Neutrophiles (en G/L)",
      adm_lympho_min = "Lymphocytes (en G/L)",
      adm_cgr = "Culot Globulaires",
      adm_pfc = "Plasma frais congelé",
      adm_cp = "Culot plaquettaire",
      adm_transfu = "Transfusion",
      adm_amines = "Amines "
    ),
    include = c(all_of(starts_with("adm"))),
    type = list(
      c(
        adm_pancreatite_aigue,
        adm_choc,
        adm_amines,
        adm_vi_cat,
        adm_dialyse,
        adm_cgr,
        adm_pfc,
        adm_cp,
        adm_transfu
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        adm_pancreatite_aigue,
        adm_choc,
        adm_vi_cat,
        adm_amines,
        adm_vi_cat,
        adm_dialyse,
        adm_cgr,
        adm_pfc,
        adm_cp,
        adm_transfu,
      ) ~ "Oui"
    )
  ) |>
  italicize_levels() |>
  add_p() |>
  add_q(method = "fdr")

tbl2_hc <-
  tbl_summary(
    data = .df_tbl2,
    by = "resultat_candida_def",
    missing = "no",
    statistic = list(
      all_continuous() ~ "{median} ({min}, {max})",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    include = c(all_of(starts_with("hc")), -hc_catheter_majeur, -hc_nacl),
    label = list(
      hc_delai = "Délai entre admission et prélèvements de l'hémoculture",
      hc_choc = "Etat de choc",
      hc_transfu = "Transfusion ", # Utilisée ici
      hc_amines = "Amines ",
      hc_temp_min = "Température minimale ",
      hc_temp_max = "Température maximale ",
      hc_diurese_tot = "Diurèse totale ",
      hc_diurese_norm = "Diurèse normalisée ",
      hc_pfio2_min = "PaO2/FiO2 ",
      hc_creat_max = "Créatinémie ",
      hc_uree_max = "Urémie ",
      hc_lactates_max = "Lactatémie maximale",
      hc_lactates_moy = "Lactatémie moyenne ",
      hc_leuco_min = "Leucocytes (en G/L)",
      hc_neutro_min = "Neutropénie min ",
      hc_lympho_min = "Lymphopénie ",
      hc_glucanes_max = "Glucanes positifs",
      hc_mannanes_max = "Mannanes positifs",
      hc_vi_cat = "Ventilation invasive ",
      hc_dialyse = "Dialyse ",
      hc_kta = "KTA ",
      hc_vvc = "VVC ",
      hc_ktd = "Cathéter de dialyse ",
      hc_ecmo = "ECMO",
      hc_cgr = "CGR",
      hc_pfc = "PFC",
      hc_cp = "CP"
    ),
    type = list(
      c(
        hc_choc,
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
        hc_glucanes_max,
        hc_mannanes_max
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        hc_choc,
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
        hc_amines
      ) ~ "Oui",
      c(hc_glucanes_max, hc_mannanes_max) ~ "Positif"
    )
  ) |>
  italicize_levels() |>
  add_p() |>
  add_q(method = "fdr")

tbl2_hospit <-
  tbl_summary(
    data = .df_tbl2,
    by = "resultat_candida_def",
    missing = "no",
    statistic = list(
      all_continuous() ~ "{median} ({min}, {max})",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    label = list(
      hospit_vi_duree = "Durée de ventilation invasive ",
      hospit_parenterale_duree = "Durée de nutrition parentérale ",
      hospit_vvc_duree = "Durée de VVC ",
      hospit_kta_duree = "Durée de KTA ",
      hospit_ktd_duree = "Durée de KTD ",
      hospit_ecmo_duree = "Durée ECMO ",
      hospit_atb_duree = "Durée ATBT ",
      hospit_ctc_duree = "Durée CTC ",
      hospit_immunosup_duree = "Durée immunosuppression ",
      hospit_neutropen_duree = "Durée neutropénie ",
      hospit_neutrophi_duree = "Durée neutrophilie ",
      hospit_lymphopenie_duree = "Durée lymphopénie ",
      hospit_cgr = "Nombres CGR avant l'hémoculture",
      hospit_pfc = "Nombres PFC avant l'hémoculture",
      hospit_cp = "Nombres CP avant l'hémoculture",
      hospit_fibro = "Fibroscopie pendant l'hospitalisation",
      hospit_chirurgie_majeure = "Chirurgie majeure pendant l'hospitalisation",
      hospit_chirurgie_abdominale = "Chirurgie abdominale pendant l'hospitalisation"
    ),
    include = c(all_of(starts_with("hospit"))),
    type = list(
      c(
        hospit_fibro,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        hospit_fibro,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale
      ) ~ "Oui"
    )
  ) |>
  italicize_levels() |>
  add_p() |>
  add_q(method = "fdr")

tbl2 <-
  tbl_stack(
    list(tbl2_demo, tbl2_adm, tbl2_hc, tbl2_hospit),
    group_header = c(
      "Données démographiques",
      "Données à l'admission",
      "Données dans les 48 heures précédant la suspicion d'infection",
      "Données au cours de l'hospitalisation préalablement à l'hémoculture"
    )
  ) |>
  as_gt() |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )
