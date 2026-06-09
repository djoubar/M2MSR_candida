################################################################################
#                                                                              #
#                            M2MSR_TBL1_DEMOGRAPHIQUE                          #
#                                                                              #
################################################################################
df_tbl1 <- df_base

# Selection des donnees patients (1 par iep)
df_tbl1 <- df_tbl1 |>
  arrange(iep, date_hemoc) |>
  distinct(iep, .keep_all = TRUE)


tbl1 <-
  tbl_summary(
    data = df_tbl1,
    missing = "no",
    include = c(
      # informations démographiques/atcd
      # demo_uf,
      demo_age,
      demo_sexe,
      demo_atcd_hemato,
      demo_atcd_diabete,
      demo_atcd_pancreatite,
      demo_atcd_tumeur,
      demo_type_rea,
      demo_atcd_transplantation,
      # variables à l'admission
      ## gravité
      adm_igs2,
      # adm_pancreatite_aigue,
      # adm_sofa_tot,
      # adm_choc,
      ## cliniques
      # adm_poids,
      # adm_temp_min,
      # adm_temp_max,
      # adm_diurese_tot,
      # adm_diurese_norm,
      ## biologiques
      # adm_creat_max,
      # adm_uree_max,
      # adm_pfio2_min,
      # adm_lactates_max,
      # adm_lactates_moy,
      # adm_leuco_min,
      # adm_neutro_min,
      # adm_lympho_min,
      # ## thérapeutiques
      adm_vi_cat,
      adm_amines,
      hc_delai,
      deces_rea,
      hospit_chirurgie_majeure,
      hospit_chirurgie_abdominale
    ),
    label = list(
      demo_sexe = "Sexe masculin",
      demo_type_rea = "Hospitalisation en réanimation médicale",
      demo_uf = "Unité Fonctionelle",
      demo_atcd_transplantation = "Antécédent de transplantation",
      # adm_sofa_tot = "Score SOFA",
      adm_choc = "Etat de choc",
      adm_poids = "Poids",
      adm_temp_min = "Température minimale (en °C)",
      adm_temp_max = "Température maximale (en °C)",
      adm_creat_max = "Créatinémie maximale (en mg/L)",
      adm_pfio2_min = "Rapport PaO2/FiO2 minimal",
      adm_lactates_max = "Lactatémie maximale (en mmol/L)",
      adm_leuco_min = "Leucocytémie (en G/L)",

      adm_amines = "Administration d'amines",
      hc_delai = "Délai entre admission & hémoculture",
      deces_rea = "Décès en réanimation"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({min}, {max})",
      all_categorical() ~
        "{n} ({p}%)"
    ),
    type = list(
      c(
        demo_sexe,
        demo_type_rea,
        demo_atcd_hemato,
        demo_atcd_diabete,
        demo_atcd_pancreatite,
        demo_atcd_tumeur,
        demo_atcd_transplantation,
        # adm_pancreatite_aigue,
        adm_choc,
        adm_vi_cat,
        adm_amines,
        deces_rea,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        demo_atcd_hemato,
        demo_atcd_diabete,
        demo_atcd_pancreatite,
        demo_atcd_tumeur,
        demo_atcd_transplantation,
        # adm_pancreatite_aigue,
        # adm_choc,
        # adm_vi_cat,
        adm_amines,
        deces_rea,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale
      ) ~ "Oui",
      demo_type_rea ~ "Medicale",
      demo_sexe ~ "Masculin"
    )
  ) |>
  bold_labels() |>
  italicize_levels()
