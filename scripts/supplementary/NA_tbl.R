# ==============================================================================
#
#                            TABLEAU VALEUR MANQUANTES
#
# ==============================================================================
library(gtsummary)
library(patchwork)

if (!exists("df_base")) {
  source("scripts/brutes/_setup.R")
}

# tbl_NA <- tbl_custom_summary(
#   df_base,
#   missing = "no",
#   include = c(
#     -id_hemoc,
#     -iep,
#     -demo_centre,
#     -demo_uf,
#     -adm_sofa_respi,
#     -adm_sofa_cardio,
#     -adm_sofa_coag,
#     -adm_sofa_hepatique,
#     -adm_sofa_neuro,
#     -hc_sofa_respi,
#     -hc_sofa_cardio,
#     -hc_sofa_coag,
#     -hc_sofa_hepatique,
#     -hc_sofa_neuro
#   ),
#   stat_fns = ~missing,
#   statistic = everything() ~ "{N_miss} ({p_miss} %)",
#   type = list(
#     c(
#       resultat_candida_def,
#       deces_rea,
#       demo_type_rea,
#       demo_sexe,
#       demo_atcd_hemato,
#       demo_atcd_diabete,
#       demo_atcd_pancreatite,
#       demo_atcd_tumeur,
#       adm_pancreatite_aigue,
#       demo_atcd_transplantation,
#       adm_choc,
#       adm_vi_cat,
#       adm_cgr,
#       adm_pfc,
#       adm_cp,
#       adm_transfu,
#       hc_choc,
#       hc_glucanes_max,
#       hc_mannanes_max,
#       hc_vi_cat,
#       hc_dialyse,
#       hc_kta,
#       hc_vvc,
#       hc_ktd,
#       hc_ecmo,
#       hc_catheter_majeur,
#       hc_cgr,
#       hc_pfc,
#       hc_transfu,
#       hc_cp,
#       hc_amines,
#       hospit_fibro,
#       hospit_chirurgie_majeure,
#       hospit_chirurgie_abdominale,
#       adm_amines
#     ) ~ "dichotomous"
#   ),
#   value = list(
#     c(
#       resultat_candida_def,
#       deces_rea,
#       demo_atcd_hemato,
#       demo_atcd_diabete,
#       demo_atcd_pancreatite,
#       demo_atcd_tumeur,
#       adm_pancreatite_aigue,
#       demo_atcd_transplantation,
#       adm_choc,
#       adm_vi_cat,
#       adm_cgr,
#       adm_pfc,
#       adm_cp,
#       adm_transfu,
#       hc_choc,
#       hc_glucanes_max,
#       hc_mannanes_max,
#       hc_vi_cat,
#       hc_dialyse,
#       hc_kta,
#       hc_vvc,
#       hc_ktd,
#       hc_ecmo,
#       hc_catheter_majeur,
#       hc_transfu,
#       hc_cgr,
#       hc_pfc,
#       hc_cp,
#       hc_amines,
#       hospit_fibro,
#       hospit_chirurgie_majeure,
#       hospit_chirurgie_abdominale,
#       adm_amines
#     ) ~ "Oui",
#     c(hc_catheter_majeur) ~ "1",
#     c(hc_glucanes_max, hc_mannanes_max) ~ "Positif",
#     demo_type_rea ~ "Medicale",
#     demo_sexe ~ "Masculin",
#     demo_centre ~ "SLG",

#   )
# ) |>
#   add_overall() |>
#   modify_footnote_header(
#     footnote = "Nombre de NA (% de NA)",
#     columns = all_stat_cols()
#   )

# ==============================================================================
#
#                           TABLEAU VALEURS MANQUANTES
#
# ==============================================================================

tbl_na_demo <-
  tbl_custom_summary(
    data = df_base,
    missing = "no",
    stat_fns = ~missing,
    statistic = everything() ~ "{N_miss} ({p_miss} %)",
    include = c(all_of(starts_with("demo")), -demo_uf),
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
    type = list(
      c(
        demo_sexe,
        demo_atcd_hemato,
        demo_atcd_diabete,
        demo_atcd_pancreatite,
        demo_atcd_tumeur,
        demo_atcd_transplantation,
        demo_centre
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
      demo_sexe ~ "Masculin",
      demo_centre ~ "SLG"
    )
  ) |>
  add_overall() |>
  modify_footnote_header(
    footnote = "Nombre de NA (% de NA)",
    columns = all_stat_cols()
  )

tbl_na_adm <-
  tbl_custom_summary(
    data = df_base,
    missing = "no",
    stat_fns = ~missing,
    statistic = everything() ~ "{N_miss} ({p_miss} %)",
    include = c(
      all_of(starts_with("adm")),
      -all_of(
        starts_with("adm_sofa")
      )
    ),
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
        adm_transfu,
        adm_hypothermie,
        adm_fievre,
        adm_pfio2_min
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        adm_pancreatite_aigue,
        adm_choc,
        adm_vi_cat,
        adm_amines,
        adm_dialyse,
        adm_cgr,
        adm_pfc,
        adm_cp,
        adm_transfu,
        adm_hypothermie,
        adm_fievre
      ) ~ "Oui",
      adm_pfio2_min = "< 100"
    )
  ) |>
  add_overall() |>
  modify_footnote_header(
    footnote = "Nombre de NA (% de NA)",
    columns = all_stat_cols()
  )

tbl_na_hc <-
  tbl_custom_summary(
    data = df_base,
    missing = "no",
    stat_fns = ~missing,
    statistic = everything() ~ "{N_miss} ({p_miss} %)",
    include = c(
      all_of(starts_with("hc")),
      -all_of(
        starts_with("hc_sofa")
      ),
      -hc_deficit_neutro,
      -hc_deficit_lympho
    ),
    label = list(
      hc_delai = "Délai entre admission et prélèvements de l'hémoculture",
      hc_choc = "Etat de choc",
      hc_transfu = "Transfusion",
      hc_amines = "Amines",
      hc_temp_min = "Température minimale",
      hc_temp_max = "Température maximale",
      hc_diurese_tot = "Diurèse totale",
      hc_diurese_norm = "Diurèse normalisée",
      hc_pfio2_min = "PaO2/FiO2",
      hc_creat_max = "Créatinémie maximale",
      hc_uree_max = "Urémie maximale",
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
      hc_ktd = "Cathéter de dialyse",
      hc_ecmo = "ECMO",
      hc_cgr = "CGR",
      hc_pfc = "PFC",
      hc_cp = "CP",
      hc_catheter_majeur = "Cathéter central (VVC, KTD, ECMO)",
      hc_antifongique = "Antifongique"
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
        hc_mannanes_max,
        hc_hypothermie,
        hc_fievre,
        hc_pfio2_min,
        hc_catheter_majeur,
        hc_antifongique
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
        hc_amines,
        hc_hypothermie,
        hc_fievre,
        hc_antifongique
      ) ~ "Oui",
      c(hc_glucanes_max, hc_mannanes_max) ~ "Positif",
      hc_pfio2_min ~ "< 100",
      hc_catheter_majeur ~ "1"
    )
  ) |>
  add_overall() |>
  modify_footnote_header(
    footnote = "Nombre de NA (% de NA)",
    columns = all_stat_cols()
  )

tbl_na_hospit <-
  tbl_custom_summary(
    data = df_base,
    missing = "no",
    stat_fns = ~missing,
    statistic = everything() ~ "{N_miss} ({p_miss} %)",
    include = starts_with("hospit"),
    label = list(
      hospit_vi_duree = "Durée de ventilation invasive ",
      hospit_parenterale_duree = "Durée de nutrition parentérale ",
      hospit_vvc_duree = "Durée de VVC",
      hospit_kta_duree = "Durée de KTA",
      hospit_ktd_duree = "Durée de KTD",
      hospit_ecmo_duree = "Durée ECMO",
      hospit_atb_duree = "Durée ATBT",
      hospit_ctc_duree = "Durée CTC",
      hospit_immunosup_duree = "Durée de immunosuppression ",
      hospit_neutropen_duree = "Durée de neutropénie ",
      hospit_neutrophi_duree = "Durée de neutrophilie ",
      hospit_lymphopenie_duree = "Durée de lymphopénie ",
      hospit_cgr = "Nombres CGR",
      hospit_pfc = "Nombres PFC",
      hospit_cp = "Nombres CP",
      hospit_fibro = "Fibroscopie",
      hospit_chirurgie_majeure = "Chirurgie majeure",
      hospit_chirurgie_abdominale = "Chirurgie abdominale",
      hospit_chirurgie_susmesocolique = "Chirurgie sus-mésocolique",
      hospit_chirurgie_hepatobiliaire = "Chirurgie hepato-biliaire"
    ),
    type = list(
      c(
        hospit_fibro,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale,
        hospit_chirurgie_susmesocolique,
        hospit_chirurgie_hepatobiliaire
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        hospit_fibro,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale,
        hospit_chirurgie_susmesocolique,
        hospit_chirurgie_hepatobiliaire
      ) ~ "Oui"
    )
  ) |>
  add_overall() |>
  modify_footnote_header(
    footnote = "Nombre de NA (% de NA)",
    columns = all_stat_cols()
  )

extract_pct <- function(x) {
  as.numeric(stringr::str_extract(x, "(?<=\\()[0-9.]+(?=\\s?%\\))"))
}

tbl_NA <-
  tbl_stack(
    list(tbl_na_demo, tbl_na_adm, tbl_na_hc, tbl_na_hospit),
    group_header = c(
      "Données démographiques",
      "Données à l'admission",
      "Données dans les 48 heures précédant la suspicion d'infection",
      "Données au cours de l'hospitalisation préalablement à l'hémoculture"
    )
  ) |>
  modify_table_styling(
    columns = stat_0,
    rows = extract_pct(stat_0) > 30,
    text_format = "bold"
  ) |>
  as_gt() |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )
