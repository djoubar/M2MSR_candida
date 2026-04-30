################################################################################
#                                                                              #
#                            M2MSR_TBL1_DEMOGRAPHIQUE                          #
#                                                                              #
################################################################################
.df_tbl1 <- df_base

theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
set_flextable_defaults(
  font.family = "Serif",
  font.size = 12,
  padding = 2,
  border.color = "#CCCCCC",
  line_spacing = 1.3,
  line_width = 2
)

# Selection des donnees patients (1 par iep)
.df_tbl1 <- .df_tbl1 |>
  arrange(iep, date_hemoc) |>
  distinct(iep, .keep_all = TRUE)

tbl1 <-
  tbl_summary(
    data = .df_tbl1,
    missing = "ifany",
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
      adm_sofa_tot,
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
      adm_neutro_min,
      adm_lympho_min,
      # ## thérapeutiques
      #   adm_vi_cat, adm_dialyse, adm_cgr, adm_pfc, adm_cp, adm_albu_20,
      #   adm_albu_4, adm_bicar, adm_gelo, adm_nacl, adm_plasmalyte, adm_ringer,
      # # variables à l'hémoculture
      #   ## gravité
      #    hc_sofa_tot,
      #   ## cliniques
      #     hc_temp_min,hc_temp_max,hc_diurese_tot,hc_diurese_norm,
      #   ## biologiques
      #     hc_pfio2_min, hc_creat_max, hc_uree_max, hc_lactates_max, hc_lactates_moy,
      #     hc_neutro_min, hc_lympho_min, hc_glucanes_max, hc_mannanes_max,
      #   ## therapeutiques
      #     hc_vi_cat,hc_dialyse,hc_kta,hc_vvc,hc_ktd,hc_ecmo,hc_cgr,hc_pfc,hc_cp,
      #     hc_albu_20,hc_albu_4,hc_bicar,hc_gelo,hc_nacl,hc_plasmalyte,hc_ringer,
      #     hc_voluven,hc_adre,hc_dobu,hc_isoprenaline,hc_noradre,hc_terlipressine,
      # # variables hospitalisation
      #   hospit_vi_duree, hospit_parenterale_duree, hospit_vvc_duree, hospit_kta_duree,
      #   hospit_ktd_duree,hospit_ecmo_duree,hospit_atb_duree,hospit_ctc_duree,
      #   hospit_immunosup_duree, hospit_neutropen_duree, hospit_neutrophi_duree,
      #   hospit_lymphopenie_duree, hospit_cgr,hospit_pfc,hospit_cp,hospit_fibro
    )
  ) |>
  bold_labels() |>
  italicize_levels()
# |>
#   as_flex_table() |>
#   width(width = 4, unit = "in") |>
#   fontsize(size = 8, part= "all")
