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
    missing = "no",
    include = c(
      # informations démographiques/atcd
      demo_type_rea,
      demo_uf,
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
      adm_neutro_min,
      adm_lympho_min,
      # ## thérapeutiques
      adm_vi_cat,
      adm_amines
    ),
    labels(
      demo_type_rea ~ "Type de Réanimation",
      demo_uf ~ "Unité Fonctionelle",
      adm_sofa_tot ~ "Score SOFA à l'admission",
      adm_choc ~ "Choc à l'admission",
      adm_amines ~ "Amines à l'admission"
    )
  ) |>
  bold_labels() |>
  italicize_levels()
