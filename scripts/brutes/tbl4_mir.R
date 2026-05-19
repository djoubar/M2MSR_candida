df_mir <- df_base |>
  subset(demo_centre == "SLG")

tbl2 <-
  tbl_summary(
    data = df_mir,
    by = "resultat_candida_def",
    missing = "no"
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_p() |>
  add_q(method = "fdr")

df_meta_mir <- df_mir |>
  mutate(
    hospit_atb_duree_72 = as.factor(ifelse(hospit_atb_duree > 3, "Oui", "Non")),
    hospit_parenterale = as.factor(ifelse(hospit_parenterale_duree > 0, "Oui", "Non"))
  ) |>
  select(
    iep,
    resultat_candida_def,
    hc_transfu,
    demo_atcd_diabete,
    hc_dialyse,
    hc_choc,
    hc_catheter_majeur,
    hospit_parenterale,
    hospit_chirurgie_abdominale,
    hospit_atb_duree_72,
    hc_delai,
    hc_vi_cat,
    demo_atcd_hemato,
  ) |>
  na.omit()

mod_meta_mir <- glmer(
  resultat_candida_def ~ . - iep + (1 | iep),
  data = df_meta_mir,
  family = "binomial"
)
tbl3 <- tbl_regression(mod_meta_mir, exponentiate = TRUE)
summary(mod_meta_mir)
