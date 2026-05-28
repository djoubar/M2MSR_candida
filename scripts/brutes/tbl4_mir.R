df_mir <- df_base |>
  subset(demo_centre == "SLG")

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

# mod_meta_mir <- glmer(
#   resultat_candida_def ~
#     hc_transfu +
#     demo_atcd_diabete +
#     hc_dialyse +
#     hc_choc +
#     hc_catheter_majeur +
#     hospit_parenterale +
#     hospit_chirurgie_abdominale +
#     hospit_atb_duree_72 +
#     hc_delai +
#     hc_vi_cat +
#     demo_atcd_hemato +
#     (1 | iep),
#   data = df_meta_mir,
#   family = "binomial"
# )

mod_meta_mir <- read_rds("models/mod_brutes_meta_mir.RDS")
tbl4 <- tbl_regression(mod_meta_mir, exponentiate = TRUE)

pred_cond <- predict(mod_meta_mir, type = "response", re.form = NULL)
auc_cond <- pROC::roc(df_meta_mir$resultat_candida_def, pred_cond)
auc_cond$auc
g1_auc_meta_mir <- plot(auc_cond, main = "Courbe ROC (prédictions conditionnelles)")


# Courbe calibration
dd <- datadist(df_base)
options(datadist = "dd")
df_meta_mir$pred_cond <- pred_cond
df_meta_mir$hc_delai <- as.numeric(df_meta_mir$hc_delai, units = "days")
calibrate_glmm <- function(
  mod_meta_mir,
  df_meta_mir,
  pred_col = "pred_cond",
  y_col = "resultat_candida_def"
) {
  # Créer un objet lrm (nécessaire pour calibrate)
  fit <- lrm(
    as.formula(paste(
      y_col,
      "~",
      paste(setdiff(names(df_meta_mir), c(y_col, pred_col, "iep")), collapse = "+")
    )),
    data = df_meta_mir,
    x = TRUE,
    y = TRUE
  )
  cal <- calibrate(fit, B = 100, method = "boot")
  plot(cal)
  return(cal)
}

# Exécuter
calibrate_mod_meta_mir <- calibrate_glmm(mod_meta_mir, df_meta_mir)
