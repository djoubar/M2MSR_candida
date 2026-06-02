df_meta <- df_base |>
  mutate(
    hospit_atb_duree_72 = as.factor(ifelse(hospit_atb_duree > 3, "Oui", "Non")),
    hospit_parenterale = as.factor(ifelse(hospit_parenterale_duree > 0, "Oui", "Non"))
  ) |>
  select(
    iep,
    resultat_candida_def,
    demo_centre,
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

mod_meta <- glmer(
  resultat_candida_def ~ demo_centre +
    hc_transfu +
    demo_atcd_diabete +
    hc_dialyse +
    hc_choc +
    hc_catheter_majeur +
    hospit_parenterale +
    hospit_chirurgie_abdominale +
    hospit_atb_duree_72 +
    hc_delai +
    hc_vi_cat +
    demo_atcd_hemato +
    (1 | iep),
  data = df_meta,
  family = "binomial",
  na.action = na.fail
)
tbl3 <- tbl_regression(mod_meta, exponentiate = TRUE)
summary(mod_meta)

df_meta$score <- predict(mod_meta, type = "response")

roc_meta <- ggroc(roc_obj, colour = "black", size = 0.5) +
  ggtitle(paste("Modèle Forward - AUC =", round(auc(roc_meta), 3))) +
  theme_minimal()

# # Courbe calibration
# dd <- datadist(df_base)
# options(datadist = "dd")
# df_meta$pred_cond <- pred_cond
# df_meta$hc_delai <- as.numeric(df_meta$hc_delai, units = "days")
# calibrate_glmm <- function(
#   mod_meta,
#   df_meta,
#   pred_col = "pred_cond",
#   y_col = "resultat_candida_def"
# ) {
#   # Créer un objet lrm (nécessaire pour calibrate)
#   fit <- lrm(
#     as.formula(paste(
#       y_col,
#       "~",
#       paste(setdiff(names(df_meta), c(y_col, pred_col, "iep")), collapse = "+")
#     )),
#     data = df_meta,
#     x = TRUE,
#     y = TRUE
#   )
#   cal <- calibrate(fit, B = 100, method = "boot")
#   plot(cal)
#   return(cal)
# }

# # Exécuter
# cal_result <- calibrate_glmm(mod_meta, df_meta)
