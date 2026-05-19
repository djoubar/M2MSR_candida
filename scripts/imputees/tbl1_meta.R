imp <- read_rds("donnees/df_impute.rds")
sous_groupe <- lapply(1:5, function(i) complete(imp, i))

fit <- with(
  imp,
  glmer(
    resultat_candida_def ~ demo_centre +
      hc_transfu +
      demo_atcd_diabete +
      hc_dialyse +
      hc_choc +
      hc_catheter_majeur +
      hospit_parenterale_duree +
      hospit_chirurgie_abdominale +
      hospit_atb_duree +
      hc_delai +
      hc_vi_cat +
      demo_atcd_hemato +
      (1 | iep),
    family = "binomial"
  )
)
tidy_fit <- broom.mixed::tidy(fit, conf.int = TRUE)
pooled_results <- pool(tidy_fit)
pooled_results <- pool(fit)
saveRDS(pooled_results, file = "mod_imp_meta.rds")
summary(pooled_results)

tbl_fit <- tbl_regression(
  fit,
  exponentiate = TRUE,
  estimate_fun = "mean",
  p.value_fun = function(x) {
    mice::pool(x)$p.value
  }
)

# # Afficher le tableau
# tbl_fit |>
#   as_gt() |>
#   gtsave("tbl_rlog_imput.docx")

# pred_cond <- predict(fit, type = "response", re.form = NULL)
