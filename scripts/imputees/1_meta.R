imp <- read_rds("donnees/df_impute.rds")

fit <- with(
  imp,
  glmer(
    resultat_candida_def ~ hospit_ctc_duree +
      (1 | iep),
    family = "binomial"
  )
)
tidy_fit <- broom.mixed::tidy(fit, conf.int = TRUE)
pooled_results <- pool(tidy_fit)
pooled_results <- pool(fit)
# saveRDS(pooled_results, file = "mod_imp_meta.rds")
summary(pooled_results)
tbl_fit <- tbl_regression(
  imp,
  resultat_candida_def ~ . + (1 | iep),
  exponentiate = TRUE,
  estimate_fun = "mean",
  p.value_fun = function(x) {
    mice::pool(x)$p.value
  }
)

# Afficher le tableau
tbl_fit |>
  as_gt() |>
  gtsave("tbl_rlog_imput.docx")

# pred_cond <- predict(fit, type = "response", re.form = NULL)
