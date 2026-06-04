imp <- read_rds("donnees/df_impute.rds")

fit <- with(
  imp,
  glmer(
    resultat_candida_def ~ demo_type_rea +
      demo_atcd_hemato +
      demo_atcd_diabete +
      adm_igs2 +
      adm_diurese_norm +
      adm_lactates_max +
      hc_delai +
      hc_diurese_norm +
      hc_pfio2_min +
      hc_lactates_max +
      hc_dialyse +
      hc_vvc +
      hc_ktd +
      hc_transfu +
      hospit_vi_duree +
      hospit_parenterale_duree +
      hospit_vvc_duree +
      hospit_kta_duree +
      hospit_ktd_duree +
      hospit_atb_duree +
      hospit_cgr +
      hospit_pfc +
      hospit_cp +
      hospit_fibro +
      hospit_chirurgie_majeure +
      hospit_chirurgie_abdominale +
      hospit_ctc_duree +
      (1 | iep),
    family = "binomial"
  )
)
# tidy_fit <- broom.mixed::tidy(fit, conf.int = TRUE)
# pooled_results <- pool(tidy_fit)
pooled_results <- pool(fit)
saveRDS(pooled_results, file = "mod_imp_meta.rds")
summary(pooled_results)
# tbl_fit <- tbl_regression(
#   imp,
#   resultat_candida_def ~ . + (1 | iep),
#   exponentiate = TRUE,
#   estimate_fun = "mean",
#   p.value_fun = function(x) {
#     mice::pool(x)$p.value
#   }
# )

# # Afficher le tableau
# tbl_fit |>
#   as_gt() |>
#   gtsave("tbl_rlog_imput.docx")

#===============================================================================
#                            FORREST PLOT
#===============================================================================
summary_results <- summary(pooled_results)
tidy_pooled <- pooled_results$pooled %>%
  mutate(
    term = factor(term, levels = c("(Intercept)", "hospit_ctc_duree")),
    std.error = summary_results$std.error,
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  ) %>%
  filter(term != "(Intercept)")


# Forest Plot avec IC
ggplot(tidy_pooled, aes(x = OR, y = term)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Odds Ratio (log scale)",
    y = "Variable",
    title = "Forest Plot - Modèle poolé avec IC"
  ) +
  scale_x_log10() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, hjust = 0))

#===============================================================================
#                                  AUC/ROC
#===============================================================================
# ===== 1. AUC =====
auc_list <- numeric(imp$m) # Initialise pour toutes les imputations

for (i in 1:imp$m) {
  # Boucle sur TOUTES les imputations (ex: 1:50)
  imp_data <- complete(imp, i)
  fit_i <- glmer(
    resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
    data = imp_data,
    family = "binomial"
  )
  pred_probs <- predict(fit_i, type = "response")
  auc_list[i] <- auc(roc(imp_data$resultat_candida_def, pred_probs))
}

auc_pooled <- list(
  estimate = mean(auc_list),
  conf.int = quantile(auc_list, probs = c(0.025, 0.975))
)

# ===== 2. Courbe ROC =====
roc_list <- lapply(1:imp$m, function(i) {
  # Utilise imp$m pour toutes les imputations
  imp_data <- complete(imp, i)
  fit_i <- glmer(
    resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
    data = imp_data,
    family = "binomial"
  )
  calc_roc(imp_data, fit_i)
})

# Pooler les courbes ROC
all_roc_points <- bind_rows(roc_list, .id = "imputation")
roc_pooled <- all_roc_points %>%
  group_by(fpr = round(fpr, 3)) %>%
  summarise(tpr = mean(tpr), .groups = "drop") %>%
  arrange(fpr)

# ===== 3. Trace la courbe ROC avec l'AUC =====
ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  annotate(
    "text",
    x = 0.95,
    y = 0.05,
    label = paste0("AUC = ", round(auc_pooled$estimate, 3)), # ✅ Corrigé ici
    color = "blue",
    size = 4
  ) +
  labs(
    x = "Taux de faux positifs (1 - Spécificité)",
    y = "Taux de vrais positifs (Sensibilité)",
    title = "Courbe ROC poolée - Modèle mixte"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1)

#===============================================================================
#                                COURBE CALIBRATION
#===============================================================================
# 1. Fonction de calibration manuelle
calc_cal <- function(data) {
  fit_i <- glmer(
    resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
    data = data,
    family = "binomial"
  )
  pred_probs <- predict(fit_i, type = "response")
  bins <- cut(
    pred_probs,
    breaks = quantile(pred_probs, probs = seq(0, 1, length.out = 11)),
    include.lowest = TRUE
  )
  data.frame(
    pred_mean = sapply(levels(bins), function(bin) mean(pred_probs[bins == bin], na.rm = TRUE)),
    observed = sapply(levels(bins), function(bin) {
      mean(data$resultat_candida_def[bins == bin], na.rm = TRUE)
    })
  )
}

# 2. Applique et poole
cal_list <- with(imp, lapply(1:m, function(i) calc_cal(complete(imp, i))))
cal_pooled <- Reduce(
  function(x, y) {
    merge(x, y, by = "pred_mean", suffixes = c("_x", "_y")) %>%
      mutate(observed = (observed_x + observed_y) / 2) %>%
      select(pred_mean, observed)
  },
  cal_list
)

# 3. Trace
ggplot(cal_pooled, aes(x = pred_mean, y = observed)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Probabilité prédite", y = "Probabilité observée", title = "Calibration poolée") +
  theme_minimal()
