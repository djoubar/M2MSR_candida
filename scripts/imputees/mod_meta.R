imp <- read_rds("donnees/df_impute.rds")

fit <- with(
  imp,
  glmer(
    resultat_candida_def ~ hospit_ctc_duree +
      (1 | iep),
    family = "binomial"
  )
)
# tidy_fit <- broom.mixed::tidy(fit, conf.int = TRUE)
# pooled_results <- pool(tidy_fit)
pooled_results <- pool(fit)
# saveRDS(pooled_results, file = "mod_imp_meta.rds")
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
auc_list <- with(
  imp,
  sapply(1:2, function(i) {
    imp_data <- complete(imp, i)
    fit_i <- glmer(
      resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
      data = imp_data,
      family = "binomial"
    )
    pred_probs <- predict(fit_i, type = "response")
    auc(roc(imp_data$resultat_candida_def, pred_probs))
  })
)
# Pooler manuellement (moyenne + IC)
auc_pooled <- list(
  estimate = mean(auc_list),
  conf.int = quantile(auc_list, probs = c(0.025, 0.975))
)
# Affiche l'AUC poolée
auc_pooled


# 1. Fonction pour calculer la courbe ROC sur une imputation
calc_roc <- function(data, fit) {
  # Prédictions
  pred_probs <- predict(fit, type = "response")
  # Courbe ROC
  roc_obj <- roc(data$resultat_candida_def, pred_probs)
  # Extrait les points de la courbe (fpr = 1-specificity, tpr = sensitivity)
  roc_df <- data.frame(
    fpr = 1 - roc_obj$specificities, # False Positive Rate (x-axis)
    tpr = roc_obj$sensitivities, # True Positive Rate (y-axis)
    thresholds = roc_obj$thresholds
  )
  roc_df
}

# 2. Applique à chaque imputation
roc_list <- with(
  imp,
  lapply(1:m, function(i) {
    imp_data <- complete(imp, i)
    fit_i <- glmer(
      resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
      data = imp_data,
      family = "binomial"
    )
    calc_roc(imp_data, fit_i)
  })
)

# 3. Pooler les courbes ROC (moyenne des tpr pour chaque fpr)
# Crée un data frame avec tous les points
all_roc_points <- bind_rows(roc_list, .id = "imputation")

# Calculer la moyenne des tpr pour chaque fpr (arrondi à 3 décimales pour le pooling)
roc_pooled <- all_roc_points %>%
  group_by(fpr = round(fpr, 3)) %>% # Regroupe par fpr arrondi
  summarise(
    tpr = mean(tpr), # Moyenne des tpr
    .groups = "drop"
  ) %>%
  arrange(fpr) # Trie par fpr croissant

# 4. Trace la courbe ROC poolée
ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Ligne AUC = 0.5
  labs(
    x = "Taux de faux positifs (1 - Spécificité)",
    y = "Taux de vrais positifs (Sensibilité)",
    title = "Courbe ROC poolée - Modèle mixte"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1) # Carré pour une meilleure visualisation

# 5. Ajoute l'AUC poolée (déjà calculée précédemment)
auc_value <- summary(auc_pooled)$estimate
ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  annotate(
    "text",
    x = 0.95,
    y = 0.05,
    label = paste0("AUC = ", round(auc_value, 3)),
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
