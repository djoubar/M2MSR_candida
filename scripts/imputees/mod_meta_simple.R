# =============================================================================
# Modele simplifié
# =============================================================================
library(mice)
library(tidyverse)
library(gtsummary)
library(gt)
library(pROC)
library(broom)
library(rms)

imp <- readRDS("donnees/df_impute_surv.rds")
m_imputations <- imp$m
cat("Nombre de datasets imputés (m) :", m_imputations, "\n\n")

formule_glm <- resultat_candida_def ~
  hc_vi_cat +
  hc_transfu +
  hc_dialyse +
  adm_igs2 +
  hospit_ctc_duree +
  hc_catheter_majeur +
  hospit_cgr +
  hospit_chirurgie_abdominale

fit_imp <- with(
  imp,
  glm(
    resultat_candida_def ~
      hc_vi_cat +
      hc_transfu +
      hc_dialyse +
      adm_igs2 +
      hospit_ctc_duree +
      hc_catheter_majeur +
      hospit_cgr +
      hospit_chirurgie_abdominale,
    family = binomial(link = "logit")
  )
)

resultats_pool <- pool(fit_imp)
resume_logodds <- summary(resultats_pool, conf.int = TRUE, exponentiate = FALSE)
resume_OR <- summary(resultats_pool, conf.int = TRUE, exponentiate = TRUE)
cat("\n=== RÉSULTATS POOLÉS (échelle log-odds) ===\n")
print(resume_logodds, digits = 3)
cat("\n=== RÉSULTATS POOLÉS (Odds Ratios) ===\n")
print(resume_OR, digits = 3)


# ==================================================================================================
#                                        FOREST PLOT
# ==================================================================================================

niveaux_termes <- c(
  "hc_vi_cat",
  "hc_transfu",
  "hc_dialyse",
  "adm_igs2",
  "hospit_ctc_duree",
  "hc_catheter_majeur",
  "hospit_cgr",
  "hospit_chirurgie_abdominale"
)

tidy_pooled <- resume_logodds %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  )

labels_lisibles <- c(
  "hc_vi_catOui" = "Ventilation mécanique invasive dans les 48h précédant l'épisode de suspicion d'infection",
  "hc_transfuOui" = "Transfusion (CGR/PFC/CP)",
  "hc_dialyseOui" = "Epuration extra-rénale",
  "adm_igs2" = "IGS 2 à l'admission",
  "hospit_ctc_duree" = "Durée corticothérapie (en jours)",
  "hc_catheter_majeurOui" = "Cathéter veineux central dans les 48h précédant l'épisode de suspicion d'infection",
  "hospit_cgr" = "Nombre de CGR administrés au cours de l'hospitalisation",
  "hospit_chirurgie_abdominaleOui" = "Chirurgie abdominale au cours de l'hospitalisation"
)

tidy_pooled <- resume_logodds %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high),
    label = case_when(
      term %in% names(labels_lisibles) ~ labels_lisibles[term],
      TRUE ~ term
    ),
    label = factor(label, levels = rev(unique(label)))
  )

forest_plot <- ggplot(tidy_pooled, aes(x = OR, y = label)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(xmin = OR_low, xmax = OR_high), height = 0.25, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (échelle log)",
    y = NULL,
    title = "Forest Plot — Modèle poolé (règle de Rubin)",
    subtitle = "Facteurs prédictifs de candidémie — IC à 95 %"
  ) +
  theme_classic(base_size = 12) +
  theme(axis.text.y = element_text(size = 10, hjust = 1))

print(forest_plot)
ggsave(filename = "figures/fp_imp.png", plot = forest_plot)


# ==================================================================================================
#                                  DISCRIMINATION : AUC & COURBE ROC
# ==================================================================================================

# tentative simplification
preds <- sapply(fit_imp$analyses, function(m) predict(m, type = "response"))
pred_moy <- rowMeans(preds)
var_predire <- complete(imp, 1)
var_predire <- var_predire$resultat_candida_def
roc_obj <- roc(var_predire, pred_moy)
auc(roc_obj)
ci.auc(roc_obj)
val.prob(pred_moy, Y_obs)
# =================================
n_imp <- imp$m
auc_list <- numeric(n_imp)
roc_list <- vector("list", n_imp)
cal_list <- vector("list", n_imp)

for (i in seq_len(n_imp)) {
  imp_data <- complete(imp, i)
  fit_i <- tryCatch(
    glm(formule_glm, data = imp_data, family = binomial(link = "logit")),
    error = function(e) {
      warning("Erreur dans glm pour l'imputation ", i, ": ", e$message)
      return(NULL)
    }
  )
  if (is.null(fit_i) || !fit_i$converged) {
    warning("Modèle non convergent pour l'imputation ", i, ". AUC = NA.")
    auc_list[i] <- NA
    next # Passe à l'imputation suivante
  }
  probs <- predict(fit_i, type = "response")
  outcome <- imp_data$resultat_candida_def

  if (length(unique(outcome)) == 1) {
    warning("Outcome constant pour l'imputation ", i, ". AUC = NA.")
    auc_list[i] <- NA
    next
  }
  roc_i <- tryCatch(
    roc(outcome, probs, quiet = TRUE),
    error = function(e) {
      warning("Erreur dans roc() pour l'imputation ", i, ": ", e$message)
      return(NULL)
    }
  )
  if (is.null(roc_i)) {
    auc_list[i] <- NA
  } else {
    auc_list[i] <- auc(roc_i)
    roc_list[[i]] <- data.frame(
      fpr = 1 - roc_i$specificities,
      tpr = roc_i$sensitivities
    )
    deciles <- ntile(probs, 10)
    cal_list[[i]] <- data.frame(
      decile = 1:10,
      pred = tapply(probs, deciles, mean),
      observed = tapply(as.numeric(outcome), deciles, mean)
    )
  }
}

# --- Nettoyage des NA et calcul de l'AUC poolée ---
auc_list_clean <- auc_list[!is.na(auc_list)] # Supprime les NA

if (length(auc_list_clean) == 0) {
  stop(
    "❌ Tous les modèles ont échoué. Vérifiez :
      - La formule (séparation complète ?)
      - Les données (outcome constant ?)
      - La convergence des modèles."
  )
}

# Calcul de l'AUC poolée avec IC (méthode de Rubin simplifiée)
auc_pooled <- c(
  mean = mean(auc_list_clean),
  lower = quantile(auc_list_clean, 0.02),
  upper = quantile(auc_list_clean, 0.98)
)

# Affichage
cat(sprintf(
  "\n✅ AUC poolée : %.3f [%.3f – %.3f] (n = %d imputations valides)\n",
  auc_pooled["mean"],
  auc_pooled["lower"],
  auc_pooled["upper"],
  length(auc_list_clean)
))

# --- Courbe ROC poolée (uniquement si au moins une imputation valide) ---
if (length(auc_list_clean) > 0) {
  roc_pooled <- bind_rows(roc_list[!sapply(roc_list, is.null)]) %>%
    mutate(fpr = round(fpr, 3)) %>%
    group_by(fpr) %>%
    summarise(tpr = mean(tpr), .groups = "drop") %>%
    arrange(fpr)

  roc_plot <- ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    annotate(
      "text",
      x = 0.75,
      y = 0.10,
      label = sprintf(
        "AUC = %.3f [%.3f – %.3f]",
        auc_pooled["mean"],
        auc_pooled["lower"],
        auc_pooled["upper"]
      ),
      color = "steelblue",
      size = 4
    ) +
    labs(x = "1 − Spécificité", y = "Sensibilité", title = "Courbe ROC poolée") +
    theme_classic() +
    theme(aspect.ratio = 1)

  print(roc_plot)
  ggsave(filename = "figures/ROC_imp.png", plot = roc_plot)
}


# =============================================================================
#         DISTRIBUTION DES PROBABILITÉS PRÉDITES POOLÉES (par statut réel)
# =============================================================================
probs_matrix <- matrix(NA_real_, nrow = nrow(complete(imp, 1)), ncol = n_imp)

for (i in seq_len(n_imp)) {
  imp_data <- complete(imp, i)

  fit_i <- glm(
    formule_glm,
    data = imp_data,
    family = binomial(link = "logit")
  )

  probs_matrix[, i] <- predict(fit_i, type = "response")
}

pred_pooled <- rowMeans(probs_matrix)
outcome_ref <- complete(imp, 1)$resultat_candida_def

df_hist <- data.frame(
  prob = pred_pooled,
  statut = outcome_ref
)


hist_plot <- ggplot(df_hist, aes(x = prob, fill = statut)) +
  geom_histogram(
    position = "identity",
    alpha = 0.6,
    bins = 30,
    color = "white"
  ) +
  xlim(0, 0.1) +
  scale_fill_manual(
    values = c("Négative" = "steelblue", "Positive" = "firebrick"),
    labels = c("Négatif", "Positif")
  ) +
  labs(
    x = "Probabilité prédite (poolée)",
    y = "Effectifs",
    fill = "Résultat candidémie",
    title = "Distribution des probabilités prédites poolées",
    subtitle = "Selon le statut réel de candidémie"
  ) +
  theme_classic(base_size = 12)

print(hist_plot)
ggsave("figures/hist_probs_pooled.png", plot = hist_plot, width = 7, height = 5)
saveRDS(hist_plot, file = "figures/hist_probs_pooled.rds")


# =============================================================================
# TABLEAU GTSUMMARY — RÉGRESSION UNIVARIÉE + MULTIVARIÉE (POOLING RÈGLE DE RUBIN)
# =============================================================================
# (packages déjà chargés en haut du script, pas besoin de les recharger)

# --- 2. VARIABLES DU MODÈLE ---------------------------------------------------
variables <- c(
  "hc_vi_cat",
  "hc_transfu",
  "hc_dialyse",
  "hc_kta",
  "adm_igs2",
  "hospit_ctc_duree",
  "hc_catheter_majeur",
  "hospit_cgr",
  "hospit_chirurgie_abdominale"
)

labels_variables <- list(
  "hc_vi_catOui" = "Ventilation invasive (catégorie)",
  "hc_transfuOui" = "Transfusion",
  "hc_dialyseOui" = "Dialyse",
  "hospit_ctc_duree" = "Durée corticothérapie (j)",
  "adm_igs2" = "IGS 2 à l'admission",
  "hc_catheter_majeurOui" = "VVC",
  "adm_igs2",
  "hospit_ctc_duree",
  "hc_catheter_majeur",
  "hospit_cgr",
  "hospit_chirurgie_abdominale"
)


# =============================================================================
# 3. RÉGRESSIONS UNIVARIÉES (une par variable, poolées sur les m imputations)
# =============================================================================
tbl_uni_list <- map(
  variables,
  function(var) {
    fit_uni <- with(
      imp,
      glm(resultat_candida_def ~ ., family = binomial(link = "logit"))
    )
    tbl_regression(
      fit_uni,
      exponentiate = TRUE,
      label = labels_variables,
      pvalue_fun = ~ style_pvalue(.x, digits = 3)
    ) %>%
      modify_header(estimate = "**OR non ajusté**") %>%
      bold_p(t = 0.05)
  }
)
# =============================================================================
# 4. RÉGRESSION MULTIVARIÉE (modèle complet, poolé)
# =============================================================================

fit_multi <- with(
  imp,
  glm(
    resultat_candida_def ~
      hc_vi_cat +
      hc_transfu +
      hc_dialyse +
      adm_igs2 +
      hospit_ctc_duree +
      hc_catheter_majeur +
      hospit_cgr +
      hospit_chirurgie_abdominale,
    family = binomial(link = "logit")
  )
)

tbl_multi <- tbl_regression(
  fit_multi,
  exponentiate = TRUE,
  pvalue_fun = ~ style_pvalue(.x, digits = 3)
) %>%
  modify_header(estimate = "**OR ajusté**") %>%
  bold_p(t = 0.05)

# =============================================================================
# 5. FUSION UNIVARIÉ + MULTIVARIÉ EN UN SEUL TABLEAU
# =============================================================================
tbl_final <- tbl_merge(
  tbls = list(tbl_uni, tbl_multi),
  tab_spanner = c("**Analyse univariée**", "**Analyse multivariée**")
) %>%
  modify_caption(
    "**Facteurs associés à la candidémie — modèle poolé (m = {m_imputations} imputations)**"
  ) %>%
  as_gt() %>%
  gt::tab_options(table.font.size = 12)

tbl_final

# --- 6. EXPORT -----------------------------------------------------------------
# gt::gtsave(tbl_final, filename = "figures/tableau_uni_multivarie.png")
saveRDS(tbl_multi, "figures/tableau_uni_multivarie.rds")
saveRDS(tbl_final, file = "models/tbl_uni_multi.rds")
