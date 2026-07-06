# =============================================================================
# AJUSTEMENT GLMER SUR OBJET MIDS + POOLING (RÈGLE DE RUBIN)
# VERSION SÉQUENTIELLE — NOUVEAUX FACTEURS PRÉDICTIFS
# =============================================================================

# --- 0. PACKAGES --------------------------------------------------------------
library(mice)
library(lme4)
library(tidyverse)
library(gtsummary)
library(gt)
library(pROC)
library(broom.mixed)

# --- 1. CHARGEMENT DES DONNÉES ------------------------------------------------
imp <- readRDS("donnees/df_impute.rds")
m_imputations <- 3
cat("Nombre de datasets imputés (m) :", m_imputations, "\n\n")

# --- 2. FORMULE DU MODÈLE -----------------------------------------------------
formule_glmer <- resultat_candida_def ~
  hc_vi_cat +
  hc_transfusion +
  hc_dialyse +
  # hc_amines +
  hc_catheter_majeur +
  # hospit_chirurgie_majeure +
  hospit_ctc_duree +
  hospit_immunosup_duree +
  demo_age +
  hc_hypothermie +
  # hc_fievre +
  hospit_parenterale_duree +
  demo_type_rea +
  (1 | iep)

# --- 3. AJUSTEMENT SÉQUENTIEL SUR LES m DATASETS IMPUTÉS ---------------------
cat("Ajustement de", m_imputations, "modèles glmer en séquentiel...\n")

liste_modeles <- list()

for (j in seq_len(m_imputations)) {
  df_j <- complete(imp, action = j)

  modele <- tryCatch(
    {
      glmer(
        formule_glmer,
        data = df_j,
        family = "binomial",
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
      )
    },
    warning = function(w) {
      message("Imputation ", j, " - WARNING : ", conditionMessage(w))
      suppressWarnings(
        glmer(
          formule_glmer,
          data = df_j,
          family = "binomial",
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
        )
      )
    },
    error = function(e) {
      message("Imputation ", j, " - ERREUR : ", conditionMessage(e))
      NULL
    }
  )

  liste_modeles[[j]] <- modele
  cat("Modèle", j, "/", m_imputations, "terminé\n")
}

cat("Ajustement terminé !\n\n")

# --- 4. VÉRIFICATION DE LA CONVERGENCE ----------------------------------------
n_valides <- sum(!sapply(liste_modeles, is.null))
cat("Modèles ajustés avec succès :", n_valides, "/", m_imputations, "\n")

if (n_valides == 0) {
  stop(
    "Aucun modèle glmer n'a convergé. Vérifiez la formule, l'effet aléatoire ",
    "'iep', ou simplifiez la structure du modèle."
  )
}

if (n_valides < m_imputations) {
  warning(m_imputations - n_valides, " modèle(s) n'ont pas convergé et seront exclus du pooling.")
}

liste_modeles <- liste_modeles[!sapply(liste_modeles, is.null)]

# --- 5. RECONSTRUCTION D'UN OBJET COMPATIBLE AVEC mice::pool() ---------------
objet_mira <- list(
  call = NULL,
  call1 = imp$call,
  nmis = imp$nmis,
  analyses = liste_modeles
)
class(objet_mira) <- "mira"

# --- 6. POOLING (RÈGLE DE RUBIN) ----------------------------------------------
resultats_pool <- pool(objet_mira)

resume_logodds <- summary(resultats_pool, conf.int = TRUE, exponentiate = FALSE)
resume_OR <- summary(resultats_pool, conf.int = TRUE, exponentiate = TRUE)

cat("\n=== RÉSULTATS POOLÉS (échelle log-odds) ===\n")
print(resume_logodds, digits = 3)

cat("\n=== RÉSULTATS POOLÉS (Odds Ratios) ===\n")
print(resume_OR, digits = 3)


# =============================================================================
#                             FOREST PLOT
# =============================================================================

# Niveaux des termes (hors intercept) dans l'ordre souhaité
niveaux_termes <- c(
  "(Intercept)",
  "hc_vi_cat",
  "hc_cgr",
  "hc_cp",
  "hc_dialyse",
  "hc_amines",
  "hc_vvc",
  "hospit_chirurgie_majeure",
  "hospit_ctc_duree",
  "hospit_immunosup_duree",
  "demo_age",
  "hc_hypothermie",
  "hc_fievre",
  "hospit_parenterale_duree",
  "demo_type_rea"
)
# Pour les variables catégorielles (hc_vi_cat, demo_type_rea), R génère
# automatiquement un terme par modalité (ex. hc_vi_catModX, demo_type_reaY) ;
# ajustez `niveaux_termes` ci-dessus en fonction des sorties de resume_OR$term.

summary_results <- summary(resultats_pool)

tidy_pooled <- resultats_pool$pooled %>%
  left_join(
    summary_results %>% select(term, std.error),
    by = "term"
  ) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    OR = exp(estimate),
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  ) %>%
  filter(term != "(Intercept)")

saveRDS(tidy_pooled, file = "models/mod_meta_pooled.rds")
# tidy_pooled <- readRDS("models/tidy_pooled_nouveaux.rds")

# Étiquettes lisibles pour le graphique
labels_lisibles <- c(
  "hc_vi_catOui" = "Ventilation invasive (catégorie)",
  "hc_cgrOui" = "Transfusion CGR",
  "hc_cpOui" = "Transfusion CP",
  "hc_dialyseOui" = "Dialyse",
  "hc_aminesOui" = "Amines vasopressives",
  "hc_vvcOui" = "Voie veineuse centrale",
  "hospit_chirurgie_majeureOui" = "Chirurgie majeure",
  "hospit_ctc_duree" = "Durée corticothérapie (j)",
  "hospit_immunosup_duree" = "Durée immunosuppresseurs (j)",
  "demo_age" = "Âge (années)",
  "hc_hypothermieOui" = "Hypothermie",
  "hc_fievreOui" = "Fièvre",
  "hospit_parenterale_duree" = "Durée nutrition parentérale (j)",
  "demo_type_rea" = "Type de réanimation"
)

# Pour les termes catégoriels, garder le nom original si non trouvé dans labels_lisibles
tidy_pooled <- tidy_pooled %>%
  mutate(
    label = ifelse(term %in% names(labels_lisibles), labels_lisibles[term], term),
    label = factor(label, levels = rev(unique(label))) # ordre du forest plot
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
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 10, hjust = 1))

print(forest_plot)
saveRDS(forest_plot, file = "models/fp_imp.rds")
forest_plot <- readRDS("models/fp_imp.rds")

# =============================================================================
#                         DISCRIMINATION : AUC & COURBE ROC
# =============================================================================
n_imp <- imp$m
auc_list <- numeric(n_imp)
roc_list <- vector("list", n_imp)
cal_list <- vector("list", n_imp)

for (i in seq_len(n_imp)) {
  imp_data <- complete(imp, i)

  fit_i <- glmer(
    formule_glmer,
    data = imp_data,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
  probs <- predict(fit_i, type = "response")
  outcome <- imp_data$resultat_candida_def

  # AUC
  roc_i <- roc(outcome, probs, quiet = TRUE)
  auc_list[i] <- as.numeric(auc(roc_i))

  # Points ROC
  roc_list[[i]] <- data.frame(
    fpr = 1 - roc_i$specificities,
    tpr = roc_i$sensitivities
  )

  # Calibration par déciles
  deciles <- ntile(probs, 10)
  cal_list[[i]] <- data.frame(
    decile = 1:10,
    pred = tapply(probs, deciles, mean),
    observed = tapply(as.numeric(outcome), deciles, mean)
  )
}

# ── AUC poolée ────────────────────────────────────────────────────────────────
auc_pooled <- c(
  mean = mean(auc_list),
  lower = quantile(auc_list, 0.025),
  upper = quantile(auc_list, 0.975)
)
cat(sprintf(
  "\nAUC poolée : %.3f [%.3f – %.3f]\n",
  auc_pooled["mean"],
  auc_pooled["lower"],
  auc_pooled["upper"]
))

# ── Courbe ROC poolée ─────────────────────────────────────────────────────────
roc_pooled <- bind_rows(roc_list) %>%
  mutate(fpr = round(fpr, 3)) %>%
  group_by(fpr) %>%
  summarise(tpr = mean(tpr), .groups = "drop") %>%
  arrange(fpr)

roc_plot <- ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_abline(linetype = "dashed", color = "red") +
  annotate(
    "text",
    x = 0.75,
    y = 0.10,
    label = sprintf("AUC = %.3f", auc_pooled["mean"]),
    color = "steelblue",
    size = 4
  ) +
  labs(
    x = "1 − Spécificité",
    y = "Sensibilité",
    title = "Courbe ROC poolée — Modèle mixte"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1)

print(roc_plot)
ggsave(roc_plot, plot = "figures/ROC_imp.png")


# =============================================================================
#                          CALIBRATION POOLÉE
# =============================================================================
cal_pooled <- bind_rows(cal_list) %>%
  group_by(decile) %>%
  summarise(
    pred = mean(pred),
    observed = mean(observed),
    n = n(), # nb de folds/points agrégés dans ce décile
    .groups = "drop"
  )

# 2. Calcul des IC (approximation normale sur la proportion observée)
#    Si vous avez le vrai nombre d'individus par bin (ex: n_obs), remplacez n par n_obs.
z <- qnorm(0.975)
cal_pooled <- cal_pooled %>%
  mutate(
    se = sqrt(observed * (1 - observed) / n),
    lower = pmax(0, observed - z * se),
    upper = pmin(1, observed + z * se)
  )

# 3. Graphique avec ruban de confiance
cal_plot <- ggplot(cal_pooled, aes(x = pred, y = observed)) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  labs(
    x = "Probabilité prédite",
    y = "Probabilité observée",
    title = "Courbe de calibration poolée"
  ) +
  theme_minimal()

print(cal_plot)
saveRDS(cal_plot, file = "figures/cal_plot.rds")
