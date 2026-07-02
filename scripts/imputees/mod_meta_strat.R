# =============================================================================
# AJUSTEMENT GLMER SUR OBJET MIDS + POOLING (RÈGLE DE RUBIN)
# VERSION SÉQUENTIELLE — NOUVEAUX FACTEURS PRÉDICTIFS
# AVEC STRATIFICATION EN 3 NIVEAUX DE RISQUE
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
imp <- readRDS("donnees/df_impute_2.rds")
m_imputations <- imp$m
cat("Nombre de datasets imputés (m) :", m_imputations, "\n\n")

# --- 2. FORMULE DU MODÈLE -----------------------------------------------------
formule_glmer <- resultat_candida_def ~
  hc_vi_cat +
  hc_cgr +
  hc_cp +
  hc_dialyse +
  hc_amines +
  hc_vvc +
  hospit_chirurgie_majeure +
  hospit_ctc_duree +
  hospit_immunosup_duree +
  demo_age +
  hc_hypothermie +
  hc_fievre +
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

tidy_pooled <- tidy_pooled %>%
  mutate(
    label = ifelse(term %in% names(labels_lisibles), labels_lisibles[term], term),
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
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 10, hjust = 1))

print(forest_plot)


# =============================================================================
#          DISCRIMINATION : AUC, COURBE ROC & STRATIFICATION EN 3 RISQUES
# =============================================================================

# Seuils de stratification (probabilités prédites)
seuil_faible <- 0.01 # < 1 %  → risque faible
seuil_eleve <- 0.20 # > 20 % → risque élevé
# Entre les deux       → risque intermédiaire

couleurs_strates <- c(
  "Faible (< 1 %)" = "#2196F3", # bleu
  "Intermédiaire (1–20 %)" = "#FF9800", # orange
  "Élevé (> 20 %)" = "#F44336" # rouge
)

n_imp <- imp$m
auc_list <- numeric(n_imp)
roc_list <- vector("list", n_imp)
cal_list <- vector("list", n_imp)

# Stockage des probabilités poolées par patient (moyenne sur les m imputations)
# On conserve aussi l'outcome de chaque dataset pour la vérification
probs_all <- matrix(NA, nrow = nrow(complete(imp, 1)), ncol = n_imp)
outcome_ref <- complete(imp, 1)$resultat_candida_def # outcome identique sur tous les jeux

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

  # Stockage pour probabilités poolées
  probs_all[, i] <- probs

  # AUC par imputation
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

# ── Probabilités moyennées sur les m imputations (pooling des prédictions) ───
probs_pooled <- rowMeans(probs_all)

# ── Stratification des patients en 3 niveaux de risque ───────────────────────
strate <- case_when(
  probs_pooled < seuil_faible ~ "Faible (< 1 %)",
  probs_pooled >= seuil_eleve ~ "Élevé (> 20 %)",
  TRUE ~ "Intermédiaire (1–20 %)"
)
strate <- factor(strate, levels = names(couleurs_strates))

# Tableau de répartition des strates
tab_strates <- data.frame(
  Strate = levels(strate),
  N = as.integer(table(strate)),
  Pct = round(100 * prop.table(table(strate)), 1),
  Evenements = as.integer(tapply(as.integer(outcome_ref), strate, sum)),
  Taux_evenem = round(100 * tapply(as.integer(outcome_ref), strate, mean), 1),
  Prob_moy = round(100 * tapply(probs_pooled, strate, mean), 1)
)

cat("\n=== RÉPARTITION PAR STRATE DE RISQUE ===\n")
print(tab_strates, row.names = FALSE)

# ── Graphique de densité des probabilités prédites par strate ─────────────────
df_strat <- data.frame(
  prob_pred = probs_pooled,
  strate = strate,
  outcome = factor(outcome_ref, labels = c("Non-événement", "Candidémie"))
)

plot_density_strate <- ggplot(df_strat, aes(x = prob_pred, fill = strate)) +
  geom_histogram(binwidth = 0.01, color = "white", alpha = 0.85, position = "stack") +
  geom_vline(xintercept = seuil_faible, linetype = "dashed", color = "grey30", linewidth = 0.8) +
  geom_vline(xintercept = seuil_eleve, linetype = "dashed", color = "grey30", linewidth = 0.8) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0, 0.01, 0.05, 0.10, 0.20, 0.50, 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(values = couleurs_strates) +
  annotate(
    "text",
    x = seuil_faible / 2,
    y = Inf,
    label = "Faible",
    vjust = 2,
    size = 3.5,
    color = "grey20"
  ) +
  annotate(
    "text",
    x = (seuil_faible + seuil_eleve) / 2,
    y = Inf,
    label = "Intermédiaire",
    vjust = 2,
    size = 3.5,
    color = "grey20"
  ) +
  annotate(
    "text",
    x = (seuil_eleve + 1) / 2,
    y = Inf,
    label = "Élevé",
    vjust = 2,
    size = 3.5,
    color = "grey20"
  ) +
  labs(
    x = "Probabilité prédite de candidémie",
    y = "Nombre de patients",
    fill = "Strate de risque",
    title = "Distribution des probabilités prédites — Stratification en 3 niveaux",
    subtitle = "Probabilités moyennées sur les m imputations"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(plot_density_strate)

# ── Courbe ROC poolée avec annotation des seuils ──────────────────────────────
roc_pooled <- bind_rows(roc_list) %>%
  mutate(fpr = round(fpr, 3)) %>%
  group_by(fpr) %>%
  summarise(tpr = mean(tpr), .groups = "drop") %>%
  arrange(fpr)

# Coordonnées ROC correspondant aux deux seuils (sur données poolées)
roc_poolee_obj <- roc(outcome_ref, probs_pooled, quiet = TRUE)

get_roc_coords <- function(roc_obj, seuil) {
  idx <- which.min(abs(roc_obj$thresholds - seuil))
  data.frame(
    seuil = seuil,
    fpr = 1 - roc_obj$specificities[idx],
    tpr = roc_obj$sensitivities[idx]
  )
}

coords_seuils <- bind_rows(
  get_roc_coords(roc_poolee_obj, seuil_faible),
  get_roc_coords(roc_poolee_obj, seuil_eleve)
) %>%
  mutate(label = paste0(seuil * 100, " %"))

roc_plot <- ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
  # Zone colorée par strate sous la courbe ROC
  geom_ribbon(
    data = roc_pooled %>% filter(fpr <= coords_seuils$fpr[2]),
    aes(ymin = 0, ymax = tpr),
    fill = couleurs_strates["Élevé (> 20 %)"],
    alpha = 0.12
  ) +
  geom_ribbon(
    data = roc_pooled %>% filter(fpr > coords_seuils$fpr[2]),
    aes(ymin = 0, ymax = tpr),
    fill = couleurs_strates["Faible (< 1 %)"],
    alpha = 0.08
  ) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_abline(linetype = "dashed", color = "red") +
  # Points aux seuils
  geom_point(
    data = coords_seuils,
    aes(x = fpr, y = tpr),
    color = "grey20",
    size = 3.5,
    shape = 21,
    fill = "white",
    stroke = 1.5
  ) +
  geom_label(
    data = coords_seuils,
    aes(x = fpr, y = tpr, label = label),
    nudge_y = 0.06,
    size = 3.2,
    color = "grey20"
  ) +
  annotate(
    "text",
    x = 0.75,
    y = 0.12,
    label = sprintf(
      "AUC = %.3f\n[%.3f – %.3f]",
      auc_pooled["mean"],
      auc_pooled["lower"],
      auc_pooled["upper"]
    ),
    color = "steelblue",
    size = 3.8,
    hjust = 0
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "1 − Spécificité (Taux de faux positifs)",
    y = "Sensibilité (Taux de vrais positifs)",
    title = "Courbe ROC poolée — Modèle mixte",
    subtitle = "Points de seuil à 1 % et 20 % indiqués"
  ) +
  theme_minimal(base_size = 12) +
  theme(aspect.ratio = 1)

print(roc_plot)


# =============================================================================
#                          CALIBRATION POOLÉE PAR STRATE
# =============================================================================
cal_pooled <- bind_rows(cal_list) %>%
  group_by(decile) %>%
  summarise(pred = mean(pred), observed = mean(observed), .groups = "drop")

# Attribution de la strate à chaque décile selon la probabilité prédite moyenne
cal_pooled <- cal_pooled %>%
  mutate(
    strate = case_when(
      pred < seuil_faible ~ "Faible (< 1 %)",
      pred >= seuil_eleve ~ "Élevé (> 20 %)",
      TRUE ~ "Intermédiaire (1–20 %)"
    ),
    strate = factor(strate, levels = names(couleurs_strates))
  )

cal_plot <- ggplot(cal_pooled, aes(x = pred, y = observed)) +
  geom_abline(linetype = "dashed", color = "red", linewidth = 0.8) +
  # Zones de fond par strate
  annotate(
    "rect",
    xmin = 0,
    xmax = seuil_faible,
    ymin = 0,
    ymax = 1,
    fill = couleurs_strates["Faible (< 1 %)"],
    alpha = 0.08
  ) +
  annotate(
    "rect",
    xmin = seuil_faible,
    xmax = seuil_eleve,
    ymin = 0,
    ymax = 1,
    fill = couleurs_strates["Intermédiaire (1–20 %)"],
    alpha = 0.08
  ) +
  annotate(
    "rect",
    xmin = seuil_eleve,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    fill = couleurs_strates["Élevé (> 20 %)"],
    alpha = 0.08
  ) +
  # Lignes verticales de seuil
  geom_vline(xintercept = seuil_faible, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = seuil_eleve, linetype = "dotted", color = "grey50") +
  # Courbe de calibration colorée par strate
  geom_line(aes(color = strate), linewidth = 0.8) +
  geom_point(aes(color = strate, fill = strate), size = 3, shape = 21, stroke = 1.2, alpha = 0.9) +
  scale_color_manual(values = couleurs_strates) +
  scale_fill_manual(values = couleurs_strates) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  # Annotations des zones
  annotate(
    "text",
    x = seuil_faible / 2,
    y = 0.95,
    label = "Faible",
    size = 3,
    color = "grey30",
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = (seuil_faible + seuil_eleve) / 2,
    y = 0.95,
    label = "Intermédiaire",
    size = 3,
    color = "grey30",
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = (seuil_eleve + max(cal_pooled$pred, na.rm = TRUE)) / 2,
    y = 0.95,
    label = "Élevé",
    size = 3,
    color = "grey30",
    fontface = "italic"
  ) +
  labs(
    x = "Probabilité prédite",
    y = "Probabilité observée",
    color = "Strate de risque",
    fill = "Strate de risque",
    title = "Courbe de calibration poolée — Stratifiée en 3 niveaux de risque",
    subtitle = "Diagonale en pointillés rouges = calibration parfaite"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(cal_plot)


# =============================================================================
#            TABLEAU RÉCAPITULATIF PAR STRATE (performances diagnostiques)
# =============================================================================

# Sensibilité / Spécificité à chaque seuil sur données poolées
coords_faible <- coords(
  roc_poolee_obj,
  x = seuil_faible,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)
coords_eleve <- coords(
  roc_poolee_obj,
  x = seuil_eleve,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)

cat("\n=== PERFORMANCES DIAGNOSTIQUES AUX SEUILS DE STRATIFICATION ===\n")
cat(sprintf(
  "Seuil < %.0f %% (risque faible)  — Sensibilité : %.1f %%  |  Spécificité : %.1f %%\n",
  seuil_faible * 100,
  coords_faible$sensitivity * 100,
  coords_faible$specificity * 100
))
cat(sprintf(
  "Seuil > %.0f %% (risque élevé)   — Sensibilité : %.1f %%  |  Spécificité : %.1f %%\n",
  seuil_eleve * 100,
  coords_eleve$sensitivity * 100,
  coords_eleve$specificity * 100
))

# saveRDS(tidy_pooled,        file = "models/tidy_pooled_nouveaux.rds")
# saveRDS(forest_plot,        file = "models/fp_imp.rds")
# saveRDS(df_strat,           file = "models/df_strat.rds")
# saveRDS(plot_density_strate, file = "models/plot_density_strate.rds")
# saveRDS(roc_plot,           file = "models/roc_plot_strat.rds")
# saveRDS(cal_plot,           file = "models/cal_plot_strat.rds")
