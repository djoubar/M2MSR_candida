library(lme4)

imp <- readRDS("donnees/df_impute.rds")

# =============================================================================
# AJUSTEMENT GLMER SUR OBJET MIDS + POOLING (RÈGLE DE RUBIN)
# VERSION PARALLÉLISÉE
#
# PRINCIPE :
#   mice::with() ajuste les m modèles en séquentiel, ce qui est lent pour
#   glmer (modèles mixtes = optimisation itérative coûteuse).
#   On remplace la boucle interne de with() par furrr::future_map(),
#   puis on reconstruit un objet compatible avec mice::pool().
# =============================================================================

# --- 0. PACKAGES --------------------------------------------------------------
library(mice)
library(lme4)
library(furrr)
library(future)
library(progressr)

# --- 1. CHARGEMENT DES DONNÉES ------------------------------------------------
imp <- readRDS("donnees/df_impute.rds")
m_imputations <- imp$m
cat("Nombre de datasets imputés (m) :", m_imputations, "\n\n")

# --- 2. SETUP PARALLÈLE --------------------------------------------------------
# Remplace .setup_parallel() : on déclare explicitement le plan furrr/future.
# glmer étant lourd en calcul, on parallélise sur les m datasets imputés
# plutôt qu'à l'intérieur de chaque ajustement.
#
# Plafond à 20 coeurs max, comme dans .setup_parallel() d'origine :
#   - jamais plus de 20 workers, même sur une machine avec plus de coeurs
#   - jamais plus de (coeurs disponibles - 1), pour laisser un coeur au système
#   - jamais plus que m_imputations, puisqu'on a au plus m modèles à paralléliser
n_cores_dispo <- parallel::detectCores()
max_cores <- min(20L, max(1L, n_cores_dispo - 1L))
n_workers <- min(max_cores, m_imputations)

plan(multisession, workers = n_workers)

cat(sprintf(
  "[future] %d workers | Coeurs dispo : %d | plafond appliqué : %d | modèles à ajuster : %d\n\n",
  n_workers,
  n_cores_dispo,
  max_cores,
  m_imputations
))

# BLAS à 1 thread pendant que future est actif, pour éviter la sur-souscription
# (chaque worker lance ses propres calculs matriciels ; sans cette limite,
# BLAS multi-threadé x N workers peut saturer la machine)
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1)
}

# --- 3. FORMULE DU MODÈLE -------------------------------------------------------
formule_glmer <- resultat_candida_def ~ hc_leuco_min +
  hc_lactates_max +
  adm_uree_max +
  adm_poids +
  hc_diurese_norm +
  adm_igs2 +
  hc_uree_max +
  hc_pfio2_min +
  hc_delai +
  demo_age +
  demo_atcd_hemato +
  (1 | iep)

# --- 4. AJUSTEMENT PARALLÉLISÉ SUR LES m DATASETS IMPUTÉS -----------------------
# On ajuste un glmer par dataset imputé, en parallèle, avec gestion des
# warnings de convergence (fréquents avec glmer, à ne pas masquer silencieusement).

cat("Ajustement de", m_imputations, "modèles glmer en parallèle...\n")

with_progress({
  p <- progressor(steps = m_imputations)

  liste_modeles <- furrr::future_map(
    seq_len(m_imputations),
    function(j) {
      df_j <- complete(imp, action = j)

      modele <- tryCatch(
        glmer(
          formule_glmer,
          data = df_j,
          family = "binomial",
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
        ),
        warning = function(w) {
          message("Imputation ", j, " - WARNING : ", conditionMessage(w))
          # On relance sans interrompre, pour conserver le modèle malgré le warning
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

      p()
      modele
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
})

cat("Ajustement terminé !\n\n")

# --- 5. VÉRIFICATION DE LA CONVERGENCE ------------------------------------------
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

# --- 6. RECONSTRUCTION D'UN OBJET COMPATIBLE AVEC mice::pool() ------------------
# pool() accepte normalement un objet de classe "mira" (sortie de with.mids()).
# On reconstruit cette classe manuellement à partir de notre liste de modèles
# ajustés en parallèle.

objet_mira <- list(
  call = match.call(),
  call1 = imp$call,
  nmis = imp$nmis,
  analyses = liste_modeles
)
class(objet_mira) <- "mira"

# --- 7. POOLING (RÈGLE DE RUBIN) ------------------------------------------------
resultats_pool <- pool(objet_mira)

resume_logodds <- summary(resultats_pool, conf.int = TRUE, exponentiate = FALSE)
resume_OR <- summary(resultats_pool, conf.int = TRUE, exponentiate = TRUE)

cat("\n=== RÉSULTATS POOLÉS (échelle log-odds) ===\n")
print(resume_logodds, digits = 3)

cat("\n=== RÉSULTATS POOLÉS (Odds Ratios) ===\n")
print(resume_OR, digits = 3)

# --- 8. RETOUR EN SÉQUENTIEL (bonne pratique en fin de script) -----------------
plan(sequential)
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(parallel::detectCores()) # restauration multi-thread
}
cat("[future] Cluster fermé | BLAS restauré\n")
# # saveRDS(fit, file = "fit.rds")
# tidy_fit <- broom.mixed::tidy(fit, conf.int = TRUE)
# pooled_results <- pool(tidy_fit)
# pooled_results <- pool(fit)
# saveRDS(pooled_results, file = "mod_imp_meta.rds")
# summary(pooled_results)
# # saveRDS(fit, file = "fit.rds")

# # tbl_fit <- tbl_regression(
# #   imp,
# #   resultat_candida_def ~ . + (1 | iep),
# #   exponentiate = TRUE,
# #   estimate_fun = "mean",
# #   p.value_fun = function(x) {
# #     mice::pool(x)$p.value
# #   }
# # )

# # # Afficher le tableau
# # tbl_fit <-
# #   tbl_regression(fit, exponentiate = TRUE) |>
# #   as_gt() |>
# #   gtsave("tbl_rlog_imput.docx")

# #===============================================================================
# #                            FORREST PLOT
# #===============================================================================
# # pooled_results <- readRDS("models/mod_imp_meta.rds")

# summary_results <- summary(pooled_results)
# tidy_pooled <- pooled_results$pooled %>%
#   mutate(
#     term = factor(
#       term,
#       levels = c(
#         "(Intercept)",
#         "demo_type_reaMedicale",
#         "demo_atcd_hematoOui",
#         "demo_atcd_diabeteOui",
#         "adm_igs2",
#         "adm_diurese_norm",
#         "adm_lactates_max",
#         "hc_delai",
#         "hc_diurese_norm",
#         "hc_pfio2_min",
#         "hc_lactates_max",
#         "hc_dialyseOui",
#         "hc_vvcOui",
#         "hc_ktdOui",
#         "hc_transfu1",
#         "hospit_vi_duree",
#         "hospit_parenterale_duree",
#         "hospit_vvc_duree",
#         "hospit_kta_duree",
#         "hospit_ktd_duree",
#         "hospit_atb_duree",
#         "hospit_cgr",
#         "hospit_pfc",
#         "hospit_cp",
#         "hospit_fibroOui",
#         "hospit_chirurgie_majeureOui",
#         "hospit_chirurgie_abdominaleOui",
#         "hospit_ctc_duree"
#       )
#     ),
#     std.error = summary_results$std.error,
#     conf.low = estimate - 1.96 * std.error,
#     conf.high = estimate + 1.96 * std.error,
#     OR = exp(estimate),
#     OR_low = exp(conf.low),
#     OR_high = exp(conf.high)
#   ) %>%
#   filter(term != "(Intercept)")
# saveRDS(tidy_pooled, file = "models/tidy_pooled.rds")

# tidy_pooled <- readRDS("models/tidy_pooled.rds")
# # Forest Plot avec IC
# ggplot(tidy_pooled, aes(x = OR, y = term)) +
#   geom_point(size = 3, color = "blue") +
#   geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), width = 0.2) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
#   labs(
#     x = "Odds Ratio (log scale)",
#     y = "Variable",
#     title = "Forest Plot - Modèle poolé avec IC"
#   ) +
#   scale_x_log10() +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10, hjust = 0))

# #===============================================================================
# #                                 TESTS CL0DE
# #===============================================================================
# model_formula <- resultat_candida_def ~ demo_age +
#   demo_type_rea +
#   adm_lactates_max +
#   hc_vi_cat +
#   hc_transfu +
#   hc_uree_max +
#   hc_dialyse +
#   hc_amines +
#   hc_choc +
#   hc_diurese_norm +
#   hc_vvc +
#   hc_cp +
#   hc_delai +
#   hospit_ctc_duree +
#   hospit_immunosup_duree +
#   hospit_cgr +
#   hospit_parenterale_duree +
#   hospit_chirurgie_majeure +
#   hospit_vvc_duree +
#   (1 | iep)

# # ── Boucle unique sur les imputations ──────────────────────────────────────
# n_imp <- imp$m
# auc_list <- numeric(n_imp)
# roc_list <- vector("list", n_imp)
# cal_list <- vector("list", n_imp)

# for (i in seq_len(n_imp)) {
#   imp_data <- complete(imp, i)

#   fit_i <- glmer(model_formula, data = imp_data, family = "binomial")
#   probs <- predict(fit_i, type = "response")
#   outcome <- imp_data$resultat_candida_def

#   # AUC
#   roc_i <- roc(outcome, probs, quiet = TRUE)
#   auc_list[i] <- as.numeric(auc(roc_i))

#   # ROC
#   roc_list[[i]] <- data.frame(
#     fpr = 1 - roc_i$specificities,
#     tpr = roc_i$sensitivities
#   )

#   # Calibration (par déciles)
#   deciles <- ntile(probs, 10)
#   cal_list[[i]] <- data.frame(
#     decile = 1:10,
#     pred = tapply(probs, deciles, mean),
#     observed = tapply(as.numeric(outcome), deciles, mean)
#   )
# }

# # ── AUC poolée ────────────────────────────────────────────────────────────
# auc_pooled <- c(
#   mean = mean(auc_list),
#   lower = quantile(auc_list, 0.025),
#   upper = quantile(auc_list, 0.975)
# )
# cat(sprintf(
#   "AUC poolée : %.3f [%.3f – %.3f]\n",
#   auc_pooled["mean"],
#   auc_pooled["lower"],
#   auc_pooled["upper"]
# ))

# # ── Courbe ROC poolée ─────────────────────────────────────────────────────
# roc_pooled <- bind_rows(roc_list) %>%
#   mutate(fpr = round(fpr, 3)) %>%
#   group_by(fpr) %>%
#   summarise(tpr = mean(tpr), .groups = "drop") %>%
#   arrange(fpr)

# ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
#   geom_line(color = "blue", linewidth = 1) +
#   geom_abline(linetype = "dashed", color = "red") +
#   annotate(
#     "text",
#     x = 0.75,
#     y = 0.1,
#     label = sprintf("AUC = %.3f", auc_pooled["mean"]),
#     color = "blue",
#     size = 4
#   ) +
#   labs(x = "1 - Spécificité", y = "Sensibilité", title = "Courbe ROC poolée") +
#   theme_minimal() +
#   theme(aspect.ratio = 1)

# # ── Courbe de calibration poolée ──────────────────────────────────────────
# cal_pooled <- bind_rows(cal_list) %>%
#   group_by(decile) %>%
#   summarise(pred = mean(pred), observed = mean(observed), .groups = "drop")

# ggplot(cal_pooled, aes(x = pred, y = observed)) +
#   geom_point(size = 2) +
#   geom_line(color = "blue") +
#   geom_abline(linetype = "dashed", color = "red") +
#   labs(
#     x = "Probabilité prédite",
#     y = "Probabilité observée",
#     title = "Courbe de calibration poolée"
#   ) +
#   theme_minimal()

# #===============================================================================
# #                                  AUC/ROC
# #===============================================================================
# # ===== 1. AUC =====
# auc_list <- numeric(imp$m) # Initialise pour toutes les imputations

# for (i in 1:imp$m) {
#   # Boucle sur TOUTES les imputations (ex: 1:50)
#   imp_data <- complete(imp, i)
#   fit_i <- glmer(
#     resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
#     data = imp_data,
#     family = "binomial"
#   )
#   pred_probs <- predict(fit_i, type = "response")
#   auc_list[i] <- auc(roc(imp_data$resultat_candida_def, pred_probs))
# }

# auc_pooled <- list(
#   estimate = mean(auc_list),
#   conf.int = quantile(auc_list, probs = c(0.025, 0.975))
# )

# # ===== 2. Courbe ROC =====
# roc_list <- lapply(1:imp$m, function(i) {
#   # Utilise imp$m pour toutes les imputations
#   imp_data <- complete(imp, i)
#   fit_i <- glmer(
#     resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
#     data = imp_data,
#     family = "binomial"
#   )
#   calc_roc(imp_data, fit_i)
# })

# # Pooler les courbes ROC
# all_roc_points <- bind_rows(roc_list, .id = "imputation")
# roc_pooled <- all_roc_points %>%
#   group_by(fpr = round(fpr, 3)) %>%
#   summarise(tpr = mean(tpr), .groups = "drop") %>%
#   arrange(fpr)

# # ===== 3. Trace la courbe ROC avec l'AUC =====
# ggplot(roc_pooled, aes(x = fpr, y = tpr)) +
#   geom_line(color = "blue", size = 1) +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
#   annotate(
#     "text",
#     x = 0.95,
#     y = 0.05,
#     label = paste0("AUC = ", round(auc_pooled$estimate, 3)), # ✅ Corrigé ici
#     color = "blue",
#     size = 4
#   ) +
#   labs(
#     x = "Taux de faux positifs (1 - Spécificité)",
#     y = "Taux de vrais positifs (Sensibilité)",
#     title = "Courbe ROC poolée - Modèle mixte"
#   ) +
#   theme_minimal() +
#   theme(aspect.ratio = 1)

# #===============================================================================
# #                                COURBE CALIBRATION
# #===============================================================================
# # 1. Fonction de calibration manuelle
# calc_cal <- function(data) {
#   fit_i <- glmer(
#     resultat_candida_def ~ hospit_ctc_duree + (1 | iep),
#     data = data,
#     family = "binomial"
#   )
#   pred_probs <- predict(fit_i, type = "response")
#   bins <- cut(
#     pred_probs,
#     breaks = quantile(pred_probs, probs = seq(0, 1, length.out = 11)),
#     include.lowest = TRUE
#   )
#   data.frame(
#     pred_mean = sapply(levels(bins), function(bin) mean(pred_probs[bins == bin], na.rm = TRUE)),
#     observed = sapply(levels(bins), function(bin) {
#       mean(data$resultat_candida_def[bins == bin], na.rm = TRUE)
#     })
#   )
# }

# # 2. Applique et poole
# cal_list <- with(imp, lapply(1:m, function(i) calc_cal(complete(imp, i))))
# cal_pooled <- Reduce(
#   function(x, y) {
#     merge(x, y, by = "pred_mean", suffixes = c("_x", "_y")) %>%
#       mutate(observed = (observed_x + observed_y) / 2) %>%
#       select(pred_mean, observed)
#   },
#   cal_list
# )

# # 3. Trace
# ggplot(cal_pooled, aes(x = pred_mean, y = observed)) +
#   geom_line(color = "blue") +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
#   labs(x = "Probabilité prédite", y = "Probabilité observée", title = "Calibration poolée") +
#   theme_minimal()
