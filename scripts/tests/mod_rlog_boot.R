# =============================================================================
# BOOTSTRAP STRATIFIÉ + RÉGRESSION LASSO (glmnet) + SÉLECTION
# Sur un objet "mids" (imputation multiple via mice)
#
# ADAPTÉ POUR ÉVÉNEMENTS RARES (~0.1% de positifs) :
#   - stepAIC/glm remplacés par glmnet (LASSO) qui gère nativement
#     la séparation parfaite via la pénalisation des coefficients
#   - prop_train relevé à 1 (bootstrap classique avec remise) pour
#     maximiser le nombre d'événements positifs par tirage
# =============================================================================

# --- 0. PACKAGES ------------------------------------------------------------------
library(mice)
library(dplyr)
library(ggplot2)
library(broom)
library(furrr) # Pour future_map()
library(future) # Pour plan()
library(glmnet) # Pour la régression LASSO
library(progressr)
library(tidyr)
library(purrr)
library(tibble)

# --- 1. PARAMÈTRES À ADAPTER ------------------------------------------------------
mids_obj <- readRDS("donnees/df_impute.rds") # Votre objet mids
cible <- "resultat_candida_def" # Variable à prédire (factor 0/1)
n_bootstrap <- 200 # Nombre d'itérations bootstrap
prop_train <- 1 # 1 = bootstrap classique avec remise
var_exclure <- c("id_hemoc", "iep", "grouphc", "deces_rea", "demo_uf")
# (recommandé pour événements rares :
#  préserve le nb d'événements positifs)
alpha_enet <- 1 # 1 = LASSO pur, 0 = Ridge, entre 0-1 = Elastic Net
seuil_selection <- 50 # % d'itérations pour retenir une variable

n_cores <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_cores)
cat("Parallélisation activée sur", n_cores, "coeurs\n\n")

# --- 2. VÉRIFICATION DE L'OBJET MIDS ------------------------------------------------
m_imputations <- mids_obj$m
cat("Nombre de datasets imputés (m) :", m_imputations, "\n")

df_exemple <- complete(mids_obj, action = 1)
df_exemple[[cible]] <- as.factor(df_exemple[[cible]])

cat("\nDistribution de la variable cible (dataset imputé #1) :\n")
print(table(df_exemple[[cible]]))
cat("\nProportions :\n")
print(prop.table(table(df_exemple[[cible]])))

n_positifs <- sum(df_exemple[[cible]] == levels(df_exemple[[cible]])[2])
cat("\nNombre d'événements positifs :", n_positifs, "\n")
if (n_positifs < 50) {
  warning(
    "Moins de 50 événements positifs : la stabilité du bootstrap sera limitée. ",
    "Interprète les résultats avec prudence."
  )
}

niveaux <- levels(df_exemple[[cible]])
cat("\nClasses détectées :", paste(niveaux, collapse = " / "), "\n")

vars_candidates <- setdiff(names(df_exemple), c(cible, var_exclure))
stopifnot(!cible %in% vars_candidates)
cat("Nombre de variables candidates :", length(vars_candidates), "\n\n")

# --- 3. FONCTION DE TIRAGE STRATIFIÉ ------------------------------------------------
tirage_stratifie <- function(y, prop, niveaux) {
  indices <- seq_along(y)
  idx_par_classe <- lapply(niveaux, function(cl) {
    idx_cl <- indices[y == cl]
    n_tirer <- round(length(idx_cl) * prop)
    sample(idx_cl, size = n_tirer, replace = TRUE)
  })
  unlist(idx_par_classe)
}

# --- 4. FONCTION POUR UNE ITÉRATION BOOTSTRAP (LASSO) -------------------------------
run_bootstrap_iteration <- function(
  i,
  mids_obj,
  cible,
  prop_train,
  niveaux,
  vars_candidates,
  alpha_enet
) {
  set.seed(i) # Seed unique par itération

  # 4a. Tirer un dataset imputé au hasard
  idx_imputation <- sample(seq_len(mids_obj$m), size = 1)
  df_complet <- complete(mids_obj, action = idx_imputation)
  df_complet[[cible]] <- as.factor(df_complet[[cible]])

  # 4b. Tirage stratifié bootstrap (avec remise)
  y <- df_complet[[cible]]
  idx_train <- tirage_stratifie(y, prop_train, niveaux)
  df_train <- df_complet[idx_train, vars_candidates, drop = FALSE]
  y_train <- df_complet[[cible]][idx_train]

  # Garde-fou : s'assurer que les deux classes sont bien présentes
  if (length(unique(y_train)) < 2) {
    return(NULL)
  }

  # 4c. Construction de la matrice de design
  # model.matrix gère automatiquement l'encodage des variables factor en dummies
  x_train <- tryCatch(
    model.matrix(~ . - 1, data = df_train),
    error = function(e) NULL
  )
  if (is.null(x_train)) {
    return(NULL)
  }

  # 4d. Validation croisée pour choisir lambda (force de la pénalisation)
  # type.measure = "deviance" est recommandé en cas de classes très déséquilibrées
  # (plus stable que "class" ou "auc" avec très peu de positifs)
  cv_fit <- tryCatch(
    cv.glmnet(
      x_train,
      y_train,
      family = "binomial",
      alpha = alpha_enet,
      type.measure = "deviance",
      nfolds = 5
    ),
    error = function(e) NULL,
    warning = function(w) {
      suppressWarnings(
        cv.glmnet(
          x_train,
          y_train,
          family = "binomial",
          alpha = alpha_enet,
          type.measure = "deviance",
          nfolds = 5
        )
      )
    }
  )

  if (is.null(cv_fit)) {
    return(NULL)
  }

  # 4e. Extraction des coefficients au lambda optimal (lambda.min)
  coefs_mat <- as.matrix(coef(cv_fit, s = "lambda.min"))
  coefs_df <- tibble(
    term = rownames(coefs_mat),
    estimate = coefs_mat[, 1]
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      iteration = i,
      imputation_id = idx_imputation,
      # En LASSO, une variable "sélectionnée" = coefficient non-nul
      selectionnee = estimate != 0
    )

  selected_vars <- coefs_df %>% filter(selectionnee) %>% pull(term)

  if (length(selected_vars) == 0) {
    return(NULL)
  }

  list(
    coefs = coefs_df,
    selected_vars = selected_vars,
    lambda_min = cv_fit$lambda.min,
    n_vars = length(selected_vars)
  )
}

# --- 5. BOUCLE BOOTSTRAP PARALLÉLISÉE -------------------------------------------------
set.seed(42)
cat("Lancement du bootstrap LASSO (", n_bootstrap, "itérations)...\n")

with_progress({
  p <- progressor(steps = n_bootstrap)
  liste_results <- furrr::future_map(
    seq_len(n_bootstrap),
    function(idx) {
      res <- run_bootstrap_iteration(
        idx,
        mids_obj,
        cible,
        prop_train,
        niveaux,
        vars_candidates,
        alpha_enet
      )
      p()
      res
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
})
cat("Bootstrap terminé !\n\n")

n_valides <- sum(!map_lgl(liste_results, is.null))
cat("Itérations valides :", n_valides, "/", n_bootstrap, "\n\n")
if (n_valides == 0) {
  stop(
    "Aucune itération n'a produit de modèle valide. Vérifiez vos données ",
    "(nombre d'événements positifs, qualité de l'imputation)."
  )
}

# --- 6. AGRÉGATION DES RÉSULTATS ------------------------------------------------------

# 6a. Fréquence de sélection des variables (coefficient non-nul en LASSO)
df_selection <- tibble(
  iteration = seq_along(liste_results),
  selected_vars = map(liste_results, ~ if (!is.null(.x)) .x$selected_vars else character(0))
) %>%
  unnest(cols = c(selected_vars)) %>%
  filter(!is.na(selected_vars)) %>%
  group_by(selected_vars) %>%
  summarise(
    pct_selected = n() / n_bootstrap * 100,
    n_selected = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_selected))

cat("=== FRÉQUENCE DE SÉLECTION DES VARIABLES (coefficient LASSO non-nul) ===\n")
print(df_selection, n = Inf)

# 6b. Statistiques des coefficients
liste_coefs_valides <- compact(map(liste_results, ~ if (!is.null(.x)) .x$coefs else NULL))

if (length(liste_coefs_valides) == 0) {
  stop("Aucun coefficient n'a pu être calculé. Vérifiez vos données.")
}

df_coefs <- bind_rows(liste_coefs_valides)

# Pour le calcul des stats, on ne garde que les coefficients non-nuls
# (un coefficient à 0 en LASSO = variable exclue, pas un effet "nul" à interpréter)
df_coefs_non_nuls <- df_coefs %>% filter(selectionnee)

resume_coefs <- df_coefs_non_nuls %>%
  group_by(term) %>%
  summarise(
    beta_moyen = mean(estimate),
    beta_sd = sd(estimate),
    beta_cv = ifelse(abs(beta_moyen) > 0, abs(beta_sd / beta_moyen) * 100, NA_real_),
    beta_ic95_low = quantile(estimate, 0.025),
    beta_ic95_high = quantile(estimate, 0.975),
    pct_signe_pos = mean(estimate > 0) * 100,
    n_iterations = n(),
    .groups = "drop"
  ) %>%
  left_join(
    df_selection %>% rename(term = selected_vars),
    by = "term"
  ) %>%
  mutate(
    pct_selected = ifelse(is.na(pct_selected), 0, pct_selected),
    OR_moyen = exp(beta_moyen)
  ) %>%
  arrange(desc(pct_selected), beta_cv)

cat("\n=== RÉSUMÉ DES COEFFICIENTS (variables sélectionnées au moins une fois) ===\n")
print(resume_coefs, n = Inf)

# 6c. Nombre de variables sélectionnées par itération
df_n_vars <- tibble(
  iteration = seq_along(liste_results),
  n_vars = map_int(liste_results, ~ if (!is.null(.x)) length(.x$selected_vars) else 0L)
)

cat("\n=== NOMBRE DE VARIABLES SÉLECTIONNÉES PAR ITÉRATION ===\n")
cat("Moyenne :", round(mean(df_n_vars$n_vars), 2), "\n")
cat("Écart-type :", round(sd(df_n_vars$n_vars), 2), "\n")
cat("Min/Max :", min(df_n_vars$n_vars), "/", max(df_n_vars$n_vars), "\n\n")

# --- 7. SÉLECTION FINALE DES VARIABLES --------------------------------------------
vars_finales <- df_selection %>%
  filter(pct_selected >= seuil_selection) %>%
  pull(selected_vars)

if (length(vars_finales) == 0) {
  warning(
    "Aucune variable sélectionnée dans >= ",
    seuil_selection,
    "% des itérations. Utilisation des top 5."
  )
  vars_finales <- df_selection %>%
    slice_max(pct_selected, n = min(5, nrow(df_selection))) %>%
    pull(selected_vars)
}

cat("=== VARIABLES SÉLECTIONNÉES POUR LE MODÈLE FINAL ===\n")
cat("Critères : sélectionnée (coef. non-nul) dans >=", seuil_selection, "% des itérations\n\n")
cat("Variables retenues : ", paste(vars_finales, collapse = ", "), "\n\n")

# --- 8. MODÈLE FINAL POOLÉ (RÈGLE DE RUBIN) -----------------------------------------
# Note méthodologique : glmnet ne fournit pas nativement de p-value/std.error
# (ce n'est pas un modèle inférentiel classique). Pour le modèle final avec
# inférence statistique propre (IC, p-value), on réajuste un GLM non pénalisé
# sur les SEULES variables retenues par LASSO : c'est l'approche "post-LASSO"
# standard, qui permet ensuite le pooling de Rubin classique.

cat("=== MODÈLE FINAL POOLÉ (RUBIN) — approche post-LASSO ===\n")
cat("(GLM non pénalisé réajusté sur les variables retenues, pour permettre l'inférence)\n\n")

formule_finale <- as.formula(paste(cible, "~", paste(vars_finales, collapse = " + ")))

modeles_pooling <- with(
  mids_obj,
  glm(formule_finale, family = binomial(link = "logit"))
)

resultats_pool <- pool(modeles_pooling)

resume_final_logodds <- summary(resultats_pool, conf.int = TRUE, exponentiate = FALSE) %>%
  as.data.frame() %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate_log_odds = estimate, std.error_log_odds = std.error, p.value)

resume_final_OR <- summary(resultats_pool, conf.int = TRUE, exponentiate = TRUE) %>%
  as.data.frame() %>%
  filter(term != "(Intercept)") %>%
  select(term, OR = estimate, OR_IC_low = conf.low, OR_IC_high = conf.high)

resume_final <- resume_final_logodds %>%
  left_join(resume_final_OR, by = "term") %>%
  select(term, estimate_log_odds, std.error_log_odds, OR, OR_IC_low, OR_IC_high, p.value)

cat("\nRésultats poolés (règle de Rubin, post-LASSO) :\n")
print(resume_final, digits = 3)

if (n_positifs < 50) {
  cat(
    "\n⚠ Rappel : avec",
    n_positifs,
    "événements positifs, ces p-values et IC ",
    "post-LASSO restent à interpréter avec prudence (risque de biais de sélection).\n"
  )
}

# --- 9. VISUALISATIONS --------------------------------------------------------------

# 9a. Fréquence de sélection des variables (barplot)
p1 <- ggplot(
  df_selection,
  aes(x = reorder(selected_vars, pct_selected), y = pct_selected, fill = pct_selected)
) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_hline(yintercept = seuil_selection, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_gradient(low = "#ABDDA4", high = "#D7191C", guide = "none") +
  labs(
    title = "Fréquence de sélection des variables (LASSO, coef. non-nul)",
    subtitle = paste0(
      n_bootstrap,
      " itérations bootstrap | ",
      n_valides,
      " valides | alpha = ",
      alpha_enet
    ),
    x = "Variable",
    y = "% d'itérations où le coefficient est non-nul"
  ) +
  theme_minimal()

# 9b. Distribution des coefficients (variables sélectionnées >= seuil)
top_vars <- df_selection %>% filter(pct_selected >= seuil_selection) %>% pull(selected_vars)

if (length(top_vars) > 0) {
  df_coefs_top <- df_coefs_non_nuls %>% filter(term %in% top_vars)

  p2 <- ggplot(df_coefs_top, aes(x = estimate, fill = term)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~term, scales = "free") +
    labs(
      title = paste0(
        "Distribution bootstrap des coefficients LASSO (sélectionnées >= ",
        seuil_selection,
        "%)"
      ),
      x = "Coefficient β (pénalisé)",
      y = "Densité"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
} else {
  p2 <- ggplot() + ggtitle("Aucune variable sélectionnée au seuil défini")
}

# 9c. Nombre de variables sélectionnées par itération
p3 <- ggplot(df_n_vars, aes(x = n_vars)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution du nombre de variables sélectionnées par itération",
    x = "Nombre de variables (coef. non-nul)",
    y = "Fréquence"
  ) +
  theme_minimal()

print(p1)
print(p2)
print(p3)

# --- 10. SAUVEGARDE DES RÉSULTATS (OPTIONNEL) --------------------------------------
saveRDS(
  list(
    df_selection = df_selection,
    resume_coefs = resume_coefs,
    resume_final = resume_final,
    vars_finales = vars_finales
  ),
  file = "resultats_bootstrap_lasso.rds"
)

cat("\nRésultats sauvegardés dans 'resultats_bootstrap_lasso.rds'\n")

plan(sequential)
