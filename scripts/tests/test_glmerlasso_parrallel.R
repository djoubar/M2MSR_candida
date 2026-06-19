# SCRIPT R : glmmLasso (toutes variables) - VERSION PARALLÉLISÉE
# Variable à expliquer : resultat_candida_def (0/1)
# Effet aléatoire      : (1 | iep)
#
# Parallélisation via future.apply, en s'appuyant sur les fonctions
# .setup_parallel() / .stop_parallel() définies dans le .Rprofile :
#   - .setup_parallel() configure future::multisession avec un nombre de
#     workers borné par les cœurs ET la RAM disponibles, et bascule le
#     BLAS à 1 thread pour éviter la sur-souscription des cœurs.
#   - .stop_parallel() referme le cluster et restaure le BLAS séquentiel.
#
# Deux boucles sont parallélisées car totalement indépendantes itération
# par itération :
#   1) La recherche du lambda optimal sur la grille (31 ajustements)
#   2) Le bootstrap pour les IC95% du forest plot (n_boot ajustements)
#
# Le reste du script (préparation des données, modèle final, ROC,
# calibration) reste inchangé et séquentiel : ce sont des étapes uniques,
# la parallélisation n'y apporterait rien.
# =====================================================================

# ---- 0. Packages ----
# future et parallelly sont déjà requis par le .Rprofile (utilisés dans
# .setup_parallel()) ; on ajoute future.apply qui fournit les fonctions
# future_lapply()/future_sapply() utilisées ici.
packages <- c("glmmLasso", "ggplot2", "pROC", "dplyr", "future.apply", "mice")
a_installer <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(a_installer) > 0) {
  install.packages(a_installer)
}
invisible(lapply(packages, library, character.only = TRUE))

# ---- 1. Données ----
imp <- readRDS("donnees/df_impute.rds")
data <- mice::complete(imp, 1)

variable_cible <- "resultat_candida_def"
variable_cluster <- "iep"

data <- data |>
  dplyr::mutate(resultat_candida_def = ifelse(resultat_candida_def == "Positive", 1, 0))
data[[variable_cluster]] <- as.factor(data[[variable_cluster]])

# ---- 2. Construction automatique de la liste des prédicteurs ----
predicteurs <- setdiff(names(data), c(variable_cible, variable_cluster))
cat("Nombre de prédicteurs candidats avant nettoyage :", length(predicteurs), "\n")

variance_nulle <- sapply(data[predicteurs], function(x) {
  if (is.numeric(x)) var(x, na.rm = TRUE) == 0 else length(unique(na.omit(x))) <= 1
})
if (any(variance_nulle)) {
  cat(
    "Colonnes retirées car constantes :",
    paste(predicteurs[variance_nulle], collapse = ", "),
    "\n"
  )
  predicteurs <- predicteurs[!variance_nulle]
}

trop_de_niveaux <- sapply(data[predicteurs], function(x) {
  !is.numeric(x) && length(unique(na.omit(x))) > 20
})
if (any(trop_de_niveaux)) {
  cat(
    "Colonnes retirées car trop de niveaux (probable identifiant/texte) :",
    paste(predicteurs[trop_de_niveaux], collapse = ", "),
    "\n"
  )
  predicteurs <- predicteurs[!trop_de_niveaux]
}

# exclusions_manuelles <- c("nom_colonne_a_exclure")
# predicteurs <- setdiff(predicteurs, exclusions_manuelles)

cat("Nombre de prédicteurs retenus :", length(predicteurs), "\n")
cat(paste(predicteurs, collapse = ", "), "\n\n")

# ---- 3. Préparation du jeu de données pour le modèle ----
data_modele <- data[, c(variable_cible, predicteurs, variable_cluster)]
data_modele <- na.omit(data_modele)

cat(
  "Nombre de lignes après suppression des NA :",
  nrow(data_modele),
  "/ initial :",
  nrow(data),
  "\n\n"
)

for (v in predicteurs) {
  if (is.character(data_modele[[v]]) || is.logical(data_modele[[v]])) {
    data_modele[[v]] <- as.factor(data_modele[[v]])
  }
}

vars_num_peu_niveaux <- predicteurs[sapply(data_modele[predicteurs], function(x) {
  is.numeric(x) && length(unique(x)) <= 5
})]
if (length(vars_num_peu_niveaux) > 0) {
  cat(
    "Variables numériques à peu de niveaux (vérifiez s'il s'agit de catégories) :",
    paste(vars_num_peu_niveaux, collapse = ", "),
    "\n"
  )
}

vars_continues <- predicteurs[sapply(data_modele[predicteurs], is.numeric)]
data_modele[vars_continues] <- scale(data_modele[vars_continues])

# ---- 4. Construction automatique des formules fixe et aléatoire ----
fix_formula <- as.formula(
  paste(variable_cible, "~", paste(predicteurs, collapse = " + "))
)
rnd_formula <- setNames(list(~1), variable_cluster)

print(fix_formula)

# =====================================================================
# 5. SÉLECTION DU LAMBDA OPTIMAL - RECHERCHE EN PARALLÈLE
# =====================================================================

lambda_grid <- seq(0, 10, by = 10)

# Fonction ajustant un seul lambda, autonome (tous les arguments nécessaires
# sont passés explicitement, rien n'est capturé implicitement depuis
# l'environnement global - plus sûr et plus lisible pour le parallélisme)
fit_un_lambda <- function(lam, data_modele, fix_formula, rnd_formula) {
  fit_tmp <- tryCatch(
    {
      glmmLasso::glmmLasso(
        fix = fix_formula,
        rnd = rnd_formula,
        data = data_modele,
        lambda = lam,
        family = binomial(link = "logit"),
        control = list(print.iter = FALSE)
      )
    },
    error = function(e) NULL
  )

  if (is.null(fit_tmp)) NA_real_ else fit_tmp$bic
}

cat("Recherche du lambda optimal (", length(lambda_grid), "valeurs) en parallèle...\n")

.setup_parallel() # défini dans le .Rprofile

bic_values <- future_sapply(
  X = lambda_grid,
  FUN = fit_un_lambda,
  data_modele = data_modele,
  fix_formula = fix_formula,
  rnd_formula = rnd_formula,
  future.seed = TRUE,
  future.packages = "glmmLasso"
)

.stop_parallel() # ferme le cluster, restaure le BLAS séquentiel

resultats_bic <- data.frame(lambda = lambda_grid, bic = bic_values)
lambda_opt <- resultats_bic$lambda[which.min(resultats_bic$bic)]
cat("\nLambda optimal retenu (BIC minimal) :", lambda_opt, "\n")

if (lambda_opt == max(lambda_grid, na.rm = TRUE)) {
  cat(
    "ATTENTION : le lambda optimal est au bord supérieur de la grille.",
    "Augmentez la borne haute de lambda_grid et relancez.\n"
  )
}
cat("\n")

print(
  ggplot(resultats_bic, aes(x = lambda, y = bic)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = lambda_opt, linetype = "dashed", color = "red") +
    labs(title = "Sélection du lambda par BIC", x = "Lambda", y = "BIC") +
    theme_minimal()
)

# ---- 6. Modèle final (un seul ajustement, reste séquentiel) ----
modele_final <- glmmLasso(
  fix = fix_formula,
  rnd = rnd_formula,
  data = data_modele,
  lambda = lambda_opt,
  family = binomial(link = "logit"),
  control = list(print.iter = TRUE)
)

coefs_tous <- modele_final$coefficients
coefs_tous <- coefs_tous[names(coefs_tous) != "(Intercept)"]
cat("\nVariables retenues par le Lasso (coefficient non nul) :\n")
print(coefs_tous[coefs_tous != 0])
cat("\nVariables éliminées par le Lasso (coefficient = 0) :\n")
print(names(coefs_tous[coefs_tous == 0]))

# =====================================================================
# 7. FOREST PLOT - BOOTSTRAP EN PARALLÈLE
# =====================================================================

clusters <- unique(data_modele[[variable_cluster]])
n_boot <- 200
noms_coefs <- names(coefs_tous)

# Fonction réalisant une itération de bootstrap, autonome également
fit_un_bootstrap <- function(
  b,
  clusters,
  data_modele,
  variable_cluster,
  fix_formula,
  rnd_formula,
  lambda_opt,
  noms_coefs
) {
  clusters_boot <- sample(clusters, length(clusters), replace = TRUE)
  data_boot <- do.call(
    rbind,
    lapply(clusters_boot, function(cl) {
      data_modele[data_modele[[variable_cluster]] == cl, ]
    })
  )
  data_boot[[variable_cluster]] <- factor(data_boot[[variable_cluster]])

  fit_boot <- tryCatch(
    {
      glmmLasso::glmmLasso(
        fix = fix_formula,
        rnd = rnd_formula,
        data = data_boot,
        lambda = lambda_opt,
        family = binomial(link = "logit"),
        control = list(print.iter = FALSE)
      )
    },
    error = function(e) NULL
  )

  out <- setNames(rep(NA_real_, length(noms_coefs)), noms_coefs)
  if (!is.null(fit_boot)) {
    cf <- fit_boot$coefficients
    cf <- cf[names(cf) != "(Intercept)"]
    noms_communs <- intersect(names(cf), noms_coefs)
    out[noms_communs] <- cf[noms_communs]
  }
  out
}

cat("Bootstrap (", n_boot, "itérations) en parallèle...\n")
# Remarque : avec le parallélisme, les messages cat() à l'intérieur de la
# fonction ne s'affichent pas en temps réel (chaque itération tourne sur un
# worker distinct). Pour une vraie barre de progression, le package
# `progressr` peut être ajouté (handlers + with_progress()), facultatif ici.

.setup_parallel()

boot_results <- future_lapply(
  X = 1:n_boot,
  FUN = fit_un_bootstrap,
  clusters = clusters,
  data_modele = data_modele,
  variable_cluster = variable_cluster,
  fix_formula = fix_formula,
  rnd_formula = rnd_formula,
  lambda_opt = lambda_opt,
  noms_coefs = noms_coefs,
  future.seed = TRUE,
  future.packages = "glmmLasso"
)

.stop_parallel()

boot_coefs <- do.call(rbind, boot_results)
colnames(boot_coefs) <- noms_coefs

ic_inf <- apply(boot_coefs, 2, quantile, probs = 0.025, na.rm = TRUE)
ic_sup <- apply(boot_coefs, 2, quantile, probs = 0.975, na.rm = TRUE)

forest_df <- data.frame(
  variable = names(coefs_tous),
  beta = coefs_tous,
  ic_inf = ic_inf,
  ic_sup = ic_sup
)
forest_df$OR <- exp(forest_df$beta)
forest_df$OR_inf <- exp(forest_df$ic_inf)
forest_df$OR_sup <- exp(forest_df$ic_sup)

forest_df <- forest_df[forest_df$beta != 0, ]
forest_df <- forest_df[order(forest_df$OR), ]
forest_df$variable <- factor(forest_df$variable, levels = forest_df$variable)

print(
  ggplot(forest_df, aes(x = OR, y = variable)) +
    geom_point(size = 3, color = "darkblue") +
    geom_errorbarh(aes(xmin = OR_inf, xmax = OR_sup), height = 0.2, color = "darkblue") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_x_log10() +
    labs(
      title = "Forest plot des Odds Ratios - variables retenues par le Lasso",
      x = "Odds Ratio (échelle log) avec IC95% bootstrap",
      y = NULL
    ) +
    theme_minimal()
)

# =====================================================================
# 8. COURBE ROC ET AUC (séquentiel - un seul calcul)
# =====================================================================

pred_lin <- predict(modele_final, data_modele)
pred_proba <- 1 / (1 + exp(-pred_lin))

roc_obj <- roc(data_modele[[variable_cible]], pred_proba, ci = TRUE)
auc_value <- auc(roc_obj)

cat("AUC :", round(auc_value, 3), "\n")
cat("IC95% AUC :", round(ci(roc_obj), 3), "\n")

plot(roc_obj, col = "darkblue", lwd = 2, main = paste0("Courbe ROC - AUC = ", round(auc_value, 3)))

# =====================================================================
# 9. COURBE DE CALIBRATION (séquentiel - un seul calcul)
# =====================================================================

data_calib <- data.frame(observe = data_modele[[variable_cible]], predit = pred_proba)

data_calib$decile <- cut(
  data_calib$predit,
  breaks = quantile(data_calib$predit, probs = seq(0, 1, 0.1)),
  include.lowest = TRUE
)

calib_summary <- data_calib %>%
  group_by(decile) %>%
  summarise(
    proba_moyenne_predite = mean(predit),
    proportion_observee = mean(observe),
    n = n(),
    .groups = "drop"
  )

print(
  ggplot(calib_summary, aes(x = proba_moyenne_predite, y = proportion_observee)) +
    geom_point(size = 3, color = "darkred") +
    geom_line(color = "darkred") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(
      title = "Courbe de calibration",
      x = "Probabilité prédite moyenne (par décile)",
      y = "Proportion observée d'événements"
    ) +
    theme_minimal()
)
