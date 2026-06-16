# =====================================================================
# SCRIPT R : Régression logistique mixte LASSO (glmmLasso)
# Variable à expliquer : resultat_candida_def (0/1)
# Effet aléatoire      : (1 | iep)
#
# Étapes :
#   1. Préparation des données
#   2. Sélection du lambda optimal (grille + BIC)
#   3. Ajustement du modèle final glmmLasso
#   4. Forest plot des coefficients (Odds Ratios, IC95% par bootstrap)
#   5. Courbe ROC + AUC
#   6. Courbe de calibration
#
# IMPORTANT :
#   - glmmLasso ne tolère pas les NA -> on filtre les lignes complètes
#   - glmmLasso ne fournit pas d'erreur standard fiable pour des
#     coefficients pénalisés -> les IC du forest plot sont obtenus par
#     bootstrap (ré-échantillonnage par cluster "iep")
#   - Les courbes ROC/calibration ci-dessous sont calculées "en interne"
#     (mêmes données que l'ajustement) : c'est optimiste. Pour une
#     évaluation plus honnête, remplacez par une validation croisée
#     (k-fold en respectant les clusters iep) ou un échantillon externe.
# =====================================================================

# ---- 0. Packages ----
packages <- c("glmmLasso", "ggplot2", "pROC", "dplyr")
a_installer <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(a_installer) > 0) {
  install.packages(a_installer)
}
invisible(lapply(packages, library, character.only = TRUE))

# ---- 1. Données ----
# Remplacer par l'import réel de vos données :
source("scripts/survie/_setup_survie.R")
data <- df_fg

# =====================================================================
# SCRIPT R : glmmLasso en utilisant TOUTES LES COLONNES du jeu de données
# comme variables explicatives candidates
# Variable à expliquer : resultat_candida_def (0/1)
# Effet aléatoire      : (1 | iep)
#
# Différence avec le script précédent : la formule fixe n'est plus écrite
# à la main, elle est construite automatiquement à partir de toutes les
# colonnes du data.frame (hors variable cible et hors identifiant de cluster).
#
# ATTENTION - points de vigilance avec une approche "toutes variables" :
#   1) glmmLasso ne tolère pas les NA -> les colonnes très incomplètes
#      peuvent faire perdre beaucoup de lignes après na.omit().
#   2) glmmLasso pénalise CHAQUE indicatrice d'une variable catégorielle
#      séparément (pas de pénalisation groupée par variable) : une variable
#      catégorielle à plusieurs niveaux peut donc être retenue partiellement
#      (certains niveaux gardés, d'autres ramenés à 0).
#   3) Plus il y a de variables, plus il faut explorer une grille de lambda
#      plus large pour trouver un optimum (sinon le modèle reste saturé).
#   4) Les colonnes de type identifiant (numéro de dossier, texte libre...)
#      doivent être exclues : elles n'ont aucun sens comme prédicteurs.
#   5) Avec beaucoup de variables et un faible nombre d'événements, le risque
#      de surapprentissage augmente fortement (cf. règle empirique des
#      "10 événements par variable" en régression logistique classique).
# =====================================================================

variable_cible <- "resultat_candida_def"
variable_cluster <- "iep"

data[[variable_cible]] <- as.numeric(as.character(data[[variable_cible]]))
data[[variable_cluster]] <- as.factor(data[[variable_cluster]])

# ---- 2. Construction automatique de la liste des prédicteurs ----
# Toutes les colonnes SAUF la variable cible et l'identifiant de cluster
predicteurs <- setdiff(names(data), c(variable_cible, variable_cluster))
cat("Nombre de prédicteurs candidats avant nettoyage :", length(predicteurs), "\n")

# --- 2a. Retirer les colonnes constantes ou quasi-constantes ---
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

# --- 2b. Retirer les colonnes probablement de type identifiant / texte libre ---
# Seuil arbitraire à ajuster : variable non numérique avec > 20 niveaux uniques
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

# --- 2c. Vous pouvez exclure ici manuellement des colonnes non pertinentes ---
# (dates brutes, doublons d'une autre variable, variables connues comme
#  des proxys de la variable cible, etc.)
# exclusions_manuelles <- c("nom_colonne_a_exclure")
# predicteurs <- setdiff(predicteurs, exclusions_manuelles)

cat("Nombre de prédicteurs retenus :", length(predicteurs), "\n")
cat(paste(predicteurs, collapse = ", "), "\n\n")

# ---- 3. Préparation du jeu de données pour le modèle ----
data_modele <- data[, c(variable_cible, predicteurs, variable_cluster)]
data_modele <- na.omit(data_modele) # glmmLasso ne tolère pas les NA

cat(
  "Nombre de lignes après suppression des NA :",
  nrow(data_modele),
  "/ initial :",
  nrow(data),
  "\n\n"
)

# Conversion en facteur des variables de type texte ou logique
for (v in predicteurs) {
  if (is.character(data_modele[[v]]) || is.logical(data_modele[[v]])) {
    data_modele[[v]] <- as.factor(data_modele[[v]])
  }
}

# Repérage des variables numériques codant potentiellement une catégorie
# (ex : 0/1/2 pour une sévérité) -> à vérifier manuellement, conversion non
# automatique pour éviter de transformer à tort une variable continue.
vars_num_peu_niveaux <- predicteurs[sapply(data_modele[predicteurs], function(x) {
  is.numeric(x) && length(unique(x)) <= 5
})]
if (length(vars_num_peu_niveaux) > 0) {
  cat(
    "Variables numériques à peu de niveaux (vérifiez s'il s'agit de catégories) :",
    paste(vars_num_peu_niveaux, collapse = ", "),
    "\n"
  )
  # Pour convertir l'une d'elles en facteur :
  # data_modele$nom_variable <- as.factor(data_modele$nom_variable)
}

# Centrage/réduction des variables continues uniquement
vars_continues <- predicteurs[sapply(data_modele[predicteurs], is.numeric)]
data_modele[vars_continues] <- scale(data_modele[vars_continues])

# ---- 4. Construction automatique des formules fixe et aléatoire ----
fix_formula <- as.formula(
  paste(variable_cible, "~", paste(predicteurs, collapse = " + "))
)
rnd_formula <- setNames(list(~1), variable_cluster)

print(fix_formula)

# ---- 5. Sélection du lambda optimal par grille + BIC ----
# Grille plus large que pour un modèle restreint, car davantage de variables
# à pénaliser : à ajuster si le minimum de BIC est atteint en bord de grille.
lambda_grid <- seq(0, 300, by = 10)
bic_values <- rep(NA_real_, length(lambda_grid))

set.seed(123)
for (i in seq_along(lambda_grid)) {
  lam <- lambda_grid[i]
  cat("Ajustement avec lambda =", lam, "\n")

  fit_tmp <- tryCatch(
    {
      glmmLasso(
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

  if (!is.null(fit_tmp)) bic_values[i] <- fit_tmp$bic
}

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

# ---- 6. Modèle final ----
modele_final <- glmmLasso(
  fix = fix_formula,
  rnd = rnd_formula,
  data = data_modele,
  lambda = lambda_opt,
  family = binomial(link = "logit"),
  control = list(print.iter = TRUE)
)

summary(modele_final)

# Variables effectivement retenues par le Lasso (coefficient != 0)
coefs_tous <- modele_final$coefficients
coefs_tous <- coefs_tous[names(coefs_tous) != "(Intercept)"]
cat("\nVariables retenues par le Lasso (coefficient non nul) :\n")
print(coefs_tous[coefs_tous != 0])
cat("\nVariables éliminées par le Lasso (coefficient = 0) :\n")
print(names(coefs_tous[coefs_tous == 0]))

# =====================================================================
# 7. FOREST PLOT (Odds Ratios + IC95% bootstrap, par cluster iep)
# =====================================================================

clusters <- unique(data_modele[[variable_cluster]])
n_boot <- 200 # à réduire si le temps de calcul est trop long (modèle saturé)
boot_coefs <- matrix(NA_real_, nrow = n_boot, ncol = length(coefs_tous))
colnames(boot_coefs) <- names(coefs_tous)

set.seed(456)
for (b in 1:n_boot) {
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
      glmmLasso(
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

  if (!is.null(fit_boot)) {
    cf <- fit_boot$coefficients
    cf <- cf[names(cf) != "(Intercept)"]
    noms_communs <- intersect(names(cf), colnames(boot_coefs))
    boot_coefs[b, noms_communs] <- cf[noms_communs]
  }
  cat("Bootstrap", b, "/", n_boot, "\r")
}
cat("\n")

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

# Ne garder que les variables sélectionnées par le Lasso (recommandé ici,
# car avec "toutes les colonnes" le graphique serait illisible sinon)
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
# 8. COURBE ROC ET AUC
# =====================================================================

pred_lin <- predict(modele_final, data_modele)
pred_proba <- 1 / (1 + exp(-pred_lin))

roc_obj <- roc(data_modele[[variable_cible]], pred_proba, ci = TRUE)
auc_value <- auc(roc_obj)

cat("AUC :", round(auc_value, 3), "\n")
cat("IC95% AUC :", round(ci(roc_obj), 3), "\n")

plot(roc_obj, col = "darkblue", lwd = 2, main = paste0("Courbe ROC - AUC = ", round(auc_value, 3)))

# =====================================================================
# 9. COURBE DE CALIBRATION
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
