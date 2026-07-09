# =====================================================================# =====================================================================
# SCRIPT R : Sélection de variables par régression logistique mixte LASSO
#            (glmmLasso) sur données multi-imputées (objet mids "imp")
#
# Variable à expliquer : resultat_candida_def (0/1)
# Effet aléatoire      : (1 | iep)
#
# Objectif : ne produire QU'une sélection de variables (pas de forest plot,
# pas de courbe ROC, pas de calibration), en tenant compte des m
# imputations contenues dans l'objet mids "imp".
#
# Principe (approche "vote majoritaire", cf. Wood et al. 2008,
# "How should variable selection be performed with multiply imputed
# data?", Statistics in Medicine) :
#   1. On ajuste un glmmLasso séparément sur CHACUN des m jeux de données
#      complétés issus de "imp".
#   2. Le lambda de pénalisation est choisi UNE SEULE FOIS pour tous les
#      jeux imputés, en minimisant l'AIC MOYEN sur les m jeux (afin de ne
#      pas obtenir m sélections de variables incohérentes entre elles).
#   3. Une variable est considérée comme "retenue" si son coefficient est
#      non nul dans au moins une proportion "seuil_selection" des m jeux
#      imputés (0.5 par défaut = majorité absolue).
#   4. Pour les variables retenues, on rapporte à titre indicatif le
#      coefficient moyen (moyenné uniquement sur les imputations où la
#      variable a été sélectionnée). Ce n'est PAS une estimation poolée
#      au sens des règles de Rubin, qui ne s'appliquent pas directement à
#      des coefficients pénalisés (biaisés par construction).
#
# Prérequis : un objet mids nommé "imp" doit déjà exister dans
# l'environnement (issu par ex. de mice::mice(...)).
# =====================================================================

# ---- 0. Packages ----
packages <- c("glmmLasso", "tidyverse", "dplyr", "purrr", "mice")
invisible(lapply(packages, library, character.only = TRUE))

imp <- readRDS("donnees/df_impute.rds")

variable_cible <- "resultat_candida_def"
variable_cluster <- "iep"

m <- imp$m
cat("Nombre d'imputations dans l'objet mids 'imp' :", m, "\n\n")

# ---- 1. Liste des jeux de données complétés ----
liste_data <- map(seq_len(m), ~ complete(imp, action = .x))

# Recodage de la cible (identique pour toutes les imputations, car la
# variable cible n'est en principe jamais imputée quand elle sert d'outcome)
liste_data <- map(liste_data, function(d) {
  d[[variable_cible]] <- ifelse(d[[variable_cible]] == "Positive", 1, 0)
  d[[variable_cluster]] <- as.factor(d[[variable_cluster]])
  d
})

# ---- 2. Construction de la liste des prédicteurs candidats ----
# Basée sur le PREMIER jeu imputé : la structure des colonnes (types,
# modalités disponibles) est la même dans les m jeux, seules les valeurs
# imputées diffèrent.
data_ref <- liste_data[[1]]

predicteurs <- setdiff(names(data_ref), c(variable_cible, variable_cluster))
cat("Nombre de prédicteurs candidats avant nettoyage :", length(predicteurs), "\n")

# --- 2a. Retirer les colonnes constantes ou quasi-constantes ---
variance_nulle <- sapply(data_ref[predicteurs], function(x) {
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
trop_de_niveaux <- sapply(data_ref[predicteurs], function(x) {
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

# --- 2c. Exclusions manuelles éventuelles ---
# exclusions_manuelles <- c("nom_colonne_a_exclure")
# predicteurs <- setdiff(predicteurs, exclusions_manuelles)

cat("Nombre de prédicteurs retenus :", length(predicteurs), "\n")
cat(paste(predicteurs, collapse = ", "), "\n\n")

# ---- 3. Préparation homogène des m jeux de données ----
preparer_jeu <- function(d) {
  d <- d[, c(variable_cible, predicteurs, variable_cluster)]

  # Comme les données sont imputées, il ne devrait normalement plus y
  # avoir de NA ; on filtre par sécurité (ex : variables non incluses
  # dans le modèle d'imputation).
  n_avant <- nrow(d)
  d <- na.omit(d)
  if (nrow(d) < n_avant) {
    cat(
      "Attention : ",
      n_avant - nrow(d),
      " lignes supprimées pour NA résiduels après imputation.\n"
    )
  }

  # Conversion en facteur des variables texte / logiques
  for (v in predicteurs) {
    if (is.character(d[[v]]) || is.logical(d[[v]])) {
      d[[v]] <- as.factor(d[[v]])
    }
  }

  # Centrage/réduction des variables continues
  vars_continues <- predicteurs[sapply(d[predicteurs], is.numeric)]
  if (length(vars_continues) > 0) {
    d[vars_continues] <- scale(d[vars_continues])
  }

  d
}

liste_data_modele <- map(liste_data, preparer_jeu)

# ---- 4. Formules fixe et aléatoire (communes aux m jeux) ----
fix_formula <- as.formula(
  paste(variable_cible, "~", paste(predicteurs, collapse = " + "))
)
rnd_formula <- setNames(list(~1), variable_cluster)
print(fix_formula)

# ---- 5. Sélection du lambda optimal : AIC moyenné sur les m imputations ----
lambda_grid <- seq(0, 300, by = 10)
aic_matrix <- matrix(NA_real_, nrow = length(lambda_grid), ncol = m)

set.seed(123)
for (i in seq_along(lambda_grid)) {
  lam <- lambda_grid[i]
  cat("Ajustement avec lambda =", lam, "\n")

  for (j in seq_len(m)) {
    fit_tmp <- tryCatch(
      {
        glmmLasso(
          fix = fix_formula,
          rnd = rnd_formula,
          data = liste_data_modele[[j]],
          lambda = lam,
          family = binomial(link = "logit"),
          control = list(print.iter = FALSE)
        )
      },
      error = function(e) NULL
    )
    if (!is.null(fit_tmp)) aic_matrix[i, j] <- fit_tmp$aic
  }
}

aic_moyen <- rowMeans(aic_matrix, na.rm = TRUE)
resultats_aic <- data.frame(lambda = lambda_grid, aic_moyen = aic_moyen)
lambda_opt <- resultats_aic$lambda[which.min(resultats_aic$aic_moyen)]

cat(
  "\nLambda optimal retenu (AIC moyen minimal sur les",
  m,
  "imputations) :",
  lambda_opt,
  "\n"
)

if (lambda_opt == max(lambda_grid, na.rm = TRUE)) {
  cat(
    "ATTENTION : le lambda optimal est au bord supérieur de la grille.",
    "Augmentez la borne haute de lambda_grid et relancez.\n"
  )
}
cat("\n")

print(
  ggplot(resultats_aic, aes(x = lambda, y = aic_moyen)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = lambda_opt, linetype = "dashed", color = "red") +
    labs(
      title = paste("Sélection du lambda par AIC moyen sur", m, "imputations"),
      x = "Lambda",
      y = "AIC moyen"
    ) +
    theme_minimal()
)

# ---- 6. Ajustement du glmmLasso final sur chacun des m jeux imputés ----
modeles_finaux <- map(liste_data_modele, function(d) {
  glmmLasso(
    fix = fix_formula,
    rnd = rnd_formula,
    data = d,
    lambda = lambda_opt,
    family = binomial(link = "logit"),
    control = list(print.iter = FALSE)
  )
})

saveRDS(modeles_finaux, file = "mod_glmmLasso_MI.rds")

# =====================================================================
# 7. SÉLECTION DE VARIABLES : fréquence de sélection sur les m imputations
# =====================================================================

noms_coefs <- setdiff(names(modeles_finaux[[1]]$coefficients), "(Intercept)")

coef_matrix <- matrix(
  NA_real_,
  nrow = m,
  ncol = length(noms_coefs),
  dimnames = list(NULL, noms_coefs)
)

for (j in seq_len(m)) {
  cf <- modeles_finaux[[j]]$coefficients
  cf <- cf[names(cf) != "(Intercept)"]
  noms_communs <- intersect(names(cf), noms_coefs)
  coef_matrix[j, noms_communs] <- cf[noms_communs]
}

selection_freq <- colMeans(coef_matrix != 0, na.rm = TRUE)
coef_moyen_si_selectionne <- apply(coef_matrix, 2, function(x) {
  mean(x[x != 0], na.rm = TRUE)
})

tableau_selection <- data.frame(
  variable = noms_coefs,
  frequence_selection = selection_freq,
  coef_moyen_si_selectionne = coef_moyen_si_selectionne,
  OR_moyen_si_selectionne = exp(coef_moyen_si_selectionne)
) |>
  arrange(desc(frequence_selection))

cat("\n===== Fréquence de sélection des variables sur les", m, "imputations =====\n")
print(tableau_selection, row.names = FALSE)

# ---- 8. Liste finale des variables retenues (seuil de majorité) ----
seuil_selection <- 0.5 # à ajuster : 0.5 = majorité absolue des m imputations

variables_retenues <- tableau_selection$variable[
  tableau_selection$frequence_selection >= seuil_selection
]

cat(
  "\nVariables retenues (fréquence de sélection >=",
  seuil_selection,
  ") :\n"
)
print(variables_retenues)
cat("\nNombre de variables retenues :", length(variables_retenues), "\n")

# ---- 9. Sauvegarde des résultats ----
saveRDS(
  list(
    lambda_opt = lambda_opt,
    tableau_selection = tableau_selection,
    variables_retenues = variables_retenues,
    seuil_selection = seuil_selection
  ),
  file = "selection_variables_glmmLasso_MI.rds"
)

write.csv2(
  tableau_selection,
  file = "selection_variables_glmmLasso_MI.csv",
  row.names = FALSE
)
