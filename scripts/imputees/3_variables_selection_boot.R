# =============================================================================
# 1. CONFIGURATION INITIALE
# =============================================================================
# Liste des variables explicatives à tester (exclure la variable dépendante)
# Exemple : toutes les colonnes sauf l'outcome et l'ID
predictors <- setdiff(colnames(dfs_imp$data), c(resultat_candida_def, "iep"))
n_boot <- 200
alpha_stepwise <- 0.05
final_threshold <- 0.6

# =============================================================================
# 2. FONCTIONS UTILITAIRES
# =============================================================================
# Fonction pour effectuer une régression logistique stepwise forward
# et retourner les variables significatives
perform_stepwise_logistic <- function(data, outcome, predictors, alpha = 0.05) {
  data <- data[, c(outcome, predictors), drop = FALSE]
  data <- na.omit(data[, outcome])
  if (nrow(data) < 10) {
    return(NULL)
  }
  full_formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  null_model <- glm(full_formula, data = data, family = "binomial")
  stepwise_model <- stepAIC(
    null_model,
    direction = "forward",
    scope = list(
      upper = full_formula,
      lower = as.formula(paste(outcome, "~ 1"))
    ),
    trace = FALSE
  )
  summary_model <- summary(stepwise_model)
  significant_vars <- names(coef(stepwise_model))[
    names(coef(stepwise_model)) != "(Intercept)" &
      summary_model$coefficients[, "Pr(>|z|)"] < alpha
  ]
  return(significant_vars)
}

# Fonction pour une réplication bootstrap sur un jeu de données
bootstrap_stepwise <- function(data, outcome, predictors, alpha = 0.05, indices) {
  boot_data <- data[indices, , drop = FALSE]
  sig_vars <- perform_stepwise_logistic(boot_data, outcome, predictors, alpha)
  return(sig_vars)
}

# =============================================================================
# 3. EXECUTION DE L'ANALYSE
# =============================================================================

cat("Début de l'analyse...\n")
cat("Nombre de jeux imputés :", dfs_imp$m, "\n")
cat("Nombre de réplications bootstrap :", n_boot, "\n")
cat("Total de modèles à estimer :", dfs_imp$m * n_boot, "\n\n")

# Initialiser une liste pour stocker tous les résultats
all_results <- list()

# Compteur global
model_count <- 0
total_models <- dfs_imp$m * n_boot

# Pour chaque jeu imputé
for (imp in 1:dfs_imp$m) {
  cat("Traitement du jeu imputé", imp, "/", dfs_imp$m, "\n")
  # Extraire le jeu de données imputé complet
  imp_data <- complete(dfs_imp, imp)
  # Vérifier que l'outcome est binaire
  if (length(unique(imp_data[[resultat_candida_def]])) != 2) {
    stop("La variable dépendante doit être binaire (2 niveaux)")
  }
  # Initialiser la liste pour ce jeu
  imp_results <- list()
  # Pour chaque réplication bootstrap
  for (b in 1:n_boot) {
    model_count <- model_count + 1
    # Afficher la progression
    if (model_count %% 100 == 0) {
      cat("  Modèle", model_count, "/", total_models, "\n")
    }
    # Générer les indices bootstrap
    boot_indices <- sample(1:nrow(imp_data), replace = TRUE)
    # Exécuter la réplication bootstrap
    tryCatch(
      {
        sig_vars <- bootstrap_stepwise(
          imp_data,
          resultat_candida_def,
          predictors,
          alpha_stepwise,
          boot_indices
        )
        imp_results[[b]] <- if (is.null(sig_vars)) character(0) else sig_vars
      },
      error = function(e) {
        # En cas d'erreur, stocker NULL
        imp_results[[b]] <- character(0)
        warning("Erreur dans le modèle ", model_count, ": ", e$message)
      }
    )
  }
  all_results[[imp]] <- imp_results
}
cat("\nAnalyse terminée !\n")

# =============================================================================
# 4. AGREGATION DES RESULTATS
# =============================================================================
# Créer un tableau de fréquences pour chaque variable
frequency_table <- data.frame(
  Variable = predictors,
  Count = integer(length(predictors)),
  Frequency = numeric(length(predictors)),
  stringsAsFactors = FALSE
)

# Compter les occurrences de chaque variable
for (var in predictors) {
  count <- 0
  for (imp in 1:length(all_results)) {
    for (b in 1:length(all_results[[imp]])) {
      if (var %in% all_results[[imp]][[b]]) {
        count <- count + 1
      }
    }
  }
  frequency_table$Count[frequency_table$Variable == var] <- count
  frequency_table$Frequency[frequency_table$Variable == var] <- count / total_models
}

# Trier par fréquence décroissante
frequency_table <- frequency_table[order(-frequency_table$Frequency), ]

# =============================================================================
# 5. SELECTION DES VARIABLES SIGNIFICATIVES (SEUIL 60%)
# =============================================================================

significant_threshold <- final_threshold
significant_vars <- frequency_table[
  frequency_table$Frequency >= significant_threshold,
  "Variable"
]

cat("\n", "=" * 60, "\n")
cat(
  "VARIABLES SIGNIFICATIVES (présentes dans ≥",
  round(significant_threshold * 100),
  "% des modèles)\n"
)
cat("=" * 60, "\n")

if (length(significant_vars) > 0) {
  for (var in significant_vars) {
    freq <- frequency_table$Frequency[frequency_table$Variable == var]
    count <- frequency_table$Count[frequency_table$Variable == var]
    cat(sprintf("- %s : %.2f%% (%d/%d modèles)\n", var, freq * 100, count, total_models))
  }
} else {
  cat("Aucune variable n'atteint le seuil de 60%.\n")
}

cat("\n", "=" * 60, "\n")
cat("TABLEAU COMPLET DES FREQUENCES\n")
cat("=" * 60, "\n")
print(frequency_table)

# =============================================================================
# 6. VISUALISATION (OPTIONNEL)
# =============================================================================

# Charger ggplot2 si disponible
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Créer un graphique des fréquences
  ggplot(frequency_table, aes(x = reorder(Variable, Frequency), y = Frequency)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = significant_threshold, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = paste(
        "Fréquence d'inclusion des variables dans les modèles",
        "(Seuil 60% =",
        significant_threshold,
        ")"
      ),
      x = "Variable",
      y = "Fréquence d'inclusion"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# =============================================================================
# 7. SAUVEGARDE DES RESULTATS
# =============================================================================

# Sauvegarder l'objet complet des résultats
saveRDS(all_results, file = "bootstrap_stepwise_results.rds")

# Sauvegarder le tableau des fréquences
write.csv(frequency_table, file = "variable_frequencies.csv", row.names = FALSE)

cat("\nRésultats sauvegardés dans :\n")
cat("  - bootstrap_stepwise_results.rds (objets R)\n")
cat("  - variable_frequencies.csv (tableau CSV)\n")
