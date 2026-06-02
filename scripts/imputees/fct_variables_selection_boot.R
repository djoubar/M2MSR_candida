# =============================================================================
# 1. CONFIGURATION INITIALE
# =============================================================================
mids_object <- read_rds("donnees/df_impute.rds")
outcome_var <- "resultat_candida_def"
predictors <- setdiff(colnames(mids_object$data), c("iep", outcome_var))
n_boot <- 200
alpha_stepwise <- 0.05
final_threshold <- 0.6

# Vérifications préalables
if (!inherits(mids_object, "mids")) {
  stop("L'objet n'est pas de type 'mids'. Vérifie ton fichier.")
}
if (length(unique(mids_object$data[[outcome_var]])) != 2) {
  stop(paste("La variable", outcome_var, "n'est pas binaire."))
}

# =============================================================================
# 2. FONCTIONS UTILITAIRES (TOUTES DÉFINIES)
# =============================================================================

# Fonction 1 : Régression stepwise
perform_stepwise_logistic <- function(data, outcome, predictors, alpha = 0.05) {
  # Sélection des colonnes nécessaires
  data <- data[, c(outcome, predictors), drop = FALSE]

  # Suppression des lignes avec NA
  data <- na.omit(data)

  # Vérification taille minimale
  if (nrow(data) < 10) {
    return(character(0))
  }

  # Vérification que l'outcome est bien binaire
  if (length(unique(data[[outcome]])) != 2) {
    return(character(0))
  }

  # Création de la formule
  full_formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))

  # Modèle initial (avec toutes les variables)
  null_model <- tryCatch(
    {
      glm(full_formula, data = data, family = binomial)
    },
    error = function(e) NULL
  )

  if (is.null(null_model)) {
    return(character(0))
  }

  # Stepwise forward
  stepwise_model <- tryCatch(
    {
      stepAIC(
        null_model,
        direction = "forward",
        scope = list(
          upper = full_formula,
          lower = as.formula(paste(outcome, "~ 1"))
        ),
        trace = FALSE
      )
    },
    error = function(e) NULL
  )

  if (is.null(stepwise_model)) {
    return(character(0))
  }

  # Extraction des variables significatives
  summary_model <- summary(stepwise_model)
  significant_vars <- names(coef(stepwise_model))[
    names(coef(stepwise_model)) != "(Intercept)" &
      !is.na(summary_model$coefficients[, "Pr(>|z|)"]) &
      summary_model$coefficients[, "Pr(>|z|)"] < alpha
  ]

  return(significant_vars)
}

# Fonction 2 : Bootstrap (DEFINIE ICI - C'EST LE PROBLEME PRINCIPAL)
bootstrap_stepwise <- function(data, outcome, predictors, alpha = 0.05, indices) {
  boot_data <- data[indices, , drop = FALSE]
  sig_vars <- perform_stepwise_logistic(boot_data, outcome, predictors, alpha)
  return(sig_vars)
}

# =============================================================================
# 3. EXÉCUTION DE L'ANALYSE (AVEC VÉRIFICATIONS SUPPLÉMENTAIRES)
# =============================================================================
cat("Début de l'analyse...\n")
cat("Nombre de jeux imputés :", mids_object$m, "\n")
cat("Nombre de réplications bootstrap :", n_boot, "\n")
cat("Total de modèles à estimer :", mids_object$m * n_boot, "\n\n")

all_results <- vector("list", mids_object$m)
model_count <- 0
total_models <- mids_object$m * n_boot
success_count <- 0

for (i in 1:mids_object$m) {
  cat("Traitement du jeu imputé", i, "/", mids_object$m, "\n")

  # Extraction du jeu imputé
  imp_data <- complete(mids_object, i)

  # Vérification des NA dans l'outcome
  if (sum(is.na(imp_data[[outcome_var]])) > 0) {
    warning(paste("NA détectés dans", outcome_var, "pour le jeu", i))
    imp_data <- imp_data[!is.na(imp_data[[outcome_var]]), ]
  }

  imp_results <- vector("list", n_boot)

  for (b in 1:n_boot) {
    model_count <- model_count + 1

    if (model_count %% 100 == 0) {
      cat("  Modèle", model_count, "/", total_models, "\n")
    }

    # Génération des indices bootstrap
    boot_indices <- sample(1:nrow(imp_data), replace = TRUE)

    # Exécution avec vérification
    sig_vars <- tryCatch(
      {
        bootstrap_stepwise(
          imp_data,
          outcome_var,
          predictors,
          alpha_stepwise,
          boot_indices
        )
      },
      error = function(e) {
        warning("Erreur modèle ", model_count, ": ", e$message)
        return(character(0))
      }
    )

    imp_results[[b]] <- sig_vars
    if (length(sig_vars) > 0) success_count <- success_count + 1
  }

  all_results[[i]] <- imp_results
}

cat("\nAnalyse terminée !\n")
cat("Modèles réussis :", success_count, "/", total_models, "\n")

# =============================================================================
# 4. AGRÉGATION DES RÉSULTATS (AVEC GESTION DES CAS VIDES)
# =============================================================================
frequency_table <- data.frame(
  Variable = predictors,
  Count = integer(length(predictors)),
  Frequency = numeric(length(predictors)),
  stringsAsFactors = FALSE
)

for (var in predictors) {
  count <- 0
  for (i in 1:length(all_results)) {
    for (b in 1:length(all_results[[i]])) {
      current_vars <- all_results[[i]][[b]]
      if (!is.null(current_vars) && var %in% current_vars) {
        count <- count + 1
      }
    }
  }
  frequency_table$Count[frequency_table$Variable == var] <- count
  frequency_table$Frequency[frequency_table$Variable == var] <- count / total_models
}

frequency_table <- frequency_table[order(-frequency_table$Frequency), ]

# =============================================================================
# 5. SÉLECTION DES VARIABLES SIGNIFICATIVES
# =============================================================================
significant_vars <- frequency_table[
  frequency_table$Frequency >= final_threshold,
  "Variable"
]

cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat(paste(
  "VARIABLES SIGNIFICATIVES (présentes dans ≥",
  round(final_threshold * 100),
  "% des modèles)\n"
))
cat(paste0(rep("=", 60), collapse = ""), "\n")

if (length(significant_vars) > 0) {
  for (var in significant_vars) {
    freq <- frequency_table$Frequency[frequency_table$Variable == var]
    count <- frequency_table$Count[frequency_table$Variable == var]
    cat(sprintf("- %s : %.2f%% (%d/%d modèles)\n", var, freq * 100, count, total_models))
  }
} else {
  cat("Aucune variable n'atteint le seuil de 60%.\n")
  cat("Seuil le plus élevé atteint :", max(frequency_table$Frequency) * 100, "%\n")
}

cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("TABLEAU COMPLET DES FRÉQUENCES\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")
print(frequency_table)

# =============================================================================
# 6. VISUALISATION
# =============================================================================
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  p <- ggplot(frequency_table, aes(x = reorder(Variable, Frequency), y = Frequency)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = final_threshold, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = paste("Fréquence d'inclusion des variables (Seuil 60%)"),
      x = "Variable",
      y = "Fréquence"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
}

# =============================================================================
# 7. SAUVEGARDE
# =============================================================================
saveRDS(all_results, file = "bootstrap_stepwise_results.rds")
write.csv(frequency_table, file = "variable_frequencies.csv", row.names = FALSE)
cat("\nRésultats sauvegardés.\n")
