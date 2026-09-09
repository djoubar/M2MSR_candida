# ==============================================================================
#
#                        Analyse exploratoire de données
#
# ==============================================================================

library(ggplot2)
library(gtsummary)
library(dplyr)

# ------------------------------------------------------------------------------
# Dataframe
# ------------------------------------------------------------------------------

# df <- read.csv("")

# ------------------------------------------------------------------------------
# Fonctions
# ------------------------------------------------------------------------------

# Fonction pour analyser une variable individuelle
analyse_variable <- function(data, var_name) {
  cat("\n", rep("=", 60), "\n")
  cat(paste("Analyse de la variable:", var_name, "\n"))
  cat(rep("=", 60), "\n\n")

  # Type de la variable
  var_type <- class(data[[var_name]])
  cat("Type de variable:", paste(var_type, collapse=", "), "\n\n")

  # Statistiques descriptives
  if (is.numeric(data[[var_name]])) {
    cat("Statistiques pour variable numérique:\n")
    stats <- summary(data[[var_name]])
    print(stats)

    # Calculer min, max, médiane
    min_val <- min(data[[var_name]], na.rm = TRUE)
    max_val <- max(data[[var_name]], na.rm = TRUE)
    median_val <- median(data[[var_name]], na.rm = TRUE)

    cat("\nValeurs clés:\n")
    cat(sprintf("  Minimum: %.2f\n", min_val))
    cat(sprintf("  Maximum: %.2f\n", max_val))
    cat(sprintf("  Médiane: %.2f\n", median_val))

    # Density plot
    # cat("\nGénération du density plot...\n")
    # p <- ggplot(data, aes(x = .data[[var_name]])) +
    #   geom_density(fill = "steelblue", alpha = 0.5) +
    #   geom_vline(xintercept = median_val, color = "red", linetype = "dashed", linewidth = 1) +
    #   labs(title = paste("Distribution de", var_name),
    #        x = var_name,
    #        y = "Densité") +
    #   theme_classic()
    # print(p)

  } else if (is.factor(data[[var_name]]) || is.character(data[[var_name]])) {
    cat("Statistiques pour variable catégorielle:\n")
    freq_table <- table(data[[var_name]], useNA = "always")
    print(freq_table)

    # Barplot
    # cat("\nGénération du barplot...\n")
    # p <- ggplot(data, aes(x = .data[[var_name]])) +
    #   geom_bar(fill = "steelblue", alpha = 0.7) +
    #   labs(title = paste("Distribution de", var_name),
    #        x = var_name,
    #        y = "Fréquence") +
    #   theme_classic() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # print(p)

  } else {
    cat("Type de variable non supporté pour cette analyse automatique.\n")
    cat("Affichage des premières valeurs:\n")
    print(head(data[[var_name]]))
  }

  # Afficher les valeurs uniques pour examen détaillé
  cat("\n\nValeurs uniques (pour examen détaillé):\n")
  unique_vals <- unique(data[[var_name]])
  if (length(unique_vals) <= 50) {
    print(unique_vals)
  } else {
    cat(sprintf("Trop de valeurs uniques (%d). Affichage des 50 premières:\n", length(unique_vals)))
    print(head(unique_vals, 50))
  }
}

# Fonction pour analyser toutes les variables d'un dataframe
analyse_dataframe <- function(data) {
  cat("\n", rep("=", 60), "\n")
  cat("ANALYSE COMPLÈTE DU DATAFRAME\n")
  cat(rep("=", 60), "\n\n")

  # Aperçu du dataframe
  cat("Structure du dataframe:\n")
  str(data)
  cat("\n")

  # Résumé avec gtsummary
  # cat("\nRésumé statistique avec gtsummary:\n")
  # summary_table <- tbl_summary(data, by = NULL, missing = "no")
  # print(summary_table)

  # Analyse variable par variable
  var_names <- names(data)

  for (var in var_names) {
    analyse_variable(data, var)

    # Demander à l'utilisateur s'il veut continuer
    if (length(var_names) > 1) {
      cat("\n\n")
      readline(prompt = "Appuyez sur Entrée pour continuer vers la prochaine variable ou tapez 'quit' pour arrêter...")
      if (tolower(readline()) == "quit") {
        break
      }
    }
  }
}

# ------------------------------------------------------------------------------
# Analyse du dataframe
# ------------------------------------------------------------------------------

analyse_dataframe(df_base)
