# =============================================================================
# BOOTSTRAP STRATIFIÉ + RANDOM FOREST + FEATURE IMPORTANCE
# Sur un objet "mids" (imputation multiple via mice)
#
# PRINCIPE :
#   Un objet mids contient m datasets complètement imputés.
#   À chaque itération bootstrap, on :
#     1. Tire aléatoirement UN des m datasets imputés
#     2. Applique un tirage stratifié bootstrap sur ce dataset
#     3. Entraîne un Random Forest
#     4. Récupère la feature importance
#   → On intègre ainsi à la fois l'incertitude d'imputation ET
#     la variabilité d'échantillonnage.
# =============================================================================

# --- 0. PACKAGES --------------------------------------------------------------
library(mice)
library(randomForest)
library(dplyr)
library(ggplot2)

# --- 1. PARAMÈTRES À ADAPTER --------------------------------------------------

mids_obj <- readRDS("donnees/df_impute.rds")
cible <- "resultat_candida_def" # variable à prédire
n_bootstrap <- 2 # nombre d'itérations bootstrap
prop_train <- 0.3 # proportion pour l'entraînement

# Hyperparamètres Random Forest
ntree_rf <- 500
mtry_rf <- NULL # NULL = sqrt(p) par défaut


# --- 2. VÉRIFICATION DE L'OBJET MIDS ------------------------------------------

# Nombre de datasets imputés disponibles
m_imputations <- mids_obj$m
cat("Nombre de datasets imputés (m) :", m_imputations, "\n")

# Extraire le 1er dataset pour inspecter la structure
df_exemple <- complete(mids_obj, action = 1)

# Vérification de la cible
df_exemple[[cible]] <- as.factor(df_exemple[[cible]])
cat("\nDistribution de la variable cible (dataset imputé #1) :\n")
print(table(df_exemple[[cible]]))
cat("Proportions :\n")
print(prop.table(table(df_exemple[[cible]])))

niveaux <- levels(df_exemple[[cible]])
cat("\nClasses détectées :", paste(niveaux, collapse = " / "), "\n\n")


# --- 3. FONCTION DE TIRAGE STRATIFIÉ ------------------------------------------
# Identique au script précédent : tirage séparé par classe pour
# conserver le ratio positifs/négatifs.

tirage_stratifie <- function(y, prop) {
  indices <- seq_along(y)
  idx_par_classe <- lapply(niveaux, function(cl) {
    idx_cl <- indices[y == cl]
    n_tirer <- round(length(idx_cl) * prop)
    sample(idx_cl, size = n_tirer, replace = TRUE)
  })
  unlist(idx_par_classe)
}


# --- 4. BOUCLE BOOTSTRAP SUR MIDS ---------------------------------------------
# Double source de variabilité intégrée :
#   - Variabilité d'imputation  : on tire un dataset imputé différent à chaque tour
#   - Variabilité d'échantillon : tirage stratifié bootstrap dans ce dataset

set.seed(42)

liste_importances <- vector("list", n_bootstrap)
oob_errors <- numeric(n_bootstrap)

cat("Lancement du bootstrap (", n_bootstrap, "itérations) sur objet mids...\n")

for (i in seq_len(n_bootstrap)) {
  # 4a. Tirer aléatoirement un des m datasets imputés
  idx_imputation <- sample(seq_len(m_imputations), size = 1)
  df_complet <- complete(mids_obj, action = idx_imputation)

  # S'assurer que la cible est bien un factor dans ce dataset
  df_complet[[cible]] <- as.factor(df_complet[[cible]])

  # 4b. Séparer features et cible
  X <- df_complet %>% dplyr::select(-all_of(cible))
  y <- df_complet[[cible]]

  # 4c. Tirage stratifié bootstrap
  idx_train <- tirage_stratifie(y, prop_train)
  X_train <- X[idx_train, , drop = FALSE]
  y_train <- y[idx_train]

  # 4d. Entraînement du Random Forest
  mtry_val <- if (is.null(mtry_rf)) floor(sqrt(ncol(X_train))) else mtry_rf

  rf_model <- randomForest(
    x = X_train,
    y = y_train,
    ntree = ntree_rf,
    mtry = mtry_val,
    importance = TRUE
  )

  # 4e. Récupération de l'importance (MeanDecreaseGini)
  imp <- importance(rf_model, type = 2)
  liste_importances[[i]] <- data.frame(
    variable = rownames(imp),
    importance = imp[, 1],
    iteration = i,
    imputation_id = idx_imputation # traçabilité : quel dataset imputé a été utilisé
  )

  # 4f. Taux d'erreur OOB
  oob_errors[i] <- rf_model$err.rate[ntree_rf, "OOB"]

  if (i %% 10 == 0) cat("  Itération", i, "/", n_bootstrap, "\n")
}

cat("Bootstrap terminé !\n\n")


# --- 5. AGRÉGATION DES IMPORTANCES --------------------------------------------

df_importances <- bind_rows(liste_importances)

resume_importance <- df_importances %>%
  group_by(variable) %>%
  summarise(
    importance_moyenne = mean(importance),
    importance_sd = sd(importance),
    importance_cv = sd(importance) / mean(importance) * 100
  ) %>%
  arrange(desc(importance_moyenne))

cat("=== TOP VARIABLES PAR IMPORTANCE MOYENNE (MeanDecreaseGini) ===\n")
print(resume_importance, n = 20)


# --- 6. DIAGNOSTIC SPÉCIFIQUE MIDS : variabilité par dataset imputé -----------
# Ce graphique est NOUVEAU par rapport au script sans mids.
# Il permet de vérifier que l'importance d'une variable n'est pas
# artificiellement gonflée par un seul dataset imputé particulier.

top10_vars <- resume_importance %>% slice_head(n = 10) %>% pull(variable)

df_par_imputation <- df_importances %>%
  filter(variable %in% top10_vars) %>%
  group_by(variable, imputation_id) %>%
  summarise(importance_moy = mean(importance), .groups = "drop")

ggplot(
  df_par_imputation,
  aes(x = reorder(variable, importance_moy), y = importance_moy, color = as.factor(imputation_id))
) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
  coord_flip() +
  labs(
    title = "Importance par dataset imputé (top 10 variables)",
    subtitle = "Points dispersés = importance sensible au choix du dataset imputé",
    x = "Variable",
    y = "MeanDecreaseGini moyen",
    color = "Dataset imputé"
  ) +
  theme_minimal(base_size = 13)


# --- 7. VISUALISATION PRINCIPALE ----------------------------------------------

# 7a. Barplot importance avec intervalles de confiance
top20_vars <- resume_importance %>% slice_head(n = 20)

ggplot(top20_vars, aes(x = reorder(variable, importance_moyenne), y = importance_moyenne)) +
  geom_bar(stat = "identity", fill = "#2C7BB6", alpha = 0.85) +
  geom_errorbar(
    aes(ymin = importance_moyenne - importance_sd, ymax = importance_moyenne + importance_sd),
    width = 0.3,
    color = "grey30"
  ) +
  coord_flip() +
  labs(
    title = "Feature Importance – Bootstrap RF sur imputation multiple",
    subtitle = paste0(
      n_bootstrap,
      " itérations | ",
      m_imputations,
      " datasets imputés | Cible : ",
      cible
    ),
    x = "Variable",
    y = "MeanDecreaseGini moyen (± 1 SD)"
  ) +
  theme_minimal(base_size = 13)

# 7b. Stabilité OOB
df_oob <- data.frame(iteration = seq_len(n_bootstrap), oob = oob_errors)

ggplot(df_oob, aes(x = iteration, y = oob)) +
  geom_line(color = "#D7191C", linewidth = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "grey40", linetype = "dashed") +
  labs(
    title = "Taux d'erreur OOB au fil des itérations bootstrap",
    x = "Itération",
    y = "Erreur OOB"
  ) +
  theme_minimal(base_size = 13)


# --- 8. SÉLECTION FINALE DES VARIABLES ----------------------------------------

seuil_importance <- quantile(resume_importance$importance_moyenne, 0.75)

variables_selectionnees <- resume_importance %>%
  filter(importance_moyenne >= seuil_importance) %>%
  arrange(importance_cv)

cat("\n=== VARIABLES SÉLECTIONNÉES (top 25%, triées par stabilité) ===\n")
print(variables_selectionnees)

vars_finales <- variables_selectionnees$variable
cat("\nVariables pour le modèle final :\n")
cat(paste(vars_finales, collapse = ", "), "\n")


# --- 9. MODÈLE FINAL : pooling sur les m datasets imputés ---------------------
# BONNE PRATIQUE avec mids :
# Plutôt qu'entraîner sur un seul dataset, on entraîne un RF par dataset imputé
# et on moyenne les probabilités prédites (règle de Rubin adaptée).

cat("\n=== MODÈLE FINAL : entraînement sur les", m_imputations, "datasets imputés ===\n")

modeles_finaux <- vector("list", m_imputations)
formula_finale <- as.formula(paste(cible, "~", paste(vars_finales, collapse = " + ")))

for (j in seq_len(m_imputations)) {
  df_j <- complete(mids_obj, action = j)
  df_j[[cible]] <- as.factor(df_j[[cible]])

  modeles_finaux[[j]] <- randomForest(
    formula_finale,
    data = df_j,
    ntree = ntree_rf,
    mtry = floor(sqrt(length(vars_finales))),
    importance = FALSE
  )
  cat("  Modèle final", j, "/", m_imputations, "entraîné\n")
}

# Fonction de prédiction poolée : moyenne des probabilités sur les m modèles
# À utiliser sur un nouveau dataframe `df_new` (sans valeurs manquantes)
predict_pooled <- function(modeles, df_new, classe_positive = niveaux[2]) {
  probs <- lapply(modeles, function(mod) {
    predict(mod, newdata = df_new, type = "prob")[, classe_positive]
  })
  prob_moyenne <- Reduce("+", probs) / length(probs)
  return(prob_moyenne)
}

# Exemple d'utilisation :
# prob_pred <- predict_pooled(modeles_finaux, df_new = mon_nouveau_df)
# classe_pred <- ifelse(prob_pred >= 0.5, niveaux[2], niveaux[1])

cat("\nModèles finaux prêts. Utilise predict_pooled() pour prédire sur de nouvelles données.\n")
