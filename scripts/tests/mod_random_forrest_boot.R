# =============================================================================
# BOOTSTRAP STRATIFIÉ + RANDOM FOREST + FEATURE IMPORTANCE
# Objectif : sélectionner les variables les plus stables et importantes
#            pour prédire "resultat_candida_def"
# =============================================================================

# --- 0. PACKAGES --------------------------------------------------------------
# install.packages(c("randomForest", "dplyr", "ggplot2")) # à décommenter si besoin
library(randomForest)
library(dplyr)
library(ggplot2)

if (!exists("df_fg")) {
  source("scripts/survie/_setup_survie.R")
}

# --- 1. PARAMÈTRES À ADAPTER --------------------------------------------------

df <- df_fg |>
  select(-c(temps, iep, outcome, outcome_cox, outcome_cat)) # <-- ton dataframe ici
cible <- "resultat_candida_def" # variable à prédire (doit être un factor 0/1)
n_bootstrap <- 100 # nombre d'itérations bootstrap
prop_train <- 0.8 # proportion de l'échantillon pour l'entraînement

# Hyperparamètres Random Forest
ntree_rf <- 500 # nombre d'arbres par forêt
mtry_rf <- NULL # nb variables testées à chaque nœud (NULL = sqrt(p) par défaut)


# --- 2. PRÉPARATION DES DONNÉES -----------------------------------------------
# S'assurer que la cible est bien un factor (obligatoire pour classification RF)
df[[cible]] <- as.factor(df[[cible]])

# Vérification de l'équilibre des classes (juste pour information)
cat("Distribution de la variable cible :\n")
print(table(df[[cible]]))
cat("Proportions :\n")
print(prop.table(table(df[[cible]])))

# Séparation features / cible
X <- df %>% select(-all_of(cible))
y <- df[[cible]]

# Niveaux de la classe positive et négative
niveaux <- levels(y)
cat("\nClasses détectées :", paste(niveaux, collapse = " / "), "\n\n")


# --- 3. FONCTION DE TIRAGE STRATIFIÉ ------------------------------------------
# Le tirage stratifié garantit que chaque échantillon bootstrap
# conserve le même ratio positifs/négatifs que les données originales.

tirage_stratifie <- function(y, prop) {
  indices <- seq_along(y)
  # On tire séparément dans chaque classe, puis on combine
  idx_par_classe <- lapply(niveaux, function(cl) {
    idx_cl <- indices[y == cl]
    n_tirer <- round(length(idx_cl) * prop)
    sample(idx_cl, size = n_tirer, replace = TRUE) # replace=TRUE = bootstrap
  })
  unlist(idx_par_classe)
}


# --- 4. BOUCLE BOOTSTRAP ------------------------------------------------------
# À chaque itération :
#   - on tire un échantillon stratifié (train)
#   - on entraîne un Random Forest
#   - on récupère la feature importance (MeanDecreaseGini)
#   - on calcule les métriques sur l'échantillon hors-sac (OOB via RF)

set.seed(42) # reproductibilité

# Stockage des importances à chaque itération
liste_importances <- vector("list", n_bootstrap)

# Stockage des métriques OOB (optionnel, utile pour contrôler la stabilité)
oob_errors <- numeric(n_bootstrap)

cat("Lancement du bootstrap (", n_bootstrap, "itérations)...\n")

for (i in seq_len(n_bootstrap)) {
  # 4a. Tirage stratifié
  idx_train <- tirage_stratifie(y, prop_train)
  X_train <- X[idx_train, , drop = FALSE]
  y_train <- y[idx_train]

  # 4b. Entraînement du Random Forest
  mtry_val <- if (is.null(mtry_rf)) floor(sqrt(ncol(X_train))) else mtry_rf

  rf_model <- randomForest(
    x = X_train,
    y = y_train,
    ntree = ntree_rf,
    mtry = mtry_val,
    importance = TRUE # nécessaire pour récupérer MeanDecreaseGini
  )

  # 4c. Récupération de l'importance (MeanDecreaseGini = stabilité du nœud)
  imp <- importance(rf_model, type = 2) # type=2 -> MeanDecreaseGini
  liste_importances[[i]] <- data.frame(
    variable = rownames(imp),
    importance = imp[, 1],
    iteration = i
  )

  # 4d. Taux d'erreur OOB de cette itération
  oob_errors[i] <- rf_model$err.rate[ntree_rf, "OOB"]

  # Affichage de progression tous les 10 tours
  if (i %% 10 == 0) cat("  Itération", i, "/", n_bootstrap, "\n")
}

cat("Bootstrap terminé !\n\n")


# --- 5. AGRÉGATION DES IMPORTANCES --------------------------------------------
# On compile toutes les itérations et on calcule :
#   - l'importance moyenne de chaque variable
#   - l'écart-type (mesure de stabilité entre les bootstrap)

df_importances <- bind_rows(liste_importances)

resume_importance <- df_importances %>%
  group_by(variable) %>%
  summarise(
    importance_moyenne = mean(importance),
    importance_sd = sd(importance),
    importance_cv = sd(importance) / mean(importance) * 100 # Coefficient de variation (%)
  ) %>%
  arrange(desc(importance_moyenne))

cat("=== TOP VARIABLES PAR IMPORTANCE MOYENNE (MeanDecreaseGini) ===\n")
print(resume_importance, n = 20)


# --- 6. VISUALISATION ---------------------------------------------------------

# 6a. Barplot des 20 variables les plus importantes (avec intervalle de confiance)
top_vars <- resume_importance %>% slice_head(n = 20)

plot_rf <- ggplot(
  top_vars,
  aes(x = reorder(variable, importance_moyenne), y = importance_moyenne)
) +
  geom_bar(stat = "identity", fill = "#2C7BB6", alpha = 0.85) +
  geom_errorbar(
    aes(ymin = importance_moyenne - importance_sd, ymax = importance_moyenne + importance_sd),
    width = 0.3,
    color = "grey30"
  ) +
  coord_flip() +
  labs(
    title = "Feature Importance – Bootstrap Random Forest",
    subtitle = paste0(n_bootstrap, " itérations stratifiées | Variable cible : ", cible),
    x = "Variable",
    y = "MeanDecreaseGini moyen (± 1 SD)"
  ) +
  theme_minimal(base_size = 13)

# 6b. Stabilité de l'erreur OOB au fil des itérations
df_oob <- data.frame(iteration = seq_len(n_bootstrap), oob = oob_errors)

ggplot(df_oob, aes(x = iteration, y = oob)) +
  geom_line(color = "#D7191C", linewidth = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "grey40", linetype = "dashed") +
  labs(
    title = "Taux d'erreur OOB au fil des itérations bootstrap",
    subtitle = "Une courbe stable = modèle convergé",
    x = "Itération",
    y = "Erreur OOB"
  ) +
  theme_minimal(base_size = 13)


# --- 7. SÉLECTION FINALE DES VARIABLES ----------------------------------------
# Critère : importance moyenne dans le top X%, et coefficient de variation faible
# (CV faible = la variable est importante de façon stable entre les bootstrap)

seuil_importance <- quantile(resume_importance$importance_moyenne, 0.75) # top 25%

variables_selectionnees <- resume_importance %>%
  filter(importance_moyenne >= seuil_importance) %>%
  arrange(importance_cv) # triées par stabilité croissante

cat("\n=== VARIABLES SÉLECTIONNÉES (top 25% + triées par stabilité) ===\n")
print(variables_selectionnees)

# Vecteur prêt à l'emploi pour un modèle final
vars_finales <- variables_selectionnees$variable
cat("\nVariables pour le modèle final :\n")
cat(paste(vars_finales, collapse = ", "), "\n")


# --- 8. MODÈLE FINAL (optionnel) ----------------------------------------------
# Entraîner un dernier Random Forest sur TOUTES les données avec les variables sélectionnées

df_final <- df %>% select(all_of(c(vars_finales, cible)))

rf_final <- randomForest(
  as.formula(paste(cible, "~ .")),
  data = df_final,
  ntree = ntree_rf,
  mtry = floor(sqrt(length(vars_finales))),
  importance = TRUE
)

cat("\n=== MODÈLE FINAL ===\n")
print(rf_final)
