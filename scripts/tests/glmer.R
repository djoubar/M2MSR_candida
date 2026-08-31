##############################################################################
#
# REGRESSION LOGISTIQUE LASSO MIXTE (glmmLasso)
#
##############################################################################

# ---------------------------------------------------------------------------
# 0. PACKAGES
# ---------------------------------------------------------------------------
packages <- c("-+/", "dplyr", "ggplot2", "tidyr")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(glmmLasso)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)

# ---------------------------------------------------------------------------
# 1. CHARGEMENT ET PREPARATION DES DONNEES
# ---------------------------------------------------------------------------
# Charger le dataframe
.df_temp <- readRDS("donnees/df_imput_3.rds")
df <- complete(.df_temp, 1)

# --- Variable réponse : doit être un facteur binaire ou 0/1 ---
df$resultat_candida_def <- as.factor(df$resultat_candida_def)
variable_cible <- "resultat_candida_def"

# --- Effet aléatoire : doit être un facteur ---
variable_cluster <- "iep"
df$iep <- as.factor(df$iep)

# --- Liste des variables explicatives candidates (A ADAPTER) ---
vars_candidates <- setdiff(names(df), c(variable_cible, variable_cluster))

for (v in vars_candidates) {
  if (is.character(df[[v]])) df[[v]] <- as.factor(df[[v]])
}

# --- Gestion des valeurs manquantes ---
vars_modele <- c("resultat_candida_def", "iep", vars_candidates)
df_clean <- df %>%
  select(all_of(vars_modele)) %>%
  drop_na()

cat("Nombre d'observations avant nettoyage :", nrow(df), "\n")
cat("Nombre d'observations après nettoyage :", nrow(df_clean), "\n")

# --- Standardisation des variables numériques ---
# Fortement recommandée pour le lasso (les coefficients doivent être comparables)
vars_numeriques <- vars_candidates[sapply(df_clean[vars_candidates], is.numeric)]
df_clean[vars_numeriques] <- scale(df_clean[vars_numeriques])

# ---------------------------------------------------------------------------
# 2. CONSTRUCTION DE LA FORMULE
# ---------------------------------------------------------------------------
formule_fixe <- as.formula(
  paste("resultat_candida_def ~", paste(vars_candidates, collapse = " + "))
)

# glmmLasso attend la variable réponse en 0/1 numérique pour family=binomial()
# df_clean$resultat_candida_def_num <- as.numeric(as.character(
#   df_clean$resultat_candida_def
# )) # si votre facteur est déjà "0"/"1"; sinon adaptez le recodage ici

formule_fixe_num <- update(formule_fixe, resultat_candida_def_num ~ .)

# ---------------------------------------------------------------------------
# 3. RECHERCHE DE LAMBDA OPTIMAL PAR VALIDATION CROISEE (K-FOLD)
# ---------------------------------------------------------------------------
# glmmLasso ne propose pas de fonction cv.glmmLasso native : on la code "à la main"

grille_lambda <- seq(0, 100, by = 5) # A ajuster selon l'échelle de vos données
grille_lambda[grille_lambda == 0] <- 0.01 # éviter lambda=0 (non pénalisé, instable)

K <- 5 # nombre de folds
n <- nrow(df_clean)
folds <- sample(rep(1:K, length.out = n))

resultats_cv <- data.frame(lambda = grille_lambda, deviance_moyenne = NA, bic_moyen = NA)

for (i in seq_along(grille_lambda)) {
  lambda_i <- grille_lambda[i]
  deviances <- c()

  for (k in 1:K) {
    train <- df_clean[folds != k, ]
    test  <- df_clean[folds == k, ]

    fit <- tryCatch(
      glmmLasso(
        fix = formule_fixe_num,
        rnd = list(iep = ~1),
        data = train,
        lambda = lambda_i,
        family = binomial(link = "logit"),
        control = glmmLassoControl(print.iter = FALSE)
      ),
      error = function(e) NULL
    )

    if (!is.null(fit)) {
      # Prédiction sur le fold test (effets fixes uniquement, approche standard
      # car les niveaux de 'iep' du test peuvent différer du train)
      pred <- tryCatch(
        predict(fit, newdata = test, type = "response"),
        error = function(e) rep(NA, nrow(test))
      )
      if (!all(is.na(pred))) {
        eps <- 1e-6
        pred <- pmin(pmax(pred, eps), 1 - eps)
        y_test <- test$resultat_candida_def_num
        dev <- -2 * mean(y_test * log(pred) + (1 - y_test) * log(1 - pred))
        deviances <- c(deviances, dev)
      }
    }
  }

  resultats_cv$deviance_moyenne[i] <- if (length(deviances) > 0) mean(deviances) else NA

  cat("Lambda =", lambda_i, "-> Deviance CV moyenne =",
      round(resultats_cv$deviance_moyenne[i], 4), "\n")
}

# ---------------------------------------------------------------------------
# 4. VISUALISATION DES CRITERES POUR CHOISIR LAMBDA
# ---------------------------------------------------------------------------
p1 <- ggplot(resultats_cv, aes(x = lambda, y = deviance_moyenne)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(title = "Déviance moyenne (validation croisée) selon lambda",
       x = "Lambda", y = "Déviance moyenne (CV)") +
  theme_minimal()

p2 <- ggplot(resultats_bic, aes(x = lambda, y = bic)) +
  geom_line(color = "darkred") +
  geom_point(color = "darkred") +
  labs(title = "BIC selon lambda", x = "Lambda", y = "BIC") +
  theme_minimal()

print(p1)
print(p2)

# ---------------------------------------------------------------------------
# 5. SELECTION DU LAMBDA OPTIMAL
# ---------------------------------------------------------------------------
# Option A : lambda minimisant la déviance CV
lambda_opt_cv <- resultats_cv$lambda[which.min(resultats_cv$deviance_moyenne)]

# Option B : lambda minimisant le BIC (souvent préféré pour ce type de modèle)
lambda_opt_bic <- resultats_bic$lambda[which.min(resultats_bic$bic)]

cat("\n===== RESULTATS =====\n")
cat("Lambda optimal (CV)  :", lambda_opt_cv, "\n")
cat("Lambda optimal (BIC) :", lambda_opt_bic, "\n")

# Choisissez la méthode retenue (par défaut : BIC, recommandé pour glmmLasso)
lambda_final <- lambda_opt_bic

# ---------------------------------------------------------------------------
# 6. AJUSTEMENT DU MODELE FINAL AVEC LAMBDA OPTIMAL
# ---------------------------------------------------------------------------
modele_final <- glmmLasso(
  fix = formule_fixe_num,
  rnd = list(iep = ~1),
  data = df_clean,
  lambda = lambda_final,
  family = binomial(link = "logit"),
  control = glmmLassoControl(print.iter = TRUE)
)

summary(modele_final)

# ---------------------------------------------------------------------------
# 7. SELECTION DES VARIABLES RETENUES (coefficients non nuls)
# ---------------------------------------------------------------------------
coefs <- modele_final$coefficients
coefs_df <- data.frame(
  variable = names(coefs),
  coefficient = as.numeric(coefs)
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(retenue = abs(coefficient) > 1e-6) %>%
  arrange(desc(abs(coefficient)))

cat("\n===== VARIABLES SELECTIONNEES (coefficient non nul) =====\n")
print(coefs_df %>% filter(retenue))

# Variance de l'effet aléatoire iep
cat("\n===== VARIANCE EFFET ALEATOIRE (iep) =====\n")
print(modele_final$StdDev)

# ---------------------------------------------------------------------------
# 8. GRAPHIQUE DES COEFFICIENTS RETENUS
# ---------------------------------------------------------------------------
p3 <- coefs_df %>%
  filter(retenue) %>%
  ggplot(aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Coefficients des variables sélectionnées (lasso mixte)",
       x = "Variable", y = "Coefficient") +
  theme_minimal()

print(p3)

# ---------------------------------------------------------------------------
# 9. EXPORT DES RESULTATS
# ---------------------------------------------------------------------------
write.csv(coefs_df, "variables_selectionnees_glmmLasso.csv", row.names = FALSE)
write.csv(resultats_cv, "resultats_cv_lambda.csv", row.names = FALSE)
write.csv(resultats_bic, "resultats_bic_lambda.csv", row.names = FALSE)
saveRDS(modele_final, "modele_final_glmmLasso.rds")

cat("\nScript terminé. Fichiers exportés :\n")
cat(" - variables_selectionnees_glmmLasso.csv\n")
cat(" - resultats_cv_lambda.csv\n")
cat(" - resultats_bic_lambda.csv\n")
cat(" - modele_final_glmmLasso.rds\n")
