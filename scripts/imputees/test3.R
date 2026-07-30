# Probabilités prédites
prob <- predict(mod, newdata = dat, type = "response")

# Classification (seuil = 0.5)
pred <- ifelse(prob >= 0.02, "Positive", "Négative")

# Facteurs avec les mêmes niveaux
pred <- factor(pred, levels = c("Négative", "Positive"))
obs <- factor(dat$resultat_candida_def, levels = c("Négative", "Positive"))

# Matrice de confusion
cm <- confusionMatrix(
  data = pred,
  reference = obs,
  positive = "Positive"
)

cm
