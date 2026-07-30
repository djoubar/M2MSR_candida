library(mice)
library(gtsummary)

# 1. Extraire les données complétées (pooled) pour l'analyse univariée
#    (tbl_uv_regression ne gère pas directement les objets mids, donc on utilise la première imputation ou pooled)
pooled_data <- complete(imp, action = "long") # Combine toutes les imputations en un seul jeu de données
# OU (si vous préférez utiliser la première imputation) :
# pooled_data <- complete(imp, 1)

# 2. Régressions univariées (une par variable explicative)
tbl_uv <- tbl_uvregression(
  data = pooled_data,
  y = resultat_candida_def,
  method = glm,
  method.args = list(family = binomial(link = "logit")), # Régression logistique
  exponentiate = TRUE, # Affiche les OR
  pvalue_fun = ~ style_pvalue(.x, digits = 3)
) %>%
  modify_header(
    estimate = "**OR brut**",
    p.value = "p"
  ) %>%
  bold_p(t = 0.05)

print(tbl_uv)
