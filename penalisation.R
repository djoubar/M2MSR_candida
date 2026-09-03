# ==================================================================================================
#                                 MODELISATION PAR PENALISATION
# ==================================================================================================

library(tidyverse)
library(glmnet)
library(mice)

# --------------------------------------------------------------------------------------------------
# Paramètres
# --------------------------------------------------------------------------------------------------

df <- readRDS("donnees/df_impute.rds") # insérer le dataframe
df <- complete(df, 1)
df <- na.omit(df[, c(variable_a_predire, variables_predictives)])
variable_a_predire <- "resultat_candida_def" # insérer la variable à prédire
variables_predictives <- setdiff(names(df), c("id_hemoc", "iep", "groupehc", variable_a_predire)) # insérer les variables prédictives

# --------------------------------------------------------------------------------------------------
# Formule
# --------------------------------------------------------------------------------------------------

# formule <- as.formula(paste(
#   variable_a_predire,
#   "~",
#   paste(variables_predictives, collapse = " + ")
# ))

# --------------------------------------------------------------------------------------------------
# Modèle
# --------------------------------------------------------------------------------------------------
x <- model.matrix(~ . - 1, data = df[, variables_predictives])
y <- df[[variable_a_predire]]
mod_pen <- glmnet(x, y, family = "binomial", data = df, standardize = TRUE, alpha = 1)
summary(mod_pen)


# --------------------------------------------------------------------------------------------------
# Lambda optimal
# --------------------------------------------------------------------------------------------------
cv.voxlasso <- cv.glmnet(
  x.train.scale,
  vm.train.scale$sexe == 1,
  family = "binomial",
  type.measure = "class",
  alpha = 1,
  standardize = FALSE
)

cv.voxlasso
