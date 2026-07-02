#  =============================================================================
#
#                                  M2MSR_MICE
#
# ==============================================================================
library(tidyverse)
library(mice)
library(miceadds)
library(JointAI)
library(VIM)
library(lme4)
library(splines)
library(reshape2)
library(RColorBrewer)

if (!exists("df_base")) {
  source("scripts/brutes/_setup.R")
}

df_imp <- df_base |>
  select(
    -c(
      date_adm_hospit,
      date_adm_rea,
      date_hemoc,
      date_deces,
      deces_rea,
      demo_uf,
      date_sortie_rea,
      date_candidemie,
      all_of(starts_with("adm_sofa")),
      all_of(starts_with("hc_sofa")),
      adm_leuco_min,
      adm_neutro_min,
      adm_lympho_min,
      adm_diurese_tot,
      hc_diurese_tot,
      hc_leuco_min,
      hc_neutro_min,
      hc_lympho_min,
      adm_pfio2_min,
      hc_choc,
      hc_pfio2_min,
      hc_lactates_max,
      hc_glucanes_max,
      hc_mannanes_max,
      hospit_lymphopenie_duree
    )
  )

# ==============================================================================
# Visualisation des données manquantes
# ==============================================================================
# df_num <- sapply(df_imp, as.numeric)
# cormat <- cor(df_num, use = "pair", method = "spearman")
# corrplot::corrplot(cormat, method = 'square', type = 'lower')

# ==============================================================================
# Matrice de prédiction
# ==============================================================================
# ── 1. Inspecter la matrice de prédicteurs générée ───────────────────────────
ini <- mice(df_imp, maxit = 0)
pred <- quickpred(df_imp, mincor = 0.1, minpuc = 0.3)
rowSums(pred)
pred[, "iep"] <- -2

# ==============================================================================
# Imputations
# ==============================================================================

imp <- mice(
  df_imp,
  m = 20,
  defaultMethod = c("2l.pan", "2l.lmer"),
  predictorMatrix = pred,
  maxit = 30
)
densityplot(imp)

# ==============================================================================
# Sauvegarde
# ==============================================================================

saveRDS(imp, file = "donnees/df_impute.rds")

# ==============================================================================
# A tester 1
#===============================================================================

# imp_long <- complete(imp, action = "long", include = TRUE)
# imp_long %>%
#   filter(.imp > 0) %>%
#   select(-.imp, -.id, -id_hemoc) %>%
#   tbl_summary(
#     by = "resultat_candida_def",
#     missing = "no"
#   ) %>%
#   add_n() %>%
#   add_p()

# ==============================================================================
# A tester 2
#===============================================================================

# imp_stacked <- complete(imp, action = "long", include = FALSE) %>%
#   select(-.imp, -.id)

# imp_stacked %>%
#   tbl_summary(
#     by = resultat_candida_def,
#     missing = "no",
#     statistic = list(
#       all_continuous() ~ "{mean} ({sd})",
#       all_categorical() ~ "{n} ({p}%)"
#     )
#   ) %>%
#   add_p() %>%
#   bold_p(t = 0.05) %>%
#   bold_labels()

# ==============================================================================
# A tester 3
#===============================================================================

# imp$predictorMatrix
# densityplot(imp$method)
# imp$loggedEvents
# imp$method
# densityplot(imp)
# table(df_imp$hc_ktd)
# colSums(is.na(df_imp))
