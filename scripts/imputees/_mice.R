#  =============================================================================
#
#                                  M2MSR_MICE
#
# ==============================================================================
library(tidyverse)
library(mice)

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
      date_sortie_rea,
      date_candidemie,
      all_of(starts_with("adm_sofa")),
      all_of(starts_with("hc_sofa")),
      adm_neutro_min,
      adm_lympho_min,
      adm_diurese_tot,
      hc_diurese_tot,
      hc_neutro_min,
      hc_lympho_min,
      adm_pfio2_min,
      hc_choc,
      hc_pfio2_min,
      hc_lactates_max,
      hc_glucanes_max,
      hc_mannanes_max
    )
  )

# ==============================================================================
# Imputations
# ==============================================================================

imp <- mice(
  df_imp,
  m = 50,
  defaultMethod = c("lasso.norm", "logreg"),
  maxit = 30
)

# ==============================================================================
# Sauvegarde
# ==============================================================================

saveRDS(imp, file = "donnees/df_impute.rds")
