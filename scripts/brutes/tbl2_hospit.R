# ==============================================================================
#
#                       M2MSR_TBL2_DONNEES AU COURS HOSPIT
#
# ==============================================================================
library(tidyverse)
library(gtsummary)

# if (!exists("df_base")) {
source("scripts/brutes/_setup.R")
# }

df_tlb2 <- df_base |>
  arrange(iep, date_hemoc) |>
  distinct(iep, .keep_all = TRUE)

tbl2 <-
  tbl_summary(
    data = df_tlb2,
    missing = "no",
    include = c(
      hospit_chirurgie_majeure,
      hospit_chirurgie_abdominale,
      hospit_fibro,
      hospit_parenterale_cat,
      hospit_vi_duree,
      hospit_ctc_duree,
      hospit_cgr,
      hospit_pfc,
      hospit_cp,
      deces_rea
    ),
    label = list(
      hc_delai = "Délai entre admission & hémoculture",
      deces_rea = "Décès en réanimation",
      hospit_chirurgie_majeure = "Chirurgie majeure",
      hospit_chirurgie_abdominale = "Chirurgie abdominale",
      hospit_fibro = "Fibroscopie",
      hospit_parenterale_cat = "Nutrition parentérale"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({min}, {max})",
      all_categorical() ~
        "{n} ({p}%)"
    ),
    type = list(
      c(
        deces_rea,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale,
        hospit_fibro,
        hospit_parenterale_cat
      ) ~ "dichotomous"
    ),
    value = list(
      c(
        deces_rea,
        hospit_chirurgie_majeure,
        hospit_chirurgie_abdominale,
        hospit_fibro
      ) ~ "Oui",
      hospit_parenterale_cat ~ 1
    )
  ) |>
  # bold_labels() |>
  italicize_levels()
