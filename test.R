library(mice)
library(lme4)

imp <- readRDS("donnees/df_impute.rds")

subres <- with(
  imp,
  glmer(
    resultat_candida_def ~
      hc_vi_cat +
      hc_transfu +
      hc_dialyse +
      # hc_amines +
      hc_catheter_majeur +
      # hospit_chirurgie_majeure +
      hospit_ctc_duree +
      # hospit_immunosup_duree +
      # demo_age +
      adm_igs2 +
      hc_hypothermie +
      # hc_fievre +
      hospit_parenterale_duree +
      demo_type_rea +
      (1 | iep),
    family = binomial()
  )
)

summary(pool(subres), conf.int = TRUE)

# ==============================================================================
# Bar test
# ==============================================================================

library(mice)
library(lme4)
library(progress)

imp <- readRDS("donnees/df_impute.rds")

m <- imp$m
fits <- vector("list", m)

pb <- progress_bar$new(
  format = "Imputation :current/:total [:bar] :percent | écoulé: :elapsed | restant: :eta",
  total = m,
  clear = FALSE,
  width = 80
)

for (i in seq_len(m)) {
  d <- complete(imp, action = i)

  fits[[i]] <- glmer(
    resultat_candida_def ~
      hc_vi_cat +
      hc_transfu +
      hc_dialyse +
      # hc_amines +
      hc_catheter_majeur +
      # hospit_chirurgie_majeure +
      hospit_ctc_duree +
      hospit_immunosup_duree +
      demo_age +
      hc_hypothermie +
      # hc_fievre +
      hospit_parenterale_duree +
      demo_type_rea +
      (1 | iep),
    data = d,
    family = binomial()
  )

  pb$tick()
}

# On reconstitue un objet "mira" comme le ferait with()
subres <- as.mira(fits)

summary(pool(subres), conf.int = TRUE)

# ===============================================================================
# TEST 3
# ===============================================================================
library(lme4)
source("scripts/brutes/_setup.R")
mod_brutes <- glmer(
  resultat_candida_def ~
    hc_vi_cat +
    hc_cgr +
    hc_dialyse +
    # hc_amines +
    hc_vvc +
    # hospit_chirurgie_majeure +
    # hospit_ctc_duree +
    # hospit_immunosup_duree +
    adm_igs2 +
    # hc_hypothermie +
    hc_fievre +
    # hospit_parenterale_duree +
    demo_type_rea +
    (1 | iep),
  data = df_base,
  family = binomial()
)

tbl_regression(mod_brutes, exponentiate = TRUE)

saveRDS(mod_brutes, file = "mod_brutes.rds")
