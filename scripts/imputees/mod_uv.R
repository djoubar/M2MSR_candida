library(mice)
library(lme4)
library(broom.mixed)
library(dplyr)
library(purrr)
library(gt)

# ==============================================================================
# Liste des variables à tester en univarié
# (on exclut l'outcome, la variable de cluster et les id/technique)
# ==============================================================================

vars_a_tester <- setdiff(
  names(imp),
  c("resultat_candida_def", "iep", "id_hemoc", "groupehc")
)

# ==============================================================================
# Boucle de régressions logistiques mixtes univariées sur les données imputées
# ==============================================================================

resultats_univaries <- map_dfr(vars_a_tester, function(v) {
  formula_v <- as.formula(
    paste0("resultat_candida_def ~ ", v, " + (1 | iep)")
  )

  fit <- tryCatch(
    with(imp, glmer(formula_v, family = binomial)),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(
      variable = v,
      term = NA,
      OR = NA,
      IC_inf = NA,
      IC_sup = NA,
      p.value = NA
    ))
  }

  pooled <- pool(fit)

  summ <- summary(pooled, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      variable = v,
      term,
      OR = estimate,
      IC_inf = `2.5 %`,
      IC_sup = `97.5 %`,
      p.value
    )

  summ
})

# ==============================================================================
# Correction FDR sur l'ensemble des p-valeurs obtenues
# ==============================================================================

resultats_univaries <- resultats_univaries %>%
  mutate(p.adj.fdr = p.adjust(p.value, method = "fdr"))

# ==============================================================================
# Mise en forme du tableau final
# ==============================================================================

table_finale <- resultats_univaries %>%
  mutate(
    OR_IC = sprintf("%.2f [%.2f–%.2f]", OR, IC_inf, IC_sup),
    p.value = signif(p.value, 3),
    p.adj.fdr = signif(p.adj.fdr, 3)
  ) %>%
  select(variable, term, OR_IC, p.value, p.adj.fdr)

table_finale %>%
  gt() %>%
  cols_label(
    variable = "Variable",
    term = "Modalité",
    OR_IC = "OR [IC 95%]",
    p.value = "p",
    p.adj.fdr = "p (FDR)"
  ) %>%
  tab_header(
    title = "Régressions logistiques mixtes univariées",
    subtitle = "Effet aléatoire sur iep — outcome : resultat_candida_def"
  )
