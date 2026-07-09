# ==============================================================================
#
#                            MODELE FINE GRAY
#
#===============================================================================
library(tidyverse)
library(riskRegression)
library(gtsummary)
library(survival)
library(cmprsk)
library(tidycmprsk)
library(broom)
library(gt)
library(ggsurvfit)
library(prodlim)
library(mice)

set.seed(142)

imp <- readRDS("donnnes/df_impute_surv.rds")
m <- imp$m

# ------------------------------------------------------------------------------
# Modèle
# ------------------------------------------------------------------------------
fg_formula <- Surv(temps, outcome_cat) ~
  hc_vi_cat +
  hc_cgr +
  hc_cp +
  hc_dialyse +
  hc_amines +
  hc_vvc +
  hospit_chirurgie_majeure +
  hospit_ctc_duree +
  hospit_immunosup_duree +
  demo_age +
  hc_hypothermie +
  hc_fievre +
  hospit_parenterale_duree +
  demo_type_rea

model_fg <- with(imp, crr(fg_formula))

# ------------------------------------------------------------------------------
# Pooling
# ------------------------------------------------------------------------------
pool_crr <- function(mira_object, exponentiate = TRUE, conf.level = 0.95) {
  model_list <- mira_object$analyses
  m <- length(model_list)

  tidy_list <- purrr::map(model_list, ~ broom::tidy(.x, exponentiate = FALSE))
  terms <- tidy_list[[1]]$term

  out <- purrr::map_dfr(terms, function(trm) {
    ests <- purrr::map_dbl(tidy_list, ~ .x$estimate[.x$term == trm])
    ses <- purrr::map_dbl(tidy_list, ~ .x$std.error[.x$term == trm])

    qbar <- mean(ests) # estimation poolee (log SHR)
    ubar <- mean(ses^2) # variance intra-imputation
    b <- var(ests) # variance inter-imputation
    t <- ubar + (1 + 1 / m) * b # variance totale (Rubin)
    se <- sqrt(t)

    # degres de liberte de Barnard-Rubin (approximation classique)
    lambda <- (1 + 1 / m) * b / t
    df <- (m - 1) / lambda^2

    tibble::tibble(
      term = trm,
      estimate = qbar,
      std.error = se,
      statistic = qbar / se,
      df = df,
      p.value = 2 * pt(-abs(qbar / se), df)
    )
  })

  out <- out %>%
    dplyr::mutate(
      conf.low = estimate - qt(1 - (1 - conf.level) / 2, df) * std.error,
      conf.high = estimate + qt(1 - (1 - conf.level) / 2, df) * std.error
    )

  if (exponentiate) {
    out <- out %>%
      dplyr::mutate(across(c(estimate, conf.low, conf.high), exp))
  }

  out
}

tidy_model <- pool_crr(model_fg, exponentiate = TRUE) %>%
  mutate(term = fct_reorder(term, estimate, .desc = FALSE))

# -----------------------------------------------------------------------------
# Forest plot
# -----------------------------------------------------------------------------
fig_fp_fg <- ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    x = paste0("Adjusted Subdistribution Hazard Ratio (aSHR) - poole (", m, " imputations)"),
    y = "Covariable"
  ) +
  theme_classic()

dir.create("models", showWarnings = FALSE)
saveRDS(fig_fp_fg, file = "models/fp_fg.rds")
saveRDS(tidy_model, file = "models/tidy_fg_pooled.rds")

# Tableau de synthese (optionnel)
tbl_fg_pooled <- tidy_model %>%
  transmute(
    Covariable = term,
    aSHR = round(estimate, 2),
    `IC95%` = paste0("[", round(conf.low, 2), " - ", round(conf.high, 2), "]"),
    p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  gt::gt() %>%
  gt::tab_header(title = "Modele de Fine & Gray - Estimations poolees (m imputations)")

# ------------------------------------------------------------------------------
#      4. PERFORMANCE PREDICTIVE (AUC / BRIER) - BOUCLE SUR LES IMPUTATIONS
# ------------------------------------------------------------------------------
# Score() ne travaille pas directement sur un objet mids : on boucle donc sur
# chacun des m jeux de donnees completes, puis on pool les resultats.

times_interest <- c(28, 60, 90)

# recuperer le nom du niveau "candidemie" a partir du 1er jeu complete
df1 <- complete(imp, 1)
cause_interet <- levels(df1$outcome_cat)[
  grepl("candid", levels(df1$outcome_cat), ignore.case = TRUE)
]

auc_list <- vector("list", m)

for (i in seq_len(m)) {
  df_i <- complete(imp, i)

  model_fg_rr_i <- FGR(
    Hist(temps, outcome_cat) ~
      hc_vi_cat +
      hc_cgr +
      hc_cp +
      hc_dialyse +
      hc_amines +
      hc_vvc +
      hospit_chirurgie_majeure +
      hospit_ctc_duree +
      hospit_immunosup_duree +
      demo_age +
      hc_hypothermie +
      hc_fievre +
      hospit_parenterale_duree +
      demo_type_rea,
    data = df_i,
    cause = cause_interet
  )

  auc_i <- Score(
    list("Fine-Gray" = model_fg_rr_i),
    formula = Hist(temps, outcome_cat) ~ 1,
    data = df_i,
    cause = cause_interet,
    times = times_interest,
    metrics = c("auc", "brier"),
    B = 100,
    split.method = "bootcv",
    seed = 123
  )

  auc_list[[i]] <- auc_i$AUC$score %>%
    select(model, times, AUC, lower, upper) %>%
    mutate(imputation = i)
}

auc_all <- bind_rows(auc_list)

# --- Pooling rigoureux (regles de Rubin) sur l'echelle logit de l'AUC ---
pool_auc_rubin <- function(auc_df) {
  auc_df %>%
    mutate(
      se = (qlogis(upper) - qlogis(lower)) / (2 * qnorm(0.975)),
      logit_auc = qlogis(AUC)
    ) %>%
    group_by(times) %>%
    summarise(
      m_imp = n(),
      qbar = mean(logit_auc),
      ubar = mean(se^2),
      b = var(logit_auc),
      t = ubar + (1 + 1 / first(m_imp)) * b,
      se_pooled = sqrt(t),
      AUC_pooled = plogis(qbar),
      lower = plogis(qbar - qnorm(0.975) * se_pooled),
      upper = plogis(qbar + qnorm(0.975) * se_pooled),
      .groups = "drop"
    ) %>%
    select(times, AUC_pooled, lower, upper)
}

auc_pooled <- pool_auc_rubin(auc_all)

auc_pooled %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  knitr::kable(
    caption = paste0(
      "AUC time-dependent poolee (regles de Rubin, ",
      m,
      " imputations) - Modele Fine & Gray"
    )
  )

# ------------------------------------------------------------------------------
#              5. COURBES DE CALIBRATION (imputation representative)
# ------------------------------------------------------------------------------
# La calibration graphique est generalement realisee sur un seul jeu
# "representatif" (ici la 1ere imputation). La repeter sur les m imputations
# est possible (boucle identique a la section 4) mais rarement necessaire
# pour une lecture visuelle.

calibration_plot <- function(model, data, time_point, cause, n_groups = 10) {
  preds <- predictRisk(model, newdata = data, times = time_point, cause = cause)

  data$pred <- as.vector(preds)
  data$decile <- dplyr::ntile(data$pred, n_groups)

  obs_list <- lapply(1:n_groups, function(g) {
    sub <- data[data$decile == g, ]
    fit2 <- tidycmprsk::cuminc(Surv(temps, outcome_cat) ~ 1, data = sub)

    tbl <- broom::tidy(fit2) %>%
      dplyr::filter(outcome == cause, time <= time_point) %>%
      dplyr::slice_tail(n = 1)

    data.frame(
      decile = g,
      pred_mean = mean(sub$pred),
      obs = ifelse(nrow(tbl) == 0, NA, tbl$estimate)
    )
  })

  cal_df <- do.call(rbind, obs_list)

  ggplot(cal_df, aes(x = pred_mean, y = obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3, color = "#2C7BB6") +
    geom_smooth(method = "loess", se = TRUE, color = "#D7191C", fill = "#fdae61", alpha = 0.2) +
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    labs(
      title = paste0("Courbe de calibration - J", time_point),
      subtitle = paste0("Modele de Fine & Gray - ", cause, " (imputation 1)"),
      x = "Risque predit (incidence cumulee)",
      y = "Incidence cumulee observee (Aalen-Johansen)",
      caption = paste0(n_groups, " groupes de risque")
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

# Modele ajuste sur la 1ere imputation, utilise pour la calibration :
model_fg_rr_1 <- FGR(
  Hist(temps, outcome_cat) ~
    hc_vi_cat +
    hc_cgr +
    hc_cp +
    hc_dialyse +
    hc_amines +
    hc_vvc +
    hospit_chirurgie_majeure +
    hospit_ctc_duree +
    hospit_immunosup_duree +
    demo_age +
    hc_hypothermie +
    hc_fievre +
    hospit_parenterale_duree +
    demo_type_rea,
  data = df1,
  cause = cause_interet
)

calibration_plot(model_fg_rr_1, df1, time_point = 28, cause = cause_interet)
calibration_plot(model_fg_rr_1, df1, time_point = 60, cause = cause_interet)
