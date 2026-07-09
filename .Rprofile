RhpcBLASctl::blas_set_num_threads(20)

# =============================================================================
# ~/.Rprofile — Configuration système
# Contraintes : ≤ 20 cœurs, ≤ 30 Go RAM
# =============================================================================
# RAM_MAX_GB <- 30

# # ── Guard : empêche tout re-sourçage accidentel ───────────────────────────────
# if (exists(".rprofile_loaded", envir = globalenv())) {
#   message("[.Rprofile] Déjà chargé — skip.")
# } else {
#   assign(".rprofile_loaded", TRUE, envir = globalenv())

#   # ── Utilitaire interne ────────────────────────────────────────────────────────
#   .silent_require <- function(pkg) {
#     suppressPackageStartupMessages(
#       suppressWarnings(
#         requireNamespace(pkg, quietly = TRUE)
#       )
#     )
#   }

#   # ── Lecture RAM disponible (Linux /proc/meminfo) ──────────────────────────────
#   .ram_disponible_gb <- function() {
#     tryCatch(
#       {
#         as.numeric(system(
#           "awk '/MemAvailable/ {print $2}' /proc/meminfo",
#           intern = TRUE
#         )) /
#           1048576 # kB → Go
#       },
#       error = function(e) NA_real_
#     )
#   }

#   # ── 1. BLAS : threads par défaut sans parallélisation ────────────────────────
#   # On garde un nombre raisonnable de threads pour le travail séquentiel normal
#   BLAS_THREADS_SEQUENTIEL <- min(20L, max(1L, parallel::detectCores() - 1L))
#   BLAS_THREADS_PARALLEL <- 1L # cède les cœurs aux workers future

#   if (.silent_require("RhpcBLASctl")) {
#     RhpcBLASctl::blas_set_num_threads(BLAS_THREADS_SEQUENTIEL)
#     message(sprintf("[BLAS] %d thread(s) — mode séquentiel", BLAS_THREADS_SEQUENTIEL))
#   }

#   # ── 2. PARALLÉLISATION ────────────────────────────────────────────────────────
#   .setup_parallel <- function(workers = NULL, ram_par_worker_gb = 1.5) {
#     if (!.silent_require("future") || !.silent_require("parallelly")) {
#       warning("[future] Packages future/parallelly non disponibles.")
#       return(invisible(NULL))
#     }

#     n_cores <- parallelly::availableCores()
#     ram_dispo_gb <- .ram_disponible_gb()

#     max_cores <- min(20L, max(1L, n_cores - 1L))

#     if (!is.na(ram_dispo_gb)) {
#       ram_utilisable_gb <- min(RAM_MAX_GB, ram_dispo_gb * 0.85)
#       max_ram <- max(1L, floor(ram_utilisable_gb / ram_par_worker_gb))
#     } else {
#       max_ram <- 8L
#       warning("[future] RAM non détectable — fallback à 8 workers.")
#     }

#     n_workers <- min(max_cores, max_ram)
#     if (!is.null(workers)) {
#       n_workers <- min(as.integer(workers), n_workers)
#     }
#     n_workers <- max(1L, n_workers)

#     # ← BLAS à 1 thread seulement quand future est actif
#     if (.silent_require("RhpcBLASctl")) {
#       RhpcBLASctl::blas_set_num_threads(BLAS_THREADS_PARALLEL)
#     }

#     future::plan(future::multisession, workers = n_workers)

#     ram_msg <- if (!is.na(ram_dispo_gb)) {
#       sprintf("RAM libre : %.1f Go | Quota R : %d Go", ram_dispo_gb, RAM_MAX_GB)
#     } else {
#       "RAM : non détectée"
#     }

#     message(sprintf(
#       "[future] %d workers | Cœurs dispo : %d | %s | ~%.0f Go estimés | BLAS → %d thread",
#       n_workers,
#       n_cores,
#       ram_msg,
#       n_workers * ram_par_worker_gb,
#       BLAS_THREADS_PARALLEL
#     ))
#   }

#   # ── Fermeture propre : restaure les threads BLAS ─────────────────────────────
#   .stop_parallel <- function() {
#     future::plan(future::sequential)
#     if (.silent_require("RhpcBLASctl")) {
#       RhpcBLASctl::blas_set_num_threads(BLAS_THREADS_SEQUENTIEL)
#     }
#     message(sprintf(
#       "[future] Cluster fermé | BLAS restauré → %d threads",
#       BLAS_THREADS_SEQUENTIEL
#     ))
#   }

#   # ── 3. OPTIONS GLOBALES ───────────────────────────────────────────────────────
#   options(
#     stringsAsFactors = FALSE,
#     warn = 0,
#     mc.cores = min(20L, max(1L, parallel::detectCores() - 1L)),
#     digits = 4,
#     scipen = 6
#   )

# ── 4. CONFLITS DE FONCTIONS ──────────────────────────────────────────────────
setHook(packageEvent("conflicted", "attach"), function(...) {
  conflicted::conflicts_prefer(
    dplyr::filter,
    dplyr::rename,
    dplyr::lag,
    dplyr::recode,
    gtsummary::select,
    tidycmprsk::cuminc,
    readxl::read_xlsx,
    ggplot2::margin,
    randomForest::importance,
    .quiet = TRUE
  )
})

# ── 5. THÈMES (déclenchés à l'attachement du package) ────────────────────────
setHook(packageEvent("gtsummary", "attach"), function(...) {
  gtsummary::theme_gtsummary_language(
    language = "fr",
    decimal.mark = ",",
    big.mark = " "
  )
})

setHook(packageEvent("flextable", "attach"), function(...) {
  flextable::set_flextable_defaults(
    font.family = "Times New Roman",
    font.size = 12,
    padding = 2,
    border.color = "#CCCCCC",
    line_spacing = 1.3,
    line_width = 2
  )
})

# ── 6. MESSAGE DE DÉMARRAGE ───────────────────────────────────────────────────
#   ram_dispo <- .ram_disponible_gb()
#   ram_msg <- if (!is.na(ram_dispo)) sprintf("%.1f Go libres", ram_dispo) else "RAM inconnue"

#   message(sprintf(
#     "[R %s] %s — Profil chargé | %s | Quota R : %d Go | .setup_parallel() pour paralléliser | .stop_parallel() pour fermer",
#     paste(R.version$major, R.version$minor, sep = "."),
#     format(Sys.time(), "%H:%M"),
#     ram_msg,
#     RAM_MAX_GB
#   ))
# } # fin du guard

# library(tidyverse)
# library(readxl)
# library(questionr)
# library(gtsummary)
# library(flextable)
# library(patchwork)
# library(labelled)
# library(gt)
# library(lme4)
# library(glmnet)
# library(pacman)
# library(parameters)
# library(see)
# library(geepack)
# library(Hmisc)
# library(mice)
# library(rsample)
# library(hebstr)
# library(MuMIn)
# library(rms)
# library(DT)
# library(pROC)
# library(car)
# library(MASS)
# library(purrr)
# library(boot)
# library(ggsurvfit)
# library(tidycmprsk)
# library(survival)
# library(survminer)
# library(randomForest)
# library(rms)
# library(riskRegression) # AUC des modèles FG
# library(pec) # AUC des modèles FG
