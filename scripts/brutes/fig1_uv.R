################################################################################
#                                                                              #
#                             M2MSR_FIG1_UNIVARIEES                            #
#                                                                              #
################################################################################
.df_fig1 <- df_base

uv_cat <- function(.df_fig1, save_plots = TRUE, output_dir = "figures/uv") {
  categories <- list(
    demo = "demo_",
    admission = "adm_",
    hc = "hc_",
    hospit = "hospit_"
  )

  # Créer le répertoire de sortie si nécessaire
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  plot_factor <- function(var) {
    p <- ggplot(.df_fig1, aes(x = !!sym(var))) +
      geom_bar(fill = "#457B9D", alpha = 0.7) +
      geom_text(
        aes(label = ..count..),
        stat = "count",
        vjust = -0.2,
        colour = "black",
        size = 0.5
      ) +
      labs(x = var, y = "Effectif") +
      theme_minimal()
    if (save_plots) {
      ggsave(
        filename = file.path(output_dir, paste0("fct_", var, ".png")),
        plot = p,
        create.dir = TRUE,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
    return(p)
  }

  plot_numeric <- function(var) {
    p <- ggplot(.df_fig1, aes(x = !!sym(var))) +
      geom_density(fill = "#E63946", alpha = 0.7) +
      geom_vline(
        aes(xintercept = median(!!sym(var))),
        color = "black",
        linewidth = 0.3,
        linetype = "dashed"
      ) +
      labs(x = var, y = "Fréquence") +
      theme_minimal()
    if (save_plots) {
      ggsave(
        filename = file.path(output_dir, paste0("num_", var, ".png")),
        create.dir = TRUE,
        plot = p,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
    return(p)
  }

  generate_category_figure <- function(category, prefix) {
    cols <- names(.df_fig1)[grepl(prefix, names(.df_fig1))]
    if (length(cols) == 0) {
      message(paste("Aucune colonne trouvée pour la catégorie", category))
      return(NULL)
    }
    plots <- map(
      cols,
      ~ {
        if (is.factor(.df_fig1[[.x]])) {
          plot_factor(.x)
        } else if (is.numeric(.df_fig1[[.x]])) {
          plot_numeric(.x)
        } else {
          message(paste("Type non supporté pour la colonne", .x))
          return(NULL)
        }
      }
    )
    plots <- compact(plots)
    if (length(plots) > 0) {
      combined_plot <- wrap_plots(plots = plots, ncol = 2)
      theme(plot.margin = margin(1, 1, 1, 1, "cm"))
      return(combined_plot)
    } else {
      return(NULL)
    }
  }

  figures <- map2(names(categories), categories, generate_category_figure)
  return(figures)
}

# Générer les graphiques et les sauvegarder
figures <- uv_cat(.df_fig1, save_plots = TRUE, output_dir = "figures/uv")


.df_description <- df_base |>
  select(-all_of(starts_with("date")), -id_hemoc, -adm_plasmalyte) |>
  mutate(hc_delai = as.numeric(hc_delai, units = "days"))

tbl_descriptif <- tbl_summary(
  .df_description,
  statistic = list(all_continuous() ~ "{min} / {median} / {max} ({mean})"),
  digits = all_continuous() ~ 2,
  missing = "ifany" # Optionnel : pour arrondir les valeurs
)

# tbl3m_uv_demo <-
#   tbl_uvregression(
#     data = df_base,
#     include = c(all_of(starts_with("demo"))),
#     method = glmer,
#     y = resultat_candida_def,
#     formula = "{y} ~ {x} + (1|iep)",
#     method.args = list(family = binomial),
#     exponentiate = TRUE
#   )

# tbl3m_uv_adm <-
#   tbl_uvregression(
#     data = df_base,
#     include = c(all_of(starts_with("adm"))),
#     method = glmer,
#     y = resultat_candida_def,
#     formula = "{y} ~ {x} + (1|iep)",
#     method.args = list(family = binomial),
#     exponentiate = TRUE
#   )

# tbl3m_uv_hc <-
#   tbl_uvregression(
#     data = df_base,
#     include = c(
#       all_of(starts_with("hc")),
#       -hc_lympho_min,
#       -hc_neutro_min,
#       -all_of(starts_with("hc_sofa")),
#       -hc_voluven
#     ),
#     method = glmer,
#     y = resultat_candida_def,
#     formula = "{y} ~ {x} + (1|iep)",
#     method.args = list(family = binomial),
#     exponentiate = TRUE
#   )

# tbl3m_uv_hospit <-
#   tbl_uvregression(
#     data = df_base,
#     include = c(all_of(starts_with("hospit"))),
#     method = glmer,
#     y = resultat_candida_def,
#     formula = "{y} ~ {x} + (1|iep)",
#     method.args = list(family = binomial),
#     exponentiate = TRUE
#   )

# mod1 <- glmer(
#   data = df_base,
#   resultat_candida_def ~ hospit_cp + (1 | iep),
#   family = "binomial"
# )
# tbl_regression(mod1, exponentiate = TRUE)
