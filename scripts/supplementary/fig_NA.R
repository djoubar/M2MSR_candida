source("scripts/brutes/_setup.R")

# 1. Liste des variables à exclure (comme dans ton code)
exclude_vars <- c(
  "id_hemoc",
  "iep",
  "demo_centre",
  "demo_uf",
  "adm_sofa_respi",
  "adm_sofa_cardio",
  "adm_sofa_coag",
  "adm_sofa_hepatique",
  "adm_sofa_neuro",
  "hc_sofa_respi",
  "hc_sofa_cardio",
  "hc_sofa_coag",
  "hc_sofa_hepatique",
  "hc_sofa_neuro"
)

# 2. Liste des variables à inclure (toutes sauf celles exclues)
include_vars <- setdiff(
  names(df_base),
  exclude_vars
)

# 3. Calcule le % de NA pour chaque variable
na_percent <- df_base %>%
  select(all_of(include_vars)) %>% # Sélectionne uniquement les variables à inclure
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>% # % de NA
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "pct_na"
  ) %>%
  arrange(desc(pct_na)) # Trie par % de NA décroissant

na_percent <- na_percent |>
  subset(pct_na != 0)

# 4. Histogramme horizontal
figNA <-
  ggplot(na_percent, aes(x = pct_na, y = reorder(variable, pct_na))) +
  geom_col(fill = "blue1", width = 0.8) +
  geom_vline(
    xintercept = 30, # Ligne à 30%
    linetype = "dashed", # Style pointillé
    color = "red", # Couleur rouge
    linewidth = 0.3 # Épaisseur de la ligne
  ) +
  geom_text(aes(label = round(pct_na, 1)), hjust = -0.3, colour = "black") +
  labs(
    x = "Pourcentage de valeurs manquantes (%)",
    y = "Variable",
    title = "Pourcentage de valeurs manquantes par variable"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
