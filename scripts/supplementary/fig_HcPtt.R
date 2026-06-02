################################################################################
#                                                                              #
#                             M2MSR_FIG_NB_HC_/PTT                             #
#                                                                              #
################################################################################

df_count <- df_base %>%
  group_by(iep) %>%
  summarise(nb_lignes = n(), .groups = "drop")

ggplot(data = df_count, aes(x = nb_lignes)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    x = "Nombre de lignes (hémocultures) par IEP",
    y = "Nombre d'IEP",
    title = "Distribution du nombre d'hémocultures par IEP"
  ) +
  theme_minimal()

median(df_count$nb_lignes)
mean(df_count$nb_lignes)
max(df_count$nb_lignes)
