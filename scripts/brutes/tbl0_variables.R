df_variables <- read_xlsx("donnees/variables.xlsx")
tbl3_variables <- datatable(
  df_variables,
  filter = "top",
  width = "100%", # Largeur à 100% du conteneur
  height = "400px" # Hauteur fixe (ou "auto" pour automatique)
)
