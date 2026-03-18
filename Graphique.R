library(tidyverse)
library(ggplot2)
library(scales)

# ====================================================================
# 1. PARAMÈTRES FACILES À MODIFIER (Filtre temporel)
# ====================================================================
# Change simplement ces deux dates pour zoomer sur la période que tu veux !
date_debut <- as.Date("2015-01-01")
date_fin   <- as.Date("2024-12-31")

# Création d'un dossier pour ranger les images proprement (s'il n'existe pas déjà)
dir.create("Export_Graphes", showWarnings = FALSE)

# ====================================================================
# 2. GÉNÉRATION ET SAUVEGARDE DES COURBES ÉTANG PAR ÉTANG
# ====================================================================

# On boucle sur tous les noms d'étangs présents dans ta liste
for (nom_etang in names(liste_etangs)) {
  
  # On extrait les données de l'étang et on applique ton filtre de date
  df_etang <- liste_etangs[[nom_etang]] %>%
    filter(dat >= date_debut & dat <= date_fin)
  
  # Création du graphique en courbe
  g_etang <- ggplot(df_etang, aes(x = dat, y = BF)) +
    geom_line(color = "dodgerblue4", linewidth = 1) + # (linewidth remplace size)
    
    scale_y_continuous(labels = scales::comma_format(big.mark = " ", decimal.mark = ",")) +
    
    labs(
      title = paste("Évolution du volume simulé - Étang", nom_etang),
      subtitle = paste("Période du", format(date_debut, "%d/%m/%Y"), "au", format(date_fin, "%d/%m/%Y")),
      x = "Date", 
      y = "Volume de l'eau (m³)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  # Affichage dans RStudio
  print(g_etang)
  
  # Sauvegarde automatique en JPEG dans le dossier (Format HD : 10x6 pouces, 300 dpi)
  nom_fichier <- paste0("Export_Graphes/Graphe_", nom_etang, ".jpg")
  ggsave(nom_fichier, plot = g_etang, width = 10, height = 6, dpi = 300)
  
  print(paste("Graphique sauvegardé :", nom_fichier))
}

# ====================================================================
# 3. GÉNÉRATION ET SAUVEGARDE DU GRAPHIQUE DE L'EXUTOIRE FINAL
# ====================================================================

# On applique le même filtre de date au tableau de la rivière
df_exutoire <- Volume_Total_Exutoire_BV %>%
  filter(dat >= date_debut & dat <= date_fin)

g_exutoire <- ggplot(df_exutoire, aes(x = dat, y = Volume_Riviere)) +
  geom_line(color = "darkred", linewidth = 1) + 
  
  scale_y_continuous(labels = scales::comma_format(big.mark = " ", decimal.mark = ",")) +
  
  labs(
    title = "Hydrogramme à l'exutoire du Bassin Versant",
    subtitle = "Volume journalier rejeté par la chaîne d'étangs vers la rivière",
    x = "Date",
    y = "Volume d'eau relâché (m³)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14, color = "darkred")
  )

# Affichage dans RStudio
print(g_exutoire)

# Sauvegarde du graphique de l'exutoire
ggsave("Export_Graphes/Exutoire_Bassin_Versant.jpg", plot = g_exutoire, width = 10, height = 6, dpi = 300)

print("✅ Tous les graphiques ont été générés et sauvegardés avec succès dans le dossier 'Export_Graphes' !")