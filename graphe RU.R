# ==============================================================================
# BATTERIE D'ANALYSE GLOBALE DES PERFORMANCES DU MODÈLE INRAE
# ==============================================================================

library(tidyverse)
library(ggplot2)

# Ouvre une fenêtre externe pour une meilleure visualisation
dev.new() 

# ------------------------------------------------------------------------------
# 1. CHARGEMENT ET NETTOYAGE CENTRALISÉ DES DONNÉES
# ------------------------------------------------------------------------------
fichier_csv <- "Analyse_ROUE_RONZUEL_2025-05-28.csv"

df_analyses <- read.csv(fichier_csv, sep = ";", dec = ",") %>%
  filter(!is.na(RMSE_INRAE_m3)) %>%
  # Séparation de l'hypothèse en 3 paramètres distincts
  separate(hypothese, into = c("Coef", "RU", "Beta"), sep = "/", remove = FALSE) %>%
  mutate(
    Coef_num = as.numeric(Coef),
    RU_num = as.numeric(RU),
    Beta_num = as.numeric(Beta),
    # Versions en facteurs pour les couleurs et les catégories des graphiques
    Coef_fac = as.factor(Coef_num),
    RU_fac = as.factor(RU_num),
    Beta_fac = as.factor(Beta_num)
  ) %>%
  arrange(RMSE_INRAE_m3)

rmse_reference_cn <- df_analyses$RMSE_CN_m3[1]
biais_reference_cn <- df_analyses$Biais_Global_CN[1]
meilleur_modele <- df_analyses[1, ]

print("✅ Données chargées. Génération de la batterie de graphiques...")

# ------------------------------------------------------------------------------
# GRAPHIQUE 1 : LE PALMARÈS (Bar Chart)
# ------------------------------------------------------------------------------
g_top <- df_analyses %>%
  head(15) %>%
  ggplot(aes(x = reorder(hypothese, -RMSE_INRAE_m3), y = RMSE_INRAE_m3, fill = RMSE_INRAE_m3)) +
  geom_col(color = "black", alpha = 0.85) +
  geom_hline(yintercept = rmse_reference_cn, color = "#e74c3c", linetype = "dashed", size = 1.2) +
  geom_text(aes(label = round(RMSE_INRAE_m3, 0)), hjust = 1.2, color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "mako") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "1. Palmarès des Paramétrages (Top 15)",
    subtitle = paste("Ligne rouge = Modèle SCS-CN (", round(rmse_reference_cn,0), "m3) | Le modèle INRAE améliore nettement la précision."),
    x = "Combinaison (Coef / RU / Beta)",
    y = "Erreur RMSE (m3)"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# GRAPHIQUE 2 : BIAIS/ERREUR (Facettes Beta + Taille RU + Couleur Coef)
# ------------------------------------------------------------------------------
g_nuage <- ggplot(df_analyses, aes(x = Biais_Global_INRAE, y = RMSE_INRAE_m3)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) + 
  geom_point(aes(color = Coef_fac, size = RU_num), alpha = 0.8) +
  geom_point(data = data.frame(Biais_Global_INRAE = biais_reference_cn, RMSE_INRAE_m3 = rmse_reference_cn), 
             color = "#e74c3c", size = 5, shape = 17) + 
  facet_wrap(~ paste("Beta =", Beta_fac)) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "2. Analyse Biais vs Erreur (Séparée par paramètre Beta)",
    subtitle = "Triangle rouge = Modèle SCS-CN de base",
    x = "Biais Global (m3)  [ <-- Sous-estime  |  Surestime --> ]",
    y = "Erreur RMSE (m3) [ Plus c'est bas, mieux c'est ]",
    color = "Coefficient",
    size = "Réserve Utile (RU)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    strip.background = element_rect(fill = "#2c3e50", color = NA),
    legend.position = "right" 
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 3 : SENSIBILITÉ INDIVIDUELLE (Boxplots séparés)
# ------------------------------------------------------------------------------
df_long <- df_analyses %>%
  select(RMSE_INRAE_m3, Beta = Beta_fac, RU = RU_fac, Coef = Coef_fac) %>%
  pivot_longer(cols = c(Beta, RU, Coef), names_to = "Parametre", values_to = "Valeur") %>%
  mutate(Parametre = factor(Parametre, levels = c("Beta", "RU", "Coef")))

g_box <- ggplot(df_long, aes(x = Valeur, y = RMSE_INRAE_m3, fill = Parametre)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, color = "#2c3e50") +
  geom_hline(yintercept = rmse_reference_cn, color = "#e74c3c", linetype = "dashed", size = 1) +
  facet_wrap(~Parametre, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "3. Impact et Sensibilité de chaque paramètre",
    subtitle = "Chaque encadré isole l'impact d'un seul paramètre sur l'erreur (Ligne rouge = SCS-CN)",
    x = "Valeur testée",
    y = "Dispersion de l'erreur RMSE (m3)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 13, color = "white"),
    strip.background = element_rect(fill = "#34495e", color = NA)
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 4 : CARTOGRAPHIE DES ZONES OPTIMALES (Heatmap)
# ------------------------------------------------------------------------------
df_heatmap <- df_analyses %>%
  group_by(Beta_fac, Coef_fac) %>%
  summarise(RMSE_moyen = mean(RMSE_INRAE_m3), .groups = "drop")

g_heat <- ggplot(df_heatmap, aes(x = Beta_fac, y = Coef_fac, fill = RMSE_moyen)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(RMSE_moyen, 0)), color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_minimal() +
  labs(
    title = "4. Cartographie des interactions (Beta vs Coef)",
    subtitle = "Couleurs sombres = Fortes erreurs | Couleurs claires (jaune) = Zone optimale de calibrage",
    x = "Paramètre Beta (Forme de la vidange du réservoir)",
    y = "Coefficient de transfert (Coef)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# AFFICHAGE FINAL ET BILAN
# ------------------------------------------------------------------------------
print(g_top)
print(g_nuage)
print(g_box)
print(g_heat)

print("=======================================================================")
print(paste("🏆 LE GAGNANT ABSOLU EST :", meilleur_modele$hypothese))
print(paste("RMSE :", round(meilleur_modele$RMSE_INRAE_m3, 0), "m3 | Biais :", round(meilleur_modele$Biais_Global_INRAE, 0), "m3"))
print("=======================================================================")















# ==============================================================================
# ANALYSE CROISÉE (ROBUSTESSE) MULTI-ÉVÉNEMENTS - MODÈLE INRAE
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(stringr)

# Ouvre une fenêtre externe pour une meilleure visualisation
dev.new() 

# ------------------------------------------------------------------------------
# 1. IMPORTATION ET FUSION DES ÉVÉNEMENTS
# ------------------------------------------------------------------------------
# Renseigne ici le nom exact de tes 3 fichiers CSV générés
fichiers_csv <- c(
  "Analyse_ROUE_RONZUEL_2025-05-28.csv",
  "Analyse_ROUE_RONZUEL_2025-08-26.csv",
  "Analyse_ROUE_RONZUEL_2025-09-20.csv",
  "Analyse_GRAND_ETANG_LA_ROUE_2025-05-31.csv",
  "Analyse_GRAND_ETANG_LA_ROUE_2025-09-20.csv",
  "Analyse_GRAND_ETANG_LA_ROUE_2025-10-17.csv",
  "Analyse_REMONDET_NORD_2023-10-18.csv",
  "Analyse_REMONDET_NORD_2025-10-29.csv",
  "Analyse_REMONDET_NORD_2025-11-12.csv"
  
  
)

# On lit, on empile (bind_rows) et on nettoie le point potentiel dans l'hypothèse
df_compile <- lapply(fichiers_csv, function(f) read.csv(f, sep = ";", dec = ",")) %>%
  bind_rows() %>%
  filter(!is.na(RMSE_INRAE_m3)) %>%
  mutate(hypothese = str_replace(hypothese, "\\./", "/"))

# ------------------------------------------------------------------------------
# 2. LE MOTEUR D'ANALYSE CROISÉE (Agrégation des performances)
# ------------------------------------------------------------------------------
df_croise <- df_compile %>%
  group_by(hypothese) %>%
  summarise(
    # La vraie performance globale : la moyenne des erreurs sur tous les orages
    RMSE_Moyen = mean(RMSE_INRAE_m3, na.rm = TRUE),
    
    # Le biais avec signe (pour voir si globalement ça surestime ou sous-estime)
    Biais_Moyen_Signe = mean(Biais_Global_INRAE, na.rm = TRUE),
    
    # L'erreur volumique réelle moyenne (sans que les + et - s'annulent)
    Biais_Absolu_Moyen = mean(abs(Biais_Global_INRAE), na.rm = TRUE),
    
    # Pour comparer avec l'ancien modèle sur les mêmes dates
    RMSE_CN_Moyen = mean(RMSE_CN_m3, na.rm = TRUE) 
  ) %>%
  ungroup() %>%
  # Découpage des paramètres pour pouvoir les mettre en graphique
  separate(hypothese, into = c("Coef", "RU", "Beta"), sep = "/", remove = FALSE) %>%
  mutate(
    Coef_num = as.numeric(Coef),
    RU_num = as.numeric(RU),
    Beta_num = as.numeric(Beta),
    Coef_fac = as.factor(Coef_num),
    RU_fac = as.factor(RU_num),
    Beta_fac = as.factor(Beta_num)
  ) %>%
  # On trie pour avoir le meilleur modèle (plus petit RMSE Moyen) en haut
  arrange(RMSE_Moyen)

# Récupération des repères du meilleur modèle
rmse_ref_croise <- df_croise$RMSE_CN_Moyen[1]
meilleur_modele <- df_croise[1, ]

print("✅ Fichiers croisés avec succès. Génération de la batterie de graphiques de robustesse...")

# ------------------------------------------------------------------------------
# GRAPHIQUE 1 : LE PALMARÈS DE ROBUSTESSE
# ------------------------------------------------------------------------------
g_top <- df_croise %>%
  head(15) %>%
  ggplot(aes(x = reorder(hypothese, -RMSE_Moyen), y = RMSE_Moyen, fill = RMSE_Moyen)) +
  geom_col(color = "black", alpha = 0.85) +
  geom_hline(yintercept = rmse_ref_croise, color = "#e74c3c", linetype = "dashed", size = 1.2) +
  geom_text(aes(label = round(RMSE_Moyen, 0)), hjust = 1.2, color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "mako") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "1. Les 15 Modèles les plus robustes (Moyenne sur 3 événements)",
    subtitle = paste("Ligne rouge = SCS-CN Moyen (", round(rmse_ref_croise,0), "m3)"),
    x = "Combinaison (Coef / RU / Beta)",
    y = "Erreur RMSE Moyenne (m3)"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# GRAPHIQUE 2 : COMPROMIS BIAIS/ERREUR GLOBAL
# ------------------------------------------------------------------------------
g_nuage <- ggplot(df_croise, aes(x = Biais_Moyen_Signe, y = RMSE_Moyen)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) + 
  geom_point(aes(color = Coef_fac, size = RU_num), alpha = 0.8) +
  facet_wrap(~ paste("Beta =", Beta_fac)) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "2. Comportement Multicritères Moyen (Séparé par Beta)",
    subtitle = "Positionnement des scénarios croisés sur les 3 orages",
    x = "Biais Moyen (m3)  [ <-- Sous-estime  |  Surestime --> ]",
    y = "Erreur RMSE Moyenne (m3)",
    color = "Coefficient",
    size = "Réserve Utile (RU)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    strip.background = element_rect(fill = "#2c3e50", color = NA)
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 3 : SENSIBILITÉ CROISÉE (Qu'est-ce qui influence la robustesse ?)
# ------------------------------------------------------------------------------
df_long <- df_croise %>%
  select(RMSE_Moyen, Beta = Beta_fac, RU = RU_fac, Coef = Coef_fac) %>%
  pivot_longer(cols = c(Beta, RU, Coef), names_to = "Parametre", values_to = "Valeur") %>%
  mutate(Parametre = factor(Parametre, levels = c("Beta", "RU", "Coef")))

g_box <- ggplot(df_long, aes(x = Valeur, y = RMSE_Moyen, fill = Parametre)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, color = "#2c3e50") +
  geom_hline(yintercept = rmse_ref_croise, color = "#e74c3c", linetype = "dashed", size = 1) +
  facet_wrap(~Parametre, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "3. Impact des paramètres sur la robustesse globale",
    x = "Valeur testée",
    y = "Dispersion du RMSE Moyen (m3)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 13, color = "white"),
    strip.background = element_rect(fill = "#34495e", color = NA)
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 4 : CARTOGRAPHIE DES ZONES OPTIMALES GLOBALES
# ------------------------------------------------------------------------------
df_heatmap <- df_croise %>%
  group_by(Beta_fac, Coef_fac) %>%
  summarise(RMSE_Lisse = mean(RMSE_Moyen), .groups = "drop")

g_heat <- ggplot(df_heatmap, aes(x = Beta_fac, y = Coef_fac, fill = RMSE_Lisse)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(RMSE_Lisse, 0)), color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_minimal() +
  labs(
    title = "4. Cartographie du couple Beta/Coef idéal multi-événements",
    subtitle = "Zone claire = Paramétrage le plus stable sur les 3 orages",
    x = "Paramètre Beta",
    y = "Coefficient de transfert"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# AFFICHAGE ET VERDICT FINAL
# ------------------------------------------------------------------------------
print(g_top)
print(g_nuage)
print(g_box)
print(g_heat)

print("=======================================================================")
print("🏆 VERDICT DE L'ANALYSE CROISÉE (Paramétrage le plus robuste) :")
print(paste("Hypothèse gagnante :", meilleur_modele$hypothese))
print("-----------------------------------------------------------------------")
print(paste("-> RMSE Moyen :", round(meilleur_modele$RMSE_Moyen, 0), "m3"))
print(paste("-> Erreur volumique moyenne (Biais Absolu) :", round(meilleur_modele$Biais_Absolu_Moyen, 0), "m3"))
print(paste("-> Biais net :", round(meilleur_modele$Biais_Moyen_Signe, 0), "m3"))
print("=======================================================================")


