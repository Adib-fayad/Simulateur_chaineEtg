library(tidyverse)
library(ggplot2)
library(scales) # Pour l'affichage propre des grands nombres

# ====================================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ====================================================================

# A. Données Terrain (Observations)
load("Volume_etang/Plat.Rdata")
load("Volume_etang/Carronnier.Rdata")
load("Volume_etang/Corveyzieux.Rdata")
load("Volume_etang/Four.Rdata")
load("Volume_etang/Ligniere.Rdata")

df_reel <- Carronnier %>%
  mutate(dat = as.Date(Date_jour)) %>%
  select(dat, Volume_Reel = Volume_m3) %>% 
  drop_na(dat)

# B. Données du Modèle (Simulations SAFRAN)
nom_etang_cible <- "CARRONNIER" 

df_simule <- liste_etangs[[nom_etang_cible]] %>%
  select(dat, Volume_Simule = BF)

# ====================================================================
# 2. FUSION ET CALCUL DES ERREURS (Métriques de calibration)
# ====================================================================

# On fusionne avec inner_join pour ne garder que la période où on a la sonde
df_comparaison <- df_reel %>%
  inner_join(df_simule, by = "dat")

# Calcul de l'erreur moyenne (Biais en mètres cubes)
biais_moyen <- mean(df_comparaison$Volume_Simule - df_comparaison$Volume_Reel, na.rm = TRUE)
rmse <- sqrt(mean((df_comparaison$Volume_Simule - df_comparaison$Volume_Reel)^2, na.rm = TRUE))

print(paste("Biais moyen :", round(biais_moyen, 0), "m³ (Erreur globale de volume)"))


# ====================================================================
# 3. GRAPHIQUE DE CALIBRATION (VOLUMES)
# ====================================================================

# On passe en format long pour ggplot
df_long <- df_comparaison %>%
  pivot_longer(
    cols = c(Volume_Reel, Volume_Simule),
    names_to = "Source",
    values_to = "Volume"
  ) %>%
  mutate(
    Source = recode(Source, 
                    "Volume_Reel" = "Observation (Sonde/Terrain)", 
                    "Volume_Simule" = "Simulation (Modèle SAFRAN)")
  )

g_calib <- ggplot(df_long, aes(x = dat, y = Volume, color = Source, linetype = Source)) +
  geom_line(size = 1) +
  # Choix des couleurs
  scale_color_manual(values = c("Observation (Sonde/Terrain)" = "steelblue", 
                                "Simulation (Modèle SAFRAN)" = "darkred")) +
  scale_linetype_manual(values = c("Observation (Sonde/Terrain)" = "solid", 
                                   "Simulation (Modèle SAFRAN)" = "dashed")) +
  
  # L'axe Y est maintenant en nombres classiques avec un espace pour les milliers
  scale_y_continuous(labels = scales::comma_format(big.mark = " ", decimal.mark = ",")) +
  
  labs(
    title = paste("Calibration Hydrologique (Volumes) : Étang", nom_etang_cible),
    subtitle = paste0("Biais moyen : ", round(biais_moyen, 0), 
                      " m³ | RMSE : ", round(rmse, 0), " m³"),
    x = "Date",
    y = "Volume de l'étang (m³)",
    color = "Légende",
    linetype = "Légende"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Affichage
print(g_calib)




library(tidyverse)
library(ggplot2)
library(scales) # Pour l'affichage en pourcentages

# ====================================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ====================================================================

# A. Données Terrain (Observations)
load("Volume_etang/Plat.Rdata")

df_reel <- Plat %>%
  mutate(dat = as.Date(Date_jour)) %>%
  select(dat, TxRempli_Reel = TxRempli) %>%
  drop_na(dat)

nom_etang_cible <- "PLAT" 

df_simule <- liste_etangs[[nom_etang_cible]] %>%
  mutate(
    # Le taux de remplissage simulé
    TxRempli_Simule = BF / Vmax 
  ) %>%
  select(dat, TxRempli_Simule)

# ====================================================================
# 2. FUSION ET CALCUL DES ERREURS 
# ====================================================================

df_comparaison <- df_reel %>%
  inner_join(df_simule, by = "dat")

biais_moyen <- mean(df_comparaison$TxRempli_Simule - df_comparaison$TxRempli_Reel, na.rm = TRUE)
rmse <- sqrt(mean((df_comparaison$TxRempli_Simule - df_comparaison$TxRempli_Reel)^2, na.rm = TRUE))

print(paste("Biais moyen :", round(biais_moyen * 100, 1), "% (Erreur globale de remplissage)"))


# ====================================================================
# 3. GRAPHIQUE DE CALIBRATION
# ====================================================================

# On passe en format long pour ggplot
df_long <- df_comparaison %>%
  pivot_longer(
    cols = c(TxRempli_Reel, TxRempli_Simule),
    names_to = "Source",
    values_to = "Taux_Remplissage"
  ) %>%
  mutate(
    Source = recode(Source, 
                    "TxRempli_Reel" = "Observation (Sonde/Terrain)", 
                    "TxRempli_Simule" = "Simulation (Modèle SAFRAN)")
  )

g_calib <- ggplot(df_long, aes(x = dat, y = Taux_Remplissage, color = Source, linetype = Source)) +
  geom_line(size = 1) +
  # Choix des couleurs (Réalité en bleu, Modèle en rouge)
  scale_color_manual(values = c("Observation (Sonde/Terrain)" = "steelblue", 
                                "Simulation (Modèle SAFRAN)" = "darkred")) +
  scale_linetype_manual(values = c("Observation (Sonde/Terrain)" = "solid", 
                                   "Simulation (Modèle SAFRAN)" = "dashed")) +
  # Axe Y en pourcentages (0% à 100% ou plus si ça déborde)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = paste("Calibration Hydrologique : Étang", nom_etang_cible),
    subtitle = paste0("Biais du modèle : ", round(biais_moyen * 100, 1), 
                      "% | RMSE : ", round(rmse * 100, 1), "%"),
    x = "Date",
    y = "Taux de Remplissage de l'étang",
    color = "Légende",
    linetype = "Légende"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Affichage
print(g_calib)
