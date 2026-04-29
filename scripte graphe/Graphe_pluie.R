library(tidyverse)
library(arrow)
library(ggplot2)

# ====================================================================
# 1. IMPORTATION DES 3 SOURCES DE DONNÉES (sur la période commune 2022-2023)
# ====================================================================

# ---> CHALAMONT
print("Chargement de Chalamont...")
fichiers_meteo_chal <- paste0("data/meteo/CHALAMON_meteo/PluieJour/Pluie_", 2022:2023, ".csv")
meteo_chal <- fichiers_meteo_chal %>%
  map_df(~read.csv2(.x, sep = ",", dec = ".")) %>%
  rename(RR_CHAL = Pluie) %>% 
  mutate(
    dat = as.Date(as.character(date), format="%Y-%m-%d"),
    RR_CHAL = as.numeric(as.character(RR_CHAL))
  ) %>% select(dat, RR_CHAL) %>% drop_na(dat)

# Fichiers sources pour Marlieux et Safran (On utilise que le 2ème fichier car on cible 2022-2023)

fichiers_meteo_mar <- c(
  "data/meteo/Q_01_previous-1950-2024_RR-T-Vent.CSV",
  "data/meteo/Q_01_latest-2025-2026_RR-T-Vent.CSV"
  
)
# ---> MARLIEUX
print("Chargement de Marlieux...")
meteo_mar <- fichiers_meteo_mar %>%
  map_df(~read.csv2(.x)) %>%  
  filter(NOM_USUEL == "MARLIEUX") %>%
  mutate(
    dat = as.Date(as.character(AAAAMMJJ), format="%Y%m%d"),
    RR_MAR = as.numeric(as.character(RR))
  ) %>% select(dat, RR_MAR) %>% drop_na(dat)

# ---> SAFRAN (Maille spécifique)
# fichiers_meteo_saf <- c(
#   "meteo/SAFRAN/QUOT_SIM2_2010-2019.CSV", 
#   "meteo/SAFRAN/QUOT_SIM2_previous-2020-202602.csv"
# )
 print("Chargement de SAFRAN...")
# coordonnees <- read.csv("meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% filter(CODE == 2)
# X <- coordonnees$LAMBX[1]
# Y <- coordonnees$LAMBY[1]
# 
# meteo_saf_brut <- open_dataset(fichiers_meteo_saf[2], format = "csv", delimiter = ";") %>%
#   filter(LAMBX >= X - 40 & LAMBX <= X + 40 & LAMBY >= Y - 40 & LAMBY <= Y + 40) %>%
#   collect() 
# 
# cases_capturees <- meteo_saf_brut %>%
#   select(LAMBX, LAMBY) %>% distinct() %>%
#   mutate(distance = sqrt((LAMBX - X)^2 + (LAMBY - Y)^2)) %>% arrange(distance)
# 
# meteo_saf <- meteo_saf_brut %>%
#   filter(LAMBX == cases_capturees$LAMBX[1] & LAMBY == cases_capturees$LAMBY[1]) %>%
#   rename(RR_SAF = PRELIQ) %>%
#   mutate(
#     dat = as.Date(as.character(DATE), format="%Y%m%d"),
#     RR_SAF = as.numeric(as.character(RR_SAF))
#   ) %>% select(dat, RR_SAF)

meteo_saf <- read.csv2("data/meteo/SAFRAN/Meteo_SAFRAN_Prete_A_L_Emploi.csv") %>%
  mutate(dat = as.Date(dat)) %>%
  # On renomme RR en RR_SAF pour que la suite de ton code fonctionne !
  rename(RR_SAF = RR) %>%
  select(dat, RR_SAF)
# ====================================================================
# 2. FUSION ET PRÉPARATION DES DONNÉES POUR GGPLOT
# ====================================================================
print("Fusion des 3 sources...")

# Fusion des 3 tables (l'inner_join va naturellement garder uniquement 2022-2023)
df_comparaison <- meteo_chal %>%
  inner_join(meteo_saf, by = "dat") %>%
  inner_join(meteo_mar, by = "dat") %>%
  mutate(annee = format(dat, "%Y")) %>%
  
  filter(annee %in% c("2022", "2023")) %>% 
  
  group_by(annee) %>%
  arrange(dat) %>%
  mutate(
    Cumul_CHAL = cumsum(replace_na(RR_CHAL, 0)),
    Cumul_SAF  = cumsum(replace_na(RR_SAF,  0)),
    Cumul_MAR  = cumsum(replace_na(RR_MAR,  0)),
    
    # Calcul des écarts par rapport à SAFRAN
    Ecart_CHAL = RR_SAF - RR_CHAL,
    Ecart_MAR  = RR_SAF - RR_MAR,
    Ecart_Cumul_CHAL = Cumul_SAF - Cumul_CHAL,
    Ecart_Cumul_MAR  = Cumul_SAF - Cumul_MAR
  ) %>% ungroup()

# Formats "Long" pour faciliter le tracé avec ggplot
df_long_rr <- df_comparaison %>%
  select(dat, annee, SAFRAN=RR_SAF, CHALAMONT=RR_CHAL, MARLIEUX=RR_MAR) %>%
  pivot_longer(cols = c(SAFRAN, CHALAMONT, MARLIEUX), names_to = "Source", values_to = "RR")

df_long_cumul <- df_comparaison %>%
  select(dat, annee, SAFRAN=Cumul_SAF, CHALAMONT=Cumul_CHAL, MARLIEUX=Cumul_MAR) %>%
  pivot_longer(cols = c(SAFRAN, CHALAMONT, MARLIEUX), names_to = "Source", values_to = "Cumul")

df_long_ecart_cumul <- df_comparaison %>%
  select(dat, annee, `Ecart CHALAMONT`=Ecart_Cumul_CHAL, `Ecart MARLIEUX`=Ecart_Cumul_MAR) %>%
  pivot_longer(cols = starts_with("Ecart"), names_to = "Station", values_to = "Ecart")

# Couleurs standardisées
couleurs_sources <- c("SAFRAN" = "darkred", "CHALAMONT" = "steelblue", "MARLIEUX" = "darkorange")
couleurs_ecarts <- c("Ecart CHALAMONT" = "steelblue", "Ecart MARLIEUX" = "darkorange")


# ====================================================================
# 3. CRÉATION DES 5 GRAPHIQUES PROPRES
# ====================================================================

# G1 : Nuage de points (Est-ce que SAFRAN correspond bien aux stations ?)
g1 <- ggplot(df_comparaison) +
  geom_point(aes(x = RR_SAF, y = RR_CHAL, color = "CHALAMONT"), alpha = 0.5) +
  geom_point(aes(x = RR_SAF, y = RR_MAR, color = "MARLIEUX"), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("CHALAMONT" = "steelblue", "MARLIEUX" = "darkorange")) +
  labs(title = "1. Corrélation : Modèle SAFRAN vs Stations locales",
       subtitle = "La ligne noire pointillée représente une égalité parfaite",
       x = "Pluie SAFRAN (mm)", y = "Pluie Station Locale (mm)", color = "Station") +
  theme_minimal()

# G2 : Chronologie superposée
g2 <- ggplot(df_long_rr, aes(x = dat, y = RR, color = Source)) +
  geom_line(alpha = 0.7, size = 0.8) +
  scale_color_manual(values = couleurs_sources) +
  facet_wrap(~annee, scales = "free_x") +
  labs(title = "2. Chronologie journalière des précipitations",
       x = "Date", y = "Précipitations (mm)", color = "Source") +
  theme_minimal()

# G3 : Cumuls annuels (Le juge de paix)
g3 <- ggplot(df_long_cumul, aes(x = dat, y = Cumul, color = Source, group = Source)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = couleurs_sources) +
  facet_wrap(~annee, scales = "free_x") +
  labs(title = "3. Cumul Annuel des précipitations",
       subtitle = "Permet de visualiser le volume global d'eau disponible",
       x = "Date", y = "Pluie cumulée (mm)", color = "Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# G4 : Écart Journalier (Facet Grid pour séparer proprement Marlieux et Chalamont)
g4 <- ggplot() +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_col(data = df_comparaison, aes(x = dat, y = Ecart_CHAL, fill = "CHALAMONT"), alpha = 0.7) +
  geom_col(data = df_comparaison, aes(x = dat, y = Ecart_MAR, fill = "MARLIEUX"), alpha = 0.7) +
  scale_fill_manual(values = c("CHALAMONT" = "steelblue", "MARLIEUX" = "darkorange")) +
  facet_wrap(~annee, scales = "free_x", ncol = 1) +
  labs(title = "4. Écart journalier (SAFRAN - Station locale)",
       subtitle = "Un pic vers le haut = Safran surestime. Un pic vers le bas = Safran sous-estime.",
       x = "Date", y = "Différence (mm)", fill = "Station") +
  theme_minimal()

# G5 : Écart Cumulé Annuel
g5 <- ggplot(df_long_ecart_cumul, aes(x = dat, y = Ecart, color = Station, group = Station)) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_line(size = 1.2) +
  scale_color_manual(values = couleurs_ecarts) +
  facet_wrap(~annee, scales = "free_x") +
  labs(title = "5. Déficit / Excédent cumulé de SAFRAN",
       subtitle = "Courbe sous 0 = SAFRAN a 'perdu' de l'eau par rapport à la station.",
       x = "Date", y = "Écart Cumulé (mm)", color = "Station") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Affichage de tous les graphiques
print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
