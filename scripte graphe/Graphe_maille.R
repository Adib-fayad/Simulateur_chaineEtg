library(tidyverse)
library(arrow)
library(ggplot2)

# ====================================================================
# 1. DÉFINITION DES CENTROÏDES ET RECHERCHE DE LA MAILLE LA PLUS PROCHE
# ====================================================================
print("Lecture du fichier des centroïdes...")
coordonnees <- read.csv("data/meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",")

# On récupère tes 23 bassins
mailles_cibles <- coordonnees %>%
  select(CODE, LAMBX, LAMBY) %>%
  distinct(CODE, .keep_all = TRUE)

# On définit une zone large (+/- 10 km) autour de tous tes bassins
X_min <- min(mailles_cibles$LAMBX) - 10000
X_max <- max(mailles_cibles$LAMBX) + 10000
Y_min <- min(mailles_cibles$LAMBY) - 10000
Y_max <- max(mailles_cibles$LAMBY) + 10000

fichiers_meteo_saf <- c(
  "data/meteo/SAFRAN/QUOT_SIM2_2010-2019.CSV", 
  "data/meteo/SAFRAN/QUOT_SIM2_previous-2020-202602.csv"
)

print("Recherche des coordonnées SAFRAN exactes...")
# On extrait JUSTE les coordonnées uniques de la grille SAFRAN sur ce secteur (pour aller vite)
grille_safran <- open_dataset(fichiers_meteo_saf[2], format = "csv", delimiter = ";") %>%
  filter(LAMBX >= X_min & LAMBX <= X_max & LAMBY >= Y_min & LAMBY <= Y_max) %>%
  select(SAF_X = LAMBX, SAF_Y = LAMBY) %>%
  distinct() %>%
  collect()

# LA MAGIE EST ICI : Pour chaque BV, on trouve la maille SAFRAN la plus proche !
correspondance_mailles <- mailles_cibles %>%
  cross_join(grille_safran) %>%
  mutate(distance = sqrt((LAMBX - SAF_X)^2 + (LAMBY - SAF_Y)^2)) %>%
  group_by(CODE) %>%
  slice_min(order_by = distance, n = 1) %>% # On garde la plus proche pour chaque BV
  ungroup() %>%
  mutate(Nom_Maille = paste0("BV_", CODE)) # On nomme la maille finale

# Note : Plusieurs BV peuvent tomber dans la MÊME maille SAFRAN (8x8km c'est grand)
# On récupère la liste des coordonnées SAFRAN uniques dont on a vraiment besoin
coordonnees_safran_utiles <- correspondance_mailles %>% distinct(SAF_X, SAF_Y)

print(paste(nrow(mailles_cibles), "bassins versants répartis sur", nrow(coordonnees_safran_utiles), "mailles SAFRAN différentes."))


# ====================================================================
# 2. EXTRACTION ET FILTRAGE EXACT DANS SAFRAN
# ====================================================================
print("Extraction des données météo...")

# On extrait toute la donnée, mais UNIQUEMENT pour les mailles SAFRAN trouvées ci-dessus
df_mailles <- open_dataset(fichiers_meteo_saf[2], format = "csv", delimiter = ";") %>%
  filter(LAMBX %in% coordonnees_safran_utiles$SAF_X & LAMBY %in% coordonnees_safran_utiles$SAF_Y) %>%
  collect() %>%
  # On raccroche nos vrais noms de BV !
  inner_join(correspondance_mailles %>% select(SAF_X, SAF_Y, Nom_Maille), 
             by = c("LAMBX" = "SAF_X", "LAMBY" = "SAF_Y")) %>%
  rename(RR = PRELIQ) %>%
  mutate(
    dat = as.Date(as.character(DATE), format="%Y%m%d"),
    RR = as.numeric(as.character(RR)),
    annee = format(dat, "%Y")
  ) %>%
  filter(annee %in% c("2022", "2023")) %>%
  select(Nom_Maille, LAMBX, LAMBY, dat, annee, RR) %>%
  drop_na(dat)

# ====================================================================
# 3. CALCULS HYDROS (Cumuls)
# ====================================================================
print("Calcul des cumuls...")

# Cumul journalier
df_mailles <- df_mailles %>%
  group_by(Nom_Maille, annee) %>%
  arrange(dat) %>%
  mutate(Cumul_Pluie = cumsum(replace_na(RR, 0))) %>%
  ungroup()

# Bilan annuel (Total de l'année par BV)
bilan_annuel <- df_mailles %>%
  group_by(Nom_Maille, LAMBX, LAMBY, annee) %>%
  summarise(Total_Annuel = max(Cumul_Pluie), .groups = "drop")


# ====================================================================
# 4. GRAPHIQUES DE COMPARAISON SPATIALE
# ====================================================================
print("Génération des graphiques...")

# GRAPHIQUE 1 : L'enveloppe des cumuls (Évolution temporelle)
g1 <- ggplot(df_mailles, aes(x = dat, y = Cumul_Pluie, group = Nom_Maille)) +
  geom_line(alpha = 0.5, color = "steelblue") +
  stat_summary(aes(group = annee), fun = mean, geom = "line", color = "red", size = 1.2) +
  facet_wrap(~annee, scales = "free_x") +
  labs(
    title = paste("Évolution des précipitations sur tes", nrow(mailles_cibles), "Bassins Versants"),
    subtitle = "Chaque ligne bleue = 1 Bassin. Ligne rouge = Moyenne globale du territoire.",
    x = "Date", y = "Pluie cumulée (mm)"
  ) +
  theme_minimal()



print(g1)

