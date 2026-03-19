library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
library(readxl)
library(arrow) 
source("fonctions.R")
#################################
#Choix des donnee meteo CHALAMONT OU MARLIEUX

SITE_CHOISI <- "SAFRAN"

# Chargement et nettoyage initial
# Utilisation de dplyr pour renommer et filtrer en une étape
os_data <- read.csv2("OS_BV_Etg_Chalamont.csv", dec = ".", sep = ",") %>%
  select(ClasseOS = 1, Etang = 2, Surface = 3) %>%
  filter(ClasseOS < 23)

tab_cn <- read.csv2("OS_CN.csv", sep = ";", encoding = "latin1")

# Fusion et calcul des surfaces cumulées
os_complet <- os_data %>%
  left_join(tab_cn, by = c("ClasseOS" = "Code_OS"))

# Visualisation 
os_complet %>%
  group_by(Type_OS) %>%
  summarise(Surface_Ha = sum(Surface) / 10000) %>%
  arrange(Surface_Ha) %>%
  { barplot(setNames(.$Surface_Ha, .$Type_OS), horiz = TRUE, las = 1, cex.names = 0.5, 
            main = "Surfaces cumulées par OS (ha)") }

# Calcul du CN moyen pondéré par étang
# Au lieu de multiplier des matrices (xtabs), on fait tout dans la même table
cnetg <- os_complet %>%
  group_by(Etang) %>%
  summarise(
    Surface_BV = round(sum(Surface) / 10000, 1),
    # La formule du CN moyen pondéré : (Somme des CN * Surface) / Somme des Surfaces
    CNII = round(sum(CN.sol.D.Fav * Surface) / sum(Surface), 1)
  ) %>%
  # 4. Calcul CNI et CNIII
  mutate(
    CNI   = round(4.2 * CNII / (10 - 0.058 * CNII)),
    CNIII = round(23 * CNII / (10 + 0.13 * CNII))
  )

head(cnetg)

cnetg$Etang


#tableau QGIS
etg = read.csv2("Etangs_Chalamont.csv", header = TRUE, dec = ",", sep = ";") %>% 
  filter(Chaine_etu == "oui") %>%  rename(SURFACE_eau=SURFACE_SI) %>% select(-num_range("Assec", 2021:2025))
ASSEC = read.csv2("ASSEC_Final_2010_2025.csv",header = TRUE, sep = ";") %>% select(-Exutoire_1,-OBJECTID)

etg = ASSEC %>%  inner_join(etg, by ="NOM")

# Calcul du Vmax
etg$Vmax = etg$SURFACE_eau * etg$Profondeur_m * 10000

# Fusion avec cnetg

tab_etg <- cnetg %>%
  rename(NOM = Etang) %>% # On renomme pour la jointure
  inner_join(etg, by = "NOM") %>% select(-Vidange)



Vidange_peche <- read.csv("Vidange_Peche_2010_2025.csv", sep = ",")

tab_etg <- tab_etg %>%
  # LA SOLUTION EST ICI : on exclut les colonnes en double avant de fusionner
  left_join(Vidange_peche %>% select(-Exutoire_1, -OBJECTID), by = "NOM") %>%
  mutate(
    jours_vidange = ceiling(SURFACE_eau)
  ) %>%
  mutate(
    across(
      .cols = starts_with("peche"),                  
      .fns = ~ as.Date(.x) - jours_vidange,          
      .names = "{gsub('peche', 'Vidange', .col)}"   
    )
  )

head(tab_etg)
head(Vidange_peche)

# Nettoyage
#rm(etg, cnetg, Prof)
head(tab_etg)



if (SITE_CHOISI == "MARLIEUX") {
  
  #  Chargement Pluvio (Marlieux)
  fichiers_meteo <- c(
    "meteo/Q_01_previous-1950-2024_RR-T-Vent.CSV",
    "meteo/Q_01_latest-2025-2026_RR-T-Vent.CSV"
    
  )
  
  meteo <- fichiers_meteo %>%
    map_df(~read.csv2(.x)) %>%  
    filter(NOM_USUEL == "MARLIEUX") %>%
    mutate(
      dat = as.Date(as.character(AAAAMMJJ), format="%Y%m%d"),
      an = format(dat, "%Y"),
      RR = as.numeric(as.character(RR))
    ) %>%
    select(dat, an, RR, NOM_USUEL)
  
  #  Chargement ETP
  files_etp <- paste0("meteo/QUOT_ETPgrille_", 2021:2025, ".csv")
  
  etp_marlieux <- files_etp %>%
    map_df(~read.csv(.x, sep=";", dec = ".")) %>%
    # On utilise une petite marge pour la latitude/longitude 
    filter(abs(latitude - 46.0385) < 0.05 & abs(longitude - 5.0443) < 0.05) %>%
    mutate(dat = as.Date(as.character(date), format="%Y%m%d")) %>%
    select(dat, ETP_grille)
  
  pluvio <- meteo %>%
    inner_join(etp_marlieux, by = "dat") %>%
    mutate(
      ETP_grille = as.numeric(as.character(ETP_grille)),
      P_ETP = RR - ETP_grille
    )
} else if (SITE_CHOISI == "CHALAMONT") {
  fichiers_meteo <- paste0("CHALAMON_meteo/PluieJour/Pluie_", 2022:2023, ".csv")
  
  meteo <- fichiers_meteo %>%
    map_df(~read.csv2(.x, sep = ",",dec = ".")) %>%
    rename(
      RR = Pluie,
    ) %>% 
    mutate(
      dat = as.Date(as.character(date), format="%Y-%m-%d"),
      an = format(dat, "%Y"),
      RR = as.numeric(as.character(RR))
    ) %>%
    select(dat, an, RR)
  
  #  Chargement ETP
  files_etp <- paste0("meteo/QUOT_ETPgrille_", 2022:2024, ".csv")
  
  etp_marlieux <- files_etp %>%
    map_df(~read.csv(.x, sep=";", dec = ".")) %>%
    # On utilise une petite marge pour la latitude/longitude 
    filter(abs(latitude - 46.018) < 0.05 & abs(longitude - 5.152) < 0.05) %>%
    mutate(dat = as.Date(as.character(date), format="%Y%m%d")) %>%
    select(dat, ETP_grille)
  
  pluvio <- meteo %>%
    inner_join(etp_marlieux, by = "dat") %>%
    mutate(
      ETP_grille = as.numeric(as.character(ETP_grille)),
      P_ETP = RR - ETP_grille
    )
} else if (SITE_CHOISI == "SAFRAN") {
  
  fichiers_meteo <- c(
    "meteo/SAFRAN/QUOT_SIM2_2010-2019.CSV", 
    "meteo/SAFRAN/QUOT_SIM2_previous-2020-202602.csv"
  )
  
  
  coordonnees <- read.csv("meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% 
    filter(CODE == 2)
  
  X <- coordonnees$LAMBX[1]
  Y <- coordonnees$LAMBY[1]
  
  X_min <- X - 40
  X_max <- X + 40
  Y_min <- Y - 40
  Y_max <- Y + 40
  
  print("Filtrage du fichier 2010-2019...")
  meteo_1 <- open_dataset(fichiers_meteo[1], format = "csv", delimiter = ";") %>%
    filter(LAMBX >= X_min & LAMBX <= X_max & LAMBY >= Y_min & LAMBY <= Y_max) %>%
    collect() 
  
  print("Filtrage du fichier 2020-2026...")
  meteo_2 <- open_dataset(fichiers_meteo[2], format = "csv", delimiter = ";") %>%
    filter(LAMBX >= X_min & LAMBX <= X_max & LAMBY >= Y_min & LAMBY <= Y_max) %>%
    collect() 
  
  # On fusionne les deux tableaux propres
  meteo <- bind_rows(meteo_1, meteo_2)
  
  print("Terminé ! Vérification des dates :")
  
  
  cases_capturees <- meteo %>%
    select(LAMBX, LAMBY) %>%
    distinct() %>%
    mutate(
      # On calcule la distance exacte entre chaque maille et le centroïde
      distance = sqrt((LAMBX - X)^2 + (LAMBY - Y)^2)
    ) %>%
    # On trie pour mettre la maille la plus proche en première ligne
    arrange(distance)
  
  # On sauvegarde les coordonnées
  le_bon_X <- cases_capturees$LAMBX[1]
  le_bon_Y <- cases_capturees$LAMBY[1]
  
  print(paste("La maille SAFRAN exacte de l'étang est : X =", le_bon_X, "et Y =", le_bon_Y))
   # On nettoie le tableau final en ne gardant QUE cette maille précise
  meteo <- meteo %>%
    filter(LAMBX == le_bon_X & LAMBY == le_bon_Y)
  
  
  head(meteo)
  meteo<- meteo %>% rename(
    RR = PRELIQ,
  ) %>% 
    mutate(
      dat = as.Date(as.character(DATE), format="%Y%m%d"),
      an = format(dat, "%Y"),
      RR = as.numeric(as.character(RR))
    ) %>%
    select(dat, an, RR,ETP)
  pluvio <- meteo %>%
    mutate(
      ETP_grille = as.numeric(as.character(ETP)),
      P_ETP = RR - ETP_grille
    ) %>%  filter(between(dat, as.Date("2010-01-01"), as.Date("2025-12-31")))
  
}




pluvio <- read.csv2("Meteo_SAFRAN_Prete_A_L_Emploi.csv") %>%
  mutate(dat = as.Date(dat))
