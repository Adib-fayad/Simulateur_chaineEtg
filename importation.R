library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
library(readxl)
source("fonctions.R")
#################################
#Choix des donnee meteo CHALAMONT OU MARLIEUX

SITE_CHOISI <- "CHALAMONT"

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
etg = read.csv2("Etangs_Chalamont.csv", header = TRUE, dec = ".", sep = ",") %>% 
  filter(Chaine_etu == "oui")


# Calcul du Vmax
Prof = 0.70
etg$Vmax = etg$SURFACE_SI * Prof * 10000

# Fusion avec cnetg

tab_etg <- cnetg %>%
  rename(NOM = Etang) %>% # On renomme pour la jointure
  inner_join(etg, by = "NOM") %>% select(-Vidange)



Vidange_peche <- read_excel("data.xlsx")
  tab_etg <- tab_etg %>%
  left_join(Vidange_peche, by = "NOM") %>%
  mutate(
    Date_temp = as.Date(paste0("2000", peche)),
    Jours_vidange = pmin(round(SURFACE_SI),10), 
    Vidange_temp = Date_temp - Jours_vidange,
    Vidange = format(Vidange_temp, "-%m-%d")
  ) %>%
  select(-Date_temp, -Vidange_temp, -Jours_vidange)
  
head(Vidange_peche)

# Nettoyage
#rm(etg, cnetg, Prof)
head(tab_etg)





if (SITE_CHOISI == "MARLIEUX") {
  
  #  Chargement Pluvio (Marlieux)
  fichiers_meteo <- c(
    "meteo/Q_01_previous-1950-2024_RR-T-Vent.CSV", 
    "meteo/Q_01_latest-2025-2026_RR-T-Vent.csv"
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
      dat = as.Date(as.character(date), format="%Y-%m-%d"),,
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
}







