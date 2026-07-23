# ==============================================================================
# SCRIPT 1 : IMPORTATION ET PRÉPARATION DES DONNÉES (importation.R)
# Objectif : Générer les tables "tab_etg" et "pluvio" pour le simulateur
# ==============================================================================

# Chargement des librairies strictes de traitement de données
library(tidyverse)
library(lubridate)
source("simulateur/fonctions.R") # On charge la boîte à outils (Script 3)

# ==============================================================================
# 1. PRÉPARATION DES DONNÉES ÉTANGS ET OCCUPATION DES SOLS
# ==============================================================================

#' Fonction pour générer le tableau des étangs selon le fichier CN choisi
#' Cela permet de basculer facilement entre Lambda 0.20, 0.10 et 0.05
generer_tab_etg <- function(chemin_fichier_cn = "data/OS_CN_0.05.csv") {
  
  # 1. Chargement OS
  os_data <- read.csv2("data/OS_BV_Etg_Chalamont.csv", dec = ".", sep = ",") %>%
    select(ClasseOS = 1, Etang = 2, Surface = 3) %>%
    filter(ClasseOS < 23)
  
  # 2. Chargement CN (dynamique selon le lambda)
  tab_cn <- read.csv2(chemin_fichier_cn, sep = ";", encoding = "latin1")
  
  # 3. Calcul du Curve Number pondéré par étang
  cnetg <- os_data %>%
    left_join(tab_cn, by = c("ClasseOS" = "Code_OS")) %>%
    group_by(Etang) %>%
    summarise(
      Surface_BV = round(sum(Surface, na.rm = TRUE) / 10000, 1),
      CNII = round(sum(CN.sol.D.Fav * Surface, na.rm = TRUE) / sum(Surface, na.rm = TRUE), 1)
    ) %>%
    mutate(
      CNI   = round(4.2 * CNII / (10 - 0.058 * CNII)),
      CNIII = round(23 * CNII / (10 + 0.13 * CNII))
    ) %>%
    rename(NOM = Etang)
  
  # 4. Chargement des caractéristiques physiques et Assecs
  etg_params <- read.csv2("data/Etangs_Chalamont.csv", header = TRUE, dec = ",", sep = ";") %>% 
    filter(Chaine_etu == "oui") %>% 
    select(-num_range("Assec", 2021:2025))
  
  assec_data <- read.csv2("data/ASSEC_Final_2010_2025.csv", header = TRUE, sep = ";") %>% 
    select(-Exutoire_1, -OBJECTID)
  
  # Fusion et calcul du Vmax
  etg_model <- assec_data %>% 
    inner_join(etg_params, by ="NOM") %>%
    mutate(Vmax = ifelse(is.na(Vmax), SURFACE_eau * Profondeur_m * 10000, Vmax))
  
  # 5. Chargement des dates de Vidange/Pêche
  vidange_raw <- read.csv("data/Vidange_Peche_2010_2025.csv", sep = ",")
  
  # 6. Assemblage final complet
  tab_etg_final <- cnetg %>%
    inner_join(etg_model, by = "NOM") %>% 
    select(-any_of("Vidange")) %>%
    left_join(vidange_raw %>% select(-Exutoire_1, -OBJECTID), by = "NOM") %>%
    mutate(
      jours_vidange = ceiling(SURFACE_eau),
      across(
        .cols = starts_with("peche"),                 
        .fns = ~ as.Date(.x) - jours_vidange,         
        .names = "{gsub('peche', 'Vidange', .col)}"   
      )
    )
  
  return(tab_etg_final)
}

# ==============================================================================
# Génération des DEUX tableaux (Base et Modifié)
# ==============================================================================
tab_etg_base  <- generer_tab_etg("data/OS_CN_0.05.csv")
tab_etg_modif <- generer_tab_etg("data/OS_CN_0.05_modif.csv")

print("✅ Préparation des paramètres terminée : Les 2 tableaux (Base et Modif) sont en mémoire.")


# ==============================================================================
# 2. CHARGEMENT DE LA MÉTÉO (Fichier Unique Simplifié)
# ==============================================================================

fichier_meteo <- "data/meteo/SAFRAN/Meteo_Dombes_Mise_A_Jour_Finale.csv"

# Cible du Bassin Versant
coordonnees <- read.csv("data/meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% 
  filter(CODE == 2)

X_ref <- coordonnees$LAMBX[1]
Y_ref <- coordonnees$LAMBY[1]

print("Lecture du fichier météo unique...")

# On lit le fichier CSV standard 
meteo_brute <- read.csv2(fichier_meteo, stringsAsFactors = FALSE) 

# Recherche de la maille la plus proche
maille_proche <- meteo_brute %>%
  select(LAMBX, LAMBY) %>%
  distinct() %>%
  mutate(distance = sqrt((LAMBX - X_ref)^2 + (LAMBY - Y_ref)^2)) %>%
  arrange(distance) %>%
  head(1)

le_bon_X <- maille_proche$LAMBX[1]
le_bon_Y <- maille_proche$LAMBY[1]

print(paste(" Maille SAFRAN capturée : X =", le_bon_X, "| Y =", le_bon_Y))

# Création de la série temporelle journalière brute
pluvio_base <- meteo_brute %>%
  filter(LAMBX == le_bon_X & LAMBY == le_bon_Y) %>%
  rename(RR = PRELIQ) %>% 
  mutate(
    # Gestion robuste des dates (20100101 ou 2010-01-01) et des virgules
    dat = as.Date(lubridate::parse_date_time(as.character(DATE), orders = c("ymd", "dmy", "Ymd", "Y-m-d"))),
    RR = as.numeric(gsub(",", ".", as.character(RR))),
    ETP_grille = as.numeric(gsub(",", ".", as.character(ETP))),
    P_ETP = RR - ETP_grille
  ) %>%
  select(dat, RR, ETP_grille, P_ETP) %>%
  filter(between(dat, as.Date("2010-01-01"), as.Date("2025-12-31"))) %>%
  arrange(dat)

print("Série Pluvio SAFRAN 2010-2025 générée et prête pour le simulateur !")
