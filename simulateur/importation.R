# ==============================================================================
# SCRIPT 1 : IMPORTATION ET PRÉPARATION DES DONNÉES (importation.R)
# Objectif : Générer les tables "tab_etg" et "pluvio" pour le simulateur
# ==============================================================================

# Chargement des librairies strictes de traitement de données
library(tidyverse)
library(lubridate)
source("simulateur/fonctions.R") # On charge la boîte à outils (Script 3)

# ==============================================================================
# 0. CONFIGURATION DU MODÈLE (BASSIN ET MÉTÉO)
# ==============================================================================
# C'est ton tableau de bord : modifie ces 3 lignes pour changer de simulation !
DOSSIER_BASSIN_ACTUEL <- "data/Chalamont_opti/Vidange_Assec"         # Ex: "data/Chalamont"
DOSSIER_METEO_ACTUEL  <- "data/meteo/HadGEM2  RegCM4-6"   # Ex: "data/meteo/Chalamont"
CODE_METEO_ACTUEL     <- 2                    # Code dans centro_BV.csv (Joyeux = 23)

# ==============================================================================
# 1. PRÉPARATION DES DONNÉES ÉTANGS ET OCCUPATION DES SOLS
# ==============================================================================

generer_tab_etg <- function(dossier_bassin, chemin_fichier_cn = "data/OS_CN_0.05.csv") {
  
  # Le script construit les chemins dynamiquement en fonction du bassin choisi
  chemin_os     <- paste0(dossier_bassin, "/OS_BV.csv")
  chemin_etangs <- paste0(dossier_bassin, "/Etangs.csv")
  chemin_assec  <- paste0(dossier_bassin, "/ASSEC_Final.csv")
  chemin_peche  <- paste0(dossier_bassin, "/Vidange_Peche.csv")
  
  # LECTURE INTELLIGENTE : Devine le séparateur et nettoie l'encodage Windows
  lire_csv_robuste <- function(chemin) {
    if (!file.exists(chemin)) stop(paste("\n❌ ERREUR : Le fichier est introuvable ->", chemin))
    ligne <- readLines(chemin, n = 1, warn = FALSE)
    separateur <- ";" # Défaut
    if (grepl(",", ligne) && !grepl(";", ligne)) separateur <- ","
    if (grepl("\t", ligne)) separateur <- "\t"
    
    df <- read.table(chemin, sep = separateur, header = TRUE, stringsAsFactors = FALSE, check.names = TRUE, fill = TRUE)
    
    names(df) <- gsub("^ï\\.\\.", "", names(df))
    names(df) <- gsub("^X\\.", "", names(df))
    names(df) <- gsub("^\ufeff", "", names(df))
    names(df) <- trimws(names(df))
    
    return(df)
  }
  
  # 1. Chargement OS 
  os_data <- lire_csv_robuste(chemin_os) %>%
    select(ClasseOS = 1, Etang = 2, Surface = 3) %>%
    mutate(
      Surface = as.numeric(gsub(",", ".", as.character(Surface))),
      ClasseOS = as.numeric(as.character(ClasseOS))
    ) %>%
    filter(ClasseOS < 23)
  
  # 2. Chargement CN 
  tab_cn <- lire_csv_robuste(chemin_fichier_cn)
  
  # 3. Calcul du Curve Number pondéré par étang
  cnetg <- os_data %>%
    left_join(tab_cn, by = c("ClasseOS" = "Code_OS")) %>%
    mutate(CN.sol.D.Fav = as.numeric(gsub(",", ".", as.character(CN.sol.D.Fav)))) %>%
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
  etg_params <- lire_csv_robuste(chemin_etangs) %>% 
    select(NOM, SURFACE_eau, any_of(c("Exutoire_1", "Exutoire_2")), Profondeur, Vmax)
  
  assec_data <- lire_csv_robuste(chemin_assec) 
  
  # --- SÉCURITÉ ANTI-PLANTAGE DYNAMIQUE ---
  if (!"NOM" %in% names(assec_data)) stop(paste("\n❌ ERREUR : 'NOM' introuvable dans", chemin_assec))
  if (!"NOM" %in% names(etg_params)) stop(paste("\n❌ ERREUR : 'NOM' introuvable dans", chemin_etangs))
  
  assec_data <- assec_data %>% select(-any_of(c("Exutoire_1", "OBJECTID")))
  
  # Fusion et calcul du Vmax
  etg_model <- assec_data %>% 
    inner_join(etg_params, by ="NOM") %>%
    mutate(
      Vmax = as.numeric(Vmax),
      Profondeur = as.numeric(gsub(",", ".", as.character(Profondeur))),
      SURFACE_eau = as.numeric(SURFACE_eau) / 10000, 
      Vmax = ifelse(is.na(Vmax), SURFACE_eau * Profondeur * 10000, Vmax)
    )
  
  # 5. Chargement des dates de Vidange/Pêche
  vidange_raw <- lire_csv_robuste(chemin_peche)
  
  # 6. Assemblage final complet
  tab_etg_final <- cnetg %>%
    inner_join(etg_model, by = "NOM") %>% 
    select(-any_of("Vidange")) %>%
    left_join(vidange_raw %>% select(-any_of(c("Exutoire_1", "OBJECTID"))), by = "NOM") %>%
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
tab_etg_base  <- generer_tab_etg(dossier_bassin = DOSSIER_BASSIN_ACTUEL, chemin_fichier_cn = "data/OS_CN_0.05.csv")
tab_etg_modif <- generer_tab_etg(dossier_bassin = DOSSIER_BASSIN_ACTUEL, chemin_fichier_cn = "data/OS_CN_0.05_modif.csv")

print(paste("✅ Préparation terminée pour le bassin :", DOSSIER_BASSIN_ACTUEL))


# ==============================================================================
# 2. CHARGEMENT DE LA MÉTÉO (Fichier Unique Simplifié et Dynamique)
# ==============================================================================

# Construction dynamique des chemins météo
chemin_meteo <- paste0(DOSSIER_METEO_ACTUEL, "/Meteo.csv")
chemin_centro <- paste0(DOSSIER_METEO_ACTUEL, "/centro_BV.csv")

# Sécurité si le fichier n'a pas été renommé correctement
if (!file.exists(chemin_meteo)) stop(paste("\n❌ ERREUR : Le fichier Météo est introuvable. As-tu bien renommé ton fichier en 'Meteo.csv' dans le dossier", DOSSIER_METEO_ACTUEL, "?"))
if (!file.exists(chemin_centro)) stop(paste("\n❌ ERREUR : Le fichier centro_BV.csv est introuvable dans", DOSSIER_METEO_ACTUEL))

print(paste("Lecture du fichier météo cible :", chemin_meteo))

# Cible du Bassin Versant dynamique
coordonnees <- read.csv(chemin_centro, header = TRUE, sep = ",") %>% 
  filter(CODE == CODE_METEO_ACTUEL)

X_ref <- coordonnees$LAMBX[1]
Y_ref <- coordonnees$LAMBY[1]

# On lit le fichier CSV standard 
meteo_brute <- read.csv2(chemin_meteo, stringsAsFactors = FALSE) 

# Recherche de la maille la plus proche
maille_proche <- meteo_brute %>%
  select(LAMBX, LAMBY) %>%
  distinct() %>%
  mutate(distance = sqrt((LAMBX - X_ref)^2 + (LAMBY - Y_ref)^2)) %>%
  arrange(distance) %>%
  head(1)

le_bon_X <- maille_proche$LAMBX[1]
le_bon_Y <- maille_proche$LAMBY[1]

print(paste("Maille Météo capturée : X =", le_bon_X, "| Y =", le_bon_Y))

# Création de la série temporelle journalière brute
pluvio_base <- meteo_brute %>%
  filter(LAMBX == le_bon_X & LAMBY == le_bon_Y) %>%
  rename(RR = PRELIQ) %>% 
  mutate(
    dat = as.Date(lubridate::parse_date_time(as.character(DATE), orders = c("ymd", "dmy", "Ymd", "Y-m-d"))),
    RR = as.numeric(gsub(",", ".", as.character(RR))),
    ETP_grille = as.numeric(gsub(",", ".", as.character(ETP))),
    P_ETP = RR - ETP_grille
  ) %>%
  select(dat, RR, ETP_grille, P_ETP) %>%
  filter(between(dat, as.Date("2010-01-01"), as.Date("2025-12-31"))) %>%
  arrange(dat)
print("Série Pluvio générée et prête pour le simulateur !")