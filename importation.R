library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
library(readxl)
library(arrow) 
source("fonctions.R")

################################################################################
# 1. PRÉPARATION DES DONNÉES (OCCUPATION DES SOLS ET ÉTANGS)
################################################################################

# Chargement et nettoyage initial de l'occupation des sols
os_data <- read.csv2("OS_BV_Etg_Chalamont.csv", dec = ".", sep = ",") %>%
  select(ClasseOS = 1, Etang = 2, Surface = 3) %>%
  filter(ClasseOS < 23)

tab_cn <- read.csv2("OS_CN.csv", sep = ";", encoding = "latin1")

# Fusion OS et table des Curve Numbers (CN)
os_complet <- os_data %>%
  left_join(tab_cn, by = c("ClasseOS" = "Code_OS"))

# Visualisation des surfaces par type d'OS
os_complet %>%
  group_by(Type_OS) %>%
  summarise(Surface_Ha = sum(Surface) / 10000) %>%
  arrange(Surface_Ha) %>%
  { barplot(setNames(.$Surface_Ha, .$Type_OS), horiz = TRUE, las = 1, cex.names = 0.5, 
            main = "Surfaces cumulées par OS (ha)", col = "steelblue") }

# Calcul du Curve Number (CN) pondéré par étang
cnetg <- os_complet %>%
  group_by(Etang) %>%
  summarise(
    Surface_BV = round(sum(Surface) / 10000, 1),
    CNII = round(sum(CN.sol.D.Fav * Surface) / sum(Surface), 1)
  ) %>%
  mutate(
    CNI   = round(4.2 * CNII / (10 - 0.058 * CNII)),
    CNIII = round(23 * CNII / (10 + 0.13 * CNII))
  )

# Chargement des caractéristiques des étangs
etg = read.csv2("Etangs_Chalamont.csv", header = TRUE, dec = ",", sep = ";") %>% 
  filter(Chaine_etu == "oui") %>% 
  select(-num_range("Assec", 2021:2025))


ASSEC = read.csv2("ASSEC_Final_2010_2025.csv", header = TRUE, sep = ";") %>% 
  select(-Exutoire_1, -OBJECTID)

etg = ASSEC %>% inner_join(etg, by ="NOM")

# Calcul du Volume Max (Vmax)
etg <- etg %>%
  mutate(Vmax = ifelse(
    is.na(Vmax),                                
    SURFACE_eau * Profondeur_m * 10000,     
    Vmax                                       
  ))
# Fusion finale Étangs + Curve Number + Vidanges
Vidange_peche <- read.csv("Vidange_Peche_2010_2025.csv", sep = ",")

tab_etg <- cnetg %>%
  rename(NOM = Etang) %>%
  inner_join(etg, by = "NOM") %>% 
  select(-Vidange) %>%
  left_join(Vidange_peche %>% select(-Exutoire_1, -OBJECTID), by = "NOM") %>%
  mutate(
    jours_vidange = ceiling(SURFACE_eau),
    across(
      .cols = starts_with("peche"),                 
      .fns = ~ as.Date(.x) - jours_vidange,         
      .names = "{gsub('peche', 'Vidange', .col)}"   
    )
  )

print("Préparation des étangs terminée.")


################################################################################
# 2. CHARGEMENT DE LA MÉTÉO (MÉTHODE SAFRAN UNIQUEMENT)
################################################################################

fichiers_meteo <- c(
  "meteo/SAFRAN/QUOT_SIM2_2010-2019.CSV", 
  "meteo/SAFRAN/QUOT_SIM2_previous-2020-202602.csv"
)

# On trouve la bonne maille SAFRAN en fonction du centroïde du Bassin Versant
coordonnees <- read.csv("meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% 
  filter(CODE == 2) # On cible le BV 2

X <- coordonnees$LAMBX[1]
Y <- coordonnees$LAMBY[1]

# Fenêtre de recherche large (40 km autour)
X_min <- X - 40 ; X_max <- X + 40
Y_min <- Y - 40 ; Y_max <- Y + 40

print("Filtrage du fichier Météo 2010-2019 (via Arrow)...")
meteo_1 <- open_dataset(fichiers_meteo[1], format = "csv", delimiter = ";") %>%
  filter(LAMBX >= X_min & LAMBX <= X_max & LAMBY >= Y_min & LAMBY <= Y_max) %>%
  collect() 

print("Filtrage du fichier Météo 2020-2026 (via Arrow)...")
meteo_2 <- open_dataset(fichiers_meteo[2], format = "csv", delimiter = ";") %>%
  filter(LAMBX >= X_min & LAMBX <= X_max & LAMBY >= Y_min & LAMBY <= Y_max) %>%
  collect() 

# Fusion des deux décennies
meteo_brute <- bind_rows(meteo_1, meteo_2)

# Identification mathématique de la maille la plus proche
cases_capturees <- meteo_brute %>%
  select(LAMBX, LAMBY) %>%
  distinct() %>%
  mutate(distance = sqrt((LAMBX - X)^2 + (LAMBY - Y)^2)) %>%
  arrange(distance)

le_bon_X <- cases_capturees$LAMBX[1]
le_bon_Y <- cases_capturees$LAMBY[1]

print(paste("La maille SAFRAN la plus proche est : X =", le_bon_X, "et Y =", le_bon_Y))

# Nettoyage final pour ne garder que la pluie et l'ETP de cette maille
pluvio <- meteo_brute %>%
  filter(LAMBX == le_bon_X & LAMBY == le_bon_Y) %>%
  rename(RR = PRELIQ) %>% 
  mutate(
    dat = as.Date(as.character(DATE), format="%Y%m%d"),
    an = format(dat, "%Y"),
    RR = as.numeric(as.character(RR)),
    ETP_grille = as.numeric(as.character(ETP)),
    P_ETP = RR - ETP_grille # Bilan Pluie Efficace
  ) %>%
  select(dat, an, RR, ETP_grille, P_ETP) %>%
  filter(between(dat, as.Date("2010-01-01"), as.Date("2025-12-31"))) %>%
  arrange(dat)

print("Série Pluvio SAFRAN 2010-2025 générée avec succès !")

