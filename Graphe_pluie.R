library(tidyverse)
library(arrow)
library(ggplot2)

# ====================================================================
# 1. IMPORTATION DES DONNÉES CHALAMONT
# ====================================================================
print("Chargement de Chalamont...")
fichiers_meteo_chal <- paste0("CHALAMON_meteo/PluieJour/Pluie_", 2022:2023, ".csv")

meteo_chal <- fichiers_meteo_chal %>%
  map_df(~read.csv2(.x, sep = ",", dec = ".")) %>%
  rename(RR_CHAL = Pluie) %>% 
  mutate(
    dat = as.Date(as.character(date), format="%Y-%m-%d"),
    RR_CHAL = as.numeric(as.character(RR_CHAL))
  ) %>%
  select(dat, RR_CHAL) %>%
  drop_na(dat) # Sécurité pour enlever les lignes vides


# ====================================================================
# 2. IMPORTATION DES DONNÉES SAFRAN
# ====================================================================
print("Chargement de SAFRAN...")
fichiers_meteo_saf <- c(
  "meteo/SAFRAN/QUOT_SIM2_2010-2019.CSV", 
  "meteo/SAFRAN/QUOT_SIM2_previous-2020-202602.csv"
)

coordonnees <- read.csv("meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% filter(CODE == 2)
X <- coordonnees$LAMBX[1]
Y <- coordonnees$LAMBY[1]

# On filtre directement sur le fichier 2 (car Chalamont n'a que 2022-2023)
meteo_saf_brut <- open_dataset(fichiers_meteo_saf[2], format = "csv", delimiter = ";") %>%
  filter(LAMBX >= X - 40 & LAMBX <= X + 40 & LAMBY >= Y - 40 & LAMBY <= Y + 40) %>%
  collect() 

# Récupération de la bonne maille
cases_capturees <- meteo_saf_brut %>%
  select(LAMBX, LAMBY) %>% distinct() %>%
  mutate(distance = sqrt((LAMBX - X)^2 + (LAMBY - Y)^2)) %>% arrange(distance)

le_bon_X <- cases_capturees$LAMBX[1]
le_bon_Y <- cases_capturees$LAMBY[1]

meteo_saf <- meteo_saf_brut %>%
  filter(LAMBX == le_bon_X & LAMBY == le_bon_Y) %>%
  rename(RR_SAF = PRELIQ) %>%
  mutate(
    dat = as.Date(as.character(DATE), format="%Y%m%d"),
    RR_SAF = as.numeric(as.character(RR_SAF))
  ) %>%
  select(dat, RR_SAF)


# ====================================================================
# 3. FUSION DES DEUX SOURCES (Inner Join sur la Date)
# ====================================================================
df_comparaison <- meteo_chal %>%
  inner_join(meteo_saf, by = "dat") %>%
  mutate(
    annee = format(dat, "%Y"),
    ecart = RR_SAF - RR_CHAL # Calcul de la différence journalière
  ) %>%
  # Calcul du cumul annuel pour voir les différences de volume total
  group_by(annee) %>%
  arrange(dat) %>%
  mutate(
    Cumul_CHAL = cumsum(replace_na(RR_CHAL, 0)),
    Cumul_SAF = cumsum(replace_na(RR_SAF, 0))
  ) %>%
  ungroup()

print("Fusion terminée ! Génération des graphiques...")

# ====================================================================
# 4. GRAPHIQUES DE COMPARAISON
# ====================================================================

# GRAPHIQUE 1 : Nuage de points (Corrélation jour par jour)
g1 <- ggplot(df_comparaison, aes(x = RR_SAF, y = RR_CHAL)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Comparaison Journalière : SAFRAN vs CHALAMONT",
    subtitle = "La ligne rouge représente une égalité parfaite",
    x = "Pluie SAFRAN (mm)",
    y = "Pluie CHALAMONT (mm)"
  ) +
  theme_minimal()

# GRAPHIQUE 2 : Chronologie superposée (Pluie au cours du temps)
g2 <- ggplot(df_comparaison) +
  geom_line(aes(x = dat, y = RR_SAF, color = "SAFRAN"), alpha = 0.7, size = 0.8) +
  geom_line(aes(x = dat, y = RR_CHAL, color = "CHALAMONT"), alpha = 0.7, size = 0.8) +
  scale_color_manual(values = c("SAFRAN" = "darkred", "CHALAMONT" = "steelblue")) +
  labs(
    title = "Chronologie des précipitations (2022-2023)",
    x = "Date", y = "Précipitations (mm)", color = "Source"
  ) +
  theme_minimal()

# GRAPHIQUE 3 : CUMULS ANNUELS (Le plus important en hydrologie !)
g3 <- ggplot(df_comparaison) +
  geom_line(aes(x = dat, y = Cumul_SAF, color = "SAFRAN"), size = 1) +
  geom_line(aes(x = dat, y = Cumul_CHAL, color = "CHALAMONT"), size = 1) +
  scale_color_manual(values = c("SAFRAN" = "darkred", "CHALAMONT" = "steelblue")) +
  facet_wrap(~annee, scales = "free_x") + # Sépare 2022 et 2023
  labs(
    title = "Cumul annuel des précipitations",
    subtitle = "Permet de voir quel modèle 'donne' le plus d'eau sur l'année",
    x = "Date", y = "Pluie cumulée (mm)", color = "Source"
  ) +
  theme_minimal()

# Affichage des graphiques
dev.new()
print(g1)
print(g2)
print(g3)

# ====================================================================
# GRAPHIQUE 4 : L'écart journalier (SAFRAN - CHALAMONT)
# ====================================================================
g4 <- ggplot(df_comparaison, aes(x = dat, y = ecart)) +
  # Ligne noire horizontale à 0 (égalité parfaite)
  geom_hline(yintercept = 0, color = "black", size = 0.8) + 
  # Les barres/lignes de l'écart
  geom_col(aes(fill = ecart > 0), show.legend = FALSE) + 
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "steelblue")) +
  labs(
    title = "Écart journalier : SAFRAN vs CHALAMONT",
    subtitle = "Rouge (>0) : SAFRAN annonce plus d'eau. Bleu (<0) : CHALAMONT a plus plu.",
    x = "Date", 
    y = "Différence de pluie (mm)"
  ) +
  theme_minimal()


# ====================================================================
# GRAPHIQUE 5 : L'écart cumulé sur l'année (CORRIGÉ)
# ====================================================================

g5 <- ggplot(df_comparaison, aes(x = dat, y = ecart_cumule, group = 1)) + # <-- L'ajout est ici (group = 1)
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = ecart_cumule), fill = "darkgreen", alpha = 0.2) + 
  facet_wrap(~annee, scales = "free_x") +
  labs(
    title = "Écart cumulé au cours de l'année",
    subtitle = "Si la courbe plonge, cela signifie que SAFRAN 'rate' un grand volume d'eau.",
    x = "Date", 
    y = "Déficit / Excédent SAFRAN (mm)"
  ) +
  theme_minimal()



# On affiche les nouveaux graphiques
print(g4)
print(g5)
