# ==============================================================================
# ANALYSE DE VULNÉRABILITÉ 2026-2070 : INDICATEURS GLOBAUX ET DYNAMIQUE RÉSEAU
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr)
library(scales) # Pour le formatage des pourcentages

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE DU SCRIPT ET DOSSIERS
# ------------------------------------------------------------------------------
dossiers_scenarios <- c(
  "simulation futur/Chalamont_aleatoire/Grand_petit",
  "simulation futur/Chalamont_aleatoire/pluriannuel_fixe",
  "simulation futur/Chalamont_aleatoire/pluriannuel_variable",
  "simulation futur/Chalamont_opti/Vidange",
  "simulation futur/Chalamont_opti/Vidange_Assec"
)

noms_propres_scenarios <- c(
  "Grand_petit" = "1. Aléatoire (Taille)",
  "pluriannuel_fixe" = "2. Aléatoire (Fixe)",
  "pluriannuel_variable" = "3. Aléatoire (Variable)",
  "Vidange" = "4. Opti (Vidange seule)",
  "Vidange_Assec" = "5. Opti (Synchronisation Totale)"
)

# ------------------------------------------------------------------------------
# 2. MOTEUR DE CALCUL DES 4 INDICATEURS GLOBAUX (Avec prise en compte des Assecs)
# ------------------------------------------------------------------------------
calculer_indicateurs_annuels <- function(chemin_rds, nom_scenario, nom_modele) {
  
  simu <- readRDS(chemin_rds)
  df_exutoire <- simu$exutoire_data
  if(length(simu$liste_finale) == 0) return(NULL)
  
  # Extraction dynamique de la surface globale du BV
  surface_totale_bv <- sum(sapply(simu$liste_finale, function(x) x$Surface_BV[1]), na.rm = TRUE)
  
  # Fusion de TOUS les étangs avec calcul précis de la pluie directe
  df_all_etangs <- bind_rows(simu$liste_finale, .id = "NOM_ETANG") %>%
    mutate(
      annee = year(dat), mois = month(dat), jour = day(dat),
      Saison_Hydro = if_else(mois > 10 | (mois == 10 & jour >= 15), annee + 1, annee),
      
      # LE CORRECTIF PHYSIQUE : La pluie ne tombe directement dans l'eau QUE si l'étang n'est pas en Assec
      Volume_Pluie_Directe = if_else(Statut_Simu == "Assec", 0, RR_num * SURFACE_eau * 10)
    )
  
  # Agrégation journalière à l'échelle du réseau
  df_daily_global <- df_all_etangs %>%
    group_by(Saison_Hydro, dat) %>%
    summarise(
      RR_jour = first(RR_num),
      Volume_Ruissellement_Reseau = sum(Volume_R, na.rm = TRUE),
      Volume_Pluie_Directe_Reseau = sum(Volume_Pluie_Directe, na.rm = TRUE),
      Volume_Evap_Reseau = sum(abs(Evap_Reelle[Evap_Reelle < 0]), na.rm = TRUE),
      
      # LE 4ÈME INDICATEUR : Somme de l'eau arrivant de l'amont (Transit interne)
      Volume_Transit_Reseau = sum(Vamont, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(df_exutoire %>% select(dat, Volume_Riviere), by = "dat")
  
  # Calcul par Saison (Bilans Annuels Bruts)
  bilan_global <- df_daily_global %>%
    group_by(Saison_Hydro) %>%
    summarise(
      Pluie_Totale_mm = sum(RR_jour, na.rm = TRUE),
      Volume_Pluie_Total_BV_m3 = Pluie_Totale_mm * surface_totale_bv * 10,
      
      Volume_Exutoire_m3 = sum(Volume_Riviere, na.rm = TRUE),
      Volume_Evap_m3     = sum(Volume_Evap_Reseau, na.rm = TRUE),
      Volume_Captage_m3  = sum(Volume_Ruissellement_Reseau + Volume_Pluie_Directe_Reseau, na.rm = TRUE),
      Volume_Transit_m3  = sum(Volume_Transit_Reseau, na.rm = TRUE),
      
      # CALCUL DES POURCENTAGES ANNUELS (Pour les Boxplots)
      Coef_Ecoulement  = Volume_Exutoire_m3 / Volume_Pluie_Total_BV_m3,
      Coef_Evaporation = Volume_Evap_m3 / Volume_Pluie_Total_BV_m3,
      Coef_Captage     = Volume_Captage_m3 / Volume_Pluie_Total_BV_m3,
      Coef_Transit     = Volume_Transit_m3 / Volume_Pluie_Total_BV_m3,
      
      .groups = "drop"
    ) %>%
    filter(Saison_Hydro >= 2026 & Saison_Hydro <= 2070) %>%
    mutate(Scenario = nom_scenario, Modele_Meteo = nom_modele)
  
  return(bilan_global)
}

# ------------------------------------------------------------------------------
# 3. LE CRAWLER (Extraction de toutes les données)
# ------------------------------------------------------------------------------
cat("Début de l'analyse et de l'extraction des données...\n")
liste_df_resultats <- list()

for (dossier in dossiers_scenarios) {
  if (!dir.exists(dossier)) next
  fichiers_rds <- list.files(dossier, pattern = "\\.rds$", full.names = TRUE)
  nom_scenario_propre <- noms_propres_scenarios[basename(dossier)]
  
  for (fichier in fichiers_rds) {
    nom_fichier <- basename(fichier)
    modele_extrait <- str_extract(nom_fichier, "(?<=Meteo_).*(?=_[0-9]{8}\\.rds)")
    if (is.na(modele_extrait)) modele_extrait <- "Modele_Inconnu"
    
    res <- calculer_indicateurs_annuels(fichier, nom_scenario_propre, modele_extrait)
    if (!is.null(res)) liste_df_resultats[[length(liste_df_resultats) + 1]] <- res
  }
}

df_master <- bind_rows(liste_df_resultats)

# Mise au propre des légendes climatiques
df_master <- df_master %>%
  mutate(
    Modele_Meteo_Desc = case_when(
      str_detect(Modele_Meteo, "ALADIN63") ~ "CNRM-CM5 ALADIN63\n(Modéré)",
      str_detect(Modele_Meteo, "REMO2009") ~ "MPI-ESM REMO2009\n(Scénario intermédiaire)",
      str_detect(Modele_Meteo, "WRF381P")  ~ "IPSL-CM5A WRF381P\n(Hiver très pluvieux, Été humide)",
      str_detect(Modele_Meteo, "RCA4")     ~ "IPSL-CM5A RCA4\n(Hiver très humide, Été extrême)",
      str_detect(Modele_Meteo, "RegCM4-6") ~ "HadGEM2 RegCM4-6\n(Très chaud, sécheresse modérée)",
      str_detect(Modele_Meteo, "CCLM4-8-17") ~ "HadGEM2 CCLM4-8-17\n(Extrême : Le plus chaud/sec en été)",
      TRUE ~ Modele_Meteo
    )
  ) %>%
  mutate(
    Modele_Meteo_Desc = factor(Modele_Meteo_Desc, levels = c(
      "CNRM-CM5 ALADIN63\n(Modéré)", "MPI-ESM REMO2009\n(Scénario intermédiaire)",
      "IPSL-CM5A WRF381P\n(Hiver très pluvieux, Été humide)", "IPSL-CM5A RCA4\n(Hiver très humide, Été extrême)",
      "HadGEM2 RegCM4-6\n(Très chaud, sécheresse modérée)", "HadGEM2 CCLM4-8-17\n(Extrême : Le plus chaud/sec en été)"
    ))
  )


# ------------------------------------------------------------------------------
# 4. LE MOTEUR DE CUMUL (Transformation en Millimètres / Lame d'eau)
# ------------------------------------------------------------------------------
cat("Calcul des trajectoires cumulées (Lame d'eau en mm)...\n")

df_master_cum <- df_master %>%
  arrange(Scenario, Modele_Meteo_Desc, Saison_Hydro) %>%
  group_by(Scenario, Modele_Meteo_Desc) %>%
  mutate(
    Facteur_Surface = Volume_Pluie_Total_BV_m3 / Pluie_Totale_mm,
    
    # 1. Cumuls en m3
    Cum_Exutoire_m3 = cumsum(Volume_Exutoire_m3),
    Cum_Evap_m3     = cumsum(Volume_Evap_m3),
    Cum_Captage_m3  = cumsum(Volume_Captage_m3),
    Cum_Transit_m3  = cumsum(Volume_Transit_m3),
    
    # 2. Reconversion pure en Lame d'eau (mm)
    Cum_Exutoire_mm = Cum_Exutoire_m3 / Facteur_Surface,
    Cum_Evap_mm     = Cum_Evap_m3 / Facteur_Surface,
    Cum_Captage_mm  = Cum_Captage_m3 / Facteur_Surface,
    Cum_Transit_mm  = Cum_Transit_m3 / Facteur_Surface
  ) %>%
  ungroup()


# ------------------------------------------------------------------------------
# 5. PALETTES DE COULEURS ET THÈMES
# ------------------------------------------------------------------------------
couleurs_scenarios <- c("1. Aléatoire (Taille)" = "#e74c3c", "2. Aléatoire (Fixe)" = "#e67e22", "3. Aléatoire (Variable)" = "#f1c40f", "4. Opti (Vidange seule)" = "#3498db", "5. Opti (Synchronisation Totale)" = "#2ecc71")
linetypes_scenarios <- c("1. Aléatoire (Taille)" = "solid", "2. Aléatoire (Fixe)" = "dashed", "3. Aléatoire (Variable)" = "dotted", "4. Opti (Vidange seule)" = "solid", "5. Opti (Synchronisation Totale)" = "dashed")
couleurs_meteo <- c("CNRM-CM5 ALADIN63\n(Modéré)" = "#3498db", "MPI-ESM REMO2009\n(Scénario intermédiaire)" = "#f1c40f", "IPSL-CM5A WRF381P\n(Hiver très pluvieux, Été humide)" = "#2980b9", "IPSL-CM5A RCA4\n(Hiver très humide, Été extrême)" = "#9b59b6", "HadGEM2 RegCM4-6\n(Très chaud, sécheresse modérée)" = "#e67e22", "HadGEM2 CCLM4-8-17\n(Extrême : Le plus chaud/sec en été)" = "#e74c3c")

graphics.off() 


# ==============================================================================
# SECTION A : GRAPHIQUES CUMULÉS (EN MM) - TYPE A (Vue par Climat)
# ==============================================================================

# -- TYPE A1 : Écoulement --
dev.new(width = 14, height = 8)
gA1 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Exutoire_mm, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A1 : Lame d'Eau Cumulée Perdue à l'Exutoire (mm)", subtitle = "Vue Météo : Équivalent en mm de pluie enfuie par la rivière depuis 2026", x = "Saison", y = "Volume Écoulé Cumulé (mm)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA1)

# -- TYPE A2 : Évaporation --
dev.new(width = 14, height = 8)
gA2 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Evap_mm, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A2 : Lame d'Eau Cumulée Évaporée (mm)", subtitle = "Vue Météo : Équivalent en mm de pluie détruite par le forçage thermique depuis 2026", x = "Saison", y = "Évaporation Cumulée (mm)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA2)

# -- TYPE A3 : Captage --
dev.new(width = 14, height = 8)
gA3 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Captage_mm, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A3 : Lame d'Eau Cumulée Captée par le Réseau (mm)", subtitle = "Vue Météo : Équivalent en mm d'eau réellement entrée dans les étangs depuis 2026", x = "Saison", y = "Captage Cumulé (mm)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA3)

# -- TYPE A4 : Transit Interne --
dev.new(width = 14, height = 8)
gA4 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Transit_mm, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A4 : Dynamique du Transit Interne (L'effet Assec)", subtitle = "Vue Météo : Lame d'eau cumulée ayant voyagé d'un étang à l'autre sans être retenue", x = "Saison", y = "Transit Inter-Étangs Cumulé (mm)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA4)


# ==============================================================================
# SECTION B : GRAPHIQUES CUMULÉS (EN MM) - TYPE B (Vue par Gestion)
# ==============================================================================

# -- TYPE B1 : Écoulement --
dev.new(width = 14, height = 8)
gB1 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Exutoire_mm, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B1 : Lame d'Eau Cumulée Perdue à l'Exutoire (mm)", subtitle = "Vue Gestion : Dispersion de la perte en eau face à l'incertitude climatique", x = "Saison", y = "Volume Écoulé Cumulé (mm)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB1)

# -- TYPE B2 : Évaporation --
dev.new(width = 14, height = 8)
gB2 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Evap_mm, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B2 : Lame d'Eau Cumulée Évaporée (mm)", subtitle = "Vue Gestion : Sensibilité de l'évaporation des étangs selon les 6 futurs DRIAS", x = "Saison", y = "Évaporation Cumulée (mm)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB2)

# -- TYPE B3 : Captage --
dev.new(width = 14, height = 8)
gB3 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Captage_mm, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  theme_minimal(base_size = 14) +
  labs(
    title = "TYPE B3 : Lame d'Eau Cumulée Captée par le Réseau (mm)", 
    subtitle = "Vue Gestion : Impact de l'incertitude climatique sur l'entrée d'eau dans le réseau", 
    x = "Saison", 
    y = "Captage Cumulé (mm)", 
    color = "Modèle Météo"
  ) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB3)

# -- TYPE B4 : Transit Interne --
dev.new(width = 14, height = 8)
gB4 <- ggplot(df_master_cum, aes(x = Saison_Hydro, y = Cum_Transit_mm, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 1, alpha = 0.9) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  theme_minimal(base_size = 14) +
  labs(
    title = "TYPE B4 : Dynamique du Transit Interne Cumulé (mm)", 
    subtitle = "Vue Gestion : Sensibilité des déversements et flux inter-étangs face aux climats DRIAS", 
    x = "Saison", 
    y = "Transit Inter-Étangs Cumulé (mm)", 
    color = "Modèle Météo"
  ) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB4)

# ==============================================================================
# SECTION C : BOXPLOTS STATISTIQUES (RÉPARTITION ANNUELLE EN %)
# ==============================================================================

# -- BOXPLOT C1 : Écoulement Annuel (%) --
dev.new(width = 15, height = 8)
g_box_eco <- ggplot(df_master, aes(x = Scenario, y = Coef_Ecoulement, fill = Scenario)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.2, outlier.alpha = 0.6) +
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_fill_manual(values = couleurs_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  labs(title = "Distribution Annuelle du Coefficient d'Écoulement (2026-2070)", subtitle = "Chaque boîte représente la variabilité de la fuite d'eau sur 44 ans.", x = "Stratégie de Gestion", y = "Coef. d'Écoulement Annuel (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), legend.position = "none", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(g_box_eco)

# -- BOXPLOT C2 : Évaporation Annuelle (%) --
dev.new(width = 15, height = 8)
g_box_evap <- ggplot(df_master, aes(x = Scenario, y = Coef_Evaporation, fill = Scenario)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.2, outlier.alpha = 0.6) +
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_fill_manual(values = couleurs_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  labs(title = "Distribution Annuelle du Coefficient d'Évaporation (2026-2070)", subtitle = "Impact des stratégies de gestion sur la taxe solaire annuelle", x = "Stratégie de Gestion", y = "Coef. d'Évaporation Annuel (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), legend.position = "none", strip.background = element_rect(fill = "#fdf2e9", color = "#e67e22"), strip.text = element_text(face = "bold", color = "#2c3e50"))
print(g_box_evap)

# -- BOXPLOT C3 : Captage Annuel (%) --
dev.new(width = 15, height = 8)
g_box_capt <- ggplot(df_master, aes(x = Scenario, y = Coef_Captage, fill = Scenario)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.2, outlier.alpha = 0.6) +
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_fill_manual(values = couleurs_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  labs(title = "Distribution Annuelle du Taux de Captage Global (2026-2070)", subtitle = "Prise en compte dynamique des cuvettes en assec comme sol absorbant", x = "Stratégie de Gestion", y = "Taux de Captage Annuel (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), legend.position = "none", strip.background = element_rect(fill = "#eef7f0", color = "#2ecc71"), strip.text = element_text(face = "bold", color = "#2c3e50"))
print(g_box_capt)

# -- BOXPLOT C4 : Transit Interne Annuel (%) --
dev.new(width = 15, height = 8)
g_box_trans <- ggplot(df_master, aes(x = Scenario, y = Coef_Transit, fill = Scenario)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.2, outlier.alpha = 0.6) +
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_fill_manual(values = couleurs_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  labs(title = "Distribution Annuelle du Coefficient de Transit Interne (2026-2070)", subtitle = "Volume circulant entre les étangs (Preuve de l'effet 'Transparence' de l'assec)", x = "Stratégie de Gestion", y = "Coef. de Transit Interne (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), legend.position = "none", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold", color = "#2c3e50"))
print(g_box_trans)




# ==============================================================================
# SCRIPT D'ANALYSE DES TRAJECTOIRES ANNUELLES BRUTES (EN %)
# Objectif : Comparer les coefficients saison après saison avec correction Assec
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr)
library(scales)

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE DU SCRIPT ET DES DOSSIERS
# ------------------------------------------------------------------------------
dossiers_scenarios <- c(
  "simulation futur/Chalamont_aleatoire/Grand_petit",
  "simulation futur/Chalamont_aleatoire/pluriannuel_fixe",
  "simulation futur/Chalamont_aleatoire/pluriannuel_variable",
  "simulation futur/Chalamont_opti/Vidange",
  "simulation futur/Chalamont_opti/Vidange_Assec"
)

noms_propres_scenarios <- c(
  "Grand_petit" = "1. Aléatoire (Taille)",
  "pluriannuel_fixe" = "2. Aléatoire (Fixe)",
  "pluriannuel_variable" = "3. Aléatoire (Variable)",
  "Vidange" = "4. Opti (Vidange seule)",
  "Vidange_Assec" = "5. Opti (Synchronisation Totale)"
)

# ------------------------------------------------------------------------------
# 2. MOTEUR DE CALCUL DES 4 INDICATEURS ANNUELS CORRIGÉS
# ------------------------------------------------------------------------------
calculer_indicateurs_annuels <- function(chemin_rds, nom_scenario, nom_modele) {
  
  simu <- readRDS(chemin_rds)
  df_exutoire <- simu$exutoire_data
  if(length(simu$liste_finale) == 0) return(NULL)
  
  # Extraction de la surface totale du bassin versant
  surface_totale_bv <- sum(sapply(simu$liste_finale, function(x) x$Surface_BV[1]), na.rm = TRUE)
  
  # Fusion de tous les étangs et application de la correction d'Assec
  df_all_etangs <- bind_rows(simu$liste_finale, .id = "NOM_ETANG") %>%
    mutate(
      annee = year(dat), mois = month(dat), jour = day(dat),
      Saison_Hydro = if_else(mois > 10 | (mois == 10 & jour >= 15), annee + 1, annee),
      
      # CORRECTION PHYSIQUE : Pluie directe = 0 si l'étang est assec (géré au pas de temps journalier)
      Volume_Pluie_Directe = if_else(Statut_Simu == "Assec", 0, RR_num * SURFACE_eau * 10)
    )
  
  # Agrégation journalière à l'échelle de tout le réseau
  df_daily_global <- df_all_etangs %>%
    group_by(Saison_Hydro, dat) %>%
    summarise(
      RR_jour = first(RR_num),
      Volume_Ruissellement_Reseau = sum(Volume_R, na.rm = TRUE),
      Volume_Pluie_Directe_Reseau = sum(Volume_Pluie_Directe, na.rm = TRUE),
      Volume_Evap_Reseau = sum(abs(Evap_Reelle[Evap_Reelle < 0]), na.rm = TRUE),
      Volume_Transit_Reseau = sum(Vamont, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(df_exutoire %>% select(dat, Volume_Riviere), by = "dat")
  
  # Calcul des bilans de masse par Saison Hydro (en %)
  bilan_global <- df_daily_global %>%
    group_by(Saison_Hydro) %>%
    summarise(
      Pluie_Totale_mm = sum(RR_jour, na.rm = TRUE),
      Volume_Pluie_Total_BV_m3 = Pluie_Totale_mm * surface_totale_bv * 10,
      
      Volume_Exutoire_m3 = sum(Volume_Riviere, na.rm = TRUE),
      Volume_Evap_m3     = sum(Volume_Evap_Reseau, na.rm = TRUE),
      Volume_Captage_m3  = sum(Volume_Ruissellement_Reseau + Volume_Pluie_Directe_Reseau, na.rm = TRUE),
      Volume_Transit_m3  = sum(Volume_Transit_Reseau, na.rm = TRUE),
      
      # Coefficients Bruts Annuels (%)
      Coef_Ecoulement  = Volume_Exutoire_m3 / Volume_Pluie_Total_BV_m3,
      Coef_Evaporation = Volume_Evap_m3 / Volume_Pluie_Total_BV_m3,
      Coef_Captage     = Volume_Captage_m3 / Volume_Pluie_Total_BV_m3,
      Coef_Transit     = Volume_Transit_m3 / Volume_Pluie_Total_BV_m3,
      .groups = "drop"
    ) %>%
    filter(Saison_Hydro >= 2026 & Saison_Hydro <= 2070) %>%
    mutate(Scenario = nom_scenario, Modele_Meteo = nom_modele)
  
  return(bilan_global)
}

# ------------------------------------------------------------------------------
# 3. LE CRAWLER
# ------------------------------------------------------------------------------
cat("Extraction des fichiers de simulation...\n")
liste_df_resultats <- list()

for (dossier in dossiers_scenarios) {
  if (!dir.exists(dossier)) next
  fichiers_rds <- list.files(dossier, pattern = "\\.rds$", full.names = TRUE)
  nom_scenario_propre <- noms_propres_scenarios[basename(dossier)]
  
  for (fichier in fichiers_rds) {
    modele_extrait <- str_extract(basename(fichier), "(?<=Meteo_).*(?=_[0-9]{8}\\.rds)")
    res <- calculer_indicateurs_annuels(fichier, nom_scenario_propre, ifelse(is.na(modele_extrait), "Inconnu", modele_extrait))
    if (!is.null(res)) liste_df_resultats[[length(liste_df_resultats) + 1]] <- res
  }
}

df_master <- bind_rows(liste_df_resultats) %>%
  mutate(Modele_Meteo_Desc = case_when(
    str_detect(Modele_Meteo, "ALADIN63") ~ "CNRM-CM5 ALADIN63\n(Modéré)",
    str_detect(Modele_Meteo, "REMO2009") ~ "MPI-ESM REMO2009\n(Scénario intermédiaire)",
    str_detect(Modele_Meteo, "WRF381P")  ~ "IPSL-CM5A WRF381P\n(Hiver très pluvieux, Été humide)",
    str_detect(Modele_Meteo, "RCA4")     ~ "IPSL-CM5A RCA4\n(Hiver très humide, Été extrême)",
    str_detect(Modele_Meteo, "RegCM4-6") ~ "HadGEM2 RegCM4-6\n(Très chaud, sécheresse modérée)",
    str_detect(Modele_Meteo, "CCLM4-8-17") ~ "HadGEM2 CCLM4-8-17\n(Extrême : Le plus chaud/sec en été)",
    TRUE ~ Modele_Meteo
  )) %>%
  mutate(Modele_Meteo_Desc = factor(Modele_Meteo_Desc, levels = c(
    "CNRM-CM5 ALADIN63\n(Modéré)", "MPI-ESM REMO2009\n(Scénario intermédiaire)",
    "IPSL-CM5A WRF381P\n(Hiver très pluvieux, Été humide)", "IPSL-CM5A RCA4\n(Hiver très humide, Été extrême)",
    "HadGEM2 RegCM4-6\n(Très chaud, sécheresse modérée)", "HadGEM2 CCLM4-8-17\n(Extrême : Le plus chaud/sec en été)"
  )))

# Palettes de styles
couleurs_scenarios <- c("1. Aléatoire (Taille)" = "#e74c3c", "2. Aléatoire (Fixe)" = "#e67e22", "3. Aléatoire (Variable)" = "#f1c40f", "4. Opti (Vidange seule)" = "#3498db", "5. Opti (Synchronisation Totale)" = "#2ecc71")
linetypes_scenarios <- c("1. Aléatoire (Taille)" = "solid", "2. Aléatoire (Fixe)" = "dashed", "3. Aléatoire (Variable)" = "dotted", "4. Opti (Vidange seule)" = "solid", "5. Opti (Synchronisation Totale)" = "dashed")
couleurs_meteo <- c("CNRM-CM5 ALADIN63\n(Modéré)" = "#3498db", "MPI-ESM REMO2009\n(Scénario intermédiaire)" = "#f1c40f", "IPSL-CM5A WRF381P\n(Hiver très pluvieux, Été humide)" = "#2980b9", "IPSL-CM5A RCA4\n(Hiver très humide, Été extrême)" = "#9b59b6", "HadGEM2 RegCM4-6\n(Très chaud, sécheresse modérée)" = "#e67e22", "HadGEM2 CCLM4-8-17\n(Extrême : Le plus chaud/sec en été)" = "#e74c3c")

graphics.off()

# ==============================================================================
# 5. GÉNÉRATION DES GRAPHIQUES BRUTS - TYPE A (Vue par Climat, Y en %)
# ==============================================================================

# -- TYPE A1 : Coef d'Ecoulement Brût --
dev.new(width = 14, height = 8)
gA1 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Ecoulement, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A1 : Évolution du Coefficient d'Écoulement Annuel à l'Exutoire", subtitle = "Vue Météo : Comparaison de l'efficacité des gestions pour un climat donné (Données annuelles brutes)", x = "Saison", y = "Coefficient d'Écoulement (%)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA1)

# -- TYPE A2 : Coef d'Evaporation Brût --
dev.new(width = 14, height = 8)
gA2 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Evaporation, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A2 : Évolution du Coefficient d'Évaporation Annuel", subtitle = "Vue Météo : Impact des assecs sur la perte par évaporation (Données annuelles brutes)", x = "Saison", y = "Coefficient d'Évaporation (%)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA2)

# -- TYPE A3 : Coef de Captage Brût Corrigé --
dev.new(width = 14, height = 8)
gA3 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Captage, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A3 : Évolution du Taux de Captage Global Annuel", subtitle = "Vue Météo : Prise en compte de la disparition de la pluie directe lors des assecs (Données brutes)", x = "Saison", y = "Taux de Captage (%)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA3)

# -- TYPE A4 : Coef de Transit Interne Brût --
dev.new(width = 14, height = 8)
gA4 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Transit, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A4 : Évolution du Coefficient de Transit Interne Réseau", subtitle = "Vue Météo : Part de la pluie annuelle qui voyage d'un étang à l'autre sans s'arrêter", x = "Saison", y = "Coefficient de Transit Interne (%)") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA4)


# ==============================================================================
# 6. GÉNÉRATION DES GRAPHIQUES BRUTS - TYPE B (Vue par Gestion, Y en %)
# ==============================================================================

# -- TYPE B1 : Coef d'Ecoulement Brût (Vue Gestion) --
dev.new(width = 14, height = 8)
gB1 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Ecoulement, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B1 : Évolution du Coefficient d'Écoulement Global", subtitle = "Vue Gestion : Éventail de la perte annuelle face à l'incertitude climatique", x = "Saison", y = "Coefficient d'Écoulement (%)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB1)

# -- TYPE B2 : Coef d'Evaporation Brût (Vue Gestion) --
dev.new(width = 14, height = 8)
gB2 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Evaporation, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B2 : Évolution du Coefficient d'Évaporation", subtitle = "Vue Gestion : Dispersion annuelle des pertes thermiques selon la stratégie", x = "Saison", y = "Coefficient d'Évaporation (%)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB2)

# -- TYPE B3 : Coef de Captage Brût Corrigé (Vue Gestion) --
dev.new(width = 14, height = 8)
gB3 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Captage, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B3 : Évolution du Taux de Captage Global", subtitle = "Vue Gestion : Impact du climat sur l'efficacité annuelle de remplissage", x = "Saison", y = "Taux de Captage (%)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB3)

# -- TYPE B4 : Coef de Transit Interne Brût (Vue Gestion) --
dev.new(width = 14, height = 8)
gB4 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Transit, color = Modele_Meteo_Desc)) +
  geom_line(linewidth = 0.8, alpha = 0.8) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B4 : Évolution du Coefficient de Transit Interne", subtitle = "Vue Gestion : Analyse de la fluidité de l'eau inter-étangs selon le climat", x = "Saison", y = "Coefficient de Transit (%)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB4)






# ==============================================================================
# SCRIPT COMPLEMENTAIRE : ANALYSE DES FORÇAGES CLIMATIQUES DRIAS (2026-2070)
# Objectif : Caractériser l'évolution des températures et du bilan hydrique
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE ET EXTRACTION DES DONNÉES MÉTÉO SANS PERTE
# ------------------------------------------------------------------------------
liste_dossiers_meteo <- c(
  "data/meteo/MPI-ESM  REMO2009",
  "data/meteo/IPSL-CM5A  WRF381P",
  "data/meteo/IPSL-CM5A  RCA4",
  "data/meteo/HadGEM2  RegCM4-6",
  "data/meteo/HadGEM2  CCLM4-8-17",
  "data/meteo/CNRM-CM5  ALADIN63"
)

CODE_METEO_ACTUEL <- 2 

cat("Extraction et parsing des chroniques thermiques et hydriques DRIAS...\n")
liste_meteo <- list()

for (dossier in liste_dossiers_meteo) {
  if (!dir.exists(dossier)) next
  nom_modele <- basename(dossier)
  
  chemin_meteo <- paste0(dossier, "/Meteo.csv")
  chemin_centro <- paste0(dossier, "/centro_BV.csv")
  
  if (!file.exists(chemin_meteo) | !file.exists(chemin_centro)) next
  
  # Sélection de la maille topo-proche
  coordonnees <- read.csv(chemin_centro, header = TRUE, sep = ",") %>% filter(CODE == CODE_METEO_ACTUEL)
  meteo_brute <- read.csv2(chemin_meteo, stringsAsFactors = FALSE) 
  
  maille_proche <- meteo_brute %>% select(LAMBX, LAMBY) %>% distinct() %>%
    mutate(distance = sqrt((LAMBX - coordonnees$LAMBX[1])^2 + (LAMBY - coordonnees$LAMBY[1])^2)) %>%
    arrange(distance) %>% head(1)
  
  # Extraction complète incluant les bornes thermiques
  df_m <- meteo_brute %>%
    filter(LAMBX == maille_proche$LAMBX[1] & LAMBY == maille_proche$LAMBY[1]) %>%
    mutate(
      dat = as.Date(lubridate::parse_date_time(as.character(DATE), orders = c("ymd", "dmy", "Ymd", "Y-m-d"))),
      RR = as.numeric(gsub(",", ".", as.character(PRELIQ))),
      ETP = as.numeric(gsub(",", ".", as.character(ETP))),
      T_MOY = as.numeric(gsub(",", ".", as.character(Tmoy))),
      T_MAX = as.numeric(gsub(",", ".", as.character(Tmax))),
      T_MIN = as.numeric(gsub(",", ".", as.character(Tmin)))
    ) %>%
    filter(year(dat) >= 2026 & year(dat) <= 2070) %>%
    # CORRECTION : Conservation explicite des données thermiques minimales et maximales
    select(dat, RR, ETP, T_MOY, T_MAX, T_MIN) %>%
    mutate(
      Modele = nom_modele,
      Annee = year(dat),
      Mois = month(dat),
      P_ETP = RR - ETP
    )
  
  liste_meteo[[nom_modele]] <- df_m
}

df_climat <- bind_rows(liste_meteo)

# Standardisation des étiquettes des modèles climatiques
df_climat <- df_climat %>%
  mutate(Modele_Desc = case_when(
    str_detect(Modele, "ALADIN63") ~ "1. CNRM (Modéré)",
    str_detect(Modele, "REMO2009") ~ "2. MPI (Intermédiaire)",
    str_detect(Modele, "WRF381P")  ~ "3. IPSL WRF (Hiver Pluvieux)",
    str_detect(Modele, "RCA4")     ~ "4. IPSL RCA4 (Contrasté)",
    str_detect(Modele, "RegCM4-6") ~ "5. HadGEM2 (Chaud)",
    str_detect(Modele, "CCLM4-8-17") ~ "6. HadGEM2 (Extrême)",
    TRUE ~ Modele
  )) %>%
  mutate(Modele_Desc = factor(Modele_Desc, levels = c(
    "1. CNRM (Modéré)", "2. MPI (Intermédiaire)", "3. IPSL WRF (Hiver Pluvieux)",
    "4. IPSL RCA4 (Contrasté)", "5. HadGEM2 (Chaud)", "6. HadGEM2 (Extrême)"
  )))

couleurs_meteo <- c(
  "1. CNRM (Modéré)" = "#3498db", "2. MPI (Intermédiaire)" = "#f1c40f", 
  "3. IPSL WRF (Hiver Pluvieux)" = "#2980b9", "4. IPSL RCA4 (Contrasté)" = "#9b59b6", 
  "5. HadGEM2 (Chaud)" = "#e67e22", "6. HadGEM2 (Extrême)" = "#e74c3c"
)

graphics.off()

# ==============================================================================
# 2. PLOT MÉTÉO 1 : BILAN HYDRIQUE ANNUEL (P - ETP)
# ==============================================================================
df_bilan_annuel <- df_climat %>%
  group_by(Modele_Desc, Annee) %>%
  summarise(Bilan_P_ETP = sum(P_ETP, na.rm = TRUE), .groups = "drop")

dev.new(width = 14, height = 8)
g1 <- ggplot(df_bilan_annuel, aes(x = Annee, y = Bilan_P_ETP, fill = Bilan_P_ETP > 0)) +
  geom_col(alpha = 0.9, width = 0.8) +
  geom_smooth(method = "lm", color = "#2c3e50", linetype = "dashed", se = FALSE, linewidth = 1) + 
  facet_wrap(~ Modele_Desc, ncol = 3) +
  scale_fill_manual(values = c("TRUE" = "#3498db", "FALSE" = "#e74c3c"), labels = c("Déficit (Stress)", "Excédent (Recharge)")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "MÉTÉO 1 : Bilan Hydrique Climatique Annuel (Pluie - ETP)",
    subtitle = "Chronique brute interannuelle et tendance linéaire de fond (2026-2070)",
    x = "Saison", y = "Bilan P - ETP (mm)", fill = "Bilan Annuel"
  ) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#f8f9fa", color = "#e9ecef"), strip.text = element_text(face = "bold"))
print(g1)

# ==============================================================================
# 3. PLOT MÉTÉO 2 : DEFORMATION DU REGIME SAISONNIER
# ==============================================================================
df_saison <- df_climat %>%
  mutate(Periode = if_else(Annee <= 2045, "2026-2045 (Horizon Proche)", "2050-2070 (Horizon Lointain)")) %>%
  group_by(Periode, Modele_Desc, Mois) %>%
  summarise(RR_Moyen = sum(RR)/n_distinct(Annee), ETP_Moyen = sum(ETP)/n_distinct(Annee), .groups = "drop")

dev.new(width = 15, height = 8)
g2 <- ggplot(df_saison, aes(x = Mois)) +
  geom_line(aes(y = RR_Moyen, color = "Pluie (RR)", linetype = Periode), linewidth = 1.1) +
  geom_line(aes(y = ETP_Moyen, color = "Évaporation (ETP)", linetype = Periode), linewidth = 1.1) +
  facet_wrap(~ Modele_Desc, ncol = 3) +
  scale_color_manual(values = c("Pluie (RR)" = "#2980b9", "Évaporation (ETP)" = "#d35400")) +
  scale_x_continuous(breaks = 1:12, labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "MÉTÉO 2 : Déformation Inter-Horizon du Régime Saisonnier",
    subtitle = "Analyse comparative des moyennes mensuelles : glissement des croisements critiques P/ETP",
    x = "Mois de l'année", y = "Lame d'eau moyenne (mm)", color = "Composante", linetype = "Horizon temporel"
  ) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#f8f9fa", color = "#e9ecef"), strip.text = element_text(face = "bold"))
print(g2)

# ==============================================================================
# 4. PLOT MÉTÉO 3 : INTENSIFICATION DU STRESS ESTIVAL (JJA)
# ==============================================================================
df_ete <- df_climat %>%
  filter(Mois %in% c(6, 7, 8)) %>% 
  group_by(Modele_Desc, Annee) %>%
  summarise(Deficit_Ete = sum(P_ETP, na.rm = TRUE), .groups = "drop")

dev.new(width = 14, height = 8)
g3 <- ggplot(df_ete, aes(x = Annee, y = Deficit_Ete, color = Modele_Desc)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.6) +
  facet_wrap(~ Modele_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  theme_minimal(base_size = 14) +
  labs(
    title = "MÉTÉO 3 : Chronique Brute du Déficit Hydrique Estival (Juin-Août)",
    subtitle = "Évolution interannuelle sans lissage de l'intensité de l'assèchement climatique estival",
    x = "Saison", y = "Déficit Hydrique Cumulé P - ETP (mm)"
  ) +
  theme(legend.position = "none", strip.background = element_rect(fill = "#f8f9fa", color = "#e9ecef"), strip.text = element_text(face = "bold"))
print(g3)

# ==============================================================================
# 5. PLOT MÉTÉO 4 : DÉRIVE THERMIQUE ANNUELLE BRUTE
# ==============================================================================
df_temp_annuelle <- df_climat %>%
  group_by(Modele_Desc, Annee) %>%
  summarise(Temp_Moyenne = mean(T_MOY, na.rm = TRUE), .groups = "drop")

dev.new(width = 14, height = 8)
g4 <- ggplot(df_temp_annuelle, aes(x = Annee, y = Temp_Moyenne, color = Modele_Desc)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, linetype = "dashed", color = "#2c3e50") +
  facet_wrap(~ Modele_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  theme_minimal(base_size = 14) +
  labs(
    title = "MÉTÉO 4 : Évolution de la Température Moyenne Annuelle",
    subtitle = "Chroniques brutes et tendances de fond du réchauffement atmosphérique (2026-2070)",
    x = "Saison", y = "Température Moyenne (°C)"
  ) +
  theme(legend.position = "none", strip.background = element_rect(fill = "#fdedec", color = "#fadbd8"), strip.text = element_text(face = "bold", color = "#78281f"))
print(g4)

# ==============================================================================
# 6. PLOT MÉTÉO 5 : ALERTE ÉCOLOGIQUE BRUTE (JOURS TMAX > 30°C)
# ==============================================================================
df_canicule <- df_climat %>%
  group_by(Modele_Desc, Annee) %>%
  summarise(Jours_Canicule = sum(T_MAX >= 30, na.rm = TRUE), .groups = "drop")

dev.new(width = 14, height = 8)
g5 <- ggplot(df_canicule, aes(x = Annee, y = Jours_Canicule, color = Modele_Desc)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.7) +
  facet_wrap(~ Modele_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  theme_minimal(base_size = 14) +
  labs(
    title = "MÉTÉO 5 : Fréquence Annuelle des Seuils de Stress Thermique Écologique (Tmax ≥ 30°C)",
    subtitle = "Évolution brute du nombre de jours menaçant l'équilibre physico-chimique des masses d'eau",
    x = "Saison", y = "Nombre de jours par an"
  ) +
  theme(legend.position = "none", strip.background = element_rect(fill = "#fdedec", color = "#fadbd8"), strip.text = element_text(face = "bold", color = "#78281f"))
print(g5)

# ==============================================================================
# 7. PLOT MÉTÉO 6 : DISTRIBUTION DÉCENNALE DES EXTRÊMES ESTIVAUX
# ==============================================================================
df_decennie_ete <- df_climat %>%
  filter(Mois %in% c(6, 7, 8)) %>%  # CORRECTION ICI : %in% au lieu de %in=
  mutate(Decennie = case_when(
    Annee < 2030 ~ "2026-2029",
    Annee >= 2030 & Annee < 2040 ~ "2030-2039",
    Annee >= 2040 & Annee < 2050 ~ "2040-2049",
    Annee >= 2050 & Annee < 2060 ~ "2050-2059",
    Annee >= 2060 ~ "2060-2070"
  )) %>%
  mutate(Decennie = factor(Decennie, levels = c("2026-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2070")))

dev.new(width = 15, height = 8)
g6 <- ggplot(df_decennie_ete, aes(x = Decennie, y = T_MAX, fill = Decennie)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8, outlier.alpha = 0.4, width = 0.6) +
  facet_wrap(~ Modele_Desc, ncol = 3) +
  scale_fill_brewer(palette = "YlOrRd") + 
  theme_minimal(base_size = 13) +
  labs(
    title = "MÉTÉO 6 : Glissement Décennal des Distributions de Chaleur Estivale",
    subtitle = "Variabilité et étalement des températures maximales quotidiennes en été (Juin-Août)",
    x = "Période d'analyse", y = "Température Maximale Quotidienne (°C)"
  ) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.background = element_rect(fill = "#fdedec", color = "#fadbd8"), 
    strip.text = element_text(face = "bold", color = "#78281f")
  )
print(g6)






















# ==============================================================================
# 8. PLOT MÉTÉO 7 : DIAGRAMMES OMBROTHERMIQUES (Bagnouls et Gaussen)
# ==============================================================================

# 1. On agresse d'abord au mois réel pour chaque année (Somme de pluie, Moyenne de T)
df_mensuel <- df_climat %>%
  mutate(Periode = if_else(Annee <= 2045, "1. Horizon Proche (2026-2045)", "2. Horizon Lointain (2050-2070)")) %>%
  group_by(Periode, Modele_Desc, Annee, Mois) %>%
  summarise(
    RR_Mensuel = sum(RR, na.rm = TRUE),
    T_Mensuel = mean(T_MOY, na.rm = TRUE),
    .groups = "drop"
  )

# 2. On fait la moyenne de ces mois sur toute la période
df_ombro <- df_mensuel %>%
  group_by(Periode, Modele_Desc, Mois) %>%
  summarise(
    RR_Moyen = mean(RR_Mensuel, na.rm = TRUE),
    T_Moyen = mean(T_Mensuel, na.rm = TRUE),
    .groups = "drop"
  )

coeff_ombro <- 2 # Règle d'or de Bagnouls-Gaussen : P = 2T

dev.new(width = 16, height = 9)
g7 <- ggplot(df_ombro, aes(x = Mois)) +
  # Remplissage Bleu : Zone Humide (Pluie > 2x Température)
  geom_ribbon(aes(ymin = T_Moyen * coeff_ombro, ymax = pmax(RR_Moyen, T_Moyen * coeff_ombro)), fill = "#3498db", alpha = 0.3) +
  # Remplissage Rouge : Zone de Sécheresse (Pluie < 2x Température)
  geom_ribbon(aes(ymin = pmin(RR_Moyen, T_Moyen * coeff_ombro), ymax = T_Moyen * coeff_ombro), fill = "#e74c3c", alpha = 0.4) +
  
  # Courbes principales
  geom_line(aes(y = RR_Moyen, color = "Pluie (P)"), linewidth = 1.2) +
  geom_line(aes(y = T_Moyen * coeff_ombro, color = "Température (T)"), linewidth = 1.2) +
  
  # Grille croisée : Modèle vs Période
  facet_grid(Periode ~ Modele_Desc) +
  
  scale_color_manual(values = c("Pluie (P)" = "#2980b9", "Température (T)" = "#c0392b")) +
  scale_x_continuous(breaks = 1:12, labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  
  # Double Axe Y mathématique
  scale_y_continuous(
    name = "Précipitations (mm)",
    sec.axis = sec_axis(~ . / coeff_ombro, name = "Température (°C)")
  ) +
  
  theme_minimal(base_size = 13) +
  labs(
    title = "MÉTÉO 7 : Diagrammes Ombrothermiques (Indice d'Aridité)",
    subtitle = "La zone rouge illustre les mois de déficit hydrique (P < 2T). Observez son élargissement à l'horizon 2070.",
    x = "Mois de l'année", color = "Données :"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#ecf0f1", color = "#bdc3c7"),
    strip.text = element_text(face = "bold"),
    axis.title.y.right = element_text(color = "#c0392b", face = "bold"),
    axis.title.y.left = element_text(color = "#2980b9", face = "bold")
  )

print(g7)

