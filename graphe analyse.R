# ==============================================================================
# ANALYSE DE VULNÉRABILITÉ 2026-2070 : INDICATEURS GLOBAUX DU RÉSEAU
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE DU SCRIPT
# ------------------------------------------------------------------------------
# Liste de tes répertoires
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
# 2. MOTEUR DE CALCUL DES 3 INDICATEURS GLOBAUX
# ------------------------------------------------------------------------------
calculer_indicateurs_annuels <- function(chemin_rds, nom_scenario, nom_modele) {
  
  simu <- readRDS(chemin_rds)
  df_exutoire <- simu$exutoire_data
  if(length(simu$liste_finale) == 0) return(NULL)
  
  # 1. Extraction dynamique des surfaces de tous les étangs
  surface_totale_bv <- sum(sapply(simu$liste_finale, function(x) x$Surface_BV[1]), na.rm = TRUE)
  surface_eau_totale <- sum(sapply(simu$liste_finale, function(x) x$SURFACE_eau[1]), na.rm = TRUE)
  
  # 2. Fusion de TOUS les étangs pour les calculs globaux
  df_all_etangs <- bind_rows(simu$liste_finale, .id = "NOM_ETANG") %>%
    mutate(
      annee = year(dat), mois = month(dat), jour = day(dat),
      Saison_Hydro = if_else(mois > 10 | (mois == 10 & jour >= 15), annee + 1, annee)
    )
  
  # 3. Agrégation journalière
  df_daily_global <- df_all_etangs %>%
    group_by(Saison_Hydro, dat) %>%
    summarise(
      RR_jour = first(RR),
      Volume_Ruissellement_Tous_Etangs = sum(Volume_R, na.rm = TRUE),
      Volume_Evap_Tous_Etangs = sum(abs(Evap_Reelle[Evap_Reelle < 0]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(df_exutoire %>% select(dat, Volume_Riviere), by = "dat")
  
  # 4. Calcul par Saison (Bilans de Masse Globaux en %)
  bilan_global <- df_daily_global %>%
    group_by(Saison_Hydro) %>%
    summarise(
      Pluie_Totale_mm = sum(RR_jour, na.rm = TRUE),
      Volume_Pluie_Total_BV_m3 = Pluie_Totale_mm * surface_totale_bv * 10,
      
      # INDICATEUR 1 : Coef d'Écoulement Global
      Volume_Exutoire_m3 = sum(Volume_Riviere, na.rm = TRUE),
      Coef_Ecoulement = Volume_Exutoire_m3 / Volume_Pluie_Total_BV_m3,
      
      # INDICATEUR 2 : Coef d'Évaporation Global
      Volume_Evap_m3 = sum(Volume_Evap_Tous_Etangs, na.rm = TRUE),
      Coef_Evaporation = Volume_Evap_m3 / Volume_Pluie_Total_BV_m3,
      
      # INDICATEUR 3 : Taux de Captage Global
      Volume_Ruiss_m3 = sum(Volume_Ruissellement_Tous_Etangs, na.rm = TRUE),
      Volume_Pluie_Directe_m3 = Pluie_Totale_mm * surface_eau_totale * 10,
      Coef_Captage = (Volume_Ruiss_m3 + Volume_Pluie_Directe_m3) / Volume_Pluie_Total_BV_m3,
      
      .groups = "drop"
    ) %>%
    filter(Saison_Hydro >= 2026 & Saison_Hydro <= 2070) %>%
    mutate(Scenario = nom_scenario, Modele_Meteo = nom_modele)
  
  return(bilan_global)
}

# ------------------------------------------------------------------------------
# 3. LE CRAWLER (Recherche de fichiers et Compilation)
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

cat("Compilation terminée ! Nombre de saisons analysées :", nrow(df_master), "\n")


# ------------------------------------------------------------------------------
# 4. PALETTES DE COULEURS ET THÈMES
# ------------------------------------------------------------------------------
couleurs_scenarios <- c(
  "1. Aléatoire (Taille)" = "#e74c3c", "2. Aléatoire (Fixe)" = "#e67e22",
  "3. Aléatoire (Variable)" = "#f1c40f", "4. Opti (Vidange seule)" = "#3498db",
  "5. Opti (Synchronisation Totale)" = "#2ecc71"
)

linetypes_scenarios <- c(
  "1. Aléatoire (Taille)" = "solid", "2. Aléatoire (Fixe)" = "dashed",
  "3. Aléatoire (Variable)" = "dotted", "4. Opti (Vidange seule)" = "solid",
  "5. Opti (Synchronisation Totale)" = "dashed"
)

couleurs_meteo <- c(
  "CNRM-CM5 ALADIN63\n(Modéré)" = "#3498db", "MPI-ESM REMO2009\n(Scénario intermédiaire)" = "#f1c40f",
  "IPSL-CM5A WRF381P\n(Hiver très pluvieux, Été humide)" = "#2980b9", "IPSL-CM5A RCA4\n(Hiver très humide, Été extrême)" = "#9b59b6",
  "HadGEM2 RegCM4-6\n(Très chaud, sécheresse modérée)" = "#e67e22", "HadGEM2 CCLM4-8-17\n(Extrême : Le plus chaud/sec en été)" = "#e74c3c"
)


# ==============================================================================
# 5. GÉNÉRATION DES GRAPHIQUES - TYPE A (Vue par Climat)
# ==============================================================================

# -- TYPE A1 : Coef d'Ecoulement --
dev.new(width = 14, height = 8)
gA1 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Ecoulement, color = Scenario, linetype = Scenario)) +
  geom_point(alpha = 0.7, size = 1.5)  + # LIGNE MODIFIÉE ICI
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A1 : Évolution du Coefficient d'Écoulement Global à l'Exutoire",
       subtitle = "Vue Météo : Comparaison de l'efficacité des gestions pour un climat donné (Données brutes)",
       x = "Saison", y = "Coefficient d'Écoulement (%)", color = "Gestion", linetype = "Gestion") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA1)

# -- TYPE A2 : Coef d'Evaporation --
dev.new(width = 14, height = 8)
gA2 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Evaporation, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9) + # LIGNE MODIFIÉE ICI
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A2 : Évolution du Coefficient d'Évaporation",
       subtitle = "Vue Météo : Impact des assecs sur l'évaporation du réseau (Données brutes)",
       x = "Saison", y = "Coefficient d'Évaporation (%)", color = "Gestion", linetype = "Gestion") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA2)

# -- TYPE A3 : Coef de Captage --
dev.new(width = 14, height = 8)
gA3 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Captage, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9) + # LIGNE MODIFIÉE ICI
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A3 : Évolution du Taux de Captage Global",
       subtitle = "Vue Météo : Part de la pluie de tout le bassin captée par le réseau d'étangs (Données brutes)",
       x = "Saison", y = "Taux de Captage (%)", color = "Gestion", linetype = "Gestion") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA3)


# ==============================================================================
# 6. GÉNÉRATION DES GRAPHIQUES - TYPE B (Vue par Gestion)
# ==============================================================================

# -- TYPE B1 : Coef d'Ecoulement --
dev.new(width = 14, height = 8)
gB1 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Ecoulement, color = Modele_Meteo_Desc)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1.2) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B1 : Évolution du Coefficient d'Écoulement Global",
       subtitle = "Vue Gestion : Éventail de l'incertitude climatique selon la stratégie choisie",
       x = "Saison", y = "Coefficient d'Écoulement (%)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB1)

# -- TYPE B2 : Coef d'Evaporation --
dev.new(width = 14, height = 8)
gB2 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Evaporation, color = Modele_Meteo_Desc)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1.2) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B2 : Évolution du Coefficient d'Évaporation",
       subtitle = "Vue Gestion : Vulnérabilité de l'évaporation face aux 6 futurs climatiques",
       x = "Saison", y = "Coefficient d'Évaporation (%)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB2)

# -- TYPE B3 : Coef de Captage --
dev.new(width = 14, height = 8)
gB3 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Captage, color = Modele_Meteo_Desc)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1.2) + 
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = couleurs_meteo) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE B3 : Évolution du Taux de Captage Global",
       subtitle = "Vue Gestion : Dégradation de la ressource en eau selon la violence du climat",
       x = "Saison", y = "Taux de Captage (%)", color = "Modèle Météo") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#fcf3cf", color = "#f1c40f"), strip.text = element_text(face = "bold"))
print(gB3)





# ==============================================================================
# ANALYSE DE VULNÉRABILITÉ : CLASSIFICATION AUTOMATIQUE & REMPLISSAGE AU 15/02
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE DES REPERTOIRES
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
# 2. AUTO-CLASSIFICATION TOPOLOGIQUE DES ÉTANGS
# ------------------------------------------------------------------------------
# On inspecte le premier fichier disponible pour cartographier le réseau
premier_dossier <- dossiers_scenarios[dir.exists(dossiers_scenarios)][1]
premier_fichier <- list.files(premier_dossier, pattern = "\\.rds$", full.names = TRUE)[1]
simu_topo <- readRDS(premier_fichier)

tous_les_etangs <- names(simu_topo$liste_finale)

# Extraction de la destination aval de chaque étang
# /!\ VERIFIE BIEN LE NOM DE TA COLONNE DANS TES DATAFRAMES (Exutoire, exutoire, ou Etang_Aval...)
destination_aval <- sapply(simu_topo$liste_finale, function(x) x$Exutoire_1[1])

# Règle 1 : Les têtes de bassin (aucun étang ne se déverse dedans)
etangs_tete <- tous_les_etangs[!(tous_les_etangs %in% destination_aval)]

# Règle 2 : Les exutoires terminaux (se déversent en dehors du réseau, ex: vers la rivière)
etangs_exutoire <- tous_les_etangs[!(destination_aval %in% tous_les_etangs)]

# Règle 3 : Le milieu de chaîne (ceux qui restent)
etangs_milieu <- setdiff(tous_les_etangs, c(etangs_tete, etangs_exutoire))

# Affichage console pour vérification de l'algorithme
cat("--- RÉSEAU HYDRAULIQUE DÉTECTÉ AUTOMATIQUEMENT ---\n")
cat("Étangs de Tête :", paste(etangs_tete, collapse = ", "), "\n")
cat("Étangs de Milieu :", paste(etangs_milieu, collapse = ", "), "\n")
cat("Étangs Exutoires :", paste(etangs_exutoire, collapse = ", "), "\n--------------------------------------------------\n")

# Fonction d'attribution de la catégorie
get_categorie <- function(nom) {
  if (nom %in% etangs_tete) return("1. Étang de Tête")
  if (nom %in% etangs_milieu) return("2. Étang de Milieu")
  if (nom %in% etangs_exutoire) return("3. Étang Exutoire")
  return("Autre")
}

# ------------------------------------------------------------------------------
# 3. FONCTION DE CALCUL DU REMPLISSAGE AU 15 FÉVRIER
# ------------------------------------------------------------------------------
calculer_remplissage_15fev <- function(chemin_rds, nom_scenario, nom_modele) {
  simu <- readRDS(chemin_rds)
  if(is.null(simu$liste_finale)) return(NULL)
  
  liste_stats <- lapply(names(simu$liste_finale), function(nom_etang) {
    df <- simu$liste_finale[[nom_etang]]
    vmax <- df$Vmax[1]
    
    df %>%
      mutate(mois = month(dat), jour = day(dat), annee = year(dat)) %>%
      filter(mois == 2, jour == 20) %>%
      mutate(
        Taux_Remplissage = (BF / vmax) * 100,
        Categorie = get_categorie(nom_etang),
        Scenario = nom_scenario,
        Modele_Meteo = nom_modele
      ) %>%
      select(annee, Taux_Remplissage, Categorie, Scenario, Modele_Meteo)
  })
  return(bind_rows(liste_stats))
}

# ------------------------------------------------------------------------------
# 4. CRAWLER (PARCOURS DES DOSSIERS)
# ------------------------------------------------------------------------------
cat("Extraction des chroniques de remplissage...\n")
liste_dfs <- list()

for (dossier in dossiers_scenarios) {
  if (!dir.exists(dossier)) next
  
  fichiers <- list.files(dossier, pattern = "\\.rds$", full.names = TRUE)
  if(length(fichiers) == 0) next
  
  nom_scenario <- noms_propres_scenarios[basename(dossier)]
  
  for (f in fichiers) {
    nom_modele <- str_extract(basename(f), "(?<=Meteo_).*(?=_[0-9]{8}\\.rds)")
    if (is.na(nom_modele)) nom_modele <- "Inconnu"
    
    res <- calculer_remplissage_15fev(f, nom_scenario, nom_modele)
    if (!is.null(res)) liste_dfs[[length(liste_dfs) + 1]] <- res
  }
}

df_final_remplissage <- bind_rows(liste_dfs) %>%
  filter(!is.na(Taux_Remplissage))

# Renommage propre des modèles météo pour la légende
df_final_remplissage <- df_final_remplissage %>%
  mutate(Modele_Meteo_Desc = case_when(
    str_detect(Modele_Meteo, "ALADIN63") ~ "CNRM-CM5 (Modéré)",
    str_detect(Modele_Meteo, "REMO2009") ~ "MPI-ESM (Intermédiaire)",
    str_detect(Modele_Meteo, "WRF381P")  ~ "IPSL-CM5A (Humide)",
    str_detect(Modele_Meteo, "RCA4")     ~ "IPSL-CM5A (Hiver humide/Été extrême)",
    str_detect(Modele_Meteo, "RegCM4-6") ~ "HadGEM2 (Chaud/Sec modéré)",
    str_detect(Modele_Meteo, "CCLM4-8-17") ~ "HadGEM2 (Extrême sec)",
    TRUE ~ Modele_Meteo
  )) %>%
  mutate(Modele_Meteo_Desc = factor(Modele_Meteo_Desc, levels = c(
    "CNRM-CM5 (Modéré)", "MPI-ESM (Intermédiaire)", "IPSL-CM5A (Humide)",
    "IPSL-CM5A (Hiver humide/Été extrême)", "HadGEM2 (Chaud/Sec modéré)", "HadGEM2 (Extrême sec)"
  )))

# ------------------------------------------------------------------------------
# 5. GÉNÉRATION DU GRAPHIQUE IRREPROCHABLE
# ------------------------------------------------------------------------------
graphics.off() 
dev.new(width = 15, height = 8)

g_boxplot <- ggplot(df_final_remplissage, aes(x = Scenario, y = Taux_Remplissage, fill = Modele_Meteo_Desc)) +
  geom_boxplot(
    alpha = 0.9, 
    outlier.shape = NA, # Supprime les points noirs parasites
    linewidth = 0.3,    # Affine les bordures pour laisser voir la couleur
    width = 0.7, 
    position = position_dodge(0.8)
  ) +
  facet_wrap(~ Categorie, ncol = 3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  coord_cartesian(ylim = c(0, 100)) + # Verrouille proprement l'axe de 0 à 100%
  theme_minimal(base_size = 13) +
  labs(
    title = "Niveau de Remplissage des Étangs au 15 Février (Période 2026-2070)",
    subtitle = "Analyse systémique par position topologique et sensibilité aux modèles climatiques DRIAS",
    x = "Stratégie de Gestion Testée",
    y = "Taux de Remplissage Stocké (%)",
    fill = "Modèle de Changement Climatique"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.background = element_rect(fill = "#2c3e50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

print(g_boxplot)



# ==============================================================================
# ANALYSE DE VULNÉRABILITÉ : RESTE À VIVRE ESTIVAL (REMPLISSAGE AU 1ER SEPT)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE DES REPERTOIRES
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
# 2. AUTO-CLASSIFICATION TOPOLOGIQUE DES ÉTANGS
# ------------------------------------------------------------------------------
premier_dossier <- dossiers_scenarios[dir.exists(dossiers_scenarios)][1]
premier_fichier <- list.files(premier_dossier, pattern = "\\.rds$", full.names = TRUE)[1]
simu_topo <- readRDS(premier_fichier)

tous_les_etangs <- names(simu_topo$liste_finale)

destination_aval <- sapply(simu_topo$liste_finale, function(x) x$Exutoire_1[1])

etangs_tete <- tous_les_etangs[!(tous_les_etangs %in% destination_aval)]
etangs_exutoire <- tous_les_etangs[!(destination_aval %in% tous_les_etangs)]
etangs_milieu <- setdiff(tous_les_etangs, c(etangs_tete, etangs_exutoire))

cat("--- RÉSEAU HYDRAULIQUE DÉTECTÉ AUTOMATIQUEMENT ---\n")
cat("Étangs de Tête :", paste(etangs_tete, collapse = ", "), "\n")
cat("Étangs de Milieu :", paste(etangs_milieu, collapse = ", "), "\n")
cat("Étangs Exutoires :", paste(etangs_exutoire, collapse = ", "), "\n--------------------------------------------------\n")

get_categorie <- function(nom) {
  if (nom %in% etangs_tete) return("1. Étang de Tête")
  if (nom %in% etangs_milieu) return("2. Étang de Milieu")
  if (nom %in% etangs_exutoire) return("3. Étang Exutoire")
  return("Autre")
}

# ------------------------------------------------------------------------------
# 3. FONCTION DE CALCUL DU REMPLISSAGE AU 1ER SEPTEMBRE
# ------------------------------------------------------------------------------
calculer_remplissage_1sept <- function(chemin_rds, nom_scenario, nom_modele) {
  simu <- readRDS(chemin_rds)
  if(is.null(simu$liste_finale)) return(NULL)
  
  liste_stats <- lapply(names(simu$liste_finale), function(nom_etang) {
    df <- simu$liste_finale[[nom_etang]]
    vmax <- df$Vmax[1]
    
    df %>%
      mutate(mois = month(dat), jour = day(dat), annee = year(dat)) %>%
      filter(mois == 9, jour == 1) %>%
      mutate(
        Taux_Remplissage = (BF / vmax) * 100,
        Categorie = get_categorie(nom_etang),
        Scenario = nom_scenario,
        Modele_Meteo = nom_modele
      ) %>%
      select(annee, Taux_Remplissage, Categorie, Scenario, Modele_Meteo)
  })
  return(bind_rows(liste_stats))
}

# ------------------------------------------------------------------------------
# 4. CRAWLER (PARCOURS DES DOSSIERS)
# ------------------------------------------------------------------------------
cat("Extraction des chroniques de survie estivale...\n")
liste_dfs <- list()

for (dossier in dossiers_scenarios) {
  if (!dir.exists(dossier)) next
  
  fichiers <- list.files(dossier, pattern = "\\.rds$", full.names = TRUE)
  if(length(fichiers) == 0) next
  
  nom_scenario <- noms_propres_scenarios[basename(dossier)]
  
  for (f in fichiers) {
    nom_modele <- str_extract(basename(f), "(?<=Meteo_).*(?=_[0-9]{8}\\.rds)")
    if (is.na(nom_modele)) nom_modele <- "Inconnu"
    
    res <- calculer_remplissage_1sept(f, nom_scenario, nom_modele)
    if (!is.null(res)) liste_dfs[[length(liste_dfs) + 1]] <- res
  }
}

df_final_remplissage <- bind_rows(liste_dfs) %>%
  filter(!is.na(Taux_Remplissage))

df_final_remplissage <- df_final_remplissage %>%
  mutate(Modele_Meteo_Desc = case_when(
    str_detect(Modele_Meteo, "ALADIN63") ~ "CNRM-CM5 (Modéré)",
    str_detect(Modele_Meteo, "REMO2009") ~ "MPI-ESM (Intermédiaire)",
    str_detect(Modele_Meteo, "WRF381P")  ~ "IPSL-CM5A (Humide)",
    str_detect(Modele_Meteo, "RCA4")     ~ "IPSL-CM5A (Hiver humide/Été extrême)",
    str_detect(Modele_Meteo, "RegCM4-6") ~ "HadGEM2 (Chaud/Sec modéré)",
    str_detect(Modele_Meteo, "CCLM4-8-17") ~ "HadGEM2 (Extrême sec)",
    TRUE ~ Modele_Meteo
  )) %>%
  mutate(Modele_Meteo_Desc = factor(Modele_Meteo_Desc, levels = c(
    "CNRM-CM5 (Modéré)", "MPI-ESM (Intermédiaire)", "IPSL-CM5A (Humide)",
    "IPSL-CM5A (Hiver humide/Été extrême)", "HadGEM2 (Chaud/Sec modéré)", "HadGEM2 (Extrême sec)"
  )))

# ------------------------------------------------------------------------------
# 5. GÉNÉRATION DU GRAPHIQUE
# ------------------------------------------------------------------------------
graphics.off() 
dev.new(width = 15, height = 8)

g_boxplot <- ggplot(df_final_remplissage, aes(x = Scenario, y = Taux_Remplissage, fill = Modele_Meteo_Desc)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.3, width = 0.7, position = position_dodge(0.8)) +
  facet_wrap(~ Categorie, ncol = 3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_minimal(base_size = 13) +
  labs(
    title = "Reste à Vivre Estival : Niveau des Étangs au 1er Septembre (2026-2070)",
    subtitle = "Capacité de résilience à l'étiage par position topologique et sensibilité climatique",
    x = "Stratégie de Gestion Testée",
    y = "Taux de Remplissage Restant (%)",
    fill = "Modèle de Changement Climatique"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.background = element_rect(fill = "#2c3e50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

print(g_boxplot)









# ==============================================================================
# OPTION ACCESSIBILITÉ : TABLEAU DE SYNTHÈSE DES GAINS PAR HORIZON TEMPOREL
# ==============================================================================

df_synthese_horizons <- df_master %>%
  mutate(Horizon = case_when(
    Saison_Hydro <= 2040 ~ "2030-2040 (Proche)",
    Saison_Hydro > 2040 & Saison_Hydro <= 2055 ~ "2041-2055 (Moyen)",
    Saison_Hydro > 2055 ~ "2056-2070 (Lointain)"
  )) %>%
  group_by(Horizon, Modele_Meteo_Desc, Scenario) %>%
  summarise(
    Moyenne_Ecoulement = mean(Coef_Ecoulement, na.rm = TRUE),
    Moyenne_Evaporation = mean(Coef_Evaporation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Horizon, Modele_Meteo_Desc, Scenario)

# Affichage des premières lignes de performance dans la console
print(head(df_synthese_horizons, 15))











# ==============================================================================
# CONTEXTE CLIMATIQUE 2026-2070 : ÉVOLUTION DU BILAN HYDRIQUE (P - ETP)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr)

# 1. PARAMÉTRAGE (On n'a besoin que d'un seul dossier pour lire la météo)
dossier_meteo <- "simulation futur/Chalamont_aleatoire/Grand_petit"
fichiers_meteo <- list.files(dossier_meteo, pattern = "\\.rds$", full.names = TRUE)

cat("Extraction des données météorologiques DRIAS...\n")
liste_meteo <- list()

# 2. EXTRACTION DE LA MÉTÉO
for (f in fichiers_meteo) {
  simu <- readRDS(f)
  nom_modele <- str_extract(basename(f), "(?<=Meteo_).*(?=_[0-9]{8}\\.rds)")
  if (is.na(nom_modele)) nom_modele <- "Inconnu"
  
  # La météo étant globale, on extrait les données du premier étang de la liste
  df_meteo_brut <- simu$liste_finale[[1]] 
  
  bilan_annuel <- df_meteo_brut %>%
    mutate(
      annee = year(dat), mois = month(dat), jour = day(dat),
      Saison_Hydro = if_else(mois > 10 | (mois == 10 & jour >= 15), annee + 1, annee)
    ) %>%
    group_by(Saison_Hydro) %>%
    summarise(
      Pluie_mm = sum(RR, na.rm = TRUE),
      ETP_mm = sum(ETP_grille, na.rm = TRUE), # Vérifie que ta colonne s'appelle bien ETP
      Bilan_Climatique = Pluie_mm - ETP_mm,
      .groups = "drop"
    ) %>%
    filter(Saison_Hydro >= 2026 & Saison_Hydro <= 2070) %>%
    mutate(Modele_Meteo = nom_modele)
  
  liste_meteo[[length(liste_meteo) + 1]] <- bilan_annuel
}

df_meteo_master <- bind_rows(liste_meteo)

# 3. MISE AU PROPRE DES LÉGENDES
df_meteo_master <- df_meteo_master %>%
  mutate(Modele_Meteo_Desc = case_when(
    str_detect(Modele_Meteo, "ALADIN63") ~ "CNRM-CM5 (Modéré)",
    str_detect(Modele_Meteo, "REMO2009") ~ "MPI-ESM (Intermédiaire)",
    str_detect(Modele_Meteo, "WRF381P")  ~ "IPSL-CM5A (Humide)",
    str_detect(Modele_Meteo, "RCA4")     ~ "IPSL-CM5A (Hiver humide/Été extrême)",
    str_detect(Modele_Meteo, "RegCM4-6") ~ "HadGEM2 (Chaud/Sec modéré)",
    str_detect(Modele_Meteo, "CCLM4-8-17") ~ "HadGEM2 (Extrême sec)",
    TRUE ~ Modele_Meteo
  )) %>%
  mutate(Modele_Meteo_Desc = factor(Modele_Meteo_Desc, levels = c(
    "CNRM-CM5 (Modéré)", "MPI-ESM (Intermédiaire)", "IPSL-CM5A (Humide)",
    "IPSL-CM5A (Hiver humide/Été extrême)", "HadGEM2 (Chaud/Sec modéré)", "HadGEM2 (Extrême sec)"
  )))

# 4. GÉNÉRATION DU GRAPHIQUE DU BILAN CLIMATIQUE
dev.new(width = 14, height = 8)

g_meteo <- ggplot(df_meteo_master, aes(x = Saison_Hydro, y = Bilan_Climatique, color = Modele_Meteo_Desc)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) + # La ligne d'équilibre
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, linewidth = 1.5) +
  geom_point(alpha = 0.3, size = 1.5) + # Affiche la dispersion annuelle en fond
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Évolution du Bilan Hydrique Climatique Annuel (Pluie - ETP)",
    subtitle = "Forçage météorologique brut de la Dombes (2026-2070). Une valeur sous 0 indique un déficit atmosphérique annuel.",
    x = "Saison Hydrologique",
    y = "Bilan P - ETP (mm)",
    color = "Modèle Climatique DRIAS"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(g_meteo)







# ------------------------------------------------------------------------------
# PRÉPARATION DES DONNÉES : MOYENNE GLOBALE AU 15 FÉVRIER
# ------------------------------------------------------------------------------
# On fait la moyenne de tous les étangs pour avoir la tendance globale du réseau
df_lignes_15fev <- df_final_remplissage %>%
  group_by(annee, Scenario, Modele_Meteo_Desc) %>%
  summarise(Taux_Moyen = mean(Taux_Remplissage, na.rm = TRUE), .groups = "drop") %>%
  rename(Saison_Hydro = annee) # Pour s'aligner avec l'axe X de tes autres graphiques


# ------------------------------------------------------------------------------
# GÉNÉRATION DU GRAPHIQUE CHRONOLOGIQUE
# ------------------------------------------------------------------------------
dev.new(width = 14, height = 8)

g_chronique_15fev <- ggplot(df_lignes_15fev, aes(x = Saison_Hydro, y = Taux_Moyen / 100, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + 
  scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Évolution du Taux de Remplissage Moyen du Réseau (15 Février)",
    subtitle = "Vue Météo : Chronique du stock hivernal global (Données brutes, 2026-2070)",
    x = "Saison", 
    y = "Remplissage Moyen au 15/02 (%)", 
    color = "Gestion", 
    linetype = "Gestion"
  ) +
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), 
    strip.text = element_text(face = "bold")
  )

print(g_chronique_15fev)

# ------------------------------------------------------------------------------
# PRÉPARATION DES DONNÉES : MOYENNE GLOBALE AU 1ER SEPTEMBRE
# ------------------------------------------------------------------------------
# On fait la moyenne de tous les étangs pour avoir la tendance globale du réseau
df_lignes_1sept <- df_final_remplissage %>%
  group_by(annee, Scenario, Modele_Meteo_Desc) %>%
  summarise(Taux_Moyen = mean(Taux_Remplissage, na.rm = TRUE), .groups = "drop") %>%
  rename(Saison_Hydro = annee) # Pour s'aligner avec l'axe X

# ------------------------------------------------------------------------------
# GÉNÉRATION DU GRAPHIQUE CHRONOLOGIQUE (1ER SEPTEMBRE)
# ------------------------------------------------------------------------------
dev.new(width = 14, height = 8)

g_chronique_1sept <- ggplot(df_lignes_1sept, aes(x = Saison_Hydro, y = Taux_Moyen / 100, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + 
  scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Évolution du Taux de Remplissage Moyen du Réseau (1er Septembre)",
    subtitle = "Vue Météo : Chronique du reste à vivre estival global (Données brutes, 2026-2070)",
    x = "Saison", 
    y = "Remplissage Moyen au 01/09 (%)", 
    color = "Gestion", 
    linetype = "Gestion"
  ) +
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "#fdf2e9", color = "#e67e22"), # Couleurs plus "chaudes/estivales" pour les bandeaux
    strip.text = element_text(color = "#2c3e50", face = "bold")
  )

print(g_chronique_1sept)

