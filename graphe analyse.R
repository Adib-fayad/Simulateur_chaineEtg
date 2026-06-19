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
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1.2) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A1 : Évolution du Coefficient d'Écoulement Global à l'Exutoire",
       subtitle = "Vue Météo : Comparaison de l'efficacité des gestions pour un climat donné",
       x = "Saison", y = "Coefficient d'Écoulement (%)", color = "Gestion", linetype = "Gestion") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA1)

# -- TYPE A2 : Coef d'Evaporation --
dev.new(width = 14, height = 8)
gA2 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Evaporation, color = Scenario, linetype = Scenario)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1.2) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A2 : Évolution du Coefficient d'Évaporation",
       subtitle = "Vue Météo : Impact des assecs sur l'évaporation du réseau",
       x = "Saison", y = "Coefficient d'Évaporation (%)", color = "Gestion", linetype = "Gestion") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#e8f4f8", color = "#b6d4fe"), strip.text = element_text(face = "bold"))
print(gA2)

# -- TYPE A3 : Coef de Captage --
dev.new(width = 14, height = 8)
gA3 <- ggplot(df_master, aes(x = Saison_Hydro, y = Coef_Captage, color = Scenario, linetype = Scenario)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1.2) + 
  facet_wrap(~ Modele_Meteo_Desc, ncol = 3) +
  scale_color_manual(values = couleurs_scenarios) + scale_linetype_manual(values = linetypes_scenarios) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "TYPE A3 : Évolution du Taux de Captage Global",
       subtitle = "Vue Météo : Part de la pluie de tout le bassin captée par le réseau d'étangs",
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
      filter(mois == 3, jour == 20) %>%
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
  geom_boxplot(alpha = 0.8, outlier.size = 0.3, width = 0.7, position = position_dodge(0.8)) +
  facet_wrap(~ Categorie, ncol = 3) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + # Dégradé logique du bleu (frais) au rouge (sec)
  theme_minimal(base_size = 13) +
  labs(
    title = "Niveau de Remplissage des Étangs au 20 mars (Période 2026-2070)",
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
# COMPILATION GÉNÉRALE ET SYNTHÈSE DES INDICATEURS HYDROLOGIQUES (2026-2070)
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
# 2. MOTEUR DE CALCUL UNIQUE ET SÉCURISÉ (TOUS INDICATEURS)
# ------------------------------------------------------------------------------
calculer_tous_indicateurs <- function(chemin_rds, nom_scenario, nom_modele) {
  simu <- readRDS(chemin_rds)
  if(length(simu$liste_finale) == 0) return(NULL)
  
  # A. Classification topologique automatique
  tous_les_etangs <- names(simu$liste_finale)
  destination_aval <- sapply(simu$liste_finale, function(x) x$Exutoire_1[1])
  
  etangs_tete <- tous_les_etangs[!(tous_les_etangs %in% destination_aval)]
  etangs_exutoire <- tous_les_etangs[!(destination_aval %in% tous_les_etangs)]
  etangs_milieu <- setdiff(tous_les_etangs, c(etangs_tete, etangs_exutoire))
  
  get_categorie <- function(nom) {
    if (nom %in% etangs_tete) return("1. Étang de Tête")
    if (nom %in% etangs_milieu) return("2. Étang de Milieu")
    if (nom %in% etangs_exutoire) return("3. Étang Exutoire")
    return("Autre")
  }
  
  # B. Données de structure
  surface_totale_bv <- sum(sapply(simu$liste_finale, function(x) x$Surface_BV[1]), na.rm = TRUE)
  surface_eau_totale <- sum(sapply(simu$liste_finale, function(x) x$SURFACE_eau[1]), na.rm = TRUE)
  
  # Fusion de l'historique journalier de tous les étangs
  df_all_etangs <- bind_rows(simu$liste_finale, .id = "NOM_ETANG") %>%
    mutate(
      annee = year(dat), mois = month(dat), jour = day(dat),
      Saison_Hydro = if_else(mois > 10 | (mois == 10 & jour >= 15), annee + 1, annee)
    )
  
  # C. Partie Globale (Bilan de masse à l'échelle du bassin versant)
  df_daily_global <- df_all_etangs %>%
    group_by(Saison_Hydro, dat) %>%
    summarise(
      RR_jour = first(RR),
      Volume_Ruiss_Tous = sum(Volume_R, na.rm = TRUE),
      Volume_Evap_Tous = sum(abs(Evap_Reelle[Evap_Reelle < 0]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(simu$exutoire_data %>% select(dat, Volume_Riviere), by = "dat")
  
  bilan_global <- df_daily_global %>%
    group_by(Saison_Hydro) %>%
    summarise(
      Pluie_Totale_mm = sum(RR_jour, na.rm = TRUE),
      Vol_Pluie_BV_m3 = Pluie_Totale_mm * surface_totale_bv * 10,
      Coef_Écoulement = sum(Volume_Riviere, na.rm = TRUE) / Vol_Pluie_BV_m3,
      Coef_Évaporation = sum(Volume_Evap_Tous, na.rm = TRUE) / Vol_Pluie_BV_m3,
      Coef_Captage = (sum(Volume_Ruiss_Tous, na.rm = TRUE) + (Pluie_Totale_mm * surface_eau_totale * 10)) / Vol_Pluie_BV_m3,
      .groups = "drop"
    )
  
  # D. Partie Typologique (Suivi fin d'hiver et fin d'été par catégorie)
  df_points_remplissage <- df_all_etangs %>%
    filter((mois == 3 & jour == 20) | (mois == 9 & jour == 1)) %>%
    mutate(
      Taux_Remplissage = (BF / Vmax) * 100,
      Categorie = sapply(NOM_ETANG, get_categorie)
    ) %>%
    group_by(Saison_Hydro, Categorie) %>%
    summarise(
      Remplissage_20Mars = mean(Taux_Remplissage[mois == 3], na.rm = TRUE),
      Remplissage_1Sept = mean(Taux_Remplissage[mois == 9], na.rm = TRUE),
      .groups = "drop"
    )
  
  # E. Fusion finale pour la chronique annuelle
  chronique_annuelle <- df_points_remplissage %>%
    left_join(bilan_global, by = "Saison_Hydro") %>%
    filter(Saison_Hydro >= 2026 & Saison_Hydro <= 2070) %>%
    mutate(Scenario = nom_scenario, Modele_Meteo = nom_modele)
  
  return(chronique_annuelle)
}

# ------------------------------------------------------------------------------
# 3. LE CRAWLER MULTI-INDICATEURS
# ------------------------------------------------------------------------------
cat("Extraction simultanée de tous les indicateurs...\n")
liste_complete <- list()

for (dossier in dossiers_scenarios) {
  if (!dir.exists(dossier)) next
  fichiers <- list.files(dossier, pattern = "\\.rds$", full.names = TRUE)
  nom_scenario <- noms_propres_scenarios[basename(dossier)]
  
  for (f in fichiers) {
    nom_modele <- str_extract(basename(f), "(?<=Meteo_).*(?=_[0-9]{8}\\.rds)")
    if (is.na(nom_modele)) nom_modele <- "Inconnu"
    
    res <- calculer_tous_indicateurs(f, nom_scenario, nom_modele)
    if (!is.null(res)) liste_complete[[length(liste_complete) + 1]] <- res
  }
}

df_master_total <- bind_rows(liste_complete)

# Uniformisation et mise en forme des modèles climatiques
df_master_total <- df_master_total %>%
  mutate(Modele_Meteo_Desc = case_when(
    str_detect(Modele_Meteo, "ALADIN63") ~ "CNRM-CM5 (Modéré)",
    str_detect(Modele_Meteo, "REMO2009") ~ "MPI-ESM (Intermédiaire)",
    str_detect(Modele_Meteo, "WRF381P")  ~ "IPSL-CM5A (Humide)",
    str_detect(Modele_Meteo, "RCA4")     ~ "IPSL-CM5A (Hiver humide/Été extrême)",
    str_detect(Modele_Meteo, "RegCM4-6") ~ "HadGEM2 (Chaud/Sec modéré)",
    str_detect(Modele_Meteo, "CCLM4-8-17") ~ "HadGEM2 (Extrême sec)",
    TRUE ~ Modele_Meteo
  ))

# Sauvegarde de la grande table brute (année par année, de 2026 à 2070)
write_excel_csv(df_master_total, file = "indicateurs_annuels_complets.csv")
cat("Fichier brut sauvegardé : 'indicateurs_annuels_complets.csv'\n")

# ------------------------------------------------------------------------------
# 4. COMPILATION DE LA MEGA-SYNTHÈSE PAR HORIZON TEMPOREL
# ------------------------------------------------------------------------------
cat("Génération de la synthèse décisionnelle par Horizon...\n")

df_synthese_horizons <- df_master_total %>%
  mutate(Horizon = case_when(
    Saison_Hydro <= 2040 ~ "2030-2040 (Proche)",
    Saison_Hydro > 2040 & Saison_Hydro <= 2055 ~ "2041-2055 (Moyen)",
    Saison_Hydro > 2055 ~ "2056-2070 (Lointain)"
  )) %>%
  group_by(Horizon, Modele_Meteo_Desc, Scenario, Categorie) %>%
  summarise(
    Pluie_Moyenne_mm = mean(Pluie_Totale_mm, na.rm = TRUE),
    Ecoulement_Global_Moy = mean(Coef_Écoulement, na.rm = TRUE),
    Evaporation_Global_Moy = mean(Coef_Évaporation, na.rm = TRUE),
    Captage_Global_Moy = mean(Coef_Captage, na.rm = TRUE),
    Remplissage_20Mars_Moy = mean(Remplissage_20Mars, na.rm = TRUE),
    Remplissage_1Sept_Moy = mean(Remplissage_1Sept, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Horizon, Modele_Meteo_Desc, Scenario, Categorie)

# Sauvegarde de la table de synthèse condensée
write_excel_csv(df_synthese_horizons, file = "synthese_indicateurs_horizons.csv")
cat("Fichier de synthèse sauvegardé : 'synthese_indicateurs_horizons.csv'\n")
# Affichage de contrôle dans la console
print(head(df_synthese_horizons, 20))
