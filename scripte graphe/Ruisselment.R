# ==============================================================================
# SCRIPT D'ANALYSE MULTI-SCÉNARIOS : COMPARAISON DES FICHIERS .rds
# ==============================================================================

library(tidyverse)

# 1. On charge l'environnement de base (indispensable pour avoir les surfaces et la météo)
source("simulateur/fonctions.R")
source("simulateur/importation.R")

# 2. Détection automatique de toutes tes simulations sauvegardées
fichiers_scenarios <- list.files(pattern = "\\.rds$")

if(length(fichiers_scenarios) == 0) {
  stop("❌ Aucun fichier .rds trouvé dans le dossier actuel !")
}

cat("✅", length(fichiers_scenarios), "scénarios détectés. Lancement du grand comparateur...\n\n")

liste_mega_bilan <- list()

# ==============================================================================
# BOUCLE 1 : SUR CHAQUE SCÉNARIO (.rds)
# ==============================================================================
for (fichier in fichiers_scenarios) {
  
  # On extrait le nom du scénario (ex: "L0.05_Pant10_CN_modif") en enlevant l'extension et la date
  nom_scenario <- str_remove(fichier, "_20260518\\.rds") 
  
  # On charge la simulation en mémoire
  resultats_sim <- readRDS(fichier)
  tous_les_etangs <- names(resultats_sim$liste_finale)
  
  liste_etangs_scenario <- list()
  
  # ==============================================================================
  # BOUCLE 2 : SUR CHAQUE ÉTANG (Ton script d'analyse classique)
  # ==============================================================================
  for (nom_etang in tous_les_etangs) {
    
    infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
    surface_eau <- infos_etg$SURFACE_eau
    surface_bv_terre <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
    
    df_terrain <- load_terrain(nom_etang)
    if(is.null(df_terrain) || nrow(df_terrain) == 0) next 
    
    df_gestion <- resultats_sim$liste_finale[[nom_etang]] %>% select(dat, Vidange, peche)
    
    df_terrain <- df_terrain %>%
      inner_join(df_gestion, by = "dat") %>%
      filter(Vidange == "non" & peche == "non")
    
    df_stats <- df_terrain %>%
      inner_join(pluvio_base, by = "dat") %>%
      arrange(dat) %>%
      mutate(
        Ecart_Jours = as.numeric(dat - lag(dat, 1)),
        Delta_V_Reel = ifelse(Ecart_Jours == 1, Volume_Reel - lag(Volume_Reel, 1), NA),
        Volume_Meteo_Direct = P_ETP * surface_eau * 10,
        Volume_Residuel = Delta_V_Reel - Volume_Meteo_Direct
      ) %>%
      drop_na(Volume_Residuel)
    
    df_ruissellement <- df_stats %>%
      filter(RR >= 5) %>% 
      mutate(
        Vol_Pluie_Sur_Terre = RR * surface_bv_terre * 10,
        Pseudo_CR = (Volume_Residuel / Vol_Pluie_Sur_Terre) * 100
      ) %>%
      filter(Pseudo_CR > 0 & Pseudo_CR <= 100)
    
    if(nrow(df_ruissellement) == 0) next
    
    df_simule <- resultats_sim$liste_finale[[nom_etang]] %>%
      select(dat, CR_Theorique = CR) %>%
      mutate(CR_Theorique = CR_Theorique * 100)
    
    df_comparaison <- df_ruissellement %>%
      inner_join(df_simule, by = "dat") %>%
      drop_na(CR_Theorique, Pseudo_CR)
    
    if(nrow(df_comparaison) == 0) next
    
    df_comparaison <- df_comparaison %>% mutate(Erreur_CR = CR_Theorique - Pseudo_CR)
    
    biais_moyen_cr <- mean(df_comparaison$Erreur_CR, na.rm = TRUE)
    
    # On sauvegarde le résultat de cet étang pour ce scénario précis
    liste_etangs_scenario[[nom_etang]] <- tibble(
      Scenario = nom_scenario,
      Etang = nom_etang,
      Nb_Orages = nrow(df_comparaison),
      CR_Terrain = round(mean(df_comparaison$Pseudo_CR, na.rm = TRUE), 1),
      CR_Modele = round(mean(df_comparaison$CR_Theorique, na.rm = TRUE), 1),
      Biais = round(biais_moyen_cr, 2)
    )
  }
  
  # On rassemble tous les étangs de ce scénario et on l'ajoute au mega-bilan
  liste_mega_bilan[[nom_scenario]] <- bind_rows(liste_etangs_scenario)
}

# ==============================================================================
# RÉSULTATS FINAUX ET CLASSEMENT
# ==============================================================================

# 1. Le Tableau Détaillé (Chaque étang pour chaque scénario)
tableau_detaille <- bind_rows(liste_mega_bilan)

# 2. Le Classement Global des Scénarios (Le juge de paix)
# On calcule l'Erreur Absolue Moyenne (MAE) pour voir quel modèle se trompe le moins globalement
classement_scenarios <- tableau_detaille %>%
  group_by(Scenario) %>%
  summarise(
    Nb_Etangs_Valides = n(),
    Biais_Moyen_Global = round(mean(Biais), 2),
    Erreur_Absolue_Moyenne = round(mean(abs(Biais)), 2) # La vraie métrique de précision
  ) %>%
  arrange(Erreur_Absolue_Moyenne) # Le plus précis en haut !

cat("\n CLASSEMENT FINAL DES SCÉNARIOS (Le meilleur est en haut) :\n")
print(as.data.frame(classement_scenarios))

cat("\n EXTRAIT DU TABLEAU DÉTAILLÉ (Par étang et par scénario) :\n")
print(head(as.data.frame(tableau_detaille), 10))


write_csv(classement_scenarios, "Classement_Scenarios_Global.csv")
write_csv(tableau_detaille, "Bilan_Detaille_Multi_Scenarios.csv")

# ==============================================================================
# SCRIPT D'AUTO-CALIBRATION : RECHERCHE DU BONUS "CN" PARFAIT (LAMBDA = 0.05)
# ==============================================================================

library(tidyverse)

source("simulateur/fonctions.R")
source("simulateur/importation.R")

tous_les_etangs <- names(resultats_sim$liste_finale)
liste_optimisation <- list()

cat("⚙️ Lancement de l'intelligence d'auto-calibration des CN... Ça va calculer fort !\n")

for (nom_etang in tous_les_etangs) {
  
  infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
  surface_eau <- infos_etg$SURFACE_eau
  surface_bv_terre <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
  
  df_terrain <- load_terrain(nom_etang)
  if(is.null(df_terrain) || nrow(df_terrain) == 0) next 
  
  # 1. GESTION ET FUITE
  df_gestion <- resultats_sim$liste_finale[[nom_etang]] %>% select(dat, Vidange, peche, VFuite, CN_jour)
  
  df_terrain <- df_terrain %>%
    inner_join(df_gestion, by = "dat") %>%
    filter(Vidange == "non" & peche == "non")
  
  # 2. CALCULS QUOTIDIENS AVEC FUITE CORRIGÉE
  df_stats <- df_terrain %>%
    inner_join(pluvio_base, by = "dat") %>%
    arrange(dat) %>%
    mutate(
      Ecart_Jours = as.numeric(dat - lag(dat, 1)),
      Delta_V_Brut = ifelse(Ecart_Jours == 1, Volume_Reel - lag(Volume_Reel, 1), NA),
      Volume_Meteo_Direct = P_ETP * surface_eau * 10,
      Volume_Residuel = Delta_V_Brut + VFuite - Volume_Meteo_Direct
    ) %>%
    drop_na(Volume_Residuel)
  
  # 3. FILTRAGE DU TERRAIN
  df_ruissellement <- df_stats %>%
    mutate(
      Vol_Pluie_Terre = RR * surface_bv_terre * 10,
      Pseudo_CR = (Volume_Residuel / Vol_Pluie_Terre) * 100
    ) %>%
    filter(RR >= 5 & Pseudo_CR > 0 & Pseudo_CR <= 100)
  
  if(nrow(df_ruissellement) == 0) next
  
  # ==============================================================================
  # 4. LE MOTEUR DE RECHERCHE (SOLVEUR)
  # ==============================================================================
  meilleur_bonus <- NA
  plus_petite_erreur <- Inf
  nouveau_cr_moyen <- NA
  
  # On teste tous les bonus possibles de CN (de -10 points à +30 points)
  for(bonus_test in seq(-10, 30, by = 1)) {
    
    df_test <- df_ruissellement %>%
      mutate(
        # On applique le bonus au CN du jour (sans jamais dépasser 99, limite physique)
        CN_opti = ifelse(CN_jour + bonus_test > 99, 99, CN_jour + bonus_test),
        
        # On recalcule l'équation SCS-CN américaine avec Lambda = 0.05
        S_max = 25.4 * ((1000 / CN_opti) - 10),
        Ia = 0.05 * S_max,
        Q_ruissele_mm = ifelse(RR > Ia, ((RR - Ia)^2) / (RR + 0.95 * S_max), 0),
        
        # Nouveau CR calculé
        CR_Simule_Test = (Q_ruissele_mm / RR) * 100,
        Erreur_Test = CR_Simule_Test - Pseudo_CR
      )
    
    biais_moyen <- mean(df_test$Erreur_Test, na.rm = TRUE)
    
    # Si cette erreur est plus proche de 0 que la précédente, on sauvegarde ce réglage !
    if(abs(biais_moyen) < plus_petite_erreur) {
      plus_petite_erreur <- abs(biais_moyen)
      meilleur_bonus <- bonus_test
      nouveau_cr_moyen <- mean(df_test$CR_Simule_Test, na.rm = TRUE)
    }
  }
  
  # 5. ENREGISTREMENT DES RÉSULTATS PARFAITS
  liste_optimisation[[nom_etang]] <- tibble(
    Etang = nom_etang,
    Nb_Orages = nrow(df_ruissellement),
    CR_Terrain_Reel = round(mean(df_ruissellement$Pseudo_CR), 1),
    CN_Moyen_Origine = round(mean(df_ruissellement$CN_jour), 1),
    BONUS_A_APPLIQUER = meilleur_bonus,
    Nouveau_CR_Modele = round(nouveau_cr_moyen, 1),
    Nouveau_Biais = round(plus_petite_erreur, 2)
  )
}

# ==============================================================================
# AFFICHAGE DU TABLEAU FINAL DE CALIBRATION
# ==============================================================================
tableau_opti <- bind_rows(liste_optimisation)

cat("\n✅ OPTIMISATION TERMINÉE ! Voici la règle de transfert pour la Dombes :\n\n")
print(as.data.frame(tableau_opti))















# ==============================================================================
# SCRIPT : CALCUL DES INDICATEURS DE GESTION PAR ANNEE (SEPTEMBRE - SEPTEMBRE)
# ==============================================================================

library(tidyverse)
library(lubridate)

# 1. Chargement de l'environnement de base
source("simulateur/fonctions.R")
source("simulateur/importation.R")

# Detection des fichiers de simulation .rds
fichiers_scenarios <- list.files(pattern = "\\.rds$")

if(length(fichiers_scenarios) == 0) {
  stop("Erreur : Aucun fichier .rds trouve dans le dossier de travail !")
}

cat("Lancement du calcul des indicateurs sur", length(fichiers_scenarios), "scenarios...\n")

liste_indicateurs_globale <- list()

# ==============================================================================
# BOUCLE SUR CHAQUE SCENARIO
# ==============================================================================
for (fichier in fichiers_scenarios) {
  
  # Nettoyage du nom de fichier pour extraire le nom du scenario
  nom_scenario <- str_remove(fichier, "\\.rds$")
  
  resultats_sim <- readRDS(fichier)
  tous_les_etangs <- names(resultats_sim$liste_finale)
  
  # ==============================================================================
  # BOUCLE SUR CHAQUE ETANG
  # ==============================================================================
  for (nom_etang in tous_les_etangs) {
    
    infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
    vmax_etang <- infos_etg$Vmax
    surface_bv_terre <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
    
    df_sim <- resultats_sim$liste_finale[[nom_etang]]
    
    df_terr <- load_terrain(nom_etang)
    if (!is.null(df_terr) && nrow(df_terr) > 0) {
      df_terr <- df_terr %>% select(dat, Volume_Reel_Sonde = Volume_Reel)
    } else {
      df_terr <- tibble(dat = as.Date(character()), Volume_Reel_Sonde = numeric())
    }
    
    df_analyse <- df_sim %>%
      left_join(df_terr, by = "dat") %>%
      mutate(
        Mois = month(dat),
        Annee_Calendrier = year(dat),
        # Creation de l'annee de gestion (septembre a septembre)
        Annee_Gestion = ifelse(Mois >= 9, 
                               paste0(Annee_Calendrier, "-", Annee_Calendrier + 1), 
                               paste0(Annee_Calendrier - 1, "-", Annee_Calendrier))
      )
    
    # ==============================================================================
    # CALCUL DES INDICATEURS PAR ANNEE DE GESTION
    # ==============================================================================
    indicateurs_par_annee <- df_analyse %>%
      group_by(Annee_Gestion) %>%
      summarise(
        Scenario = nom_scenario,
        Etang = nom_etang,
        Nb_Jours_Total = n(),
        
        # 1. Indicateurs de niveau d'eau (Seuils critiques)
        Jours_Rempli_Plus_80 = sum(BF >= (0.8 * vmax_etang), na.rm = TRUE),
        Jours_Moins_15_Pourcent = sum(BF <= (0.15 * vmax_etang), na.rm = TRUE),
        
        # 2. Indicateurs de flux (Bilans de masse)
        Volume_Ruissele_m3 = round(sum(Volume_R, na.rm = TRUE)),
        Volume_Evapore_m3 = round(sum(Evap_Reelle, na.rm = TRUE)),
        Volume_Sortant_m3 = round(sum(Vsortant, na.rm = TRUE)),
        
        # 3. Indicateurs de performance hydrologique
        Taux_Renouvellement = round(Volume_Ruissele_m3 / vmax_etang, 2),
        CR_Annuel_Global = round((Volume_Ruissele_m3 / sum(RR * surface_bv_terre * 10, na.rm = TRUE)) * 100, 1),
        
        # 4. Validation terrain
        Nb_Jours_Avec_Sonde = sum(!is.na(Volume_Reel_Sonde)),
        RMSE_Volume_m3 = ifelse(
          Nb_Jours_Avec_Sonde > 5,
          round(sqrt(mean((BF - Volume_Reel_Sonde)^2, na.rm = TRUE))),
          NA
        ),
        .groups = "drop"
      ) %>%
      # Filtre de securite pour ignorer les annees tronquees en bordure de dataset
      filter(Nb_Jours_Total >= 360)
    
    cle_liste <- paste0(nom_scenario, "_", nom_etang)
    liste_indicateurs_globale[[cle_liste]] <- indicateurs_par_annee
  }
}

# ==============================================================================
# COMPILATION ET EXPORT
# ==============================================================================
tableau_indicateurs_final <- bind_rows(liste_indicateurs_globale)

cat("\nAnalyse terminee. Apercu des indicateurs :\n\n")
print(head(as.data.frame(tableau_indicateurs_final), 10))

write_csv(tableau_indicateurs_final, "Indicateurs_Gestion_Annuels_Dombes.csv")
cat("\nLe fichier 'Indicateurs_Gestion_Annuels_Dombes.csv' a ete enregistre avec succes.\n")









# ==============================================================================
# SCRIPT : CALCUL DES INDICATEURS DE GESTION ET DE RUISSELLEMENT PAR ANNEE
# ==============================================================================

library(tidyverse)
library(lubridate)

# 1. Chargement de l'environnement de base
source("simulateur/fonctions.R")
source("simulateur/importation.R")

# Detection des fichiers de simulation .rds
fichiers_scenarios <- list.files(pattern = "\\.rds$")

if(length(fichiers_scenarios) == 0) {
  stop("Erreur : Aucun fichier .rds trouve dans le dossier de travail !")
}

cat("Lancement du calcul sur", length(fichiers_scenarios), "scenarios...\n")

liste_indicateurs_globale <- list()

# ==============================================================================
# BOUCLE SUR CHAQUE SCENARIO
# ==============================================================================
for (fichier in fichiers_scenarios) {
  
  nom_scenario <- str_remove(fichier, "\\.rds$")
  resultats_sim <- readRDS(fichier)
  tous_les_etangs <- names(resultats_sim$liste_finale)
  
  # ==============================================================================
  # BOUCLE SUR CHAQUE ETANG
  # ==============================================================================
  for (nom_etang in tous_les_etangs) {
    
    infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
    vmax_etang <- infos_etg$Vmax
    surface_eau <- infos_etg$SURFACE_eau
    surface_bv_terre <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
    
    df_sim <- resultats_sim$liste_finale[[nom_etang]]
    
    df_terr <- load_terrain(nom_etang)
    if (!is.null(df_terr) && nrow(df_terr) > 0) {
      df_terr <- df_terr %>% select(dat, Volume_Reel_Sonde = Volume_Reel)
    } else {
      df_terr <- tibble(dat = as.Date(character()), Volume_Reel_Sonde = numeric())
    }
    
    # Fusion et preparations des calculs journaliers (dont le ruissellement terrain)
    df_analyse <- df_sim %>%
      left_join(df_terr, by = "dat") %>%
      arrange(dat) %>%
      mutate(
        Mois = month(dat),
        Annee_Calendrier = year(dat),
        Annee_Gestion = ifelse(Mois >= 9, 
                               paste0(Annee_Calendrier, "-", Annee_Calendrier + 1), 
                               paste0(Annee_Calendrier - 1, "-", Annee_Calendrier)),
        
        # --- CALCUL DU RUISSELLEMENT TERRAIN (Reverse Engineering) ---
        Ecart_Jours = as.numeric(dat - lag(dat, 1)),
        Delta_V_Reel = ifelse(Ecart_Jours == 1, Volume_Reel_Sonde - lag(Volume_Reel_Sonde, 1), NA),
        Volume_Meteo_Direct = P_ETP * surface_eau * 10,
        Volume_Residuel = Delta_V_Reel - Volume_Meteo_Direct,
        
        Vol_Pluie_Sur_Terre = RR * surface_bv_terre * 10,
        Pseudo_CR_Terrain = (Volume_Residuel / Vol_Pluie_Sur_Terre) * 100,
        
        # Filtre strict : Uniquement les vrais orages sans intervention humaine
        Est_Orage_Valide = (RR >= 5 & Vidange == "non" & peche == "non" & 
                              Pseudo_CR_Terrain > 0 & Pseudo_CR_Terrain <= 100 & 
                              !is.na(Pseudo_CR_Terrain)),
        
        CR_Theorique_Pct = CR * 100,
        Erreur_CR = CR_Theorique_Pct - Pseudo_CR_Terrain
      )
    
    # ==============================================================================
    # CALCUL DES INDICATEURS PAR ANNEE DE GESTION
    # ==============================================================================
    indicateurs_par_annee <- df_analyse %>%
      group_by(Annee_Gestion) %>%
      summarise(
        Scenario = nom_scenario,
        Etang = nom_etang,
        Nb_Jours_Total = n(),
        
        # 1. Indicateurs de niveau d'eau
        Jours_Rempli_Plus_80 = sum(BF >= (0.8 * vmax_etang), na.rm = TRUE),
        Jours_Moins_15_Pourcent = sum(BF <= (0.15 * vmax_etang), na.rm = TRUE),
        
        # 2. Indicateurs de flux
        Volume_Ruissele_m3 = round(sum(Volume_R, na.rm = TRUE)),
        Volume_Evapore_m3 = round(sum(Evap_Reelle, na.rm = TRUE)),
        Volume_Sortant_m3 = round(sum(Vsortant, na.rm = TRUE)),
        
        # 3. Performance hydrologique
        Taux_Renouvellement = round(Volume_Ruissele_m3 / vmax_etang, 2),
        CR_Annuel_Global = round((Volume_Ruissele_m3 / sum(RR * surface_bv_terre * 10, na.rm = TRUE)) * 100, 1),
        
        # 4. Validation Volume Global (RMSE)
        Nb_Jours_Avec_Sonde = sum(!is.na(Volume_Reel_Sonde)),
        RMSE_Volume_m3 = ifelse(Nb_Jours_Avec_Sonde > 5, round(sqrt(mean((BF - Volume_Reel_Sonde)^2, na.rm = TRUE))), NA),
        
        # 5. RUISSELLEMENT ÉVÉNEMENTIEL (Ton ajout)
        Nb_Orages_Valides = sum(Est_Orage_Valide, na.rm = TRUE),
        CR_Terrain_Moyen = ifelse(Nb_Orages_Valides > 0, round(mean(Pseudo_CR_Terrain[Est_Orage_Valide], na.rm = TRUE), 1), NA),
        CR_Modele_Moyen = ifelse(Nb_Orages_Valides > 0, round(mean(CR_Theorique_Pct[Est_Orage_Valide], na.rm = TRUE), 1), NA),
        Biais_CR = ifelse(Nb_Orages_Valides > 0, round(mean(Erreur_CR[Est_Orage_Valide], na.rm = TRUE), 1), NA),
        
        .groups = "drop"
      ) %>%
      filter(Nb_Jours_Total >= 360)
    
    cle_liste <- paste0(nom_scenario, "_", nom_etang)
    liste_indicateurs_globale[[cle_liste]] <- indicateurs_par_annee
  }
}

# ==============================================================================
# COMPILATION ET EXPORT
# ==============================================================================
tableau_indicateurs_final <- bind_rows(liste_indicateurs_globale)

cat("\nAnalyse terminee. Apercu des indicateurs :\n\n")
print(head(as.data.frame(tableau_indicateurs_final), 10))

write_csv(tableau_indicateurs_final, "Indicateurs_Gestion_Annuels_Dombes.csv")
cat("\nLe fichier 'Indicateurs_Gestion_Annuels_Dombes.csv' a ete enregistre avec succes.\n")




# ==============================================================================
# SCRIPT : ANALYSE DU RUISSELLEMENT PAR SAISON HYDROLOGIQUE
# ==============================================================================

library(tidyverse)
library(lubridate)

source("simulateur/fonctions.R")
source("simulateur/importation.R")

# On charge les bons fichiers 
# (Attention, si tu as gardé les vieux fichiers d'hier, mets "20260519\\.rds$" à la place)
fichiers_scenarios <- list.files(pattern = "\\.rds$")

cat("Lancement de l'analyse saisonniere sur", length(fichiers_scenarios), "scenarios...\n")

liste_saisons <- list()

for (fichier in fichiers_scenarios) {
  
  nom_scenario <- str_remove(fichier, "\\.rds$")
  resultats_sim <- readRDS(fichier)
  tous_les_etangs <- names(resultats_sim$liste_finale)
  
  for (nom_etang in tous_les_etangs) {
    
    infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
    surface_eau <- infos_etg$SURFACE_eau
    surface_bv_terre <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
    
    df_sim <- resultats_sim$liste_finale[[nom_etang]]
    df_terr <- load_terrain(nom_etang)
    
    if (!is.null(df_terr) && nrow(df_terr) > 0) {
      df_terr <- df_terr %>% select(dat, Volume_Reel_Sonde = Volume_Reel)
    } else {
      next # On passe l'etang s'il n'y a pas de sonde
    }
    
    df_analyse <- df_sim %>%
      left_join(df_terr, by = "dat") %>%
      arrange(dat) %>%
      mutate(
        Mois = month(dat),
        
        # --- CREATION DES PERIODES HYDROLOGIQUES ---
        Saison = case_when(
          Mois %in% c(12, 1, 2)  ~ "1_Hiver (Sature)",
          Mois %in% c(3, 4, 5)   ~ "2_Printemps (Maintien)",
          Mois %in% c(6, 7, 8)   ~ "3_Ete (Basses Eaux)",
          Mois %in% c(9, 10, 11) ~ "4_Automne (Avant Vidange)",
          TRUE ~ "Autre"
        ),
        
        # Calculs des CR
        Ecart_Jours = as.numeric(dat - lag(dat, 1)),
        Delta_V_Reel = ifelse(Ecart_Jours == 1, Volume_Reel_Sonde - lag(Volume_Reel_Sonde, 1), NA),
        Volume_Meteo_Direct = P_ETP * surface_eau * 10,
        Volume_Residuel = Delta_V_Reel - Volume_Meteo_Direct,
        
        Vol_Pluie_Sur_Terre = RR * surface_bv_terre * 10,
        Pseudo_CR_Terrain = (Volume_Residuel / Vol_Pluie_Sur_Terre) * 100,
        
        Est_Orage_Valide = (RR >= 5 & Vidange == "non" & peche == "non" & 
                              Pseudo_CR_Terrain > 0 & Pseudo_CR_Terrain <= 100 & 
                              !is.na(Pseudo_CR_Terrain)),
        
        CR_Theorique_Pct = CR * 100,
        Erreur_CR = CR_Theorique_Pct - Pseudo_CR_Terrain
      )
    
    # [CORRECTION ICI] On integre les noms dans le tableau AVANT de grouper
    indicateurs_saison = df_analyse %>%
      filter(Est_Orage_Valide == TRUE) %>%
      mutate(
        Scenario = nom_scenario,
        Etang = nom_etang
      ) %>%
      group_by(Scenario, Etang, Saison) %>%
      summarise(
        Nb_Orages_Analyses = n(),
        Pluie_Moyenne_Orage_mm = round(mean(RR), 1),
        CR_Terrain_Moyen = round(mean(Pseudo_CR_Terrain), 1),
        CR_Modele_Moyen = round(mean(CR_Theorique_Pct), 1),
        Biais_Moyen_CR = round(mean(Erreur_CR), 1),
        .groups = "drop"
      )
    
    cle_liste <- paste0(nom_scenario, "_", nom_etang)
    liste_saisons[[cle_liste]] <- indicateurs_saison
  }
}

tableau_saisons_final <- bind_rows(liste_saisons) %>% arrange(Scenario, Etang, Saison)

cat("\nAnalyse terminee. Apercu du tableau :\n\n")
print(head(as.data.frame(tableau_saisons_final), 10))

write_csv(tableau_saisons_final, "Analyse_Biais_CR_Par_Saison.csv")
cat("\nLe fichier 'Analyse_Biais_CR_Par_Saison.csv' a ete genere.\n")