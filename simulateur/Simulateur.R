# ==============================================================================
# SCRIPT 2 : MOTEUR DE SIMULATION HYDROLOGIQUE (simulateur.R)
# Objectif : Calculer le bilan hydrologique complet de la chaîne d'étangs
# Méthode : Modèle Réservoir INRAE (Saturation Progressive, Beta=4, Drainage=0)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)

# Note : Le script importation.R doit avoir été exécuté en amont pour charger 
# les tables tab_etg_base et pluvio_base dans l'environnement.

# ==============================================================================
# 1. FONCTION DE BILAN JOURNALIER DE L'ÉTANG (Routage hydraulique)
# ==============================================================================
Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Statut_Assec, Volume_Vidange_Jour, Peche_Jour){
  
  Eau_Dispo = BF + Volume_R + Vamont
  
  Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
  Eau_Dispo = Eau_Dispo - Fuite_Reelle
  Vsortant = Fuite_Reelle 
  
  if (Statut_Assec == "Assec" || Peche_Jour == "oui") {
    Evap_Reelle = max(0, Vp_etp)
  } else {
    if (Vp_etp < 0) {
      Evap_Reelle = max(Vp_etp, -Eau_Dispo) 
    } else {
      Evap_Reelle = Vp_etp 
    }
  }
  Eau_Dispo = Eau_Dispo + Evap_Reelle 
  
  if (Peche_Jour == "oui") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
  } else if (Volume_Vidange_Jour > 0) {
    Objectif_Volume = max(0, BF - Volume_Vidange_Jour)
    if (Eau_Dispo > Objectif_Volume) {
      Volume_a_vider = Eau_Dispo - Objectif_Volume
      Volume_theorique = max(Volume_a_vider, Volume_Vidange_Jour)
    } else {
      Volume_theorique = Volume_Vidange_Jour
    }
    Volume_reel_vide = min(Volume_theorique, max(0, Eau_Dispo))
    Vsortant = Vsortant + Volume_reel_vide
    Eau_Dispo = Eau_Dispo - Volume_reel_vide
    
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
    
  } else if (Statut_Assec == "Assec") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
  } else {
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
  } 
  
  return(list(BF = BF, Vsortant = Vsortant, Evap_Reelle = Evap_Reelle, Fuite_Reelle = Fuite_Reelle))
}

# ==============================================================================
# 2. LE MOTEUR DE SIMULATION (Réseau et Bilan Sol Type INRAE)
# ==============================================================================
run_hydrological_model <- function(pluvio_data, tab_etg_data, RU_defaut = 150, beta_val = 3, C_transfert = 0.05) {
  
  print(paste("Demarrage de la simulation | Modele INRAE | RU =", RU_defaut, "mm | Beta =", beta_val))
  
  # --- A. Pivot des dates Assec ---
  table_assec <- tab_etg_data %>%
    select(NOM, starts_with("Assec")) %>%
    pivot_longer(
      cols = -NOM,
      names_to = c(".value", "annee"),
      names_pattern = "([A-Za-z]+)(\\d{4})"
    ) %>%
    arrange(NOM, annee) %>%
    group_by(NOM) %>%
    mutate(Assec_Futur = coalesce(lead(Assec), Assec)) %>%
    ungroup()
  
  # --- B. Extraction ABSOLUE des dates d'intervention ---
  dates_calendrier <- tab_etg_data %>%
    select(NOM, starts_with("Vidange"), starts_with("peche")) %>%
    mutate(across(-NOM, as.character)) %>%
    pivot_longer(cols = -NOM, names_to = "colonne", values_to = "date_exacte") %>%
    filter(!is.na(date_exacte) & date_exacte != "") %>%
    mutate(
      type_event = if_else(grepl("Vidange", colonne), "Vidange", "peche"),
      date_exacte = as.Date(date_exacte)
    ) %>%
    select(NOM, type_event, date_exacte) %>%
    distinct()
  
  vidanges_seules <- dates_calendrier %>% filter(type_event == "Vidange") %>% mutate(Vidange_bool = "oui") %>% select(-type_event)
  peches_seules   <- dates_calendrier %>% filter(type_event == "peche") %>% mutate(peche_bool = "oui") %>% select(-type_event)
  
  # --- C. Création du Bilan initial Vectorisé ---
  df_bilan <- tab_etg_data %>%
    select(NOM, Surface_BV, SURFACE_eau, Vmax, Exutoire_1, any_of("RU_max")) %>%
    mutate(RU_max = if("RU_max" %in% names(.)) RU_max else RU_defaut) %>%
    cross_join(pluvio_data) %>%
    mutate(
      annee = format(dat, "%Y"),
      mois = format(dat, "%m")
    ) %>%
    left_join(table_assec, by = c("NOM", "annee")) %>%
    left_join(vidanges_seules, by = c("NOM" = "NOM", "dat" = "date_exacte")) %>%
    left_join(peches_seules, by = c("NOM" = "NOM", "dat" = "date_exacte")) %>%
    mutate(
      RR_num = replace_na(as.numeric(RR), 0),
      ETP_num = replace_na(as.numeric(ETP_grille), 0),
      VFuite = round(0.1 * 3600 * 24) / 1000, 
      Vidange = replace_na(Vidange_bool, "non"),
      peche   = replace_na(peche_bool, "non")
    ) %>% 
    group_by(NOM, annee) %>%
    mutate(
      Statut_Simu = case_when(
        peche == "oui" | Vidange == "oui" ~ "Evolage", 
        cumany(peche == "oui") ~ Assec_Futur, 
        Assec == "Assec" & mois >= "10" ~ Assec_Futur,
        TRUE ~ Assec
      )
    ) %>% 
    ungroup() %>%
    mutate(
      Surface_Active_Ruissellement = if_else(Statut_Simu == "Assec", Surface_BV, Surface_BV - SURFACE_eau),
      Vp_etp = if_else(Statut_Simu == "Assec", 0, (RR_num - (ETP_num * 1.15)) * SURFACE_eau * 10),
      Vamont = 0,
      BF = case_when(dat == min(dat) & Assec == "Evolage" ~ Vmax / 2, TRUE ~ 0),
      RU_courante = 0,
      Volume_R = 0
    ) %>%
    select(-annee, -mois, -Assec, -Assec_Futur, -Vidange_bool, -peche_bool)
  
  liste_etangs <- df_bilan %>% split(.$NOM)
  Volume_Total_Exutoire_BV <- data.frame(dat = liste_etangs[[1]]$dat, Volume_Riviere = rep(0, nrow(liste_etangs[[1]])))
  
  # --- D. Routage Topologique (De l'Amont vers l'Aval) ---
  liens <- tab_etg_data %>% select(NOM, Exutoire_1) %>% filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT" & Exutoire_1 != "") %>% distinct()
  ordre_topologique <- names(topo_sort(graph_from_data_frame(liens, directed = TRUE), mode = "out"))
  ordre_topologique <- ordre_topologique[ordre_topologique %in% names(liste_etangs)]
  
  for (nom_etang in ordre_topologique) {
    etangs_calcule <- liste_etangs[[nom_etang]]
    Stockage_Vamont <- numeric(nrow(etangs_calcule))
    etangs_calcule$Vol_Vidange_Jour <- 0
    etangs_calcule$Vsortant <- 0  
    etangs_calcule$Evap_Reelle <- 0
    etangs_calcule$Fuite_Reelle <- 0
    
    lignes_vidange <- which(etangs_calcule$Vidange == "oui")
    lignes_peche <- which(etangs_calcule$peche == "oui")
    
    if (length(lignes_vidange) > 0) {
      for (t0 in lignes_vidange) {
        Volume_Etang <- etangs_calcule$Vmax[t0]
        Surface_ha <- etangs_calcule$SURFACE_eau[t0] 
        Vol_1ha_jour <- Volume_Etang / Surface_ha
        duree_vidange_normale <- ceiling(Surface_ha)
        
        peches_futures <- lignes_peche[lignes_peche > t0]
        
        if (length(peches_futures) > 0) {
          tfin <- peches_futures[1]
          delta_T <- tfin - t0
          jours_effectifs <- min(delta_T, duree_vidange_normale)
        } else {
          jours_effectifs <- duree_vidange_normale
        }
        
        jours_effectifs <- min(jours_effectifs, nrow(etangs_calcule) - t0 + 1)
        
        if (jours_effectifs > 0) {
          etangs_calcule$Vol_Vidange_Jour[t0:(t0 + jours_effectifs - 1)] <- Vol_1ha_jour
        }
      }
    }
    
    etangs_calcule$RU_courante[1] <- etangs_calcule$RU_max[1]
    
    for (jour in 2:nrow(etangs_calcule)) {
      
      pluie_jour <- etangs_calcule$RR_num[jour]
      etp_jour <- etangs_calcule$ETP_num[jour]
      ru_max <- etangs_calcule$RU_max[jour]
      ru_prec <- etangs_calcule$RU_courante[jour-1]
      
      taux_remplissage <- ru_prec / ru_max
      Q_mm <- pluie_jour * (taux_remplissage ^ beta_val)
      I_mm <- pluie_jour - Q_mm
      ETa_mm <- min(etp_jour, ru_prec + I_mm)
      
      bilan_sol <- ru_prec + I_mm - ETa_mm
      
      if (bilan_sol > ru_max) {
        exces_mm <- bilan_sol - ru_max
        etangs_calcule$RU_courante[jour] <- ru_max
        ruissellement_total_mm <- Q_mm + exces_mm
      } else if (bilan_sol < 0) {
        etangs_calcule$RU_courante[jour] <- 0
        ruissellement_total_mm <- Q_mm
      } else {
        etangs_calcule$RU_courante[jour] <- bilan_sol
        ruissellement_total_mm <- Q_mm
      }
      
      etangs_calcule$Volume_R[jour] <- ruissellement_total_mm * etangs_calcule$Surface_Active_Ruissellement[jour] * 10 * C_transfert
      
      res <- Bfinal(etangs_calcule$Vmax[jour], etangs_calcule$BF[jour-1], etangs_calcule$Vp_etp[jour], 
                    etangs_calcule$Volume_R[jour], etangs_calcule$Vamont[jour], etangs_calcule$VFuite[jour], 
                    etangs_calcule$Statut_Simu[jour], etangs_calcule$Vol_Vidange_Jour[jour], etangs_calcule$peche[jour])
      
      etangs_calcule$BF[jour] = res$BF
      Stockage_Vamont[jour] = res$Vsortant
      etangs_calcule$Vsortant[jour] = res$Vsortant 
      etangs_calcule$Evap_Reelle[jour] = res$Evap_Reelle
      etangs_calcule$Fuite_Reelle[jour] = res$Fuite_Reelle
    }
    
    liste_etangs[[nom_etang]] <- etangs_calcule
    exutoire <- etangs_calcule$Exutoire_1[1]
    
    if (!is.na(exutoire) && exutoire != "") {
      if (exutoire != "OUTPUT" && exutoire %in% names(liste_etangs)) {
        liste_etangs[[exutoire]]$Vamont <- liste_etangs[[exutoire]]$Vamont + Stockage_Vamont
      } else {
        Volume_Total_Exutoire_BV$Volume_Riviere <- Volume_Total_Exutoire_BV$Volume_Riviere + Stockage_Vamont
      }
    }
  }
  
  print("Calcul termine avec succes.")
  return(list(liste_finale = liste_etangs, exutoire_data = Volume_Total_Exutoire_BV))
}

# ==============================================================================
# 3. BOUCLE D'EXÉCUTION AUTOMATIQUE SUR LES 6 SCÉNARIOS MÉTÉO
# ==============================================================================

# Paramètres hydrologiques
beta = 4
RU = 200
coef = 0.2
date_heure <- format(Sys.time(), "%Y%m%d")

# Paramètre spatial (Code du bassin à adapter : Chalamont = 2, Joyeux = 23)
CODE_METEO_ACTUEL <- 2 

# Liste de tes dossiers météo (A modifier avec les noms exacts de tes dossiers)
liste_dossiers_meteo <- c(
  "data/meteo/MPI-ESM  REMO2009",
  "data/meteo/IPSL-CM5A  WRF381P",
  "data/meteo/IPSL-CM5A  RCA4",
  "data/meteo/HadGEM2  RegCM4-6",
  "data/meteo/HadGEM2  CCLM4-8-17",
  "data/meteo/CNRM-CM5  ALADIN63"
)

cat("\n===================================================================\n")
cat("DÉMARRAGE DU TRAITEMENT EN LOT (BATCH) - 6 SCÉNARIOS MÉTÉO\n")
cat("===================================================================\n")

# Boucle principale
for (dossier_meteo in liste_dossiers_meteo) {
  
  nom_scenario <- basename(dossier_meteo)
  cat(paste("\n---> TRAITEMENT EN COURS :", nom_scenario, "<---\n"))
  
  # --- A. CHARGEMENT DYNAMIQUE DE LA MÉTÉO ---
  chemin_meteo <- paste0(dossier_meteo, "/Meteo.csv")
  chemin_centro <- paste0(dossier_meteo, "/centro_BV.csv")
  
  if (!file.exists(chemin_meteo)) {
    cat(paste("ERREUR : Fichier Meteo.csv introuvable dans", dossier_meteo, "- Passage au suivant.\n"))
    next # Passe au dossier suivant si le fichier n'existe pas
  }
  
  # Extraction des coordonnées du centre
  coordonnees <- read.csv(chemin_centro, header = TRUE, sep = ",") %>% 
    filter(CODE == CODE_METEO_ACTUEL)
  
  X_ref <- coordonnees$LAMBX[1]
  Y_ref <- coordonnees$LAMBY[1]
  
  meteo_brute <- read.csv2(chemin_meteo, stringsAsFactors = FALSE) 
  
  maille_proche <- meteo_brute %>%
    select(LAMBX, LAMBY) %>%
    distinct() %>%
    mutate(distance = sqrt((LAMBX - X_ref)^2 + (LAMBY - Y_ref)^2)) %>%
    arrange(distance) %>%
    head(1)
  
  le_bon_X <- maille_proche$LAMBX[1]
  le_bon_Y <- maille_proche$LAMBY[1]
  
  # Création de la série pluvio pour ce scénario spécifique
  pluvio_scenario <- meteo_brute %>%
    filter(LAMBX == le_bon_X & LAMBY == le_bon_Y) %>%
    rename(RR = PRELIQ) %>% 
    mutate(
      dat = as.Date(lubridate::parse_date_time(as.character(DATE), orders = c("ymd", "dmy", "Ymd", "Y-m-d"))),
      RR = as.numeric(gsub(",", ".", as.character(RR))),
      ETP_grille = as.numeric(gsub(",", ".", as.character(ETP))),
      P_ETP = RR - ETP_grille
    ) %>%
    select(dat, RR, ETP_grille, P_ETP) %>%
    filter(between(dat, as.Date("2010-01-01"), as.Date("2070-12-31"))) %>% # Ajusté à 2070 pour couvrir la simu
    arrange(dat)
  
  # --- B. LANCEMENT DE LA SIMULATION ---
  resultats_sim <- run_hydrological_model(
    pluvio_data = pluvio_scenario,
    tab_etg_data = tab_etg_base,  # Utilise le tableau d'étangs chargé par le Script 1
    RU_defaut = RU, 
    beta_val = beta,      
    C_transfert = coef
  )
  
  # --- C. SAUVEGARDE PERSONNALISÉE ---
  # Le nom du fichier contiendra le nom du dossier météo pour bien les différencier
  nom_fichier_sortie <- paste0("Analyse_R", beta, "_RU_", RU, "_Coef_", coef, "_Meteo_", nom_scenario, "_", date_heure, ".rds")
  saveRDS(resultats_sim, file = nom_fichier_sortie)
  
  cat(paste("Terminé ! Fichier sauvegardé :", nom_fichier_sortie, "\n"))
}

cat("\n===================================================================\n")
cat("TOUS LES SCÉNARIOS SONT TERMINÉS AVEC SUCCÈS.\n")
cat("===================================================================\n")