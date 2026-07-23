# ==============================================================================
# SCRIPT 2 : MOTEUR DE SIMULATION HYDROLOGIQUE (simulateur.R) - VERSION OPTIMISÉE
# Objectif : Calculer le bilan hydrologique complet de la chaîne d'étangs
# Méthode : Modèle Réservoir INRAE (Saturation Progressive, Beta=4, Drainage=0)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)

# ==============================================================================
# LE MOTEUR DE SIMULATION (Réseau et Bilan Sol Type INRAE - ULTRA RAPIDE)
# ==============================================================================
run_hydrological_model <- function(pluvio_data, tab_etg_data, RU_defaut = 150, beta_val = 3, C_transfert = 0.05) {
  
  print(paste("Demarrage de la simulation | Modele INRAE | RU =", RU_defaut, "mm | Beta =", beta_val))
  
  # --- Préparation des données ---
  table_assec <- tab_etg_data %>%
    select(NOM, starts_with("Assec")) %>%
    pivot_longer(
      cols = -NOM,
      names_to = c(".value", "annee"),
      names_pattern = "([A-Za-z]+)(\\d{4})"
    )
  
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
  
  df_bilan <- tab_etg_data %>%
    select(NOM, Surface_BV, SURFACE_eau, Vmax, Exutoire_1, any_of("RU_max")) %>%
    mutate(RU_max = if("RU_max" %in% names(.)) RU_max else RU_defaut) %>%
    cross_join(pluvio_data) %>%
    mutate(
      annee = as.numeric(format(dat, "%Y")),
      mois_jour = format(dat, "%m-%d"),
      Saison_Hydro = if_else(mois_jour >= "10-15", annee + 1, annee)
    ) %>%
    left_join(
      table_assec %>% select(NOM, annee, Assec_Saison = Assec) %>% mutate(annee = as.numeric(annee)),
      by = c("NOM", "Saison_Hydro" = "annee")
    ) %>%
    mutate(Assec_Saison = replace_na(Assec_Saison, "Evolage")) %>%
    left_join(
      table_assec %>% select(NOM, annee, Assec_Saison_Prec = Assec) %>% mutate(annee = as.numeric(annee) + 1),
      by = c("NOM", "Saison_Hydro" = "annee")
    ) %>%
    mutate(Assec_Saison_Prec = replace_na(Assec_Saison_Prec, "Evolage")) %>%
    left_join(vidanges_seules, by = c("NOM" = "NOM", "dat" = "date_exacte")) %>%
    left_join(peches_seules, by = c("NOM" = "NOM", "dat" = "date_exacte")) %>%
    mutate(
      RR_num = replace_na(as.numeric(RR), 0),
      ETP_num = replace_na(as.numeric(ETP_grille), 0),
      VFuite = round(0.1 * 3600 * 24) / 1000, 
      Vidange = replace_na(Vidange_bool, "non"),
      peche   = replace_na(peche_bool, "non"),
      
      Vidange = if_else(mois_jour >= "10-15" & Assec_Saison_Prec == "Assec", "non", Vidange),
      peche   = if_else(mois_jour >= "10-15" & Assec_Saison_Prec == "Assec", "non", peche)
    ) %>% 
    group_by(NOM) %>%
    arrange(dat) %>%
    mutate(
      Evenement_Marquant = case_when(
        peche == "oui" ~ "Peche",
        mois_jour == "10-15" ~ "Reprise_Automne",
        TRUE ~ NA_character_
      )
    ) %>%
    fill(Evenement_Marquant, .direction = "down") %>%
    mutate(
      Statut_Simu = case_when(
        Assec_Saison == "Assec" & Evenement_Marquant == "Peche" ~ "Assec",
        TRUE ~ "Evolage"
      ),
      Surface_Active_Ruissellement = if_else(Statut_Simu == "Assec", Surface_BV, Surface_BV - SURFACE_eau),
      Vp_etp = if_else(Statut_Simu == "Assec", 0, (RR_num - (ETP_num * 1.15)) * SURFACE_eau * 10),
      Vamont = 0,
      BF = case_when(dat == min(dat) & Statut_Simu == "Evolage" ~ Vmax / 2, TRUE ~ 0),
      RU_courante = 0,
      Volume_R = 0
    ) %>%
    ungroup() %>%
    select(-annee, -mois_jour, -Saison_Hydro, -Assec_Saison, -Assec_Saison_Prec, -Evenement_Marquant, -Vidange_bool, -peche_bool)
  
  liste_etangs <- df_bilan %>% split(.$NOM)
  Volume_Total_Exutoire_BV <- data.frame(dat = liste_etangs[[1]]$dat, Volume_Riviere = rep(0, nrow(liste_etangs[[1]])))
  
  liens <- tab_etg_data %>% select(NOM, Exutoire_1) %>% filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT" & Exutoire_1 != "") %>% distinct()
  ordre_topologique <- names(topo_sort(graph_from_data_frame(liens, directed = TRUE), mode = "out"))
  ordre_topologique <- ordre_topologique[ordre_topologique %in% names(liste_etangs)]
  
  # --- Boucle Topologique ---
  for (nom_etang in ordre_topologique) {
    etangs_calcule <- liste_etangs[[nom_etang]]
    N_jours <- nrow(etangs_calcule)
    
    # Remplissage initial des volumes de vidange (Opération vectorisée)
    etangs_calcule$Vol_Vidange_Jour <- 0
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
          jours_effectifs <- min(peches_futures[1] - t0, duree_vidange_normale)
        } else {
          jours_effectifs <- duree_vidange_normale
        }
        jours_effectifs <- min(jours_effectifs, N_jours - t0 + 1)
        
        if (jours_effectifs > 0) {
          etangs_calcule$Vol_Vidange_Jour[t0:(t0 + jours_effectifs - 1)] <- Vol_1ha_jour
        }
      }
    }
    
    # ---------------------------------------------------------
    # OPTIMISATION MAJEURE : EXTRACTION EN VECTEURS NATIFS R
    # ---------------------------------------------------------
    RR_vec <- etangs_calcule$RR_num
    ETP_vec <- etangs_calcule$ETP_num
    RU_max_vec <- etangs_calcule$RU_max
    Surf_Act_vec <- etangs_calcule$Surface_Active_Ruissellement
    Vamont_vec <- etangs_calcule$Vamont
    VFuite_vec <- etangs_calcule$VFuite
    Statut_vec <- etangs_calcule$Statut_Simu
    Peche_vec <- etangs_calcule$peche
    Vp_etp_vec <- etangs_calcule$Vp_etp
    Vol_Vid_Jour_vec <- etangs_calcule$Vol_Vidange_Jour
    Vmax_vec <- etangs_calcule$Vmax
    
    # Allocation des vecteurs de sortie (beaucoup plus rapide que modifier un dataframe)
    RU_courante_vec <- numeric(N_jours)
    Volume_R_vec <- numeric(N_jours)
    BF_vec <- numeric(N_jours)
    Vsortant_vec <- numeric(N_jours)
    Evap_Reelle_vec <- numeric(N_jours)
    Fuite_Reelle_vec <- numeric(N_jours)
    
    # Initialisation
    RU_courante_vec[1] <- RU_max_vec[1]
    BF_vec[1] <- etangs_calcule$BF[1]
    
    # BOUCLE EXTREMEMENT RAPIDE (C-level performance dans R)
    for (jour in 2:N_jours) {
      
      # 1. Modèle Sol (Ruissellement)
      ru_max <- RU_max_vec[jour]
      ru_prec <- RU_courante_vec[jour-1]
      pluie <- RR_vec[jour]
      
      Q_mm <- pluie * ((ru_prec / ru_max) ^ beta_val)
      I_mm <- pluie - Q_mm
      ETa_mm <- min(ETP_vec[jour], ru_prec + I_mm)
      
      bilan_sol <- ru_prec + I_mm - ETa_mm
      
      if (bilan_sol > ru_max) {
        RU_courante_vec[jour] <- ru_max
        ruiss_tot <- Q_mm + (bilan_sol - ru_max)
      } else if (bilan_sol < 0) {
        RU_courante_vec[jour] <- 0
        ruiss_tot <- Q_mm
      } else {
        RU_courante_vec[jour] <- bilan_sol
        ruiss_tot <- Q_mm
      }
      
      Vol_R <- ruiss_tot * Surf_Act_vec[jour] * 10 * C_transfert
      Volume_R_vec[jour] <- Vol_R
      
      # 2. Modèle Réservoir (Bfinal intégré)
      Eau_Dispo <- BF_vec[jour-1] + Vol_R + Vamont_vec[jour]
      VFuite <- VFuite_vec[jour]
      
      Fuite_Reelle <- min(VFuite, max(0, Eau_Dispo))
      Eau_Dispo <- Eau_Dispo - Fuite_Reelle
      Vsort <- Fuite_Reelle 
      
      vp <- Vp_etp_vec[jour]
      Statut <- Statut_vec[jour]
      Peche <- Peche_vec[jour]
      
      if (Statut == "Assec" || Peche == "oui") {
        Evap_Reelle <- max(0, vp)
      } else {
        Evap_Reelle <- if(vp < 0) max(vp, -Eau_Dispo) else vp 
      }
      
      Eau_Dispo <- Eau_Dispo + Evap_Reelle 
      
      v_mx <- Vmax_vec[jour]
      v_vid <- Vol_Vid_Jour_vec[jour]
      
      if (Peche == "oui") {
        Vsort <- Vsort + Eau_Dispo
        bf_fin <- 0
      } else if (v_vid > 0) {
        Obj_Vol <- max(0, BF_vec[jour-1] - v_vid)
        Vol_a_vider <- if(Eau_Dispo > Obj_Vol) Eau_Dispo - Obj_Vol else 0
        Vol_reel_vide <- min(max(Vol_a_vider, v_vid), max(0, Eau_Dispo))
        
        Vsort <- Vsort + Vol_reel_vide
        Eau_Dispo <- Eau_Dispo - Vol_reel_vide
        
        if (Eau_Dispo > v_mx) {
          Vsort <- Vsort + (Eau_Dispo - v_mx)
          bf_fin <- v_mx 
        } else {
          bf_fin <- Eau_Dispo
        }
      } else if (Statut == "Assec") {
        Vsort <- Vsort + Eau_Dispo
        bf_fin <- 0
      } else {
        if (Eau_Dispo > v_mx) {
          Vsort <- Vsort + (Eau_Dispo - v_mx)
          bf_fin <- v_mx 
        } else {
          bf_fin <- Eau_Dispo
        }
      } 
      
      BF_vec[jour] <- bf_fin
      Vsortant_vec[jour] <- Vsort
      Evap_Reelle_vec[jour] <- Evap_Reelle
      Fuite_Reelle_vec[jour] <- Fuite_Reelle
    }
    
    # ---------------------------------------------------------
    # REINJECTION DES DONNEES DANS LE TABLEAU
    # ---------------------------------------------------------
    etangs_calcule$RU_courante <- RU_courante_vec
    etangs_calcule$Volume_R <- Volume_R_vec
    etangs_calcule$BF <- BF_vec
    etangs_calcule$Vsortant <- Vsortant_vec
    etangs_calcule$Evap_Reelle <- Evap_Reelle_vec
    etangs_calcule$Fuite_Reelle <- Fuite_Reelle_vec
    
    liste_etangs[[nom_etang]] <- etangs_calcule
    
    # Propagation en Aval
    exutoire <- etangs_calcule$Exutoire_1[1]
    if (!is.na(exutoire) && exutoire != "") {
      if (exutoire != "OUTPUT" && exutoire %in% names(liste_etangs)) {
        liste_etangs[[exutoire]]$Vamont <- liste_etangs[[exutoire]]$Vamont + Vsortant_vec
      } else {
        Volume_Total_Exutoire_BV$Volume_Riviere <- Volume_Total_Exutoire_BV$Volume_Riviere + Vsortant_vec
      }
    }
  }
  
  print("Calcul termine avec succes.")
  return(list(liste_finale = liste_etangs, exutoire_data = Volume_Total_Exutoire_BV))
}

# ==============================================================================
# 3. BOUCLE D'EXÉCUTION AUTOMATIQUE SUR LES 6 SCÉNARIOS MÉTÉO
# ==============================================================================

beta = 4
RU = 200
coef = 0.2
date_heure <- format(Sys.time(), "%Y%m%d")

CODE_METEO_ACTUEL <- 2 

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

for (dossier_meteo in liste_dossiers_meteo) {
  
  nom_scenario <- basename(dossier_meteo)
  cat(paste("\n---> TRAITEMENT EN COURS :", nom_scenario, "<---\n"))
  
  chemin_meteo <- paste0(dossier_meteo, "/Meteo.csv")
  chemin_centro <- paste0(dossier_meteo, "/centro_BV.csv")
  
  if (!file.exists(chemin_meteo)) {
    cat(paste("ERREUR : Fichier Meteo.csv introuvable dans", dossier_meteo, "- Passage au suivant.\n"))
    next
  }
  
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
    filter(between(dat, as.Date("2010-01-01"), as.Date("2070-12-31"))) %>%
    arrange(dat)
  
  resultats_sim <- run_hydrological_model(
    pluvio_data = pluvio_scenario,
    tab_etg_data = tab_etg_base,
    RU_defaut = RU, 
    beta_val = beta,      
    C_transfert = coef
  )
  
  nom_fichier_sortie <- paste0("Analyse_R", beta, "_RU_", RU, "_Coef_", coef, "_Meteo_", nom_scenario, "_", date_heure, ".rds")
  saveRDS(resultats_sim, file = nom_fichier_sortie)
  
  cat(paste("Terminé ! Fichier sauvegardé :", nom_fichier_sortie, "\n"))
}

cat("\n===================================================================\n")
cat("TOUS LES SCÉNARIOS SONT TERMINÉS AVEC SUCCÈS.\n")
cat("===================================================================\n")