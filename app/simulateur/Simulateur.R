# ==============================================================================
# SCRIPT 2 : MOTEUR DE SIMULATION (simulateur.R)
# Objectif : Calculer le bilan hydrologique complet de la chaîne d'étangs
# ==============================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)

# Note : Assure-toi d'avoir exécuté importation.R avant (pour avoir tab_etg_base et pluvio_base)

# ==============================================================================
# 1. FONCTION DE BILAN JOURNALIER (La règle mathématique pure)
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
# 2. LA GRANDE FONCTION DE SIMULATION (Automatisée et Modulaire)
# ==============================================================================
run_hydrological_model <- function(pluvio_data, tab_etg_data, lambda_val = 0.20, jours_pant = 5) {
  
  print(paste(" Démarrage simulation | Lambda =", lambda_val, "| Mémoire =", jours_pant, "jrs"))
  
  # --- A. Préparation de la Pluie Antécédente (Pant) ---
  pluvio_calc <- pluvio_data %>%
    arrange(dat) %>%
    mutate(
      cumul_pant = rollsum(RR, k = jours_pant, fill = NA, align = "right"),
      Pant = lag(cumul_pant, n = 1, default = 0)
    ) %>%
    mutate(Pant = replace_na(Pant, 0))
  
  # --- B. Pivot des dates de gestion ---
  table_dates <- tab_etg_data %>%
    select(NOM, starts_with("Assec"), starts_with("Vidange"), starts_with("peche")) %>%
    pivot_longer(
      cols = -NOM,
      names_to = c(".value", "annee"),
      names_pattern = "([A-Za-z]+)(\\d{4})"
    ) %>%
    arrange(NOM, annee) %>%
    group_by(NOM) %>%
    mutate(Assec_Futur = coalesce(lead(assec), assec)) %>%
    ungroup()
  
  # --- C. Création du Bilan initial Vectorisé (Logique Shiny) ---
  df_bilan <- tab_etg_data %>%
    select(NOM, Surface_BV, SURFACE_eau, Vmax, CNI, CNII, CNIII, Exutoire_1) %>%
    cross_join(pluvio_calc) %>%
    mutate(annee = format(dat, "%Y")) %>%
    left_join(table_dates, by = c("NOM", "annee")) %>%
    mutate(
      mois = as.numeric(format(dat, "%m")),
      is_ete = (mois >= 6 & mois <= 9),
      Pant_num = replace_na(as.numeric(Pant), 0),
      
      # Logique de bascule du CN
      CN_jour = case_when(
        is_ete & Pant_num < 36 ~ CNI, is_ete & Pant_num > 53 ~ CNIII, is_ete ~ CNII,
        !is_ete & Pant_num < 13 ~ CNI, !is_ete & Pant_num > 28 ~ CNIII, !is_ete ~ CNII,
        TRUE ~ CNII
      ),
      S_cn = (25400 / CN_jour) - 254,
      Ia = lambda_val * S_cn,
      RR_num = replace_na(as.numeric(RR), 0),
      
      # Ruissellement
      Vol_R_brut = case_when(RR_num > Ia ~ ((RR_num - Ia)^2) / (RR_num - Ia + S_cn), TRUE ~ 0),
      CR = case_when(RR_num > 0 ~ Vol_R_brut / RR_num, TRUE ~ 0),
      VFuite = round(0.1 * 3600 * 24) / 1000,
      
      Vidange = if_else(dat == as.Date(Vidange), "oui", "non", missing = "non"),
      peche   = if_else(dat == as.Date(peche), "oui", "non", missing = "non")
    ) %>% 
    group_by(NOM, annee) %>%
    mutate(Statut_Simu = case_when(peche == "oui" | Vidange == "oui" ~ "Evolage", cumany(peche == "oui") ~ Assec_Futur, TRUE ~ assec)) %>% 
    ungroup() %>%
    mutate(
      # Adaptation de l'évaporation et du ruissellement selon le statut (Assec/Evolage)
      Surface_Active_Ruissellement = if_else(Statut_Simu == "Assec", Surface_BV, Surface_BV - SURFACE_eau),
      Volume_R = CR * RR_num * Surface_Active_Ruissellement * 10,
      Vp_etp = if_else(Statut_Simu == "Assec", 0, replace_na(as.numeric(P_ETP), 0) * SURFACE_eau * 10),
      Vamont = 0,
      BF = case_when(dat == min(dat) & assec == "Evolage" ~ Vmax / 2, TRUE ~ 0)
    ) %>%
    select(-annee, -assec, -Assec_Futur)
  
  liste_etangs <- df_bilan %>% split(.$NOM)
  Volume_Total_Exutoire_BV <- data.frame(dat = liste_etangs[[1]]$dat, Volume_Riviere = rep(0, nrow(liste_etangs[[1]])))
  
  # --- D. Routage Topologique ---
  liens <- tab_etg_data %>% select(NOM, Exutoire_1) %>% filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT") %>% distinct()
  ordre_topologique <- names(topo_sort(graph_from_data_frame(liens, directed = TRUE), mode = "out"))
  
  for (nom_etang in ordre_topologique) {
    etangs_calcule <- liste_etangs[[nom_etang]]
    Stockage_Vamont <- numeric(nrow(etangs_calcule))
    etangs_calcule$Vol_Vidange_Jour <- 0
    etangs_calcule$Vsortant <- 0  
    etangs_calcule$Evap_Reelle <- 0
    etangs_calcule$Fuite_Reelle <- 0
    
    lignes_vidange <- which(etangs_calcule$Vidange == "oui")
    lignes_peche <- which(etangs_calcule$peche == "oui")
    
    # Préchauffage des vidanges linéaires
    if (length(lignes_vidange) > 0) {
      for (t0 in lignes_vidange) {
        peches_futures <- lignes_peche[lignes_peche > t0]
        if (length(peches_futures) > 0) {
          tfin <- peches_futures[1]
          delta_T <- tfin - t0
          if (delta_T > 0) {
            Volume_Etang <- etangs_calcule$Vmax[t0]
            Surface_ha <- etangs_calcule$SURFACE_eau[t0] 
            Vol_1ha_jour <- Volume_Etang / Surface_ha
            jours_effectifs <- min(delta_T, ceiling(Surface_ha))
            etangs_calcule$Vol_Vidange_Jour[t0:(t0 + jours_effectifs - 1)] <- Vol_1ha_jour
          }
        }
      }
    }
    
    # Boucle temporelle (Jour par Jour)
    for (jour in 2:nrow(etangs_calcule)) {
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
    
    # Propagation de l'eau
    if (!is.na(exutoire)) {
      if (exutoire != "OUTPUT") {
        liste_etangs[[exutoire]]$Vamont <- liste_etangs[[exutoire]]$Vamont + Stockage_Vamont
      } else {
        Volume_Total_Exutoire_BV$Volume_Riviere <- Volume_Total_Exutoire_BV$Volume_Riviere + Stockage_Vamont
      }
    }
  }
  
  print("Calcul terminé avec succès.")
  return(list(liste_finale = liste_etangs, exutoire_data = Volume_Total_Exutoire_BV))
}

# ==============================================================================
# 3. EXÉCUTION DU MODÈLE ET SAUVEGARDE AUTOMATIQUE DES 2 SCÉNARIOS
# ==============================================================================
date_heure <- format(Sys.time(), "%Y%m%d")

# --- SCÉNARIO 1 : LE TEST DE BASE ---
cat("\n⏳ Lancement du scénario de BASE...\n")
resultats_sim_base <- run_hydrological_model(
  pluvio_data = pluvio_base,
  tab_etg_data = tab_etg_base,  # <--- On utilise le tableau NORMAL
  lambda_val = 0.05,
  jours_pant = 10
)
nom_fichier_base <- paste0("L0.05_Pant10_CN_base_", date_heure, ".rds")
saveRDS(resultats_sim_base, file = nom_fichier_base)


# --- SCÉNARIO 2 : LE TEST MODIFIÉ (+5) ---
cat("\n⏳ Lancement du scénario MODIFIÉ (+5)...\n")
resultats_sim_modif <- run_hydrological_model(
  pluvio_data = pluvio_base,
  tab_etg_data = tab_etg_modif, # <--- On utilise le tableau MODIFIÉ
  lambda_val = 0.05,
  jours_pant = 10
)
nom_fichier_modif <- paste0("L0.05_Pant10_CN_modif_", date_heure, ".rds")
saveRDS(resultats_sim_modif, file = nom_fichier_modif)

cat(paste("\n SUCCÈS TOTAL ! Les fichiers", nom_fichier_base, "et", nom_fichier_modif, "sont sauvegardés.\n"))