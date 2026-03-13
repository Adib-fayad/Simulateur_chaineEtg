library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
source("fonctions.R")
#################################
#Choix des donnee meteo CHALAMONT OU MARLIEUX

SITE_CHOISI <- "SAFRAN"
# PRÉPARATION DES DONNÉES PLUIE (Calcul de Pant)
# On calcule d'abord Pant sur le fichier météo (car c'est le même pour tous les étangs)
# Pant = Pluie Antécédente sur 5 jours (excluant le jour même)

pluvio_calc <- pluvio %>%
  arrange(dat) %>%
  mutate(
    cumul_5j = rollsum(RR, k = 5, fill = NA, align = "right"),
    
    Pant = lag(cumul_5j, n = 1, default = 0)
  ) %>%
  # On remplace les NA
  mutate(Pant = replace_na(Pant, 0))

# CRÉATION DU TABLEAU GLOBAL (ETANGS x JOURS)
# On extrait les colonnes avec des années et on les transforme en lignes
table_dates <- tab_etg %>%
  select(NOM, starts_with("Assec"), starts_with("Vidange"), starts_with("peche")) %>%
  pivot_longer(
    cols = -NOM,
    names_to = c(".value", "annee"),       # Sépare le mot (Assec) de l'année (2010)
    names_pattern = "([A-Za-z]+)(\\d{4})"  # Ex: "Vidange2010" devient "Vidange", annee "2010"
  ) %>%
  arrange(NOM, annee) %>%
  group_by(NOM) %>%
  mutate(
    # Le statut futur est l'Assec de l'année suivante (ou l'actuel si c'est la fin)
    Assec_Futur = coalesce(lead(assec), assec) 
  ) %>%
  ungroup()

# ====================================================================
# ÉTAPE 2 : La création unique du Bilan (Valable pour TOUS les sites)
# ====================================================================
df_bilan <- tab_etg %>%
  # 1. On ne garde que les caractéristiques fixes de l'étang
  select(NOM, Surface_BV, SURFACE_eau, SURFACE_eau, Vmax, CNI, CNII, CNIII, Exutoire_1, Position) %>%
  
  # 2. On croise avec la météo choisie plus haut (pluvio_calc)
  cross_join(pluvio_calc) %>%
  
  # 3. On extrait l'année météo en cours
  mutate(annee = format(dat, "%Y")) %>%
  
  # 4. On rattache automatiquement les bonnes dates de l'année !
  left_join(table_dates, by = c("NOM", "annee")) %>%
  
  # 5. Calculs hydrologiques
  mutate(
    CN_jour = calculer_cn_du_jour(Pant, CNI, CNII, CNIII, dat),
    CR = case_when(
      RR > 0 ~ (ruisselement(RR, CN_jour)) / RR,
      TRUE ~ 0
    ),
    Volume_R = CR * RR * (Surface_BV - SURFACE_eau) * 10,
    VFuite = round(0.1 * 3600 * 24) / 1000,
    Vp_etp = P_ETP * SURFACE_eau * 10,
    
    # Validation des dates de vidange et pêche
    Vidange = if_else(dat == as.Date(Vidange), "oui", "non", missing = "non"),
    peche   = if_else(dat == as.Date(peche), "oui", "non", missing = "non"),
    
    Vamont = 0,
    
    # Initialisation pour le tout premier jour de la simulation 
    BF = case_when(
      dat == as.Date("2010-01-01") & assec == "Evolage" ~ Vmax / 2, 
      TRUE ~ 0
    )
  ) %>% 
  
  # 6. Détermination du statut (Assec / Evolage)
  group_by(NOM, annee) %>%
  mutate(
    Statut_Simu = case_when(
      peche == "oui" | Vidange == "oui" ~ "Evolage",
      cumany(peche == "oui") ~ Assec_Futur,
      TRUE ~ assec
    )
  ) %>% 
  ungroup() %>%
  
  # Nettoyage des colonnes temporaires
  select(-annee, -assec, -Assec_Futur)

# ====================================================================
# FIN DU REMPLACEMENT - Tu peux laisser la suite de ton code !
# ====================================================================


liste_etangs <- df_bilan %>% split(.$NOM)


#On donne juste les deux colonnes à igraph (De qui -> Vers qui)
liens <- df_bilan %>% 
  select(NOM, Exutoire_1) %>% 
  filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT") # On enlève les vides et la fin

# On crée le réseau mathématique
reseau <- graph_from_data_frame(liens, directed = TRUE)

# On demande l'ordre parfait de calcul 
ordre_topologique <- names(topo_sort(reseau, mode = "out"))


## boucle de calcule BF



for (nom_etang in ordre_topologique) {
  print(paste("Calcul de l'étang :", nom_etang))
  etangs_calcule <- liste_etangs[[nom_etang]]
  Stockage_Vamont <- numeric(nrow(etangs_calcule))
  
  etangs_calcule$Vol_Vidange_Jour <- 0
  
  # Repérage des jours clés
  lignes_vidange <- which(etangs_calcule$Vidange == "oui")
  lignes_peche <- which(etangs_calcule$peche == "oui")
  
  #VIDANGE LINÉAIRE (1 ha/jour)
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
          
          jours_pour_vider <- ceiling(Surface_ha)
          jours_effectifs <- min(delta_T, jours_pour_vider)
          
          jours_actifs <- t0:(t0 + jours_effectifs - 1)
          etangs_calcule$Vol_Vidange_Jour[jours_actifs] <- Vol_1ha_jour
        }
      }
    }
  }
  for (jour in 2:nrow(etangs_calcule)) {
    
    statut_du_jour <- etangs_calcule$Statut_Simu[jour]
    
    resultat <- Bfinal(
      Vmax = etangs_calcule$Vmax[jour],
      BF = etangs_calcule$BF[jour-1] ,
      Vp_etp = etangs_calcule$Vp_etp[jour], 
      Volume_R = etangs_calcule$Volume_R[jour],
      Vamont = etangs_calcule$Vamont[jour],
      VFuite = etangs_calcule$VFuite[jour], 
      Statut_Assec = statut_du_jour,
      Volume_Vidange_Jour = etangs_calcule$Vol_Vidange_Jour[jour],
      Peche_Jour = etangs_calcule$peche[jour]
    )
    
    etangs_calcule$BF[jour] = resultat$BF
    Stockage_Vamont[jour] = resultat$Vsortant
  }
  
  liste_etangs[[nom_etang]] <- etangs_calcule
  
  exutoire <- etangs_calcule$Exutoire_1[1]
  
  if (!is.na(exutoire) && exutoire != "OUTPUT") {
    
    liste_etangs[[exutoire]]$Vamont <- liste_etangs[[exutoire]]$Vamont + Stockage_Vamont
  }
  
}


liste_etangs[["CORVEYZIEUX"]] %>% select(dat, RR, Pant, CN_jour,CR,Volume_R,Vamont,BF,peche,Vidange,Vol_Vidange_Jour) %>% filter(BF==0)



liste_etangs[["CORVEYZIEUX"]] %>% 
  select(dat,SURFACE_eau, RR, Pant, CN_jour, CR, Volume_R, Vamont, BF, peche, Vidange, Vol_Vidange_Jour) %>% 
  filter(between(dat, as.Date("2023-10-30"), as.Date("2023-11-16")))


df<- df_bilan %>% select(,c("NOM","Vidange","dat")) %>% filter(Vidange=="oui")



