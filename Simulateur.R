library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
source("fonctions.R")
#################################
#Choix des donnee meteo CHALAMONT OU MARLIEUX

SITE_CHOISI <- "CHALAMONT"
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

if (SITE_CHOISI == "MARLIEUX"){

  df_bilan <- tab_etg %>%
    # On ne garde que les colonnes utiles 
    select(NOM, Surface_BV,SURFACE_SI, Vmax, CNI, CNII, CNIII, Vidange,peche, Exutoire_1,Position,num_range("Assec", 2021:2025)) %>%
    # Fusion (chaque étang reçoit toute la chronologie météo)
    cross_join(pluvio_calc) %>% 
    # Calcul du CN du jour ligne par ligne
    mutate(
      CN_jour = calculer_cn_du_jour(Pant, CNI, CNII, CNIII,dat),
      CR = case_when(
        RR>0 ~ (ruisselement(RR,CN_jour))/RR,
        RR<= 0 ~ 0
      ),
      Volume_R = CR*RR*(Surface_BV-SURFACE_SI)*10,
      # d=0.1,
      VFuite=round(0.1*3600*24)/1000,
      Vp_etp = P_ETP*SURFACE_SI*10,
      Vidange= vidange(Vidange,dat),
      peche= Peche(peche,dat),
      Vamont= 0,
      BF= case_when(
        format(dat, "%Y-%m-%d") == "2021-01-01" & Assec2021 == "Evolage" ~ Vmax/2,  
        TRUE ~ 0
      )
    )
  
  df_bilan <- df_bilan %>%
    arrange(NOM, dat) %>% 
    
    group_by(NOM, annee_calcul = format(dat, "%Y")) %>%     
    
    mutate(
      across(
        .cols = num_range("Assec", 2021:2025),
        .fns = ~ case_when(
          cumany(Vidange == "oui") ~ "Evolage", # Ne bascule en Evolage que pour la fin de CETTE année-là
          TRUE ~ .x                             
        )
      )
    ) %>%
    ungroup() %>%
    select(-annee_calcul)
}else if (SITE_CHOISI == "CHALAMONT") {
  df_bilan <- tab_etg %>%
    # On ne garde que les colonnes utiles 
    select(NOM, Surface_BV,SURFACE_SI, Vmax, CNI, CNII, CNIII, Vidange,peche, Exutoire_1,Position,num_range("Assec", 2022:2023)) %>%
    # Fusion (chaque étang reçoit toute la chronologie météo)
    cross_join(pluvio_calc) %>% 
    # Calcul du CN du jour ligne par ligne
    mutate(
      CN_jour = calculer_cn_du_jour(Pant, CNI, CNII, CNIII,dat),
      CR = case_when(
        RR>0 ~ (ruisselement(RR,CN_jour))/RR,
        RR<= 0 ~ 0
      ),
      Volume_R = CR*RR*(Surface_BV-SURFACE_SI)*10,
      # d=0.1,
      VFuite=round(0.1*3600*24)/1000,
      Vp_etp = P_ETP*SURFACE_SI*10,
      Vidange= vidange(Vidange,dat),
      peche= Peche(peche,dat),
      Vamont= 0,
      BF= case_when(
        format(dat, "%Y-%m-%d") == "2022-01-01" & Assec2022 == "Evolage" ~ Vmax/2,  
        TRUE ~ 0
      )
    )
  
  df_bilan <- df_bilan %>%
    arrange(NOM, dat) %>% 
    
    group_by(NOM, annee_calcul = format(dat, "%Y")) %>%     
    
    mutate(
      across(
        .cols = num_range("Assec", 2022:2023),
        .fns = ~ case_when(
          cumany(Vidange == "oui") ~ "Evolage", # Ne bascule en Evolage que pour la fin de CETTE année-là
          TRUE ~ .x                             
        )
      )
    ) %>%
    ungroup() %>%
    select(-annee_calcul)
}
head(df_bilan)

liste_etangs <- df_bilan %>% split(.$NOM)

#liste_etangs[[1]] %>% select(dat, RR, Pant, CN_jour)


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
  etangs_calcule<- liste_etangs[[nom_etang]]
  
  Stockage_Vamont <- numeric(nrow(etangs_calcule))
  
  #calcule boucle exponetielle
  etangs_calcule$Vol_Vidange_Jour <- 0
  #numero des ligne de vidange et peche pour calculer le nbre de jours
  lignes_vidange <- which(etangs_calcule$Vidange == "oui")
  lignes_peche <- which(etangs_calcule$peche == "oui")
  
  if (length(lignes_vidange) > 0) {
    # On parcourt chaque date d'ouverture de bonde
    for (t0 in lignes_vidange) {
      
      # On cherche uniquement les pêches qui sont APRÈS l'ouverture
      peches_futures <- lignes_peche[lignes_peche > t0]
      
      if (length(peches_futures) > 0) {
        
        tfin <- peches_futures[1] # On prend le jour de pêche le plus proche
        delta_T <- tfin - t0
        
        if (delta_T > 0) {
          # Paramètres de la courbe de tarissement
          # On prend par exemple 30% du Vmax pour le premier jour de vidange, et 5% pour le dernier
          Volume_Etang = etangs_calcule$Vmax[t0]
          Qmax <- Volume_Etang * 0.30  
          Qmin <- Volume_Etang * 0.05
          
          # Calcul de la chute exponentielle
          k <- -(1 / delta_T) * log(Qmin / Qmax)
          jours_ecoules <- 0:delta_T
          
          # On injecte la belle courbe lisse sur les X jours
          etangs_calcule$Vol_Vidange_Jour[t0:tfin] <- Qmax * exp(-k * jours_ecoules)
        }
      }
    }
  }
  forcage_ouverture_assec <- FALSE
  for (jour in 2:nrow(etangs_calcule)) {
    
    if (format(etangs_calcule$dat[jour], "%m-%d") == "01-01") {
      forcage_ouverture_assec <- FALSE 
    }
    
    annee_actuelle <- format(etangs_calcule$dat[jour], "%Y") 
    nom_colonne <- paste0("Assec", annee_actuelle)
    statut_du_jour <- as.character(etangs_calcule[jour, nom_colonne])
    
    #jour de la peche
    if (etangs_calcule$peche[jour] == "oui") {
      # On regarde l'année d'après
      annee_prochaine <- as.character(as.numeric(annee_actuelle) + 1)
      colonne_prochaine <- paste0("Assec", annee_prochaine)
    # Si l'année prochaine est prévue en Assec laisse ouvert
      if (colonne_prochaine %in% names(etangs_calcule)) {
        if (etangs_calcule[jour, colonne_prochaine] == "Assec") {
          forcage_ouverture_assec <- TRUE
        }
      }
    }
    if (forcage_ouverture_assec == TRUE) {
      statut_du_jour <- "Assec"
    }  
    
    resultat <- Bfinal(
      Vmax = etangs_calcule$Vmax[jour],
      BF = etangs_calcule$BF[jour-1] ,
      Vp_etp = etangs_calcule$Vp_etp[jour], 
      Volume_R = etangs_calcule$Volume_R[jour],
      Vamont = etangs_calcule$Vamont[jour],
      VFuite = etangs_calcule$VFuite[jour], 
      Statut_Assec = statut_du_jour,       # <--- R utilise le statut corrigé !
      Volume_Vidange_Jour = etangs_calcule$Vol_Vidange_Jour[jour]
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


liste_etangs[["CORVEYZIEUX"]] %>% select(dat, RR, Pant, CN_jour,CR,Volume_R,Vamont,BF)





df<- df_bilan %>% select(,c("NOM","Vidange","dat")) %>% filter(Vidange=="oui")



