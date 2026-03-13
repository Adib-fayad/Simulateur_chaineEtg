library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
source("fonctions.R")
#################################
#Choix des donnee meteo CHALAMONT OU MARLIEUX

SITE_CHOISI <- "MARLIEUX"
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
    select(NOM, Surface_BV,SURFACE_SI, Vmax, CNI, CNII, CNIII, Exutoire_1,Position,
           num_range("Assec", 2021:2025), starts_with("peche"), starts_with("Vidange")) %>%    # Fusion (chaque étang reçoit toute la chronologie météo)
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
      annee_en_cours = format(dat, "%Y"),
      
      Vidange = case_when(
        annee_en_cours == "2021" & dat == as.Date(Vidange2021) ~ "oui",
        annee_en_cours == "2022" & dat == as.Date(Vidange2022) ~ "oui",
        annee_en_cours == "2023" & dat == as.Date(Vidange2023) ~ "oui",
        annee_en_cours == "2024" & dat == as.Date(Vidange2024) ~ "oui",
        annee_en_cours == "2025" & dat == as.Date(Vidange2025) ~ "oui",
        TRUE ~ "non"
      ),
      
      peche = case_when(
        annee_en_cours == "2021" & dat == as.Date(peche2021) ~ "oui",
        annee_en_cours == "2022" & dat == as.Date(peche2022) ~ "oui",
        annee_en_cours == "2023" & dat == as.Date(peche2023) ~ "oui",
        annee_en_cours == "2024" & dat == as.Date(peche2024) ~ "oui",
        annee_en_cours == "2025" & dat == as.Date(peche2025) ~ "oui",
        TRUE ~ "non"
      ),
      Vamont= 0,
      BF= case_when(
        format(dat, "%Y-%m-%d") == "2021-01-01" & Assec2021 == "Evolage" ~ Vmax/2,  
        TRUE ~ 0
      )) %>% 
     group_by(NOM, annee = format(dat, "%Y")) %>%     
    
    mutate(
      S_Actuel = case_when(
        annee == "2021" ~ Assec2021, 
        annee == "2022" ~ Assec2022,
        annee == "2023" ~ Assec2023, 
        annee == "2024" ~ Assec2024,
        annee == "2025" ~ Assec2025, 
        TRUE ~ "Evolage"
      ),
      S_Futur = case_when(
        annee == "2021" ~ Assec2022, 
        annee == "2022" ~ Assec2023,
        annee == "2023" ~ Assec2024, 
        annee == "2024" ~ Assec2025,
        TRUE ~ S_Actuel
      ),                            
      Statut_Simu = case_when(
        peche == "oui" | Vidange == "oui" ~ "Evolage",
        cumany(peche == "oui") ~ S_Futur,
        TRUE ~ S_Actuel
      )
    ) %>% ungroup() %>% select(-annee, -S_Actuel, -S_Futur)
}else if (SITE_CHOISI == "CHALAMONT") {
  df_bilan <- tab_etg %>%
    # On ne garde que les colonnes utiles 
    select(NOM, Surface_BV,SURFACE_SI, Vmax, CNI, CNII, CNIII, Exutoire_1,Position,
           num_range("Assec", 2022:2023), starts_with("peche"), starts_with("Vidange")) %>%    # Fusion (chaque étang reçoit toute la chronologie météo)
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
      annee_en_cours = format(dat, "%Y"),
      
      Vidange = case_when(
        annee_en_cours == "2022" & dat == as.Date(Vidange2022) ~ "oui",
        annee_en_cours == "2023" & dat == as.Date(Vidange2023) ~ "oui",
        TRUE ~ "non"
      ),
      
      peche = case_when(
        annee_en_cours == "2022" & dat == as.Date(peche2022) ~ "oui",
        annee_en_cours == "2023" & dat == as.Date(peche2023) ~ "oui",
        TRUE ~ "non"
      ),
      Vamont= 0,
      BF= case_when(
        format(dat, "%Y-%m-%d") == "2022-01-01" & Assec2022 == "Evolage" ~ Vmax/2,  
        TRUE ~ 0
      )
    ) %>%
  group_by(NOM, annee = format(dat, "%Y")) %>%
  mutate(
    S_Actuel = case_when(
      annee == "2022" ~ Assec2022, 
      annee == "2023" ~ Assec2023, 
      TRUE ~ "Evolage"
    ),
    S_Futur = case_when(
      annee == "2022" ~ Assec2023, 
      TRUE ~ S_Actuel
    ),
    Statut_Simu = case_when(
      peche == "oui" | Vidange == "oui" ~ "Evolage",
      cumany(peche == "oui") ~ S_Futur,
      TRUE ~ S_Actuel
    )
  ) %>% ungroup() %>% select(-annee, -S_Actuel, -S_Futur)
}
df_bilan <- df_bilan %>%
  select(
    -starts_with("Assec"),         
    -matches("^peche\\d{4}$"),     
    -matches("^Vidange\\d{4}$"),  
    -annee_en_cours                
  )

liste_etangs <- df_bilan %>% split(.$NOM)
liste_etangs[["VIEUX"]] %>% select(dat, RR, Pant, CN_jour,Vidange,peche,Statut_Simu,an) %>% filter(an == 2023)

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
          
          Surface_ha <- etangs_calcule$SURFACE_SI[t0] 
          
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
  select(dat,SURFACE_SI, RR, Pant, CN_jour, CR, Volume_R, Vamont, BF, peche, Vidange, Vol_Vidange_Jour) %>% 
  filter(between(dat, as.Date("2023-10-30"), as.Date("2023-11-16")))


df<- df_bilan %>% select(,c("NOM","Vidange","dat")) %>% filter(Vidange=="oui")



