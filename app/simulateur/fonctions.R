# ==============================================================================
# SCRIPT 3 : BOÎTE À OUTILS ET FONCTIONS MATHÉMATIQUES (fonctions.R)
# Objectif : Stocker les fonctions complexes appelées par le simulateur
# ==============================================================================

# Note : Les anciennes fonctions "calculer_cn_du_jour" et "ruisselement" ont été 
# supprimées car elles sont désormais calculées de manière vectorisée et 
# beaucoup plus rapide directement dans le script "simulateur.R".


# ==============================================================================
# 1. FONCTION DE BILAN HYDROLOGIQUE JOURNALIER (ROUTAGE DE L'ÉTANG)
# ==============================================================================
#' Calcule les flux d'eau entrants et sortants pour un étang sur une journée
Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Statut_Assec, Volume_Vidange_Jour, Peche_Jour) {
  
  # 1. Bilan initial d'eau disponible
  Eau_Dispo = BF + Volume_R + Vamont
  
  # 2. Gestion de la fuite (Infiltration profonde)
  Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
  Eau_Dispo = Eau_Dispo - Fuite_Reelle
  Vsortant = Fuite_Reelle 
  
  # 3. Gestion de l'évaporation (Directe et évapotranspiration)
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
  
  # 4. GESTION DES DIFFÉRENTS CAS DE FIGURES (Pêche, Vidange, Assec, Normal)
  
  # CAS A : Jour de pêche (On vide tout instantanément)
  if (Peche_Jour == "oui") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
    # CAS B : Période de vidange (On baisse le volume de X m3 par jour)
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
    
    # Sécurité trop plein
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
    
    # CAS C : Période d'Assec (La bonde est ouverte, tout part dans la rivière)
  } else if (Statut_Assec == "Assec") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
    # CAS D : Période normale d'Évolage (L'étang stocke l'eau jusqu'à Vmax)
  } else {
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
  } 
  
  # On retourne une liste complète des flux pour pouvoir les analyser plus tard
  return(list(
    BF = BF, 
    Vsortant = Vsortant, 
    Evap_Reelle = Evap_Reelle, 
    Fuite_Reelle = Fuite_Reelle
  ))
}


# ==============================================================================
# 2. FONCTION DE LECTURE DES DONNÉES TERRAIN (SONDES)
# ==============================================================================
#' Cherche et formate les données réelles mesurées sur le terrain pour un étang donné
load_terrain <- function(nom_etang) {
  # Formatage du nom pour éviter les erreurs de casse (ex: LIGNIERE vs Ligniere)
  nom_propre <- stringr::str_to_title(tolower(nom_etang))
  
  chemins_possibles <- c(
    paste0("data/Volume_etang/", nom_etang, ".Rdata"),
    paste0("data/Volume_etang/", nom_propre, ".Rdata"),
    paste0("data/Volume_etang/", tolower(nom_etang), ".Rdata")
  )
  
  chemin_valide <- chemins_possibles[file.exists(chemins_possibles)][1]
  
  if (!is.na(chemin_valide)) {
    env <- new.env()
    load(chemin_valide, envir = env)
    nom_obj <- ls(env)[1] 
    
    df <- env[[nom_obj]] %>%
      mutate(dat = as.Date(Date_jour)) %>%
      select(dat, Volume_Reel = Volume_m3) %>%
      drop_na(dat)
    
    return(df)
  }
  
  return(NULL) # Retourne vide si aucun fichier sonde n'est trouvé
}