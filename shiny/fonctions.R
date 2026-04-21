# ==============================================================================
# CALCUL DU CURVE NUMBER (CN) JOURNALIER
# Rôle : Déterminer le coefficient de ruissellement selon l'état hydrique du sol.
# Méthode : Classification par seuils de précipitations antérieures (été/hiver).
# ==============================================================================
calculer_cn_du_jour <- function(Pant, CNI, CNII, CNIII, dat) {
  # Sécurisation des types de données pour les calculs arithmétiques
  Pant <- as.numeric(Pant)
  CNI <- as.numeric(CNI)
  CNII <- as.numeric(CNII)
  CNIII <- as.numeric(CNIII)
  
  # Traitement des données manquantes (NA) pour éviter les ruptures de calcul
  Pant[is.na(Pant)] <- 0
  
  # Détermination du mois de simulation pour la gestion de la saisonnalité
  mois <- as.numeric(format(as.Date(dat), "%m"))
  mois[is.na(mois)] <- 1 
  
  # Définition de la période estivale (mai à octobre inclus)
  is_ete <- (mois >= 5 & mois <= 10)
  
  # Initialisation du vecteur de sortie (pré-allocation de mémoire)
  res <- rep(0, length(Pant))
  
  # Boucle itérative pour l'affectation du CN jour par jour
  for(i in seq_along(Pant)) {
    # Vérification de l'intégrité de la donnée source CNII
    if(is.na(CNII[i])) {
      res[i] <- 0
      next
    }
    
    p <- Pant[i]
    
    # Application de la logique conditionnelle saisonnière et hydrique
    if(is_ete[i]) {
      # Seuils spécifiques à la période estivale
      if(p < 36) res[i] <- CNI[i]
      else if(p > 53) res[i] <- CNIII[i]
      else res[i] <- CNII[i]
    } else {
      # Seuils spécifiques à la période hivernale (sol saturé plus rapidement)
      if(p < 13) res[i] <- CNI[i]
      else if(p > 28) res[i] <- CNIII[i]
      else res[i] <- CNII[i]
    }
  }
  return(res)
}

# ==============================================================================
# CALCUL DU RUISSELLEMENT (MÉTHODE SCS-CN)
# Rôle : Calculer la lame d'eau ruisselée à partir de la pluie et du CN.
# ==============================================================================
ruisselement <- function(RR, CN_jour) {
  # Normalisation numérique des paramètres d'entrée
  RR <- as.numeric(RR)
  CN_jour <- as.numeric(CN_jour)
  
  res <- rep(0, length(RR))
  
  # Itération sur la série temporelle
  for(i in seq_along(RR)) {
    # Filtrage des cas d'erreur ou d'absence de données
    if(is.na(RR[i]) || is.na(CN_jour[i]) || CN_jour[i] <= 0) {
      res[i] <- 0
    } else {
      # Calcul de S (Rétention maximale potentielle du sol)
      S <- (25400 / CN_jour[i]) - 254
      # Calcul de Ia (Pertes initiales : interception et stockage de surface)
      Ia <- 0.2 * S
      
      # Application de l'équation du ruissellement effectif
      if(RR[i] > Ia) {
        res[i] <- ((RR[i] - Ia)^2) / (RR[i] - Ia + S)
      } else {
        # Si la pluie est inférieure aux pertes initiales, le ruissellement est nul
        res[i] <- 0
      }
    }
  }
  return(res)
}

# ==============================================================================
# BILAN HYDROLOGIQUE JOURNALIER (FONCTION BFINAL)
# Rôle : Calculer le volume stocké et les sorties d'eau d'un étang.
# ==============================================================================
Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Statut_Assec, Volume_Vidange_Jour, Peche_Jour){
  
  # Évaluation de l'eau totale disponible avant pertes et évacuation
  Eau_Dispo = BF + Volume_R + Vamont
  
  # Calcul de la fuite (infiltration) plafonnée par le stock disponible
  Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
  Eau_Dispo = Eau_Dispo - Fuite_Reelle
  Vsortant = Fuite_Reelle 
  
  # Gestion du bilan atmosphérique (Pluie directe - Évapotranspiration)
  if (Statut_Assec == "Assec" || Peche_Jour == "oui") {
    # Cas particulier : pas de stock, seule la pluie positive est considérée
    Evap_Reelle = max(0, Vp_etp)
  } else {
    if (Vp_etp < 0) {
      # Évaporation limitée par la ressource en eau physique
      Evap_Reelle = max(Vp_etp, -Eau_Dispo) 
    } else {
      Evap_Reelle = Vp_etp 
    }
  }
  Eau_Dispo = Eau_Dispo + Evap_Reelle 
  
  # Branchement décisionnel selon l'état de gestion piscicole et hydraulique
  
  # CAS 1 : Opération de pêche (vidange totale immédiate)
  if (Peche_Jour == "oui") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
    # CAS 2 : Phase de vidange (baisse progressive du niveau)
  } else if (Volume_Vidange_Jour > 0) {
    
    # Détermination du volume cible par rapport au stock de la veille
    Objectif_Volume = max(0, BF - Volume_Vidange_Jour)
    
    # Calcul du volume à évacuer pour atteindre ou maintenir l'objectif
    if (Eau_Dispo > Objectif_Volume) {
      Volume_a_vider = Eau_Dispo - Objectif_Volume
      # Garantie d'évacuation minimale du quota défini
      Volume_theorique = max(Volume_a_vider, Volume_Vidange_Jour)
    } else {
      Volume_theorique = Volume_Vidange_Jour
    }
    
    # Application de la sortie réelle (bridée par la disponibilité d'eau)
    Volume_reel_vide = min(Volume_theorique, max(0, Eau_Dispo))
    
    Vsortant = Vsortant + Volume_reel_vide
    Eau_Dispo = Eau_Dispo - Volume_reel_vide
    
    # Gestion de la sécurité de trop-plein malgré la vidange (pluie forte)
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
    
    # CAS 3 : Étang en assec (vanne ouverte, transit total des apports)
  } else if (Statut_Assec == "Assec") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
    # CAS 4 : Fonctionnement normal en évolage (stockage et trop-plein)
  } else {
    if (Eau_Dispo > Vmax) {
      # Évacuation de l'excédent par l'ouvrage de régulation (moine)
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
  } 
  
  return(list(BF = BF, Vsortant = Vsortant))
}