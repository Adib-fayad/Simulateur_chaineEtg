# fonction de calcul de la valeur du CN

calculer_cn_du_jour <- function(Pant, CNI, CNII, CNIII, dat) {
  
  mois <- as.numeric(format(dat, "%m"))
  
  ifelse(
    mois >= 5 & mois <= 10, 
    
    case_when(
      Pant < 36 ~ CNI,
      Pant > 53 ~ CNIII,
      TRUE      ~ CNII
    ),
    
    case_when(
      Pant < 13 ~ CNI,
      Pant > 28 ~ CNIII,
      TRUE      ~ CNII
    )
  )
}

ruisselement <- function(RR, CN_jour) {
  
  S = (25400 / CN_jour) - 254
  Ia = 0.2 * S
  
  case_when(
    RR > Ia  ~ ((RR - Ia)^2) / (RR - Ia + S),
    RR <= Ia ~ 0
  )
}
Peche <- function(peche,dat){
  jour <- format(dat, "-%m-%d")
  ifelse(jour == peche, "oui", "non")
  
}

vidange <- function(Vidange,dat){
  jour <- format(dat, "-%m-%d")
  ifelse(jour == Vidange, "oui", "non")
  
}


Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Statut_Assec, Volume_Vidange_Jour){
  
  Eau_Dispo = BF + Volume_R + Vamont
  
  # LA FUITE
  Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
  Eau_Dispo = Eau_Dispo - Fuite_Reelle
  Vsortant = Fuite_Reelle 
  
  # L'ÉVAPORATION
  if (Statut_Assec == "Assec") {
    Evap_Reelle = max(0, Vp_etp)
  } else {
    if (Vp_etp < 0) {
      Evap_Reelle = max(Vp_etp, -Eau_Dispo) 
    } else {
      Evap_Reelle = Vp_etp 
    }
  }
  Eau_Dispo = Eau_Dispo + Evap_Reelle 
  
  # GESTION DU NIVEAU ET DE LA VIDANGE
  if (Volume_Vidange_Jour > 0) {
    
    Volume_reel_vide = min(Volume_Vidange_Jour, max(0, Eau_Dispo))
    Vsortant = Vsortant + Volume_reel_vide
    Eau_Dispo = Eau_Dispo - Volume_reel_vide
    
    # Sécurité au cas où il pleut énormément pendant la vidange
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
    
    # La vidange est finie, et l'étang est officiellement en Assec
  } else if (Statut_Assec == "Assec") {
    # L'étang ne garde rien
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
    #L'étang est en Evolage il se remplit ou déborde
  } else {
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
  } 
  return(list(BF = BF, Vsortant = Vsortant))
}
  