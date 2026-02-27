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

vidange <- function(Vidange,dat){
  jour <- format(dat, "-%m-%d")
  ifelse(jour == Vidange, "oui", "non")
  
}


Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Vidange, Statut_Assec){
  
  Eau_Dispo = BF + Volume_R + Vamont
  
  #LA FUITE  ne peut pas dépasser l'eau disponible
  Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
  Eau_Dispo = Eau_Dispo - Fuite_Reelle
  Vsortant = Fuite_Reelle 
  
  #L'ÉVAPORATION (Vp_etp est négatif en été, positif s'il pleut)
  if (Statut_Assec == "Assec") {
    # on pert pas le peux d eau qui tombe mais on en pert pas plus.
    Evap_Reelle = max(0, Vp_etp)
  } else {
    if (Vp_etp < 0) {
      # on ne peut évaporer que l'eau qui existe vraiment
      Evap_Reelle = max(Vp_etp, -Eau_Dispo) 
    } else {
      Evap_Reelle = Vp_etp 
    }
  }
  Eau_Dispo = Eau_Dispo + Evap_Reelle 
  
  
  if (Statut_Assec == "Assec") {
    # L'étang ne garde rien, tout ce qui reste part en aval
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
  } else if (Statut_Assec == "Evolage") {
    
    if (Vidange == "oui") {
      # Toute l'eau part en une seule fois a voir comment lisser sur plusieur jours.
      Volume_vidange = Eau_Dispo
      
      Vsortant = Vsortant + Volume_vidange
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
  }
  
  return(list(BF = BF, Vsortant = Vsortant))
}