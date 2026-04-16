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
# Peche <- function(peche,dat){
#   jour <- format(dat, "-%m-%d")
#   ifelse(jour == peche, "oui", "non")
#   
# }
# 
# vidange <- function(Vidange,dat){
#   jour <- format(dat, "-%m-%d")
#   ifelse(jour == Vidange, "oui", "non")
#   
# }


Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Statut_Assec, Volume_Vidange_Jour,Peche_Jour){
  
  Eau_Dispo = BF + Volume_R + Vamont
  
  # LA FUITE
  Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
  Eau_Dispo = Eau_Dispo - Fuite_Reelle
  Vsortant = Fuite_Reelle 
  
  # L'ÉVAPORATION
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
  
  # jour de peche on vide tt l'étang
  if (Peche_Jour == "oui") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
    # si autre et si on a un volume vidanger 
  } else if (Volume_Vidange_Jour > 0) {
    
    # L'objectif d est de faire baisser le volume par rapport à HIER
    
    Objectif_Volume = max(0, BF - Volume_Vidange_Jour)
    
    # On évacue tout ce qui dépasse cet objectif (le quota + la pluie + l'amont)
    if (Eau_Dispo > Objectif_Volume) {
      Volume_a_vider = Eau_Dispo - Objectif_Volume
      # on évacue AU MOINS notre quota 
      Volume_theorique = max(Volume_a_vider, Volume_Vidange_Jour)
    } else {
      # Si l'étang est déjà en dessous de l'objectif, on maintient le quota habituel
      Volume_theorique = Volume_Vidange_Jour
    }
    
    Volume_reel_vide = min(Volume_theorique, max(0, Eau_Dispo))
    
    # On met à jour les stocks
    Vsortant = Vsortant + Volume_reel_vide
    Eau_Dispo = Eau_Dispo - Volume_reel_vide
    
    # Sécurité (si pluie extrême)
    if (Eau_Dispo > Vmax) {
      Surplus = Eau_Dispo - Vmax
      Vsortant = Vsortant + Surplus
      BF = Vmax 
    } else {
      BF = Eau_Dispo
    }
    
    # CAS 3 : L'étang est vide (Assec) - Il laisse tout passer
  } else if (Statut_Assec == "Assec") {
    Vsortant = Vsortant + Eau_Dispo
    BF = 0
    
    # CAS 4 : L'étang est en culture (Evolage) - Il se remplit
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
  