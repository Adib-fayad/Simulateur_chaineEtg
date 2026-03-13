library(sf)
library(dplyr)
library(stringr)

# =========================================================
# 1. PARAMÈTRES ET DOSSIERS
# =========================================================
dossier <- "SIG/GPKG"

fichiers_gpkg <- list.files(dossier, pattern = "\\.gpkg$", full.names = TRUE)


# =========================================================
# 2. LECTURE, NETTOYAGE ET FUSION
# =========================================================
liste_bv <- lapply(fichiers_gpkg, function(fichier) {
  
  # Lecture du polygone
  bv <- st_read(fichier, quiet = TRUE)
  
  # Extraction du nom du fichier
  nom_fichier <- tools::file_path_sans_ext(basename(fichier))
  
  #on remplace les "_" par des espaces
  nom_propre <- toupper(str_replace_all(nom_fichier, "_", " "))
  
  # On détruit toutes les colonnes puis on ajoute Etang
  bv_propre <- bv %>% 
    st_geometry() %>% 
    st_as_sf() %>% 
    mutate(Etang = nom_propre)
  
  return(bv_propre)
})

# On empile tous les bassins  dans une seule couche
tous_les_bv <- bind_rows(liste_bv) %>% st_make_valid()

# =========================================================
# 3. EXPORT DU FICHIER FUSIONNÉ POUR QGIS
# =========================================================
fichier_sortie <- "Tous_Les_BV.gpkg"
st_write(tous_les_bv, fichier_sortie, delete_dsn = TRUE, quiet = TRUE)

