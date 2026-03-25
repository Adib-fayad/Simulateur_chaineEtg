library(sf)           # Pour manipuler les vecteurs (points, lignes, polygones)
library(terra)        # Pour manipuler les rasters (MNT, Accumulation)
library(dplyr)        # Pour filtrer et trier les tableaux de données
library(qgisprocess)  # Pour appeler les algorithmes de QGIS/GRASS
library(DBI)          # Pour interagir avec la base de données
library(RSQLite)      # Pilote pour lire le format GeoPackage

# ====================================================================
# ÉTAPE 1 : CONFIGURATION ET CHARGEMENT DES DONNÉES
# ====================================================================
print("--- DÉMARRAGE DU TRAITEMENT HYDROLOGIQUE ---")

options(qgisprocess.path = "C:/Users/adfayad/AppData/Local/Programs/OSGeo4W/bin/qgis_process-qgis.bat")
qgis_configure()

chemin_gpkg   <- "GPKG_Sortie/BV_2_Complet.gpkg"

# Chargement depuis les fichiers sources originaux
thoux    <- st_read("SIG/Thou/ouvrage chalamont.gpkg", quiet = TRUE) %>% st_transform(2154)
routes   <- st_read("SIG/Thou/route depression.gpkg",  quiet = TRUE) %>% st_transform(2154)
chaussee <- st_read("SIG/Thou/chausser.gpkg",          quiet = TRUE) %>% st_transform(2154)

# Chargement du MNT depuis le GeoPackage et FORÇAGE de l'EPSG:2154 (Lambert 93)
mnt <- rast(paste0("GPKG:", chemin_gpkg, ":MNT_5m"))
crs(mnt) <- "EPSG:2154"


# ====================================================================
# ÉTAPE 2 : MODIFICATION DU MNT POUR L'ÉCOULEMENT
# ====================================================================
print("Modification du MNT (Transparence hydraulique)...")

if (nrow(chaussee) > 0) {
  chaussee_buffer  <- terra::buffer(vect(chaussee), width = 10)
  ids_chaussee     <- unique(cells(mnt, chaussee_buffer)[, "cell"])
  mnt[ids_chaussee] <- mnt[ids_chaussee] - 20
  print(paste("->", length(ids_chaussee), "pixels creusés (chaussées)"))
}

if (nrow(routes) > 0) {
  routes_buffer  <- terra::buffer(vect(routes), width = 15)
  ids_routes     <- unique(cells(mnt, routes_buffer)[, "cell"])
  mnt[ids_routes] <- mnt[ids_routes] - 20
  print(paste("->", length(ids_routes), "pixels creusés (routes)"))
}

if (nrow(thoux) > 0) {
  thoux_buffer  <- terra::buffer(vect(thoux), width = 15)
  ids_thoux     <- unique(cells(mnt, thoux_buffer)[, "cell"])
  mnt[ids_thoux] <- mnt[ids_thoux] - 50
  print(paste("->", length(ids_thoux), "pixels creusés (thoux)"))
}

mnt_modifie_tif <- file.path(getwd(), "temp_mnt_modifie.tif")
writeRaster(mnt, mnt_modifie_tif, overwrite = TRUE)


# ====================================================================
# ÉTAPE 3 : MODÉLISATION HYDROLOGIQUE GLOBALE (r.watershed)
# ====================================================================
print("Calcul des directions de drainage et accumulations...")

acc_tif <- file.path(getwd(), "temp_accumulation.tif")
dir_tif <- file.path(getwd(), "temp_direction.tif")

seuil_pixels <- 500

qgis_run_algorithm(
  "grass:r.watershed",
  elevation    = mnt_modifie_tif,
  threshold    = seuil_pixels,
  accumulation = acc_tif,
  drainage     = dir_tif,
  "-s"         = TRUE,
  .quiet       = TRUE
)

# Chargement des rasters créés par GRASS
acc_rast <- rast(acc_tif)
dir_rast <- rast(dir_tif)

# Sécurité Spatiale ABSOLUE : On force l'EPSG en "dur" pour éviter le "?" dans QGIS
ext(acc_rast) <- ext(mnt)
crs(acc_rast) <- "EPSG:2154"

ext(dir_rast) <- ext(mnt)
crs(dir_rast) <- "EPSG:2154"


# ====================================================================
# ÉTAPE 4 : CALCUL DES SOUS-BASSINS VERSANTS (r.water.outlet)
# ====================================================================
print("Calcul des bassins versants individuels pour chaque étang...")

liste_bv_etangs <- list()

for (k in seq_len(nrow(thoux))) {
  thou_pt <- thoux[k, ]
  
  zone_recherche <- st_buffer(thou_pt, dist = 15)
  acc_local      <- mask(crop(acc_rast, vect(zone_recherche)), vect(zone_recherche))
  pixels_acc     <- as.data.frame(acc_local, xy = TRUE, na.rm = TRUE)
  
  if (nrow(pixels_acc) > 0) {
    colnames(pixels_acc)[3] <- "Score_Acc"
    pixel_max <- pixels_acc[which.max(pixels_acc$Score_Acc), ]
    X_opti    <- pixel_max$x
    Y_opti    <- pixel_max$y
    print(paste("   Thou", k, ": recalé sur flux max (", pixel_max$Score_Acc, "px)"))
  } else {
    print(paste("   Thou", k, ": aucun pixel d'accumulation - Utilisation du point brut."))
    coords <- st_coordinates(thou_pt)
    X_opti <- coords[1, "X"]
    Y_opti <- coords[1, "Y"]
  }
  
  outlet_tif <- file.path(getwd(), paste0("temp_outlet_", k, ".tif"))
  
  qgis_run_algorithm(
    "grass:r.water.outlet",
    input       = dir_tif,
    coordinates = paste0(X_opti, ",", Y_opti),
    output      = outlet_tif,
    .quiet      = TRUE
  )
  
  bv_poly <- as.polygons(rast(outlet_tif)) %>%
    st_as_sf() %>% st_make_valid() %>%
    rename(valeur = 1) %>% filter(valeur > 0)
  
  if (nrow(bv_poly) > 0) {
    bv_poly               <- st_union(bv_poly) %>% st_as_sf()
    bv_poly$ID_Thou       <- k
    bv_poly$Surface_Brute_m2 <- as.numeric(st_area(bv_poly))
    liste_bv_etangs[[k]] <- bv_poly
  }
  
  file.remove(outlet_tif)
}


# ====================================================================
# ÉTAPE 5 : NETTOYAGE DES SUPERPOSITIONS ET SAUVEGARDE
# ====================================================================
print("Résolution des superpositions et sauvegarde...")

if (length(liste_bv_etangs) > 0) {
  
  bv_tous <- bind_rows(Filter(Negate(is.null), liste_bv_etangs)) %>%
    arrange(desc(Surface_Brute_m2))
  
  modele_raster   <- crop(dir_rast, ext(vect(bv_tous)))
  modele_raster[] <- NA
  bv_raster       <- rasterize(vect(bv_tous), modele_raster, field = "ID_Thou")
  
  bv_propres <- as.polygons(bv_raster) %>%
    st_as_sf() %>% st_make_valid()
  colnames(bv_propres)[1] <- "ID_Thou"
  bv_propres$Surface_Propre_ha <- as.numeric(st_area(bv_propres)) / 10000
  
  print("-> Écriture des résultats...")
  
  writeRaster(acc_rast, filename = chemin_gpkg, names = "Accumulation",
              filetype = "GPKG", datatype = "FLT4S",
              gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=Accumulation"))
  
  writeRaster(dir_rast, filename = chemin_gpkg, names = "Direction_Drainage",
              filetype = "GPKG", datatype = "FLT4S",
              gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=Direction_Drainage"))
  
  st_write(bv_propres, dsn = chemin_gpkg, layer = "BV_Etangs_Propres",
           driver = "GPKG", append = FALSE, quiet = TRUE,
           layer_options = "GEOMETRY_NAME=geom")
  
  print(paste("Terminé !", nrow(bv_propres), "bassins sauvegardés."))
  
} else {
  print("Échec : Aucun bassin généré.")
}

# Nettoyage fichiers temporaires
# ====================================================================
# NETTOYAGE RADICAL DES FICHIERS TEMPORAIRES (TIF, TFW, PRJ, XML...)
# ====================================================================
# On liste tous les fichiers qui commencent par nos noms temporaires
fichiers_fantomes <- list.files(
  path = getwd(), 
  pattern = "^temp_(mnt_modifie|accumulation|direction|outlet).*", 
  full.names = TRUE
)

# On supprime toute la liste d'un seul coup
if(length(fichiers_fantomes) > 0) {
  file.remove(fichiers_fantomes)
  print(paste("->", length(fichiers_fantomes), "fichiers résiduels supprimés du disque."))
}

print("--- FIN DU SCRIPT ---")
