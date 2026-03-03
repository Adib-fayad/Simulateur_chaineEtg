# --- 1. Chargement des librairies ---
library(sf)    # Pour le vecteur (étangs, routes, BV)
library(terra) # Pour le raster (LiDAR)
library(dplyr) # Pour la manipulation des tables

# --- 2. Chemins des fichiers (A adapter) ---
path_etangs <- "SIG/Etg_complet.shp"
path_routes <- "data/ain_routes.shp"
path_lidar  <- "data/ain_lidar.tif"
path_bv     <- "data/bassins_versants.shp"

dir.create("GPKG_Sortie", showWarnings = FALSE)

# --- 3. Chargement des couches sources ---
# On charge tout une seule fois en mémoire pour gagner du temps
etangs_ain <- st_read(path_etangs)
routes_ain <- st_read(path_routes)
lidar_ain  <- rast(path_lidar)
tous_les_bv <- st_read(path_bv)

# --- 4. Traitement pour UN Bassin Versant (Test) ---
# On prend le premier BV pour l'exemple
bv_selection <- tous_les_bv[1, ] 
nom_bv <- bv_selection$NOM_BV # Vérifie le nom de ta colonne Nom

# Création du Buffer (ex: 500m)
# Attention : Assure-toi d'être en Lambert-93 (unité = mètres)
bv_buffer <- st_buffer(bv_selection, dist = 500)

# --- 5. Découpage et Mise en forme des tables ---

# A. ETANGS
etangs_final <- etangs_ain %>%
  st_make_valid() %>% # Sécurité géométrie
  st_intersection(bv_buffer) %>%
  # On nettoie et on crée les colonnes de saisie
  select(ID_ORIGINE = 1) %>% # On garde juste l'ID d'origine
  mutate(
    NOM_ETANG  = NA_character_,  # Pour saisie texte
    TYPE_USAGE = "Inconnu",      # Liste déroulante future
    EST_PRIVE  = FALSE,          # Case à cocher (Logic)
    NB_VANNES  = 0L,             # Nombre entier (Integer)
    COMMENT    = NA_character_   # Texte libre
  )

# B. ROUTES
routes_final <- routes_ain %>%
  st_intersection(bv_buffer) %>%
  select(TYPE_ROUTE = 1) # On garde le strict minimum

# C. LIDAR (Découpe raster)
# crop coupe le rectangle, mask coupe selon la forme exacte du buffer
lidar_final <- crop(lidar_ain, bv_buffer, mask = TRUE)

# --- 6. Exportation dans le GeoPackage ---
gpkg_path <- paste0("GPKG_Sortie/Etude_", nom_bv, ".gpkg")

# Première couche : on écrase le fichier s'il existe (delete_dsn)
st_write(etangs_final, gpkg_path, layer = "etangs", delete_dsn = TRUE)

# Couches suivantes : on ajoute au même fichier (append)
st_write(routes_final, gpkg_path, layer = "routes", append = TRUE)

# Pour le LiDAR (Raster)
writeRaster(lidar_final, gpkg_path, name = "lidar", overwrite = TRUE, gdal="APPEND_SUBDATASET=YES")

print(paste("GeoPackage généré :", gpkg_path))