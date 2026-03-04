library(sf)    # Pour le vecteur (étangs, routes, BV)
library(terra) # Pour le raster (LiDAR)
library(dplyr) # Pour la manipulation des tables

path_etangs <- "SIG/inpe.gpkg"
path_routes <- "SIG/Route/ROUTE_NUMEROTEE_OU_NOMMEE.shp"
path_lidar  <- "SIG/Dombes_2021_MNT_50cm_L93.cog.tif"
path_bv     <- "SIG/Hydrologie/BV_test.shp"
path_Fosses     <- "SIG/Hydrologie/Fosses_test.shp"
path_OS     <- "SIG/OCCUPATION_SOL.gpkg"
dir.create("GPKG_Sortie", showWarnings = FALSE)

# Chargement des couches sources
# On charge tout une seule fois en mémoire pour gagner du temps
etangs <- st_read(path_etangs)
routes <- st_read(path_routes)
lidar  <- rast(path_lidar)
bv <- st_read(path_bv)
fosses <- st_read(path_Fosses)
OS <- st_read(path_OS)

# On prend le premier BV pour l'exemple
bv_selection <- tous_les_bv[1, ] 
nom_bv <- bv_selection$layer 

# Création du Buffer (ex: 250)
bv_buffer <- st_buffer(bv_selection, dist = 250)

# ETANGS
etangs_final <- etangs %>%
  st_make_valid() %>%
  st_intersection(bv_buffer) %>%
  select(ID_ORIGINE = 1) %>% 
  mutate(
    NOM_ETANG  = NA_character_,  # Pour saisie texte
    TYPE_USAGE = "Inconnu",      # Liste déroulante future
    EST_PRIVE  = FALSE,          # Case à cocher (Logic)
    NB_VANNES  = 0L,             # Nombre entier (Integer)
    COMMENT    = NA_character_   # Texte libre
  )

# ROUTES
routes_final <- routes %>%
  st_intersection(bv_buffer) %>%
  select(TYPE_ROUTE = 1) 

#LIDAR (Découpe raster)
# crop coupe le rectangle, mask coupe selon la forme exacte du buffer
lidar_final <- crop(lidar, bv_buffer)

# Définir le nom du fichier GeoPackage de sortie
nom_fichier_gpkg <- paste0("GPKG_Sortie/BV_", nom_bv, "_Complet.gpkg")

# auvegarde des VECTEURS
st_write(etangs_final, nom_fichier_gpkg, layer = "Etangs", append = FALSE) 
st_write(routes_final, nom_fichier_gpkg, layer = "Routes", append = TRUE)

# Sauvegarde du RASTER DANS LE MÊME FICHIER
# On utilise des arguments spéciaux (gdal) pour forcer l'ajout sans écraser le reste
writeRaster(
  lidar_final, 
  filename = nom_fichier_gpkg, 
  filetype = "GPKG",
  gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=MNT_50cm") 
)
print(paste("Génial, tout est dans le même GeoPackage pour le BV :", nom_bv))
