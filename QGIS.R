library(sf)    # Pour le vecteur (étangs, routes, BV)
library(terra) # Pour le raster (LiDAR)
library(dplyr) # Pour la manipulation des tables

path_etangs <- "SIG/inpe.gpkg"
path_routes <- "SIG/Route/ROUTE_NUMEROTEE_OU_NOMMEE.shp"
path_lidar  <- "SIG/Dombes_2021_MNT_50cm_L93.cog.tif"
path_bv     <- "SIG/Hydrologie/bv_intervention.shp"
path_OS     <- "SIG/OCCUPATION_SOL.gpkg"
dir.create("GPKG_Sortie", showWarnings = FALSE)

# Chargement des couches sources
# On charge tout une seule fois en mémoire pour gagner du temps
etangs <- st_read(path_etangs)
routes <- st_read(path_routes)
lidar  <- rast(path_lidar)
bv <- st_read(path_bv) %>%  st_transform(bv, crs = 2154) 
OS <- st_read(path_OS)

# On prend le premier BV pour l'exemple
bv_selection <- bv[1, ] 
nom_bv <- bv_selection$CODE 

# Création du Buffer (ex: 250m)
bv_buffer <- st_buffer(bv_selection, dist = 750)

# ETANGS
etangs_final <- etangs %>%
  st_make_valid() %>%
  st_intersection(bv_buffer) %>%
  select() %>% 
  mutate(
    CODE_ETANG = 0,                 
    NOM        = NA_character_,
    SURFACE_SI = NA_character_,
    Exutoire_1 = 0,
  )
  

# ROUTES
routes_final <- routes %>%
  st_intersection(bv_buffer) %>%
  select(TYPE_ROUTE = 1) 

#os
os_final <- OS %>%
  st_intersection(bv_buffer) %>%
  select(code_cs,code_us) 



#LIDAR (Découpe raster)
# crop coupe le rectangle, mask coupe selon la forme exacte du buffer
lidar_final <- crop(lidar, bv_buffer)

# Définir le nom du fichier GeoPackage de sortie
nom_fichier_gpkg <- paste0("GPKG_Sortie/BV_", nom_bv, "_Complet.gpkg")
if (file.exists(nom_fichier_gpkg)) file.remove(nom_fichier_gpkg)

# Sauvegarde du RASTER DANS LE MÊME FICHIER
# On utilise des arguments spéciaux (gdal) pour forcer l'ajout sans écraser le reste
writeRaster(
  lidar_final, 
  filename = nom_fichier_gpkg, 
  names = "MNT_50cm",
  filetype = "GPKG",
  overwrite = TRUE
)

# auvegarde des VECTEURS
st_write(etangs_final, nom_fichier_gpkg, layer = "Etangs", append = TRUE) 
st_write(routes_final, nom_fichier_gpkg, layer = "Routes", append = TRUE)
st_write(os_final, nom_fichier_gpkg, layer = "OS", append = TRUE) 
st_write(bv_selection, nom_fichier_gpkg, layer = "bv", append = TRUE) 


print(paste("Génial, tout est dans le même GeoPackage pour le BV :", nom_bv))


















