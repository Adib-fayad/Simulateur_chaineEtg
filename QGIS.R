library(sf)    # Pour le vecteur (étangs, routes, BV)
library(terra) # Pour le raster (LiDAR)
library(dplyr) # Pour la manipulation des tables
library(RSQLite)
library(qgisprocess)

options(qgisprocess.path = "C:/Users/adfayad/AppData/Local/Programs/OSGeo4W/bin/qgis_process-qgis.bat")
qgis_configure()

path_etangs <- "SIG/inpe.gpkg"
path_routes <- "SIG/Route/ROUTE_NUMEROTEE_OU_NOMMEE.shp"
path_lidar  <- "SIG/Dombes_2021_MNT_50cm_L93.cog.tif"
path_bv     <- "SIG/Hydrologie/bv_intervention.shp"
path_OS     <- "SIG/OS.gpkg"
dir.create("GPKG_Sortie", showWarnings = FALSE)

###
con_src <- dbConnect(SQLite(), path_etangs)
style_data <- dbGetQuery(con_src, "SELECT * FROM layer_styles WHERE f_table_name = 'inpe' LIMIT 1")
dbDisconnect(con_src)
style_data$f_table_name <- "Etangs"
style_data$f_geometry_column <- "geom"



con_src_os <- dbConnect(SQLite(), path_OS)
style_os <- dbGetQuery(con_src_os, "SELECT * FROM layer_styles WHERE f_table_name = 'departement_01' LIMIT 1")
dbDisconnect(con_src_os)

style_os$f_table_name <- "OS"
style_os$f_geometry_column <- "geom"

#style raster
chemin_qml_acc <- "SIG/test.qml" 

qml_texte <- paste(readLines(chemin_qml_acc, warn = FALSE), collapse = "\n")

style_accumulation$f_table_name      <- "Accumulation"
style_accumulation$f_geometry_column <- NA_character_
style_accumulation$styleName         <- "Style Accumulation"
style_accumulation$styleQML          <- qml_texte
style_accumulation$useAsDefault      <- 1
style_accumulation$update_time       <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ")

# S'il y a une colonne 'id', on la met à NA pour que la nouvelle base gère l'auto-incrément
if("id" %in% names(style_accumulation)) style_accumulation$id <- NA

# On fait pareil pour style_os pour être sûr qu'il a bien les 13 colonnes aussi
# (Optionnel si style_os vient déjà d'un GeoPackage QGIS, mais plus sûr)
temp_os <- style_data[1, ]
temp_os[1, names(style_os)] <- style_os[1, ] # On transfère les données de style_os dans la structure de 13 col
style_os <- temp_os


# Chargement des couches sources
# On charge tout une seule fois en mémoire pour gagner du temps
etangs <- st_read(path_etangs)
routes <- st_read(path_routes)
lidar  <- rast(path_lidar)
bv <- st_read(path_bv) %>%  st_transform(crs = 2154) 
OS <- st_read(path_OS)


for (i in 1:nrow(bv)) {
# On prend le premier BV pour l'exemple
bv_selection <- bv[1, ] 
nom_bv <- bv_selection$CODE 
print(paste("--- Traitement du BV :", nom_bv, "(", i, "/", nrow(bv), ") ---"))
# Création du Buffer 
bv_buffer <- st_buffer(bv_selection, dist = 750)

# ETANGS
etangs_final <- etangs %>%
  st_make_valid() %>%
  st_intersection(bv_buffer) %>%
  mutate(
    CODE_ETANG = 0,                 
    NOM        = stringr::str_extract(nom_p_eau, "(?<=\": \").*(?=\")"),
    SURFACE_SI = superficie,
    Exutoire_1 = NA_character_,
  ) %>% 
  select(nature, insee_com, NOM, SURFACE_SI, CODE_ETANG, Exutoire_1)
  

# ROUTES
routes_final <- routes %>%
  st_intersection(bv_buffer) %>%
  select(TYPE_ROUTE = 1) 

#os
os_final <- OS %>%
  st_intersection(bv_buffer) %>%
  select(Classe) 



#LIDAR (Découpe raster)
# crop coupe le rectangle, mask coupe selon la forme exacte du buffer
lidar_final <- crop(lidar, bv_buffer)
lidar_5m <- aggregate(lidar_final, fact = 10, fun = "mean")

print(paste("Lancement de l'hydrologie GRASS pour le BV :", nom_bv))

mnt_temp <- "GPKG_Sortie/temp_mnt.tif"
writeRaster(lidar_5m, mnt_temp, overwrite = TRUE)

#Préparation des chemins de sortie pour les 4 fichiers 
acc_tif  <- paste0("GPKG_Sortie/temp_acc_", nom_bv, ".tif")
dir_tif  <- paste0("GPKG_Sortie/temp_dir_", nom_bv, ".tif")
bas_tif  <- paste0("GPKG_Sortie/temp_bas_", nom_bv, ".tif")
demi_tif <- paste0("GPKG_Sortie/temp_demi_", nom_bv, ".tif")

seuil_pixels <- (as.numeric(bv_selection$SURFACE) * 10000) / 25

qgis_run_algorithm(
  "grass:r.watershed",
  elevation    = mnt_temp,
  threshold    = seuil_pixels,
  accumulation = acc_tif,
  drainage     = dir_tif,
  basin        = bas_tif,
  half_basin   = demi_tif,
  #"-s"         = TRUE,
  .quiet       = TRUE 
)


# Définir le nom du fichier GeoPackage de sortie
nom_fichier_gpkg <- paste0("GPKG_Sortie/BV_", nom_bv, "_Complet.gpkg")
if (file.exists(nom_fichier_gpkg)) file.remove(nom_fichier_gpkg)



writeRaster(
  lidar_final, 
  filename = nom_fichier_gpkg, 
  names = "MNT_50cm",
  filetype = "GPKG",
  overwrite = TRUE
)

writeRaster(
  lidar_5m, 
  filename = nom_fichier_gpkg, 
  names = "MNT_5m",
  filetype = "GPKG",
  gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=MNT_5m")
)

writeRaster(rast(acc_tif),  filename = nom_fichier_gpkg, names = "Accumulation",       filetype = "GPKG",datatype = "FLT4S", gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=Accumulation"))
writeRaster(rast(dir_tif),  filename = nom_fichier_gpkg, names = "Direction_Drainage", filetype = "GPKG", gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=Direction_Drainage"))
writeRaster(rast(bas_tif),  filename = nom_fichier_gpkg, names = "Bassins",            filetype = "GPKG", gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=Bassins"))
writeRaster(rast(demi_tif), filename = nom_fichier_gpkg, names = "Demi_Bassins",       filetype = "GPKG", gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=Demi_Bassins"))



# auvegarde des VECTEURS
st_write(etangs_final, nom_fichier_gpkg, layer = "Etangs", append = TRUE,quiet = TRUE) 
st_write(routes_final, nom_fichier_gpkg, layer = "Routes", append = TRUE,quiet = TRUE)
st_write(os_final, nom_fichier_gpkg, layer = "OS", append = TRUE,quiet = TRUE) 
st_write(bv_selection, nom_fichier_gpkg, layer = "bv", append = TRUE,quiet = TRUE) 

#style
con_dest <- dbConnect(SQLite(), nom_fichier_gpkg)

dbWriteTable(con_dest, "layer_styles", style_data, append = TRUE)
dbWriteTable(con_dest, "layer_styles", style_os, append = TRUE)
dbWriteTable(con_dest, "layer_styles", style_accumulation, append = TRUE)

dbDisconnect(con_dest)

fichiers_a_supprimer <- list.files(path = "GPKG_Sortie", pattern = "^temp_", full.names = TRUE)
file.remove(fichiers_a_supprimer)
gc()
}
print(paste("Génial, tout est dans le même GeoPackage pour le BV :", nom_bv))


















