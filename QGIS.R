library(sf)    # Pour le vecteur (étangs, routes, BV)
library(terra) # Pour le raster (LiDAR)
library(dplyr) # Pour la manipulation des tables
library(RSQLite)
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

# Chargement des couches sources
# On charge tout une seule fois en mémoire pour gagner du temps
etangs <- st_read(path_etangs)
routes <- st_read(path_routes)
lidar  <- rast(path_lidar)
bv <- st_read(path_bv) %>%  st_transform(bv, crs = 2154) 
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
writeRaster(
  lidar_5m, 
  filename = nom_fichier_gpkg, 
  names = "MNT_5m",
  filetype = "GPKG",
  gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=MNT_5m")
)

# auvegarde des VECTEURS
st_write(etangs_final, nom_fichier_gpkg, layer = "Etangs", append = TRUE,quiet = TRUE) 
st_write(routes_final, nom_fichier_gpkg, layer = "Routes", append = TRUE,quiet = TRUE)
st_write(os_final, nom_fichier_gpkg, layer = "OS", append = TRUE,quiet = TRUE) 
st_write(bv_selection, nom_fichier_gpkg, layer = "bv", append = TRUE,quiet = TRUE) 

con_dest <- dbConnect(SQLite(), nom_fichier_gpkg)

dbWriteTable(con_dest, "layer_styles", style_data, append = TRUE)
dbWriteTable(con_dest, "layer_styles", style_os, append = TRUE)

dbDisconnect(con_dest)

gc()
}
print(paste("Génial, tout est dans le même GeoPackage pour le BV :", nom_bv))


















