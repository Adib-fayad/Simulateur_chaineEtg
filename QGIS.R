library(sf)    # Pour le vecteur (étangs, routes, BV)
library(terra) # Pour le raster (LiDAR)
library(dplyr) # Pour la manipulation des tables
library(RSQLite)
library(qgisprocess)
library(readxl)

options(qgisprocess.path = "C:/Users/adfayad/AppData/Local/Programs/OSGeo4W/bin/qgis_process-qgis.bat")
qgis_configure()

path_etangs <- "SIG/inpe.gpkg"
path_routes <- "SIG/Route/ROUTE_NUMEROTEE_OU_NOMMEE.shp"
path_lidar  <- "SIG/Dombes_2021_MNT_50cm_L93.cog.tif"
path_bv     <- "SIG/Hydrologie/bv_intervention.shp"
path_OS     <- "SIG/OS.gpkg"


dir.create("GPKG_Sortie", showWarnings = FALSE)

###
# Style Étangs
# PRÉPARATION DES MODÈLES DE STYLES 
con_src <- dbConnect(SQLite(), path_etangs)
style_data <- dbGetQuery(con_src, "SELECT * FROM layer_styles WHERE f_table_name = 'inpe' LIMIT 1")
dbDisconnect(con_src)
names(style_data) <- tolower(names(style_data))

con_src_os <- dbConnect(SQLite(), path_OS)
style_os_modele <- dbGetQuery(con_src_os, "SELECT * FROM layer_styles WHERE f_table_name = 'departement_01' LIMIT 1")
dbDisconnect(con_src_os)
names(style_os_modele) <- tolower(names(style_os_modele))

chemin_qml_acc <- "SIG/test.qml" 
qml_texte <- paste(readLines(chemin_qml_acc, warn = FALSE), collapse = "\n")
style_acc_modele <- style_data[1, ] # On clone la structure

# Chargement des couches sources
# On charge tout une seule fois en mémoire pour gagner du temps
etangs <- st_read(path_etangs)
routes <- st_read(path_routes)
lidar  <- rast(path_lidar)
bv <- st_read(path_bv) %>%  st_transform(crs = 2154) 
OS <- st_read(path_OS)


#nrow(bv)
for (i in which(bv$CODE %in% c(19,1))) {
bv_selection <- bv[i, ] 
nom_bv <- bv_selection$CODE 
print(paste("--- Traitement du BV :", nom_bv, "(", i, "/", nrow(bv), ") ---"))
# Création du Buffer 
bv_buffer <- st_buffer(bv_selection, dist = 0)

# ETANGS
etangs_final <- etangs %>%
  st_make_valid() %>%
  st_intersection(bv_buffer) %>%
  mutate(
    CODE_ETANG = 0,                 
    NOM        = stringr::str_extract(nom_p_eau, "(?<=\": \").*(?=\")"),
    SURFACE_eau = superficie,
    Exutoire_1 = NA_character_,
    Exutoire_2 = NA_character_
    
  ) %>% 
  select(nature, insee_com, NOM, SURFACE_eau, CODE_ETANG, Exutoire_1,Exutoire_2)


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

seuil_pixels <- ((as.numeric(bv_selection$SURFACE) * 10000) / 25)



qgis_run_algorithm(
  "grass:r.watershed",
  elevation    = mnt_temp,
  threshold    = seuil_pixels,
  accumulation = acc_tif,
  drainage     = dir_tif,
  basin        = bas_tif,
  half_basin   = demi_tif,
  "-s"         = TRUE,
  .quiet       = TRUE 
)


bas_poly <- as.polygons(rast(bas_tif)) %>% 
  st_as_sf() %>% 
  st_make_valid()

demi_poly <- as.polygons(rast(demi_tif)) %>% 
  st_as_sf() %>% 
  st_make_valid()



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

# auvegarde des VECTEURS
st_write(etangs_final, nom_fichier_gpkg, layer = "Etangs", append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom") 
st_write(routes_final, nom_fichier_gpkg, layer = "Routes", append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom")
st_write(os_final, nom_fichier_gpkg, layer = "OS", append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom") 
st_write(bv_selection, nom_fichier_gpkg, layer = "bv_life", append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom") 
st_write(bas_poly,  nom_fichier_gpkg, layer = "Bassins_Versant", append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom")
st_write(demi_poly, nom_fichier_gpkg, layer = "Demi_Bassins", append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom")




#style
con_dest <- dbConnect(SQLite(), nom_fichier_gpkg)

#STYLE ÉTANGS
style_etangs <- style_data
style_etangs$f_table_name <- "Etangs"
style_etangs$f_geometry_column <- "geom"
style_etangs$useasdefault <- 1   

#STYLE OS
style_os <- style_os_modele
style_os$f_table_name <- "OS"
style_os$f_geometry_column <- "geom"
style_os$useasdefault <- 1       

#STYLE ACCUMULATION
style_acc <- style_acc_modele
style_acc$f_table_name <- "Accumulation"
style_acc$f_geometry_column <- ""  
style_acc$styleqml <- qml_texte
style_acc$stylename <- "Style Accumulation"
style_acc$useasdefault <- 1

# On assemble tout
styles_complet <- rbind(style_etangs, style_os, style_acc)
if("id" %in% names(styles_complet)) styles_complet <- styles_complet %>% select(-id)

# Écriture dans SQLite
dbWriteTable(con_dest, "layer_styles", styles_complet, overwrite = TRUE, row.names = FALSE)
dbExecute(con_dest, "CREATE INDEX IF NOT EXISTS msg_layer_styles_idx ON layer_styles (f_table_name, f_geometry_column);")
dbDisconnect(con_dest)

# Nettoyage
fichiers_a_supprimer <- list.files(path = "GPKG_Sortie", pattern = "^temp_", full.names = TRUE)
file.remove(fichiers_a_supprimer)
gc()
}




##########################

print("Démarrage de l'injection des tableaux et des formulaires...")

# On charge les données des tableaux 
table_assec <- st_read("SIG/assecevolage/Assec.gpkg", quiet = TRUE) %>% 
st_drop_geometry() %>% 
as.data.frame()

table_peche <- st_read("SIG/peche_Vidange/peche_Vidange.gpkg", quiet = TRUE) %>% 
st_drop_geometry() %>% 
as.data.frame()

con_src_assec <- dbConnect(SQLite(), "SIG/assecevolage/Assec.gpkg")
style_assec <- dbGetQuery(con_src_assec, "SELECT * FROM layer_styles LIMIT 1")
dbDisconnect(con_src_assec)

con_src_peche <- dbConnect(SQLite(), "SIG/peche_Vidange/peche_Vidange.gpkg")
style_peche <- dbGetQuery(con_src_peche, "SELECT * FROM layer_styles LIMIT 1")
dbDisconnect(con_src_peche)

# On adapte les noms des styles pour qu'ils correspondent aux nouvelles couches
style_assec$f_table_name <- "Tableau_Assec"
style_assec$f_geometry_column <- NA_character_
style_assec$useasdefault <- 1

style_peche$f_table_name <- "Tableau_peche_Vidange"
style_peche$f_geometry_column <- NA_character_
style_peche$useasdefault <- 1

# On rassemble les deux formulaires et on enlève 'id' pour ne pas écraser les styles de la Boucle 1
styles_tableaux <- rbind(style_assec, style_peche)
if("id" %in% names(styles_tableaux)) styles_tableaux <- styles_tableaux %>% select(-id)


# On liste tous les GeoPackages de la Boucle 1
liste_gpkg_finis <- list.files(path = "GPKG_Sortie", pattern = "^BV_.*_Complet\\.gpkg$", full.names = TRUE)

# On boucle sur chaque fichier
for (fichier_cible in liste_gpkg_finis) {

nom_court <- basename(fichier_cible)
print(paste("-> Ajout des tableaux et formulaires dans :", nom_court))

st_write(table_assec, dsn = fichier_cible, layer = "Tableau_Assec", 
         driver = "GPKG", append = TRUE, quiet = TRUE)

st_write(table_peche, dsn = fichier_cible, layer = "Tableau_peche_Vidange", 
         driver = "GPKG", append = TRUE, quiet = TRUE)

# Injection des formulaires dans la base SQLite
con_cible <- dbConnect(SQLite(), fichier_cible)
# dbAppendTable rajoute nos 2 lignes à la suite des styles
dbAppendTable(con_cible, "layer_styles", styles_tableaux)
dbDisconnect(con_cible)
}

print("pération terminée avec succès!")









