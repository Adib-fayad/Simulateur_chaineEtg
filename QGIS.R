# ============================================================================
# GUIDE D'UTILISATION - SCRIPT PRÉPARATION GEOPACKAGES POND CHAIN SIMULATOR
# ============================================================================
# 
# OBJECTIF GÉNÉRAL
# 
# Ce script automatise l'intégration et le conditionnement des données 
# géospatiales pour la modélisation hydrologique des chaînes d'étangs.
# Les étapes principales sont :
#
#   1. Charger et traiter les données géospatiales (étangs, routes, LiDAR)
#   2. Découper selon les bassins versants LIFE
#   3. Conditionner le modèle numérique de terrain (MNT)
#   4. Générer la hydrologie GRASS (accumulation, drainage, bassins)
#   5. Créer des GeoPackages complets avec styles cartographiques
#   6. Ajouter les tableaux de données métier (assec, pêche/vidange)
#
# STRUCTURE ET FLUX DE TRAVAIL
#
# Section 1 : Configuration initiale et chemins des fichiers
# Section 2 : Préparation des styles cartographiques QGIS
# Section 3 : Chargement centralisé des données sources
# Section 4 : Boucle de traitement par bassin versant
#            - 4.1 à 4.5 : Extraction et préparation des données
#            - 4.6 : Exécution de la modélisation GRASS
#            - 4.7 : Création des GeoPackages
#            - 4.8 : Injection des styles
# Section 5 : Intégration des tableaux métier
#
# ============================================================================

# --- SECTION 1 : CONFIGURATION & CHEMINS ---
# 
# Déclaration des dépendances R nécessaires et configuration de l'accès 
# à QGIS/GRASS via le paquet qgisprocess.

library(sf)         # Gestion des données vecteur
library(terra)      # Gestion des données raster
library(dplyr)      # Manipulations de tables de données
library(RSQLite)    # Accès aux bases de données SQLite
library(qgisprocess)# Interface R vers QGIS/GRASS
library(readxl)     # Lecture de fichiers Excel (si nécessaire)

# Configuration du chemin d'accès à QGIS
# À adapter selon l'installation locale de QGIS sur le système
options(qgisprocess.path = "C:/Users/adfayad/AppData/Local/Programs/OSGeo4W/bin/qgis_process-qgis.bat")
qgis_configure()

# CHEMINS VERS LES DONNÉES D'ENTRÉE
# Chaque chemin pointe vers un fichier ou répertoire de données source
path_etangs   <- "SIG/inpe.gpkg"                    # Géométries des étangs
path_routes   <- "SIG/Route/ROUTE_NUMEROTEE_OU_NOMMEE.shp"  # Réseau routier
path_lidar    <- "SIG/Dombes_2021_MNT_50cm_L93.cog.tif"     # MNT 50cm original
path_bv       <- "SIG/Hydrologie/bv_intervention.shp"       # Limites des BV
path_OS       <- "SIG/OS.gpkg"                     # Occupation des sols

# Création du répertoire de sortie pour les résultats
dir.create("GPKG_Sortie", showWarnings = FALSE)

# ============================================================================
# --- SECTION 2 : PRÉPARATION DES STYLES CARTOGRAPHIQUES ---
#
# Les styles QGIS sont stockés sous forme de lignes dans les tables SQLite
# layer_styles. Cette section extrait les styles existants pour les réutiliser
# dans les GeoPackages générés. L'étape est technique et permettra que les 
# couches s'affichent avec les bonnes apparences visuelles à l'ouverture.

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
style_acc_modele <- style_data[1, ]

# ============================================================================
# --- SECTION 3 : CHARGEMENT CENTRALISÉ DES DONNÉES ---
#
# Tous les fichiers source sont chargés une seule fois en mémoire avant 
# d'entrer dans la boucle de traitement. Cette approche optimise les performances
# en évitant les lectures répétées et garantit que les opérations de découpe
# travaillent sur des objets spatiaux cohérents.

etangs    <- st_read(path_etangs)
routes    <- st_read(path_routes)
lidar     <- rast(path_lidar)
bv        <- st_read(path_bv) %>%  st_transform(crs = 2154)
OS        <- st_read(path_OS)
thou      <- st_read(path_thou) %>% st_transform(2154)
pts_route <- st_read(path_pts_route) %>% st_transform(2154)
chausser  <- st_read(path_chausser) %>% st_transform(2154)

# Sélection des bassins versants à traiter
# Par défaut, seuls les BV numérotés 2 et 3 sont traités
# Cette ligne doit être modifiée si un sous-ensemble différent est requis
indices_bv <- which(bv$CODE %in% c(2,3))

# ============================================================================
# --- SECTION 4 : BOUCLE PRINCIPALE DE TRAITEMENT PAR BASSIN VERSANT ---
#
# Cette boucle constitue le cœur du processus. Pour chaque bassin versant,
# l'algorithme applique une séquence d'étapes : extraction des données,
# conditionnement du MNT, exécution de la modélisation hydrologique GRASS,
# création des GeoPackages et injection des styles cartographiques.

for (i in indices_bv) {
  bv_selection <- bv[i, ] 
  nom_bv <- bv_selection$CODE 
  print(paste("--- Traitement du BV :", nom_bv, "(", i, "/", nrow(bv), ") ---"))
  
  # 4.1 : CRÉATION D'UNE ZONE TAMPON
  # 
  # Un buffer de 2000 m est créé autour du bassin versant pour garantir que
  # l'ensemble des données contributives (même légèrement excentrées) sont 
  # capturées dans la découpe.
  
  bv_buffer <- st_buffer(bv_selection, dist = 2000)
  
  # 4.2 : EXTRACTION ET FORMATAGE DES ÉTANGS
  # 
  # Les polygones des étangs sont découpés selon le buffer du BV et enrichis
  # de colonnes structurées : code étang, nom (extrait d'un champ au format JSON),
  # surface en eau, profondeur, etc. Cette étape standardise la géométrie et 
  # les attributs pour compatibilité avec le simulateur.
  
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
    select(CODE_ETANG,NOM,nature,insee_com,SURFACE_eau,Exutoire_1,Exutoire_2,Profondeur)
  
  # 4.3 : EXTRACTION DU RÉSEAU ROUTIER
  # 
  # Les segments routiers sont découpés selon le buffer du BV.
  # Seul le type de route est conservé dans l'exportation.
  
  routes_final <- routes %>%
    st_intersection(bv_buffer) %>%
    select(TYPE_ROUTE = 1) 
  
  # 4.4 : EXTRACTION DE L'OCCUPATION DES SOLS
  # 
  # Les classes d'occupation des sols sont découpées selon le buffer du BV.
  
  os_final <- OS %>%
    st_intersection(bv_buffer) %>%
    select(Classe) 
  
  # 4.5 : DÉCOUPE ET AGRÉGATION DU LIDAR
  # 
  # Le MNT LiDAR original (50 cm) est d'abord découpé au buffer. Ensuite,
  # une agrégation spatiale est appliquée (facteur 10, passant de 0.5 m à 5 m)
  # pour réduire la taille des données et accélérer les calculs GRASS tout en
  # conservant la variabilité topographique essentielle.
  
  lidar_final <- crop(lidar, bv_buffer)
  lidar_5m <- aggregate(lidar_final, fact = 10, fun = "mean")
  
  # 4.6 : LANCEMENT DE LA MODÉLISATION HYDROLOGIQUE GRASS
  # 
  # L'algorithme r.watershed de GRASS calcule :
  #   - Accumulation : nombre de cellules contribuant à chaque point
  #   - Drainage : direction d'écoulement
  #   - Bassins : zones de convergence vers une exutoire
  #   - Demi-bassins : subdivision des bassins
  # 
  # Le seuil de délinéation (en pixels) est calculé automatiquement en fonction
  # de la surface du BV (formule : surface en ha / 25). Ce paramètre contrôle
  # la densité du réseau hydrographique généré.
  
  mnt_temp <- "GPKG_Sortie/temp_mnt.tif"
  writeRaster(lidar_5m, mnt_temp, overwrite = TRUE)
  
  # Préparation des chemins de sortie pour les fichiers temporaires
  acc_tif  <- paste0("GPKG_Sortie/temp_acc_", nom_bv, ".tif")
  dir_tif  <- paste0("GPKG_Sortie/temp_dir_", nom_bv, ".tif")
  bas_tif  <- paste0("GPKG_Sortie/temp_bas_", nom_bv, ".tif")
  demi_tif <- paste0("GPKG_Sortie/temp_demi_", nom_bv, ".tif")
  
  # Calcul du seuil d'accumulation
  seuil_pixels <- ((as.numeric(bv_selection$SURFACE) * 10000) / 25)
  
  # Appel à l'algorithme GRASS via QGIS
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
  
  # Conversion des rasters de bassins en polygones
  bas_poly <- as.polygons(rast(bas_tif)) %>% 
    st_as_sf() %>% 
    st_make_valid()
  
  demi_poly <- as.polygons(rast(demi_tif)) %>% 
    st_as_sf() %>% 
    st_make_valid()
  
  # 4.7 : CRÉATION DU GEOPACKAGE COMPLET
  # 
  # Un GeoPackage est créé pour chaque bassin versant. Il contient l'ensemble
  # des données raster (MNT 50 cm et 5 m) et vecteur (étangs, routes, OS, 
  # limites BV, bassins versants et demi-bassins générés par GRASS).
  
  nom_fichier_gpkg <- paste0("GPKG_Sortie/BV_", nom_bv, "_Complet.gpkg")
  if (file.exists(nom_fichier_gpkg)) file.remove(nom_fichier_gpkg)
  
  # Écriture du MNT 50 cm brut
  writeRaster(
    lidar_final, 
    filename = nom_fichier_gpkg, 
    names = "MNT_50cm",
    filetype = "GPKG",
    overwrite = TRUE
  )
  
  # Ajout du MNT agrégé à 5 m
  writeRaster(
    lidar_5m, 
    filename = nom_fichier_gpkg, 
    names = "MNT_5m",
    filetype = "GPKG",
    gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=MNT_5m")
  )
  
  # Écriture des couches vecteurs
  st_write(etangs_final, nom_fichier_gpkg, layer = "Etangs", 
           append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom") 
  st_write(routes_final, nom_fichier_gpkg, layer = "Routes", 
           append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom")
  st_write(os_final, nom_fichier_gpkg, layer = "OS", 
           append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom") 
  st_write(bv_selection, nom_fichier_gpkg, layer = "bv_life", 
           append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom") 
  st_write(bas_poly,  nom_fichier_gpkg, layer = "Bassins_Versant_2000", 
           append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom")
  st_write(demi_poly, nom_fichier_gpkg, layer = "Demi_Bassins_2000", 
           append = TRUE, quiet = TRUE, layer_options = "GEOMETRY_NAME=geom")
  
  # 4.8 : INJECTION DES STYLES CARTOGRAPHIQUES
  # 
  # Les styles QGIS sont insérés dans la table layer_styles de la base SQLite
  # du GeoPackage. Lors de l'ouverture du fichier dans QGIS, les couches 
  # retrouvent automatiquement leurs symbologies, couleurs et apparences.
  
  con_dest <- dbConnect(SQLite(), nom_fichier_gpkg)
  
  # Configuration du style pour les étangs
  style_etangs <- style_data
  style_etangs$f_table_name <- "Etangs"
  style_etangs$f_geometry_column <- "geom"
  style_etangs$useasdefault <- 1   
  
  # Configuration du style pour l'occupation des sols
  style_os <- style_os_modele
  style_os$f_table_name <- "OS"
  style_os$f_geometry_column <- "geom"
  style_os$useasdefault <- 1       
  
  # Configuration du style pour l'accumulation
  style_acc <- style_acc_modele
  style_acc$f_table_name <- "Accumulation"
  style_acc$f_geometry_column <- ""  
  style_acc$styleqml <- qml_texte
  style_acc$stylename <- "Style Accumulation"
  style_acc$useasdefault <- 1
  
  # Assemblage et écriture de tous les styles
  styles_complet <- rbind(style_etangs, style_os, style_acc)
  if("id" %in% names(styles_complet)) styles_complet <- styles_complet %>% select(-id)
  
  dbWriteTable(con_dest, "layer_styles", styles_complet, overwrite = TRUE, row.names = FALSE)
  dbExecute(con_dest, "CREATE INDEX IF NOT EXISTS msg_layer_styles_idx ON layer_styles (f_table_name, f_geometry_column);")
  dbDisconnect(con_dest)
  
  # Nettoyage des fichiers temporaires
  fichiers_a_supprimer <- list.files(path = "GPKG_Sortie", pattern = "^temp_", full.names = TRUE)
  file.remove(fichiers_a_supprimer)
  gc()
}

# ============================================================================
# --- SECTION 5 : INJECTION DES TABLEAUX DE DONNÉES MÉTIER ---
#
# Après la génération des GeoPackages, des tableaux de données supplémentaires
# sont ajoutés : assecs (arrêts d'écoulement), pêche et vidange des étangs.
# Ces tableaux enrichissent l'information disponible pour la simulation.

print("Démarrage de l'injection des tableaux et des formulaires...")

# Chargement des tableaux métier
# Les géométries sont supprimées puisqu'il s'agit de données tabulaires
# sans composante spatiale
table_assec <- st_read("SIG/assecevolage/Assec.gpkg", quiet = TRUE) %>% 
  st_drop_geometry() %>% 
  as.data.frame()

table_peche <- st_read("SIG/peche_Vidange/peche_Vidange.gpkg", quiet = TRUE) %>% 
  st_drop_geometry() %>% 
  as.data.frame()

# Extraction des styles associés aux tableaux
con_src_assec <- dbConnect(SQLite(), "SIG/assecevolage/Assec.gpkg")
style_assec <- dbGetQuery(con_src_assec, "SELECT * FROM layer_styles LIMIT 1")
dbDisconnect(con_src_assec)

con_src_peche <- dbConnect(SQLite(), "SIG/peche_Vidange/peche_Vidange.gpkg")
style_peche <- dbGetQuery(con_src_peche, "SELECT * FROM layer_styles LIMIT 1")
dbDisconnect(con_src_peche)

# Adaptation des noms de couches et styles pour la nouvelle intégration
style_assec$f_table_name <- "Tableau_Assec"
style_assec$f_geometry_column <- NA_character_
style_assec$useasdefault <- 1

style_peche$f_table_name <- "Tableau_peche_Vidange"
style_peche$f_geometry_column <- NA_character_
style_peche$useasdefault <- 1

# Assemblage des styles des tableaux
styles_tableaux <- rbind(style_assec, style_peche)
if("id" %in% names(styles_tableaux)) styles_tableaux <- styles_tableaux %>% select(-id)

# Boucle finale pour ajouter les tableaux à chaque GeoPackage généré
liste_gpkg_finis <- list.files(path = "GPKG_Sortie", 
                               pattern = "^BV_.*_Complet\\.gpkg$", 
                               full.names = TRUE)

for (fichier_cible in liste_gpkg_finis) {
  nom_court <- basename(fichier_cible)
  print(paste("-> Ajout des tableaux et formulaires dans :", nom_court))
  
  # Ajout des couches de données tabulaires
  st_write(table_assec, dsn = fichier_cible, layer = "Tableau_Assec", 
           driver = "GPKG", append = TRUE, quiet = TRUE)
  
  st_write(table_peche, dsn = fichier_cible, layer = "Tableau_peche_Vidange", 
           driver = "GPKG", append = TRUE, quiet = TRUE)
  
  # Ajout des styles associés
  con_cible <- dbConnect(SQLite(), fichier_cible)
  dbAppendTable(con_cible, "layer_styles", styles_tableaux)
  dbDisconnect(con_cible)
}

print("Opération terminée avec succès!")

# ============================================================================
# RÉSUMÉ DE LA STRUCTURE DES GEOPACKAGES GÉNÉRÉS
# ============================================================================
# 
# Chaque fichier BV_X_Complet.gpkg contient l'ensemble des données intégrées
# et est prêt à être utilisé comme entrée pour la modélisation hydrologique.
#
# COUCHES RASTER :
#   - MNT_50cm : Modèle numérique de terrain original (résolution 50 cm)
#   - MNT_5m : MNT agrégé et conditionné (résolution 5 m)
#
# COUCHES VECTEUR DE RÉFÉRENCE :
#   - Etangs : Polygones des plans d'eau avec noms, surfaces, profondeurs
#   - Routes : Réseau routier du secteur d'étude
#   - OS : Occupation des sols classifiée
#   - bv_life : Limite du bassin versant d'intervention LIFE Dombes
#
# COUCHES VECTEUR HYDROGRAPHIQUES (GRASS) :
#   - Bassins_Versant_2000 : Bassins versants contributifs (seuil 2000 pixels)
#   - Demi_Bassins_2000 : Subdivision des bassins versants
#
# TABLEAUX DE DONNÉES MÉTIER :
#   - Tableau_Assec : Historique et planification des arrêts d'écoulement
#   - Tableau_peche_Vidange : Calendrier des pêches et vidanges
#
# STYLES CARTOGRAPHIQUES :
#   - Tous les styles QGIS sont automatiquement injectés et persistants
#   - Les couches s'affichent avec les bonnes symbologies à l'ouverture
#   - Les formules et formulaires QGIS sont préservés
#
# ============================================================================