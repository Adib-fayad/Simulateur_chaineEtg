library(sf)    
library(terra) 
library(dplyr) 
library(qgisprocess)
library(DBI)
library(RSQLite)

options(qgisprocess.path = "C:/Users/adfayad/AppData/Local/Programs/OSGeo4W/bin/qgis_process-qgis.bat")
qgis_configure()

path_thou <- "SIG/Thou/ouvrage chalamont.gpkg"
thou_global <- st_read(path_thou, quiet = TRUE) %>% st_transform(2154)

liste_gpkg <- list.files(path = "GPKG_Sortie", pattern = "^BV_.*_Complet\\.gpkg$", full.names = TRUE)
print(paste("Démarrage du traitement sur", length(liste_gpkg), "GeoPackages..."))

for (fichier_gpkg in liste_gpkg) {
  
  nom_court <- basename(fichier_gpkg)
  print(paste("--- Traitement de :", nom_court, "---"))
  
  # Tous les thous, sans filtre spatial
  thou_local <- thou_global
  
  if (nrow(thou_local) == 0) {
    print("-> Aucun thou trouvé, on passe au suivant.")
    next
  }
  
  print(paste("->", nrow(thou_local), "thoux à traiter. Extraction de la direction de drainage..."))
  
  dir_rast <- rast(paste0("GPKG:", fichier_gpkg, ":Direction_Drainage"))
  dir_tif  <- "GPKG_Sortie/temp_dir_pour_outlet.tif"
  writeRaster(dir_rast, dir_tif, overwrite = TRUE)
  acc_rast <- rast(paste0("GPKG:", fichier_gpkg, ":Accumulation"))
  
  liste_bv_etangs <- list()
  
  for (k in seq_len(nrow(thou_local))) {
    thou_pt <- thou_local[k, ]
    
    zone_recherche <- st_buffer(thou_pt, dist = 15)
    acc_local <- crop(acc_rast, vect(zone_recherche))
    acc_local <- mask(acc_local, vect(zone_recherche))
    pixels_acc <- as.data.frame(acc_local, xy = TRUE, na.rm = TRUE)
    
    if (nrow(pixels_acc) > 0) {
      colnames(pixels_acc)[3] <- "Score_Acc"
      pixel_max <- pixels_acc[which.max(pixels_acc$Score_Acc), ]
      X_opti <- pixel_max$x
      Y_opti <- pixel_max$y
      print(paste("   Thou", k))
    } else {
      stop(paste("Thou", k, ": aucun pixel d'accumulation trouvé dans la zone de recherche. Vérifie le point ou le raster."))
    }
    
    coord_str  <- paste0(X_opti, ",", Y_opti)
    outlet_tif <- paste0("GPKG_Sortie/temp_outlet_", k, ".tif")
    
    qgis_run_algorithm(
      "grass:r.water.outlet",
      input       = dir_tif,
      coordinates = coord_str,
      output      = outlet_tif,
      .quiet      = TRUE
    )
    
    bv_poly <- as.polygons(rast(outlet_tif)) %>%
      st_as_sf() %>%
      st_make_valid() %>%
      rename(valeur = 1) %>%
      filter(valeur > 0)
    
    if (nrow(bv_poly) > 0) {
      bv_poly               <- st_union(bv_poly) %>% st_as_sf()
      bv_poly$ID_Thou       <- k
      bv_poly$Surface_Brute_m2 <- as.numeric(st_area(bv_poly))
      liste_bv_etangs[[k]] <- bv_poly
    }
    
    file.remove(outlet_tif)
  }
  
  if (length(liste_bv_etangs) > 0) {
    print("-> Découpage  par rasterisation...")
    
    bv_tous <- bind_rows(Filter(Negate(is.null), liste_bv_etangs)) %>%
      arrange(desc(Surface_Brute_m2))
    
    modele_raster   <- crop(dir_rast, ext(vect(bv_tous)))
    modele_raster[] <- NA
    
    bv_vect   <- vect(bv_tous)
    bv_raster <- rasterize(bv_vect, modele_raster, field = "ID_Thou")
    
    bv_propres <- as.polygons(bv_raster) %>%
      st_as_sf() %>%
      st_make_valid()
    colnames(bv_propres)[1] <- "ID_Thou"
    
    attrs <- st_drop_geometry(bv_tous) %>%
      select(ID_Thou) %>%
      distinct(ID_Thou, .keep_all = TRUE)
    
    bv_propres <- bv_propres %>%
      left_join(attrs, by = "ID_Thou")
    
    bv_propres$Surface_Propre_ha <- as.numeric(st_area(bv_propres)) / 10000
    
    # if ("BV_Etangs_Propres" %in% st_layers(fichier_gpkg)$name) {
    #   conn <- DBI::dbConnect(RSQLite::SQLite(), fichier_gpkg)
    #   DBI::dbExecute(conn, 'DROP TABLE IF EXISTS "BV_Etangs_Propres"')
    #   DBI::dbDisconnect(conn)
    #   print("   Ancienne couche BV_Etangs_Propres supprimée.")
    # }
    
    st_write(bv_propres, dsn = fichier_gpkg, layer = "BV_Etangs_Propres",
             driver = "GPKG", append = FALSE, quiet = TRUE,
             layer_options = "GEOMETRY_NAME=geom")
    
    print(paste(nrow(bv_propres), "bassins propres sauvegardés dans", nom_court))
    
  } else {
    print("-> Échec : Aucun bassin généré pour ce GeoPackage.")
  }
  
  fichiers_temp <- c(
    dir_tif,
    paste0(dir_tif, ".aux.xml"),
    sub("\\.tif$", ".tfw", dir_tif),
    sub("\\.tif$", ".prj", dir_tif)
  )
  file.remove(fichiers_temp[file.exists(fichiers_temp)])
}