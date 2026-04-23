library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)
library(plotly)

# =======================================================
# SERVEUR (LOGIQUE DE CALCUL)
# =======================================================
server <- function(input, output, session) {
  
  # Fonction de calcul du bilan hydrologique journalier
  Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Statut_Assec, Volume_Vidange_Jour, Peche_Jour){
    
    # Calcul des ressources en eau disponibles (Stock initial + apports par ruissellement et amont)
    Eau_Dispo = BF + Volume_R + Vamont
    
    # Gestion des pertes par infiltration (fuite) plafonnées par l'eau disponible
    Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
    Eau_Dispo = Eau_Dispo - Fuite_Reelle
    Vsortant = Fuite_Reelle 
    
    # Calcul de l'évapotranspiration réelle selon l'état de l'étang
    if (Statut_Assec == "Assec" || Peche_Jour == "oui") {
      Evap_Reelle = max(0, Vp_etp)
    } else {
      if (Vp_etp < 0) {
        # L'évaporation est limitée par la disponibilité physique de l'eau
        Evap_Reelle = max(Vp_etp, -Eau_Dispo) 
      } else {
        Evap_Reelle = Vp_etp 
      }
    }
    Eau_Dispo = Eau_Dispo + Evap_Reelle 
    
    # Cas spécifique du jour de pêche : vidange intégrale du volume stocké
    if (Peche_Jour == "oui") {
      Vsortant = Vsortant + Eau_Dispo
      BF = 0
      
      # Cas de la période de vidange : gestion du volume selon un objectif de baisse de niveau
    } else if (Volume_Vidange_Jour > 0) {
      
      # Définition de la cible de volume basée sur l'état de la veille
      Objectif_Volume = max(0, BF - Volume_Vidange_Jour)
      
      # Calcul de l'excédent par rapport à l'objectif théorique
      if (Eau_Dispo > Objectif_Volume) {
        Volume_a_vider = Eau_Dispo - Objectif_Volume
        # Garantie d'évacuation du quota minimum de vidange
        Volume_theorique = max(Volume_a_vider, Volume_Vidange_Jour)
      } else {
        # Maintien du quota si le volume est déjà inférieur à l'objectif
        Volume_theorique = Volume_Vidange_Jour
      }
      
      # Volume réellement évacué limité par le stock disponible
      Volume_reel_vide = min(Volume_theorique, max(0, Eau_Dispo))
      
      # Actualisation des flux sortants et du stock résiduel
      Vsortant = Vsortant + Volume_reel_vide
      Eau_Dispo = Eau_Dispo - Volume_reel_vide
      
      # Sécurité de débordement en cas d'apport extrême
      if (Eau_Dispo > Vmax) {
        Surplus = Eau_Dispo - Vmax
        Vsortant = Vsortant + Surplus
        BF = Vmax 
      } else {
        BF = Eau_Dispo
      }
      
      # Cas de l'étang en assec : transfert intégral des apports vers l'exutoire
    } else if (Statut_Assec == "Assec") {
      Vsortant = Vsortant + Eau_Dispo
      BF = 0
      
      # Cas standard d'évolage : stockage de l'eau jusqu'à la capacité maximale
    } else {
      if (Eau_Dispo > Vmax) {
        Surplus = Eau_Dispo - Vmax
        Vsortant = Vsortant + Surplus
        BF = Vmax 
      } else {
        BF = Eau_Dispo
      }
    } 
    
    return(list(BF = BF, Vsortant = Vsortant))
  }
  
  # Fonction de chargement des données d'observation terrain
  load_terrain <- function(nom_etang) {
    # Normalisation typographique pour la correspondance avec les fichiers de stockage
    nom_propre <- stringr::str_to_title(tolower(nom_etang))
    
    chemins_possibles <- c(
      paste0("Volume_etang/", nom_etang, ".Rdata"),
      paste0("Volume_etang/", nom_propre, ".Rdata"),
      paste0("Volume_etang/", tolower(nom_etang), ".Rdata")
    )
    
    # Identification du chemin valide selon l'existence physique du fichier
    chemin_valide <- chemins_possibles[file.exists(chemins_possibles)][1]
    
    if (!is.na(chemin_valide)) {
      env <- new.env()
      load(chemin_valide, envir = env)
      nom_obj <- ls(env)[1] 
      df <- env[[nom_obj]] %>%
        mutate(dat = as.Date(Date_jour)) %>%
        select(dat, Volume_Reel = Volume_m3) %>%
        drop_na(dat)
      return(df)
    }
    return(NULL)
  }
  
  # Initialisation des variables réactives pour l'interface
  data_res <- reactiveValues(liste_finale = NULL, log = "En attente d'exécution...", pond_names = NULL, exutoire_data = NULL)
  
  # Gestionnaire d'événements pour le lancement de la simulation
  observeEvent(input$run_sim, {
    req(input$file_os, input$file_etg, input$file_assec, input$file_vidange)
   
    # Affichage d'une notification de progression
    id_chargement <- showNotification("Lecture des fichiers en cours, veuillez patienter...", 
                                      duration = NULL, closeButton = FALSE, type = "message")
    
    data_res$log <- "Initialisation des calculs...\n"
    
    tryCatch({
      # -------------------------------------------------------
      # PARTIE 1 : PRÉPARATION DES DONNÉES (OS, Étangs, Vidanges)
      # -------------------------------------------------------
      
      # Importation des données d'occupation des sols et filtrage des classes
      os_data <- read.csv2(input$file_os$datapath, dec = ".", sep = ",") %>%
        select(ClasseOS = 1, Etang = 2, Surface = 3) %>%
        filter(ClasseOS < 23)
      
      # Chargement de la table de correspondance Curve Number
      tab_cn <- read.csv2("OS_CN.csv", sep = ";", encoding = "latin1")
      
      # Jointure des données spatiales et des coefficients hydrologiques
      os_complet <- os_data %>%
        left_join(tab_cn, by = c("ClasseOS" = "Code_OS"))
      
      # Calcul des paramètres hydrologiques agrégés par bassin versant d'étang
      cnetg <- os_complet %>%
        group_by(Etang) %>%
        summarise(
          Surface_BV = round(sum(Surface) / 10000, 1),
          CNII = round(sum(CN.sol.D.Fav * Surface) / sum(Surface), 1)
        ) %>%
        mutate(
          CNI   = round(4.2 * CNII / (10 - 0.058 * CNII)),
          CNIII = round(23 * CNII / (10 + 0.13 * CNII))
        )
      
      # Chargement des caractéristiques géométriques et physiques des étangs
      etg_params <- read.csv2(input$file_etg$datapath, header = TRUE, dec = ",", sep = ";") %>% 
        filter(Chaine_etu == "oui") %>% 
        select(-num_range("Assec", 2021:2025))
      
      # Chargement des données historiques d'assec
      assec_data <- read.csv2(input$file_assec$datapath, header = TRUE, sep = ";") %>% 
        select(-Exutoire_1, -OBJECTID)
      
      # Intégration des paramètres et calcul du volume maximum théorique si absent
      etg_model <- assec_data %>% 
        inner_join(etg_params, by ="NOM") %>%
        mutate(Vmax = ifelse(
          is.na(Vmax),                                
          SURFACE_eau * Profondeur_m * 10000,     
          Vmax                                        
        ))
      
      # Lecture des calendriers de vidange
      vidange_raw <- read.csv(input$file_vidange$datapath, sep = ",")
      
      # Consolidation de la table globale des paramètres étangs
      tab_etg <- cnetg %>%
        rename(NOM = Etang) %>%
        inner_join(etg_model, by = "NOM") %>% 
        select(-any_of("Vidange")) %>%
        left_join(vidange_raw %>% select(-Exutoire_1, -OBJECTID), by = "NOM") %>%
        mutate(
          jours_vidange = ceiling(SURFACE_eau),
          across(
            .cols = starts_with("peche"),                 
            .fns = ~ as.Date(.x) - jours_vidange,         
            .names = "{gsub('peche', 'Vidange', .col)}"   
          )
        )
      
      # -------------------------------------------------------
      # PARTIE 2 : MÉTÉO SAFRAN
      # -------------------------------------------------------
      data_res$log <- paste0(data_res$log, "Extraction des données SAFRAN...\n")
      
      # Identification de la maille SAFRAN de référence selon le code bassin
      coordonnees <- read.csv("meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% 
        filter(CODE == input$bv_code)
      
      X_ref <- coordonnees$LAMBX[1]
      Y_ref <- coordonnees$LAMBY[1]
      
      # Lecture des séries temporelles climatiques
      meteo_all <- read.csv2("meteo/SAFRAN/Meteo_Dombes_Legere.csv")
      
      # Calcul de proximité spatiale pour l'attribution de la maille météo
      maille_proche <- meteo_all %>%
        select(LAMBX, LAMBY) %>% distinct() %>%
        mutate(dist = sqrt((LAMBX - X_ref)^2 + (LAMBY - Y_ref)^2)) %>% 
        arrange(dist) %>% head(1)
      
      # Structuration et nettoyage des variables pluviométrie et évapotranspiration
      pluvio <- meteo_all %>%
        filter(LAMBX == maille_proche$LAMBX & LAMBY == maille_proche$LAMBY) %>%
        rename(RR = PRELIQ) %>%
        mutate(
          dat = as.Date(lubridate::parse_date_time(as.character(DATE), orders = c("ymd", "dmy", "Ymd", "Y-m-d"))),
          RR = as.numeric(gsub(",", ".", as.character(RR))),
          ETP_grille = as.numeric(gsub(",", ".", as.character(ETP))),
          P_ETP = RR - ETP_grille
        ) %>%
        filter(between(dat, input$dates[1], input$dates[2])) %>%
        arrange(dat)
      
      # -------------------------------------------------------
      # PARTIE 3 : SIMULATION HYDROLOGIQUE GLOBALE
      # -------------------------------------------------------
      data_res$log <- paste0(data_res$log, "Préparation des tables de calcul...\n")
      
      # Calcul de l'indice de précipitation antérieure (Pant)
      pluvio_calc <- pluvio %>%
        arrange(dat) %>%
        mutate(
          cumul_5j = rollsum(RR, k = 5, fill = NA, align = "right"),
          Pant = lag(cumul_5j, n = 1, default = 0)
        ) %>%
        mutate(Pant = replace_na(Pant, 0))
      
      # Reconstruction de la chronologie des états de gestion (Assec / Vidange)
      table_dates <- tab_etg %>%
        select(NOM, starts_with("Assec"), starts_with("Vidange"), starts_with("peche")) %>%
        pivot_longer(
          cols = -NOM,
          names_to = c(".value", "annee"),
          names_pattern = "([A-Za-z]+)(\\d{4})"
        ) %>%
        arrange(NOM, annee) %>%
        group_by(NOM) %>%
        mutate(Assec_Futur = coalesce(lead(assec), assec)) %>%
        ungroup()
      
      # Calcul des paramètres journaliers de ruissellement (CN et volumes)
      df_bilan <- tab_etg %>%
        select(NOM, Surface_BV, SURFACE_eau, Vmax, CNI, CNII, CNIII, Exutoire_1) %>%
        cross_join(pluvio_calc) %>%
        mutate(annee = format(dat, "%Y")) %>%
        left_join(table_dates, by = c("NOM", "annee")) %>%
        mutate(
          mois = as.numeric(format(dat, "%m")),
          is_ete = (mois >= 5 & mois <= 10),
          Pant_num = replace_na(as.numeric(Pant), 0),
          
          # Détermination du CN selon l'humidité du sol
          CN_jour = case_when(
            is_ete & Pant_num < 36 ~ CNI,
            is_ete & Pant_num > 53 ~ CNIII,
            is_ete ~ CNII,
            !is_ete & Pant_num < 13 ~ CNI,
            !is_ete & Pant_num > 28 ~ CNIII,
            !is_ete ~ CNII,
            TRUE ~ CNII
          ),
          
          # Application du modèle SCS-CN pour le ruissellement
          S_cn = (25400 / CN_jour) - 254,
          Ia = 0.2 * S_cn,
          RR_num = replace_na(as.numeric(RR), 0),
          
          Vol_R_brut = case_when(
            RR_num > Ia ~ ((RR_num - Ia)^2) / (RR_num - Ia + S_cn),
            TRUE ~ 0
          ),
          CR = case_when(
            RR_num > 0 ~ Vol_R_brut / RR_num,
            TRUE ~ 0
          ),
          
          # Conversion des lames d'eau en volumes
          Volume_R = CR * RR_num * (Surface_BV - SURFACE_eau) * 10,
          VFuite = round(0.1 * 3600 * 24) / 1000,
          Vp_etp = replace_na(as.numeric(P_ETP), 0) * SURFACE_eau * 10,
          Vidange = if_else(dat == as.Date(Vidange), "oui", "non", missing = "non"),
          peche   = if_else(dat == as.Date(peche), "oui", "non", missing = "non"),
          Vamont = 0,
          BF = case_when(
            dat == input$dates[1] & assec == "Evolage" ~ Vmax / 2, 
            TRUE ~ 0
          )
        ) %>% 
        group_by(NOM, annee) %>%
        mutate(
          # Mise à jour dynamique de l'état de l'étang
          Statut_Simu = case_when(
            peche == "oui" | Vidange == "oui" ~ "Evolage",
            cumany(peche == "oui") ~ Assec_Futur,
            TRUE ~ assec
          )
        ) %>% 
        ungroup() %>%
        select(-annee, -assec, -Assec_Futur)
      
      # Segmentation des données par unité d'étang
      liste_etangs <- df_bilan %>% split(.$NOM)
      
      # Modélisation topologique du réseau hydrographique
      liens <- df_bilan %>% 
        select(NOM, Exutoire_1) %>% 
        filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT") %>%
        distinct()
      
      # Tri topologique pour respecter l'ordre d'écoulement amont-aval
      reseau <- graph_from_data_frame(liens, directed = TRUE)
      ordre_topologique <- names(topo_sort(reseau, mode = "out"))
      
      # Initialisation du réceptacle des flux à l'exutoire final
      Volume_Total_Exutoire_BV <- data.frame(
        dat = liste_etangs[[1]]$dat, 
        Volume_Riviere = rep(0, nrow(liste_etangs[[1]])) 
      )
      
      # Boucle de propagation des flux dans le réseau
      withProgress(message = 'Simulation hydrologique en cours...', value = 0, {
        
        nombre_total_etangs <- length(ordre_topologique)
        compteur <- 0
        
        for (nom_etang in ordre_topologique) {
          compteur <- compteur + 1
          incProgress(1 / nombre_total_etangs, 
                      detail = paste("Étang :", nom_etang, "(", compteur, "/", nombre_total_etangs, ")"))
          
          etangs_calcule <- liste_etangs[[nom_etang]]
          Stockage_Vamont <- numeric(nrow(etangs_calcule))
          etangs_calcule$Vol_Vidange_Jour <- 0
          etangs_calcule$Vsortant <- 0  
          # Identification temporelle des opérations de vidange et de pêche
          lignes_vidange <- which(etangs_calcule$Vidange == "oui")
          lignes_peche <- which(etangs_calcule$peche == "oui")
          
          # Calcul des quotas de vidange journaliers requis pour l'assec
          if (length(lignes_vidange) > 0) {
            for (t0 in lignes_vidange) {
              peches_futures <- lignes_peche[lignes_peche > t0]
              if (length(peches_futures) > 0) {
                tfin <- peches_futures[1]
                delta_T <- tfin - t0
                if (delta_T > 0) {
                  Volume_Etang <- etangs_calcule$Vmax[t0]
                  Surface_ha <- etangs_calcule$SURFACE_eau[t0] 
                  Vol_1ha_jour <- Volume_Etang / Surface_ha
                  jours_pour_vider <- ceiling(Surface_ha)
                  jours_effectifs <- min(delta_T, jours_pour_vider)
                  jours_actifs <- t0:(t0 + jours_effectifs - 1)
                  etangs_calcule$Vol_Vidange_Jour[jours_actifs] <- Vol_1ha_jour
                }
              }
            }
          }
          
          # Résolution du bilan hydrologique journalier pour l'unité courante
          for (jour in 2:nrow(etangs_calcule)) {
            statut_du_jour <- etangs_calcule$Statut_Simu[jour]
            
            resultat <- Bfinal(
              Vmax = etangs_calcule$Vmax[jour],
              BF = etangs_calcule$BF[jour-1] ,
              Vp_etp = etangs_calcule$Vp_etp[jour], 
              Volume_R = etangs_calcule$Volume_R[jour],
              Vamont = etangs_calcule$Vamont[jour],
              VFuite = etangs_calcule$VFuite[jour], 
              Statut_Assec = statut_du_jour,
              Volume_Vidange_Jour = etangs_calcule$Vol_Vidange_Jour[jour],
              Peche_Jour = etangs_calcule$peche[jour]
            )
            
            etangs_calcule$BF[jour] = resultat$BF
            Stockage_Vamont[jour] = resultat$Vsortant
            etangs_calcule$Vsortant[jour] = resultat$Vsortant 
          }
          
          # Mise à jour des données et transfert des flux vers l'exutoire défini
          liste_etangs[[nom_etang]] <- etangs_calcule
          
          exutoire <- etangs_calcule$Exutoire_1[1]
          if (!is.na(exutoire)) {
            if (exutoire != "OUTPUT") {
              liste_etangs[[exutoire]]$Vamont <- liste_etangs[[exutoire]]$Vamont + Stockage_Vamont
            } else {
              Volume_Total_Exutoire_BV$Volume_Riviere <- Volume_Total_Exutoire_BV$Volume_Riviere + Stockage_Vamont
            }
          }
        }
      })
      
      # Actualisation de l'état réactif de l'application
      data_res$exutoire_data <- Volume_Total_Exutoire_BV
      data_res$liste_finale <- liste_etangs
      data_res$pond_names <- ordre_topologique
      data_res$log <- paste0(data_res$log, "Simulation terminée avec succès.\n")
      
      # Mise à jour du sélecteur d'étang pour l'utilisateur
      updateSelectInput(session, "etang_choisi", choices = data_res$pond_names)
      updateSelectInput(session, "etang_superpose_ex",choices = c("Aucun", data_res$pond_names))
      removeNotification(id_chargement)
      showNotification("Simulation terminée avec succès !", type = "message", duration = 5)
      
    }, error = function(e) {
      # Traitement des exceptions et affichage des erreurs système
      removeNotification(id_chargement)
      data_res$log <- paste0(data_res$log, "Erreur lors de l'exécution : ", e$message)
      showNotification("Une erreur s'est produite. Regardez la console RStudio.", type = "error")
      
      message("==================================================")
      message("ERREUR DÉTECTÉE : ", e$message)
      message("==================================================")
    })
  })
  
  # Génération dynamique du contrôle de sélection de dates
  output$ui_zoom_dates <- renderUI({
    req(data_res$liste_finale)
    df <- data_res$liste_finale[[1]] 
    dateRangeInput("viz_dates", "Plage de visualisation :",
                   start = min(df$dat), end = max(df$dat),
                   min = min(df$dat), max = max(df$dat))
  })
  
  # Génération dynamique du contrôle de sélection de dates pour l'exutoire
  output$ui_zoom_dates_exutoire <- renderUI({
    req(data_res$exutoire_data)
    df <- data_res$exutoire_data 
    dateRangeInput("viz_dates_exutoire", "Plage de visualisation :",
                   start = min(df$dat), end = max(df$dat),
                   min = min(df$dat), max = max(df$dat))
  })
  # =======================================================
  # AFFICHAGE ET EXPORT (Calibration)
  # =======================================================
  
  output$plot_calibration <- renderPlotly({
    req(data_res$liste_finale)
    req(input$etang_choisi) 
    req(input$var_plot)
    
    nom_etang <- input$etang_choisi
    var_select <- input$var_plot
    df_plot <- data_res$liste_finale[[nom_etang]]
    
    # 1. Filtre sur les dates
    if (!is.null(input$viz_dates)) {
      df_plot <- df_plot %>% 
        filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
    }
    
    # 2. Application du lissage temporel selon le choix de l'utilisateur
    req(input$time_step)
    if (input$time_step != "day") {
      df_plot <- df_plot %>%
        # Création des paliers de temps (arrondis à la décade, au mois ou à l'année)
        mutate(dat = lubridate::floor_date(dat, unit = input$time_step)) %>%
        group_by(dat) %>%
        summarise(
          # Règle physique 1 : Moyenne pour les variables de stock
          BF = mean(BF, na.rm = TRUE),
          
          # Règle physique 2 : Cumul pour les variables de flux
          Vsortant = sum(Vsortant, na.rm = TRUE),
          RR = sum(RR, na.rm = TRUE),
          Volume_R = sum(Volume_R, na.rm = TRUE),
          Vp_etp = sum(Vp_etp, na.rm = TRUE),
          Vol_Vidange_Jour = sum(Vol_Vidange_Jour, na.rm = TRUE),
          
          # Conservation du paramètre statique
          Vmax = first(Vmax),
          .groups = "drop"
        )
    }
    
    # 3. Injection des données Terrain si la case est cochée
    terrain_dispo <- FALSE
    if (input$show_terrain) {
      df_terrain <- load_terrain(nom_etang)
      if (!is.null(df_terrain)) {
        
        # Si le modèle est lissé, il faut appliquer strictement le même lissage au terrain
        if (input$time_step != "day") {
          df_terrain <- df_terrain %>%
            mutate(dat = lubridate::floor_date(dat, unit = input$time_step)) %>%
            group_by(dat) %>%
            # Le terrain est un relevé de niveau (stock), on utilise donc la moyenne
            summarise(Volume_Reel = mean(Volume_Reel, na.rm = TRUE), .groups = "drop")
        }
        
        df_plot <- df_plot %>% left_join(df_terrain, by = "dat")
        terrain_dispo <- TRUE
      }
    }
    
    # 4. Construction du graphique avec ggplot
    p <- ggplot(df_plot, aes(x = dat)) + theme_minimal()
    
    if (var_select == "BF") {
      p <- p + geom_line(aes(y = BF), color = "#2c3e50", linewidth = 0.8) +
        geom_hline(yintercept = df_plot$Vmax[1], color = "black", linetype = "dotted", alpha = 0.5) +
        geom_hline(yintercept = df_plot$Vmax[1] * 0.80, color = "red", linetype = "solid", linewidth = 1) 
      
      if (terrain_dispo && "Volume_Reel" %in% names(df_plot)) {
        p <- p + geom_line(aes(y = Volume_Reel), color = "#e67e22", linetype = "solid", linewidth = 1) +
          labs(title = paste("Volume quotidien -", nom_etang), 
               subtitle = "Rouge: Seuil 80% | Orange: Mesures Terrain (Sonde)",
               y = "Volume (m³)", x = "Date")
      } else {
        p <- p + labs(title = paste("Volume quotidien -", nom_etang), 
                      subtitle = "Trait rouge : Seuil de 80% de remplissage",
                      y = "Volume (m³)", x = "Date")
      }
      
    } else if (var_select == "RR") {
      p <- p + geom_col(aes(y = RR), fill = "#3498db") +
        labs(title = paste("Pluviométrie SAFRAN -", nom_etang), y = "Pluie (mm/période)", x = "Date")
    } else if (var_select == "Vsortant") {
      p <- p + geom_area(aes(y = Vsortant), fill = "#e74c3c", alpha = 0.5) +
        geom_line(aes(y = Vsortant), color = "#c0392b") +
        labs(title = paste("Déversement & Fuites -", nom_etang), y = "Volume sortant (m³)", x = "Date")
    } else {
      p <- p + geom_line(aes_string(y = var_select), color = "#16a085", linewidth = 0.8) +
        labs(title = paste("Analyse de :", var_select, "-", nom_etang), y = "Valeur", x = "Date")
    }
    
    # 5. Rendu interactif Plotly
    ggplotly(p, dynamicTicks = TRUE) %>%
      layout(
        hovermode = "x unified",
        xaxis = list(rangeslider = list(type = "date"))
      )
  })
  
  # Génération du tableau des indicateurs de performance (KPI)
  output$kpi_table <- renderTable({
    req(data_res$liste_finale)
    req(input$etang_choisi) 
    
    df <- data_res$liste_finale[[input$etang_choisi]]
    
    if (!is.null(input$viz_dates)) {
      df <- df %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
    }
    
    v_max <- df$Vmax[1] 
    
    # Calcul des statistiques descriptives et hydrologiques
    indics <- c("Volume Maximum Théorique (Vmax)",
                "Volume Maximum Atteint (sur la période)",
                "Nombre de jours à sec",
                "Nombre de jours >= 80% de remplissage", 
                "Nombre de jours <= 15% de remplissage", 
                "Taux de remplissage moyen")
    
    valeurs <- c(paste(format(round(v_max, 0), big.mark = " "), "m³"),
                 paste(format(round(max(df$BF, na.rm = TRUE), 0), big.mark = " "), "m³"),
                 paste(sum(df$BF <= 1, na.rm = TRUE), "jours"),
                 paste(sum(df$BF >= 0.80 * v_max, na.rm = TRUE), "jours"), 
                 paste(sum(df$BF <= 0.15 * v_max, na.rm = TRUE), "jours"), 
                 paste(round(mean(df$BF / v_max, na.rm = TRUE) * 100, 1), "%"))
    
    # Analyse comparative si les données terrain sont activées
    if (input$show_terrain) {
      df_terrain <- load_terrain(input$etang_choisi)
      if (!is.null(df_terrain)) {
        df_comp <- df %>% left_join(df_terrain, by = "dat") %>% drop_na(Volume_Reel)
        if (nrow(df_comp) > 0) {
          biais <- mean(df_comp$BF - df_comp$Volume_Reel, na.rm = TRUE)
          rmse <- sqrt(mean((df_comp$BF - df_comp$Volume_Reel)^2, na.rm = TRUE))
          
          indics <- c(indics, "BIAIS du modèle (Volume Simulé - Terrain)", "RMSE (Erreur absolue moyenne)")
          valeurs <- c(valeurs, paste(round(biais, 0), "m³"), paste(round(rmse, 0), "m³"))
        }
      }
    }
    
    data.frame(Indicateur = indics, Valeur = valeurs)
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  output$plot_exutoire <- renderPlotly({
    req(data_res$exutoire_data)
    
    # 1. Préparation des données Exutoire
    df_ex <- data_res$exutoire_data
    
    if (!is.null(input$viz_dates_exutoire)) {
      df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
    }
    
    # Lissage Exutoire (C'est un flux, on fait la somme)
    req(input$time_step_exutoire)
    if (input$time_step_exutoire != "day") {
      df_ex <- df_ex %>%
        mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>%
        group_by(dat) %>%
        summarise(Volume_Riviere = sum(Volume_Riviere, na.rm = TRUE), .groups = "drop")
    }
    
    # 2. Base du graphique : Tracer l'exutoire EN PREMIER (Arrière-plan)
    p <- ggplot() + theme_minimal() +
      geom_area(data = df_ex, aes(x = dat, y = Volume_Riviere, fill = "Débit Exutoire Total"), alpha = 0.2) +
      geom_line(data = df_ex, aes(x = dat, y = Volume_Riviere, color = "Débit Exutoire Total"), linewidth = 1)
    
    # 3. Préparation et ajout des données Étang EN DEUXIÈME (Par-dessus)
    if (!is.null(input$etang_superpose_ex) && input$etang_superpose_ex != "Aucun") {
      df_etang <- data_res$liste_finale[[input$etang_superpose_ex]]
      
      # On applique le même filtre de date
      if (!is.null(input$viz_dates_exutoire)) {
        df_etang <- df_etang %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
      }
      
      # On applique le lissage : ATTENTION, BF est un STOCK, on utilise la moyenne (mean)
      if (input$time_step_exutoire != "day") {
        df_etang <- df_etang %>%
          mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>%
          group_by(dat) %>%
          summarise(BF = mean(BF, na.rm = TRUE), .groups = "drop")
      }
      
      # Ajout de la courbe de l'étang (Variable BF) avec une ligne épaisse
      p <- p + geom_line(data = df_etang, aes(x = dat, y = BF, color = "Volume Stocké Étang (BF)"), linewidth = 1.2)
    }
    
    # 4. Paramétrage final des couleurs et des axes
    p <- p + 
      scale_color_manual(values = c("Volume Stocké Étang (BF)" = "#3498db", "Débit Exutoire Total" = "darkred")) +
      scale_fill_manual(values = c("Débit Exutoire Total" = "darkred")) +
      scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
      labs(title = "Comparaison Volume de l'Étang (BF) vs Débit de l'Exutoire",
           y = "Volume (m³)", x = "Date", color = "Légende", fill = "Zone")
    
    ggplotly(p, dynamicTicks = TRUE) %>%
      layout(hovermode = "x unified", xaxis = list(rangeslider = list(type = "date")))
  })
  # =======================================================
  # TABLEAU DES STATISTIQUES GLOBALES (EXUTOIRE)
  # =======================================================
  output$table_stats_exutoire <- renderTable({
    req(data_res$exutoire_data)
    req(data_res$liste_finale)
    
    df_ex <- data_res$exutoire_data
    
    # NOUVEAU : On filtre les données avec la plage de date choisie par l'utilisateur
    if (!is.null(input$viz_dates_exutoire)) {
      df_ex <- df_ex %>% 
        filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
    }
    
    # Si la plage de date ne renvoie aucune donnée, on évite le crash
    if(nrow(df_ex) == 0) return(NULL)
    
    # 1. Nombre de jours avec écoulement supérieur à 0 (Sur la période filtrée)
    jours_sup_0 <- sum(df_ex$Volume_Riviere > 8.64, na.rm = TRUE)
    
    # 2. Moyenne par jour (Sur la période filtrée)
    moy_jour <- mean(df_ex$Volume_Riviere, na.rm = TRUE)
    
    # 3. Moyenne par mois (Sur la période filtrée)
    df_mois <- df_ex %>%
      mutate(mois_an = lubridate::floor_date(dat, "month")) %>%
      group_by(mois_an) %>%
      summarise(vol = sum(Volume_Riviere, na.rm = TRUE), .groups = "drop")
    moy_mois <- mean(df_mois$vol, na.rm = TRUE)
    
    # 4. Moyenne par an (Sur la période filtrée)
    df_an <- df_ex %>%
      mutate(annee = lubridate::floor_date(dat, "year")) %>%
      group_by(annee) %>%
      summarise(vol = sum(Volume_Riviere, na.rm = TRUE), .groups = "drop")
    moy_an <- mean(df_an$vol, na.rm = TRUE)
    
    # 5. Calcul du nombre d'étangs pleins (BF >= Vmax) à la date sélectionnée
    nb_etangs_pleins <- 0
    date_choisie <- input$date_stat
    
    if (!is.null(date_choisie)) {
      # On boucle sur la liste de tous les étangs simulés
      for (nom in names(data_res$liste_finale)) {
        df_etang <- data_res$liste_finale[[nom]]
        ligne_date <- df_etang[df_etang$dat == as.Date(date_choisie), ]
        
        if (nrow(ligne_date) > 0) {
          # Arrondi pour éviter les erreurs informatiques sur les petites décimales
          if (round(ligne_date$BF, 0) >= round(ligne_date$Vmax, 0)) {
            nb_etangs_pleins <- nb_etangs_pleins + 1
          }
        }
      }
    }
    
    # Construction du tableau de présentation
    data.frame(
      "Indicateur" = c(
        "Nombre de jours avec écoulement (> 0 m³) sur la période",
        "Volume moyen relâché par JOUR sur la période",
        "Volume moyen relâché par MOIS sur la période",
        "Volume moyen relâché par AN sur la période",
        paste("Nombre d'étangs pleins le", format(as.Date(date_choisie), "%d/%m/%Y"))
      ),
      "Valeur" = c(
        paste(format(jours_sup_0, big.mark = " "), "jours"),
        paste(format(round(moy_jour, 0), big.mark = " "), "m³"),
        paste(format(round(moy_mois, 0), big.mark = " "), "m³"),
        paste(format(round(moy_an, 0), big.mark = " "), "m³"),
        paste(nb_etangs_pleins, "étang(s) sur", length(data_res$liste_finale))
      )
    )
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
}