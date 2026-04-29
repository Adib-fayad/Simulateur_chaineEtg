library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)
library(plotly)
library(sf)        
library(leaflet)  

# =======================================================
# SERVEUR (LOGIQUE DE CALCUL)
# =======================================================
server <- function(input, output, session) {
  
  # Fonction de calcul du bilan hydrologique journalier
  Bfinal <- function(Vmax, BF, Vp_etp, Volume_R, Vamont, VFuite, Statut_Assec, Volume_Vidange_Jour, Peche_Jour){
    
    Eau_Dispo = BF + Volume_R + Vamont
    
    Fuite_Reelle = min(VFuite, max(0, Eau_Dispo))
    Eau_Dispo = Eau_Dispo - Fuite_Reelle
    Vsortant = Fuite_Reelle 
    
    if (Statut_Assec == "Assec" || Peche_Jour == "oui") {
      Evap_Reelle = max(0, Vp_etp)
    } else {
      if (Vp_etp < 0) {
        Evap_Reelle = max(Vp_etp, -Eau_Dispo) 
      } else {
        Evap_Reelle = Vp_etp 
      }
    }
    Eau_Dispo = Eau_Dispo + Evap_Reelle 
    
    if (Peche_Jour == "oui") {
      Vsortant = Vsortant + Eau_Dispo
      BF = 0
    } else if (Volume_Vidange_Jour > 0) {
      Objectif_Volume = max(0, BF - Volume_Vidange_Jour)
      if (Eau_Dispo > Objectif_Volume) {
        Volume_a_vider = Eau_Dispo - Objectif_Volume
        Volume_theorique = max(Volume_a_vider, Volume_Vidange_Jour)
      } else {
        Volume_theorique = Volume_Vidange_Jour
      }
      Volume_reel_vide = min(Volume_theorique, max(0, Eau_Dispo))
      Vsortant = Vsortant + Volume_reel_vide
      Eau_Dispo = Eau_Dispo - Volume_reel_vide
      
      if (Eau_Dispo > Vmax) {
        Surplus = Eau_Dispo - Vmax
        Vsortant = Vsortant + Surplus
        BF = Vmax 
      } else {
        BF = Eau_Dispo
      }
    } else if (Statut_Assec == "Assec") {
      Vsortant = Vsortant + Eau_Dispo
      BF = 0
    } else {
      if (Eau_Dispo > Vmax) {
        Surplus = Eau_Dispo - Vmax
        Vsortant = Vsortant + Surplus
        BF = Vmax 
      } else {
        BF = Eau_Dispo
      }
    } 
    
    return(list(BF = BF, Vsortant = Vsortant, Evap_Reelle = Evap_Reelle, Fuite_Reelle = Fuite_Reelle))
  }
  
  # Fonction de chargement des données d'observation terrain
  load_terrain <- function(nom_etang) {
    nom_propre <- stringr::str_to_title(tolower(nom_etang))
    
    chemins_possibles <- c(
      paste0("Volume_etang/", nom_etang, ".Rdata"),
      paste0("Volume_etang/", nom_propre, ".Rdata"),
      paste0("Volume_etang/", tolower(nom_etang), ".Rdata")
    )
    
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
  
  # Initialisation des variables réactives
  data_res <- reactiveValues(
    sim_safran = list(liste_finale = NULL, exutoire_data = NULL),
    sim_capteurs = list(liste_finale = NULL, exutoire_data = NULL),
    log = "En attente d'exécution...", 
    pond_names = NULL
  )
  
  # =======================================================
  # LECTURE DE LA COUCHE GEOGRAPHIQUE (SIG)
  # =======================================================
  etgs_shape <- reactive({
    req(file.exists("SIG/ETG.gpkg"))
    st_read("SIG/ETG.gpkg", layer = "CHALAMONT", quiet = TRUE) %>%
      st_transform(4326) 
  })
  
  # --- FONCTION PRINCIPALE DE SIMULATION ---
  run_hydrological_model <- function(fichier_meteo, tab_etg, bv_code, dates_input, ordre_topologique) {
    
    coordonnees <- read.csv("meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% 
      filter(CODE == bv_code)
    
    X_ref <- coordonnees$LAMBX[1]
    Y_ref <- coordonnees$LAMBY[1]
    
    meteo_all <- read.csv2(paste0("meteo/SAFRAN/", fichier_meteo), stringsAsFactors = FALSE)
    
    maille_proche <- meteo_all %>%
      select(LAMBX, LAMBY) %>% distinct() %>%
      mutate(dist = sqrt((LAMBX - X_ref)^2 + (LAMBY - Y_ref)^2)) %>% 
      arrange(dist) %>% head(1)
    
    pluvio <- meteo_all %>%
      filter(LAMBX == maille_proche$LAMBX & LAMBY == maille_proche$LAMBY) %>%
      rename(RR = PRELIQ) %>%
      mutate(
        dat = as.Date(lubridate::parse_date_time(as.character(DATE), orders = c("ymd", "dmy", "Ymd", "Y-m-d"))),
        RR = as.numeric(gsub(",", ".", as.character(RR))),
        ETP_grille = as.numeric(gsub(",", ".", as.character(ETP))),
        P_ETP = RR - ETP_grille
      ) %>%
      filter(between(dat, dates_input[1], dates_input[2])) %>%
      arrange(dat)
    
    pluvio_calc <- pluvio %>%
      arrange(dat) %>%
      mutate(
        cumul_5j = rollsum(RR, k = 5, fill = NA, align = "right"),
        Pant = lag(cumul_5j, n = 1, default = 0)
      ) %>%
      mutate(Pant = replace_na(Pant, 0))
    
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
    
    df_bilan <- tab_etg %>%
      select(NOM, Surface_BV, SURFACE_eau, Vmax, CNI, CNII, CNIII, Exutoire_1) %>%
      cross_join(pluvio_calc) %>%
      mutate(annee = format(dat, "%Y")) %>%
      left_join(table_dates, by = c("NOM", "annee")) %>%
      mutate(
        mois = as.numeric(format(dat, "%m")),
        is_ete = (mois >= 5 & mois <= 10),
        Pant_num = replace_na(as.numeric(Pant), 0),
        
        CN_jour = case_when(
          is_ete & Pant_num < 36 ~ CNI,
          is_ete & Pant_num > 53 ~ CNIII,
          is_ete ~ CNII,
          !is_ete & Pant_num < 13 ~ CNI,
          !is_ete & Pant_num > 28 ~ CNIII,
          !is_ete ~ CNII,
          TRUE ~ CNII
        ),
        
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
        
        VFuite = round(0.1 * 3600 * 24) / 1000,
        Vidange = if_else(dat == as.Date(Vidange), "oui", "non", missing = "non"),
        peche   = if_else(dat == as.Date(peche), "oui", "non", missing = "non")
      ) %>% 
      # 1. On détermine d'abord le statut de l'étang (Assec ou Evolage)
      group_by(NOM, annee) %>%
      mutate(
        Statut_Simu = case_when(
          peche == "oui" | Vidange == "oui" ~ "Evolage",
          cumany(peche == "oui") ~ Assec_Futur,
          TRUE ~ assec
        )
      ) %>% 
      ungroup() %>%
      # 2. NOUVEAU : On calcule l'eau EN FONCTION du statut
      mutate(
        # L'ASTUCE EST ICI : Si Assec, le fond de l'étang redevient de la terre (Surface_BV entière)
        Surface_Active_Ruissellement = if_else(Statut_Simu == "Assec", Surface_BV, Surface_BV - SURFACE_eau),
        
        Volume_R = CR * RR_num * Surface_Active_Ruissellement * 10,
        
        # Si Assec, il n'y a plus de plan d'eau, donc la pluie directe et l'évaporation d'eau libre tombent à zéro 
        # (L'évaporation et l'infiltration sont maintenant gérées par la formule du Curve Number !)
        Vp_etp = if_else(Statut_Simu == "Assec", 0, replace_na(as.numeric(P_ETP), 0) * SURFACE_eau * 10),
        
        Vamont = 0,
        BF = case_when(
          dat == dates_input[1] & assec == "Evolage" ~ Vmax / 2, 
          TRUE ~ 0
        )
      ) %>%
      select(-annee, -assec, -Assec_Futur)
    
    liste_etangs <- df_bilan %>% split(.$NOM)
    
    Volume_Total_Exutoire_BV <- data.frame(
      dat = liste_etangs[[1]]$dat, 
      Volume_Riviere = rep(0, nrow(liste_etangs[[1]])) 
    )
    
    for (nom_etang in ordre_topologique) {
      etangs_calcule <- liste_etangs[[nom_etang]]
      Stockage_Vamont <- numeric(nrow(etangs_calcule))
      etangs_calcule$Vol_Vidange_Jour <- 0
      etangs_calcule$Vsortant <- 0  
      etangs_calcule$Evap_Reelle <- 0
      etangs_calcule$Fuite_Reelle <- 0
      
      lignes_vidange <- which(etangs_calcule$Vidange == "oui")
      lignes_peche <- which(etangs_calcule$peche == "oui")
      
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
        etangs_calcule$Evap_Reelle[jour] = resultat$Evap_Reelle
        etangs_calcule$Fuite_Reelle[jour] = resultat$Fuite_Reelle
      }
      
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
    
    return(list(liste_finale = liste_etangs, exutoire_data = Volume_Total_Exutoire_BV))
  }
  
  
  # Gestionnaire d'événements pour le lancement de la simulation
  observeEvent(input$run_sim, {
    req(input$file_os, input$file_etg, input$file_assec, input$file_vidange)
    
    id_chargement <- showNotification("Lecture des fichiers en cours, veuillez patienter...", 
                                      duration = NULL, closeButton = FALSE, type = "message")
    
    data_res$log <- "Initialisation des calculs...\n"
    
    tryCatch({
      os_data <- read.csv2(input$file_os$datapath, dec = ".", sep = ",") %>%
        select(ClasseOS = 1, Etang = 2, Surface = 3) %>%
        filter(ClasseOS < 23)
      
      tab_cn <- read.csv2("OS_CN.csv", sep = ";", encoding = "latin1")
      
      os_complet <- os_data %>% left_join(tab_cn, by = c("ClasseOS" = "Code_OS"))
      
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
      
      etg_params <- read.csv2(input$file_etg$datapath, header = TRUE, dec = ",", sep = ";") %>% 
        filter(Chaine_etu == "oui") %>% 
        select(-num_range("Assec", 2021:2025))
      
      assec_data <- read.csv2(input$file_assec$datapath, header = TRUE, sep = ";") %>% 
        select(-Exutoire_1, -OBJECTID)
      
      etg_model <- assec_data %>% 
        inner_join(etg_params, by ="NOM") %>%
        mutate(Vmax = ifelse(is.na(Vmax), SURFACE_eau * Profondeur_m * 10000, Vmax))
      
      vidange_raw <- read.csv(input$file_vidange$datapath, sep = ",")
      
      tab_etg <- cnetg %>%
        rename(NOM = Etang) %>%
        inner_join(etg_model, by = "NOM") %>% 
        select(-any_of("Vidange")) %>%
        left_join(vidange_raw %>% select(-Exutoire_1, -OBJECTID), by = "NOM") 
      
      # Préparation des jours de vidange et soustraction pour créer la date de début de vidange
      tab_etg <- tab_etg %>%
        mutate(
          jours_vidange = ceiling(SURFACE_eau),
          across(
            .cols = starts_with("peche"),                 
            .fns = ~ as.Date(.x) - jours_vidange,         
            .names = "{gsub('peche', 'Vidange', .col)}"   
          )
        )
      
      liens <- tab_etg %>% 
        select(NOM, Exutoire_1) %>% 
        filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT") %>%
        distinct()
      
      reseau <- graph_from_data_frame(liens, directed = TRUE)
      ordre_topologique <- names(topo_sort(reseau, mode = "out"))
      
      data_res$log <- paste0(data_res$log, "Lancement Simulation SAFRAN...\n")
      res_safran <- run_hydrological_model(
        fichier_meteo = "Meteo_Dombes_Legere.csv", 
        tab_etg = tab_etg, 
        bv_code = input$bv_code, 
        dates_input = input$dates, 
        ordre_topologique = ordre_topologique
      )
      
      data_res$log <- paste0(data_res$log, "Lancement Simulation Capteurs Locaux...\n")
      res_capteurs <- run_hydrological_model(
        fichier_meteo = "Meteo_Dombes_Mise_A_Jour_Finale.csv", 
        tab_etg = tab_etg, 
        bv_code = input$bv_code, 
        dates_input = input$dates, 
        ordre_topologique = ordre_topologique
      )
      
      data_res$sim_safran$liste_finale <- res_safran$liste_finale
      data_res$sim_safran$exutoire_data <- res_safran$exutoire_data
      
      data_res$sim_capteurs$liste_finale <- res_capteurs$liste_finale
      data_res$sim_capteurs$exutoire_data <- res_capteurs$exutoire_data
      
      data_res$pond_names <- ordre_topologique
      data_res$log <- paste0(data_res$log, "Simulations terminées avec succès.\n")
      
      # On mémorise ce que tu regardais AVANT le recalcul
      etang_actuel <- isolate(input$etang_choisi)
      etang_ex_actuel <- isolate(input$etang_superpose_ex)
      
      # On met à jour les listes en forçant la sélection à rester la même (sauf si c'est la 1ere fois)
      if (is.null(etang_actuel)) {
        updateSelectInput(session, "etang_choisi", choices = data_res$pond_names)
        updateSelectInput(session, "etang_superpose_ex", choices = c("Aucun", data_res$pond_names))
      } else {
        updateSelectInput(session, "etang_choisi", choices = data_res$pond_names, selected = etang_actuel)
        updateSelectInput(session, "etang_superpose_ex", choices = c("Aucun", data_res$pond_names), selected = etang_ex_actuel)
      }
      
      removeNotification(id_chargement)
      showNotification("Simulations terminées avec succès !", type = "message", duration = 5)
      
    }, error = function(e) {
      removeNotification(id_chargement)
      data_res$log <- paste0(data_res$log, "Erreur lors de l'exécution : ", e$message)
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  }, ignoreInit = TRUE)
  
  get_active_sim <- reactive({
    if(input$source_meteo == "safran") return(data_res$sim_safran)
    return(data_res$sim_capteurs)
  })
  
  get_alt_sim <- reactive({
    if(input$source_meteo == "safran") return(data_res$sim_capteurs)
    return(data_res$sim_safran)
  })
  
  output$ui_zoom_dates <- renderUI({
    req(get_active_sim()$liste_finale)
    df <- get_active_sim()$liste_finale[[1]] 
    dateRangeInput("viz_dates", "Plage de visualisation :",
                   start = min(df$dat), end = max(df$dat),
                   min = min(df$dat), max = max(df$dat))
  })
  
  output$ui_zoom_dates_exutoire <- renderUI({
    req(get_active_sim()$exutoire_data)
    df <- get_active_sim()$exutoire_data 
    req(nrow(df) > 0)
    
    dateRangeInput("viz_dates_exutoire", "Plage de visualisation :",
                   start = min(df$dat), end = max(df$dat),
                   min = min(df$dat), max = max(df$dat))
  })
  
  output$plot_calibration <- renderPlotly({
    req(get_active_sim()$liste_finale)
    req(input$etang_choisi) 
    req(input$var_plot)
    
    nom_etang <- input$etang_choisi
    var_select <- input$var_plot
    
    df_plot <- get_active_sim()$liste_finale[[nom_etang]]
    
    req(df_plot)
    
    df_alt <- NULL
    if(input$show_alt_meteo) {
      df_alt <- get_alt_sim()$liste_finale[[nom_etang]]
    }
    
    if (!is.null(input$viz_dates)) {
      df_plot <- df_plot %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
      if(!is.null(df_alt)) df_alt <- df_alt %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
    }
    
    req(input$time_step)
    if (input$time_step != "day") {
      
      lisser_df <- function(d){
        d %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step)) %>%
          group_by(dat) %>%
          summarise(
            BF = mean(BF, na.rm = TRUE),
            Vsortant = sum(Vsortant, na.rm = TRUE),
            RR = sum(RR, na.rm = TRUE),
            Volume_R = sum(Volume_R, na.rm = TRUE),
            Vp_etp = sum(Vp_etp, na.rm = TRUE),
            Vol_Vidange_Jour = sum(Vol_Vidange_Jour, na.rm = TRUE),
            Vmax = first(Vmax),
            .groups = "drop"
          )
      }
      
      df_plot <- lisser_df(df_plot)
      if(!is.null(df_alt)) df_alt <- lisser_df(df_alt)
    }
    
    terrain_dispo <- FALSE
    if (input$show_terrain) {
      df_terrain <- load_terrain(nom_etang)
      if (!is.null(df_terrain)) {
        if (input$time_step != "day") {
          df_terrain <- df_terrain %>%
            mutate(dat = lubridate::floor_date(dat, unit = input$time_step)) %>%
            group_by(dat) %>%
            summarise(Volume_Reel = mean(Volume_Reel, na.rm = TRUE), .groups = "drop")
        }
        df_plot <- df_plot %>% left_join(df_terrain, by = "dat")
        terrain_dispo <- TRUE
      }
    }
    
    p <- ggplot(df_plot, aes(x = dat)) + theme_minimal()
    
    label_main <- ifelse(input$source_meteo == "safran", "Modèle (SAFRAN)", "Modèle (Capteurs)")
    label_alt <- ifelse(input$source_meteo == "safran", "Modèle (Capteurs)", "Modèle (SAFRAN)")
    
    if (var_select == "BF") {
      
      p <- p + geom_line(aes(y = BF, color = label_main), linewidth = 0.8) +
        geom_hline(yintercept = df_plot$Vmax[1], color = "black", linetype = "dotted", alpha = 0.5) +
        geom_hline(yintercept = df_plot$Vmax[1] * 0.80, color = "red", linetype = "solid", linewidth = 1) 
      
      if (!is.null(df_alt)) {
        p <- p + geom_line(data = df_alt, aes(y = BF, color = label_alt), linetype = "dashed", linewidth = 0.8)
      }
      
      if (terrain_dispo && "Volume_Reel" %in% names(df_plot)) {
        p <- p + geom_line(aes(y = Volume_Reel, color = "Sonde (Terrain)"), linetype = "solid", linewidth = 1) 
      }
      
      my_colors <- setNames(c("#2c3e50", "#27ae60", "#e67e22"), c(label_main, label_alt, "Sonde (Terrain)"))
      
      p <- p + scale_color_manual(values = my_colors) +
        labs(title = paste("Volume quotidien -", nom_etang), 
             subtitle = "Rouge: Seuil 80%", y = "Volume (m³)", x = "Date", color = "Légende")
      
    } else if (var_select == "RR") {
      p <- p + geom_col(aes(y = RR, fill = label_main), alpha = 0.7)
      if(!is.null(df_alt)){
        p <- p + geom_col(data = df_alt, aes(y = RR, fill = label_alt), alpha = 0.4, position = "identity")
      }
      my_fills <- setNames(c("#3498db", "#9b59b6"), c(label_main, label_alt))
      p <- p + scale_fill_manual(values = my_fills) +
        labs(title = paste("Pluviométrie -", nom_etang), y = "Pluie (mm/période)", x = "Date", fill="Source")
      
    } else {
      p <- p + geom_line(aes_string(y = var_select, color = shQuote(label_main)), linewidth = 0.8)
      if(!is.null(df_alt)){
        p <- p + geom_line(data=df_alt, aes_string(y = var_select, color = shQuote(label_alt)), linetype="dashed", linewidth=0.8)
      }
      my_colors <- setNames(c("#16a085", "#8e44ad"), c(label_main, label_alt))
      p <- p + scale_color_manual(values = my_colors) +
        labs(title = paste("Analyse de :", var_select, "-", nom_etang), y = "Valeur", x = "Date", color="Source")
    }
    
    ggplotly(p, dynamicTicks = TRUE) %>%
      layout(
        hovermode = "x unified",
        xaxis = list(rangeslider = list(type = "date"))
      )
  })
  
  output$kpi_table <- renderTable({
    req(get_active_sim()$liste_finale)
    req(input$etang_choisi) 
    
    df <- get_active_sim()$liste_finale[[input$etang_choisi]]
    
    req(df)
    
    if (!is.null(input$viz_dates)) {
      df <- df %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
    }
    
    v_max <- df$Vmax[1] 
    
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
    req(get_active_sim()$exutoire_data)
    
    df_ex <- get_active_sim()$exutoire_data
    
    if(nrow(df_ex) == 0) {
      return(plot_ly() %>% layout(title = "En attente de données..."))
    }
    
    if (!is.null(input$viz_dates_exutoire)) {
      df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
    }
    
    req(input$time_step_exutoire)
    if (input$time_step_exutoire != "day") {
      df_ex <- df_ex %>%
        mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>%
        group_by(dat) %>%
        summarise(Volume_Riviere = sum(Volume_Riviere, na.rm = TRUE), .groups = "drop")
    }
    
    p <- ggplot() + theme_minimal() +
      geom_area(data = df_ex, aes(x = dat, y = Volume_Riviere, fill = "Débit Exutoire Total"), alpha = 0.2) +
      geom_line(data = df_ex, aes(x = dat, y = Volume_Riviere, color = "Débit Exutoire Total"), linewidth = 1)
    
    if (!is.null(input$etang_superpose_ex) && input$etang_superpose_ex != "Aucun") {
      df_etang <- get_active_sim()$liste_finale[[input$etang_superpose_ex]]
      
      if (!is.null(df_etang)) {
        if (!is.null(input$viz_dates_exutoire)) {
          df_etang <- df_etang %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
        }
        
        if (input$time_step_exutoire != "day") {
          df_etang <- df_etang %>%
            mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>%
            group_by(dat) %>%
            summarise(BF = mean(BF, na.rm = TRUE), .groups = "drop")
        }
        
        p <- p + geom_line(data = df_etang, aes(x = dat, y = BF, color = "Volume Stocké Étang (BF)"), linewidth = 1.2)
      }
    }
    
    p <- p + 
      scale_color_manual(values = c("Volume Stocké Étang (BF)" = "#3498db", "Débit Exutoire Total" = "darkred")) +
      scale_fill_manual(values = c("Débit Exutoire Total" = "darkred")) +
      scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
      labs(title = "Comparaison Volume de l'Étang (BF) vs Débit de l'Exutoire",
           y = "Volume (m³)", x = "Date", color = "Légende", fill = "Zone")
    
    ggplotly(p, dynamicTicks = TRUE) %>%
      layout(hovermode = "x unified", xaxis = list(rangeslider = list(type = "date")))
  })
  
  output$table_stats_exutoire <- renderTable({
    req(get_active_sim()$exutoire_data)
    req(get_active_sim()$liste_finale)
    
    df_ex <- get_active_sim()$exutoire_data
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    
    if (!is.null(input$viz_dates_exutoire)) {
      df_ex <- df_ex %>% 
        filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
      df_all <- df_all %>% 
        filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
    }
    
    if(nrow(df_ex) == 0) return(NULL)
    
    jours_sup_0 <- sum(df_ex$Volume_Riviere > 8.64, na.rm = TRUE)
    vol_total_sorti <- sum(df_ex$Volume_Riviere, na.rm = TRUE)
    moy_jour <- mean(df_ex$Volume_Riviere, na.rm = TRUE)
    
    capa_max_totale <- sum(sapply(get_active_sim()$liste_finale, function(x) x$Vmax[1]))
    date_fin <- max(df_all$dat)
    vol_final_stocke <- sum(df_all$BF[df_all$dat == date_fin], na.rm = TRUE)
    taux_remplissage_final <- (vol_final_stocke / capa_max_totale) * 100
    apport_total_pluie <- sum(df_all$Volume_R, na.rm = TRUE)
    
    df_fin <- df_all %>% filter(dat == date_fin)
    nb_etangs_pleins <- sum(round(df_fin$BF, 0) >= round(df_fin$Vmax, 0), na.rm = TRUE)
    nb_total_etangs <- length(unique(df_all$NOM))
    
    data.frame(
      "Indicateur_Global_du_Bassin" = c(
        "Capacité maximale de stockage de toute la chaîne",
        paste("Volume total d'eau retenu le", format(date_fin, "%d/%m/%Y")),
        "Taux de remplissage global de la chaîne à cette date",
        paste("Nombre d'étangs pleins le", format(date_fin, "%d/%m/%Y")),
        "Volume total apporté par le ruissellement (Pluie) sur la période",
        "Volume total définitivement évacué par l'exutoire final",
        "Nombre de jours où l'exutoire final a coulé (> 0 m³)",
        "Volume moyen relâché par l'exutoire final par jour"
      ),
      "Valeur" = c(
        paste(format(round(capa_max_totale, 0), big.mark = " "), "m³"),
        paste(format(round(vol_final_stocke, 0), big.mark = " "), "m³"),
        paste(round(taux_remplissage_final, 1), "%"),
        paste(nb_etangs_pleins, "étang(s) sur", nb_total_etangs),
        paste(format(round(apport_total_pluie, 0), big.mark = " "), "m³"),
        paste(format(round(vol_total_sorti, 0), big.mark = " "), "m³"),
        paste(format(jours_sup_0, big.mark = " "), "jours"),
        paste(format(round(moy_jour, 0), big.mark = " "), "m³")
      )
    )
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # =======================================================
  # GRAPHIQUE BILAN HYDRIQUE GLOBAL (Flux purs : Entrées vs Sorties)
  # =======================================================
  output$plot_bilan_pie <- renderPlotly({
    req(get_active_sim()$exutoire_data)
    req(get_active_sim()$liste_finale)
    
    df_ex <- get_active_sim()$exutoire_data
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    
    if (!is.null(input$viz_dates_exutoire)) {
      df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
      df_all <- df_all %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
    }
    
    # ---------------------------------------------------------
    # 1. CALCUL DES ENTRÉES (FLUX ENTRANTS purs)
    # ---------------------------------------------------------
    apport_ruissellement <- sum(df_all$Volume_R, na.rm = TRUE)
    
    # L'eau qui tombe directement sur l'étang (jours où Pluie > Evapo)
    apport_pluie_directe <- sum(df_all$Evap_Reelle[df_all$Evap_Reelle > 0], na.rm = TRUE)
    
    df_gains <- data.frame(
      Categorie = c("Ruissellement du Bassin Versant", "Pluie directe sur les Étangs"),
      Volume = c(apport_ruissellement, apport_pluie_directe)
    )
    
    # ---------------------------------------------------------
    # 2. CALCUL DES SORTIES (FLUX SORTANTS purs)
    # ---------------------------------------------------------
    # L'eau qui s'évapore (jours où Evapo > Pluie)
    perte_evap <- sum(abs(df_all$Evap_Reelle[df_all$Evap_Reelle < 0]), na.rm = TRUE)
    
    # L'eau qui quitte définitivement la chaîne
    sortie_riviere <- sum(df_ex$Volume_Riviere, na.rm = TRUE)
    
    df_pertes <- data.frame(
      Categorie = c("Évaporation", "Évacuée à la Rivière (Exutoire)"),
      Volume = c(perte_evap, sortie_riviere)
    )
    
    # ---------------------------------------------------------
    # 3. CRÉATION DU DOUBLE GRAPHIQUE EN ANNEAU
    # ---------------------------------------------------------
    plot_ly() %>%
      
      # --- PREMIER CAMEMBERT (GAINS) - Placé à gauche ---
      add_pie(data = df_gains, labels = ~Categorie, values = ~Volume,
              name = "Entrées", textinfo = 'percent', hoverinfo = 'label+text+percent',
              text = ~paste(format(round(Volume, 0), big.mark = " "), "m³"),
              marker = list(colors = c("#2980b9", "#7fb3d5")), # Bleu fort (Ruissellement) et Bleu clair (Pluie)
              domain = list(x = c(0, 0.45), y = c(0, 1)),
              title = list(text = "<b>APPORTS D'EAU</b><br>(Gains)", font = list(size = 14))) %>%
      
      # --- DEUXIÈME CAMEMBERT (SORTIES) - Placé à droite ---
      add_pie(data = df_pertes, labels = ~Categorie, values = ~Volume,
              name = "Sorties", textinfo = 'percent', hoverinfo = 'label+text+percent',
              text = ~paste(format(round(Volume, 0), big.mark = " "), "m³"),
              marker = list(colors = c("#e74c3c", "#2c3e50")), # Rouge (Evap), Bleu très foncé (Exutoire)
              domain = list(x = c(0.55, 1), y = c(0, 1)),
              title = list(text = "<b>PERTES D'EAU</b><br>(Sorties)", font = list(size = 14))) %>%
      
      # --- MISE EN PAGE GLOBALE ---
      layout(title = list(text = "<b>Bilan des Flux du Réseau (Entrées vs Sorties)</b>", font = list(size = 18)),
             showlegend = TRUE,
             margin = list(t = 80, b = 20, l = 10, r = 10))
  })
  
  output$plot_gantt_assec <- renderPlotly({
    req(get_active_sim()$liste_finale)
    req(data_res$pond_names)
    
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    
    if (!is.null(input$viz_dates_gantt)) {
      df_all <- df_all %>%
        filter(dat >= input$viz_dates_gantt[1] & dat <= input$viz_dates_gantt[2])
    }
    
    gestion_periods <- df_all %>%
      arrange(NOM, dat) %>%
      group_by(NOM) %>%
      mutate(
        Statut = ifelse(is.na(Statut_Simu), "Evolage", Statut_Simu),
        changement = coalesce(Statut != lag(Statut), FALSE),
        period_id = cumsum(changement)
      ) %>%
      group_by(NOM, period_id, Statut) %>%
      summarise(
        debut = min(dat),
        fin = max(dat),
        duree = as.numeric(fin - debut) + 1,
        .groups = "drop"
      ) %>%
      filter(duree > 0)
    
    if(nrow(gestion_periods) == 0) return(NULL)
    
    ordre_inverse <- rev(data_res$pond_names) 
    gestion_periods$NOM <- factor(gestion_periods$NOM, levels = ordre_inverse)
    
    p <- ggplot(gestion_periods, aes(x = debut, xend = fin, y = NOM, yend = NOM, 
                                     color = Statut, 
                                     text = paste("Statut :", Statut, "<br>Du", format(debut, "%d/%m/%Y"), "au", format(fin, "%d/%m/%Y"), "<br>Durée :", duree, "jours"))) +
      geom_segment(linewidth = 1.5) + 
      scale_color_manual(values = c("Assec" = "#e67e22", "Evolage" = "#3498db")) +
      theme_minimal() +
      labs(
        title = "Calendrier de Gestion (Assec & Évolage)",
        x = "Date",
        y = "Étangs (Amont en haut -> Aval en bas)",
        color = "Statut"
      ) +
      theme(
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest") %>%
      config(displayModeBar = TRUE)
  })
  
  output$plot_operations <- renderPlotly({
    req(get_active_sim()$liste_finale)
    req(data_res$pond_names)
    
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    
    if (!is.null(input$viz_dates_operations)) {
      df_all <- df_all %>%
        filter(dat >= input$viz_dates_operations[1] & dat <= input$viz_dates_operations[2])
    }
    
    df_points_op <- df_all %>%
      mutate(
        Operation = case_when(
          peche == "oui" ~ "Pêche",
          Vol_Vidange_Jour > 0 ~ "Vidange",
          TRUE ~ "Rien"
        )
      ) %>%
      filter(Operation %in% c("Pêche", "Vidange")) %>%
      mutate(
        Texte_Survol = paste0(
          "<b>Date :</b> ", format(dat, "%d/%m/%Y"), "<br>",
          "<b>Action :</b> ", Operation, "<br>",
          "<b>Eau évacuée :</b> ", format(round(Vsortant, 0), big.mark = " "), " m³"
        )
      ) %>%
      select(NOM, dat, Operation, Texte_Survol)
    
    if(nrow(df_points_op) == 0) return(NULL)
    
    ordre_inverse <- rev(data_res$pond_names) 
    df_points_op$NOM <- factor(df_points_op$NOM, levels = ordre_inverse)
    df_points_op$Operation <- factor(df_points_op$Operation, levels = c("Vidange", "Pêche"))
    
    p <- ggplot(df_points_op, aes(x = dat, y = NOM, color = Operation, text = Texte_Survol)) +
      geom_point(shape = 16, size = 1.5) +
      scale_color_manual(values = c(
        "Vidange" = "#f39c12", 
        "Pêche"   = "#c0392b"  
      )) +
      theme_minimal() +
      labs(
        title = "Opérations Journalières (Vidanges et Pêches)",
        x = "Date",
        y = "Étangs (Amont -> Aval)",
        color = "Opération"
      ) +
      theme(
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest") %>%
      config(displayModeBar = TRUE)
  })
  
  # =======================================================
  # MODULE DE CARTOGRAPHIE (Avec Synchronisation & Superposition)
  # =======================================================
  
  output$ui_calendrier_carte <- renderUI({
    req(get_active_sim()$liste_finale)
    df <- get_active_sim()$liste_finale[[1]]
    dateInput("date_carte_calendrier", "Choisir une date précise :",
              min = min(df$dat), max = max(df$dat),
              value = min(df$dat),
              format = "dd/mm/yyyy",
              language = "fr",
              width = "100%")
  })
  
  output$ui_slider_carte <- renderUI({
    req(get_active_sim()$liste_finale)
    df <- get_active_sim()$liste_finale[[1]]
    sliderInput("date_carte_slider", "Faire défiler ou animer :",
                min = min(df$dat), max = max(df$dat),
                value = min(df$dat),
                timeFormat = "%d/%m/%Y",
                animate = animationOptions(interval = 1000, loop = FALSE), 
                width = "100%")
  })
  
  observeEvent(input$date_carte_slider, {
    updateDateInput(session, "date_carte_calendrier", value = input$date_carte_slider)
  }, ignoreInit = TRUE)
  
  observeEvent(input$date_carte_calendrier, {
    updateSliderInput(session, "date_carte_slider", value = as.Date(input$date_carte_calendrier))
  }, ignoreInit = TRUE)
  
  output$map_etangs <- renderLeaflet({
    req(etgs_shape())
    limites <- st_bbox(etgs_shape())
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(lng1 = as.numeric(limites["xmin"]), lat1 = as.numeric(limites["ymin"]), 
                lng2 = as.numeric(limites["xmax"]), lat2 = as.numeric(limites["ymax"])) %>%
      addMapPane("polygones_pane", zIndex = 410) %>%
      addMapPane("bulles_pane", zIndex = 420)
  })
  
  observe({
    req(input$date_carte_slider, get_active_sim()$liste_finale, etgs_shape())
    
    data_jour <- bind_rows(get_active_sim()$liste_finale, .id = "NOM") %>%
      filter(dat == input$date_carte_slider)
    
    map_data <- etgs_shape() %>%
      left_join(data_jour, by = "NOM") 
    
    map_data$Statut_Simu <- ifelse(is.na(map_data$Statut_Simu), "Evolage", map_data$Statut_Simu)
    map_data$BF <- ifelse(is.na(map_data$BF), 0, map_data$BF)
    map_data$Volume_R <- ifelse(is.na(map_data$Volume_R), 0, map_data$Volume_R)
    
    map_data$Taux_Remplissage <- ifelse(!is.na(map_data$Vmax) & map_data$Vmax > 0, 
                                        (map_data$BF / map_data$Vmax) * 100, 0)
    map_data$Taux_Remplissage <- ifelse(map_data$Taux_Remplissage > 100, 100, map_data$Taux_Remplissage)
    
    map_data <- map_data %>%
      mutate(
        Etat_Sol = case_when(
          round(CN_jour, 1) == round(CNI, 1) ~ "Sec (CN I)",
          round(CN_jour, 1) == round(CNIII, 1) ~ "Saturé (CN III)",
          TRUE ~ "Normal (CN II)"
        )
      )
    
    proxy <- leafletProxy("map_etangs") %>% clearShapes() %>% clearControls() %>% clearMarkers()
    
    if (input$color_carte == "etat_sol") {
      pal_fond <- colorFactor(palette = c("#f1c40f", "#2ecc71", "#2980b9"),
                              levels = c("Sec (CN I)", "Normal (CN II)", "Saturé (CN III)"))
      color_vals <- pal_fond(map_data$Etat_Sol)
      
      proxy <- proxy %>% addLegend(position = "bottomright", pal = pal_fond, values = map_data$Etat_Sol, 
                                   title = "État de Saturation (CN)", opacity = 1)
      
      label_poly <- ~paste0(NOM, " | État du sol : ", Etat_Sol, " (CN = ", round(CN_jour, 1), ")")
      
    } else {
      pal_fond <- colorNumeric(palette = "Blues", domain = c(0, 100))
      color_vals <- pal_fond(map_data$Taux_Remplissage)
      
      proxy <- proxy %>% addLegend(position = "bottomright", pal = pal_fond, values = c(0, 100), 
                                   title = "Taux Remplissage (%)", opacity = 1)
      
      label_poly <- ~paste0(NOM, " | Rempli à ", round(Taux_Remplissage), "% | Statut: ", Statut_Simu)
    }
    
    vars_overlay <- input$overlay_carte 
    
    if (!is.null(vars_overlay) && "statut" %in% vars_overlay) {
      color_vals <- ifelse(map_data$Statut_Simu == "Assec", "#e67e22", color_vals) 
      proxy <- proxy %>% addLegend(position = "bottomleft", colors = c("#e67e22"), labels = c("Assec Planifié"), 
                                   title = "Gestion", opacity = 1)
    }
    
    proxy %>%
      addPolygons(data = map_data,
                  fillColor = color_vals,
                  fillOpacity = 0.85, 
                  color = "#2c3e50", weight = 1, 
                  label = label_poly,
                  highlightOptions = highlightOptions(weight = 4, color = "white", bringToFront = TRUE),
                  options = pathOptions(pane = "polygones_pane"))
    
    if (!is.null(vars_overlay) && "ruissellement" %in% vars_overlay) {
      
      map_centroids <- suppressWarnings(st_centroid(map_data))
      map_ruiss <- map_centroids %>% filter(Volume_R > 10)
      
      if (nrow(map_ruiss) > 0) {
        proxy %>% 
          addCircleMarkers(data = map_ruiss,
                           radius = ~pmax(5, sqrt(Volume_R) / 10), 
                           fillColor = "#00d2d3", color = "#0097e6", weight = 2, fillOpacity = 0.9,
                           label = ~paste0(" APPORT PLUIE : ", round(Volume_R), " m³ ruisselés !"),
                           options = pathOptions(pane = "bulles_pane"))
      }
    }
  })
}