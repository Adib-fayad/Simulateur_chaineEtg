library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)
library(plotly)
library(leaflet)
library(sf)
library(DT)
# =======================================================
# CHARGEMENT DES FONCTIONS ET DE L'ENVIRONNEMENT
# =======================================================
source("simulateur/fonctions.R")
source("simulateur/importation.R")

# =======================================================
# 1. INTERFACE UTILISATEUR (UI)
# =======================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Plateforme de Simulation Hydrologique - Étangs de la Dombes"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$h4("Sélection des Scénarios (.rds)"),
      p("L'application charge instantanément les simulations pré-calculées."),
      hr(),
      
      # Remplacement des fileInput par la sélection de fichiers .rds
      selectInput("fichier_rds_1", "Scénario Principal (Base) :", choices = NULL),
      selectInput("fichier_rds_2", "Scénario de Comparaison (Modifié) :", choices = NULL),
      
      hr(),
      dateRangeInput("dates", "Période de simulation globale :", 
                     start = "2010-01-01", end = "2025-12-31"),
      
      hr(),
      tags$h4("Visualisation"),
      selectInput("etang_choisi", "Choisir un étang à afficher :", choices = NULL)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        
        # --- ONGLET 1 : CALIBRATION CLASSIQUE ---
        tabPanel("Graphiques de Calibration", 
                 br(),
                 fluidRow(
                   column(3, selectInput("var_plot", "Variable à analyser :", 
                                         choices = c("Volume stocké (BF) [m³]" = "BF",
                                                     "Volume déversé (Vsortant) [m³]" = "Vsortant",
                                                     "Pluviométrie (RR) [mm]" = "RR",
                                                     "Ruissellement capté (Volume_R) [m³]" = "Volume_R",
                                                     "Bilan Pluie-Évapo (Vp_etp) [m³]" = "Vp_etp",
                                                     "Volume forcé par Vidange [m³]" = "Vol_Vidange_Jour"), 
                                         width = "100%")),
                   column(3, selectInput("time_step", "Pas de temps (Lissage) :", 
                                         choices = c("Journalier (Brut)" = "day", "Hebdomadaire (7 jours)" = "week", "Mensuel" = "month", "Annuel" = "year"), 
                                         width = "100%")),
                   column(3, uiOutput("ui_zoom_dates")),
                   column(3, br(), 
                          checkboxInput("show_terrain", "Superposer les données Terrain", value = FALSE),
                          checkboxInput("show_alt_meteo", "Superposer le Scénario 2 (Comparaison)", value = FALSE))
                 ),
                 plotlyOutput("plot_calibration", height = "500px"),
                 hr(),
                 tags$h4("Indicateurs Clés de l'Étang"),
                 tableOutput("kpi_table")
        ),
        
        # --- ONGLET 2 : EXUTOIRE ---
        tabPanel("Exutoire Final", 
                 br(), h4("Débit global en sortie de bassin versant"),
                 fluidRow(
                   column(3, selectInput("time_step_exutoire", "Pas de temps :", choices = c("Journalier" = "day", "Hebdomadaire" = "week", "Mensuel" = "month", "Annuel" = "year"), width = "100%")),
                   column(3, uiOutput("ui_zoom_dates_exutoire")),
                   column(3, selectInput("etang_superpose_ex", "Comparer avec l'étang :", choices = NULL, width = "100%"))
                 ),
                 hr(),
                 plotlyOutput("plot_exutoire", height = "500px"),
                 hr(),
                 tags$h4("Statistiques Globales du Réseau"),
                 plotlyOutput("plot_bilan_pie", height = "400px"),
                 tableOutput("table_stats_exutoire")
        ),
        
        # --- ONGLET 3 : ASSEC ---
        tabPanel("Chronologie des Assecs",
                 br(), h4("Diagramme de Gantt des périodes de mise à sec"),
                 fluidRow(column(4, dateRangeInput("viz_dates_gantt", "Plage :", start = "2010-01-01", end = "2025-12-31", width = "100%"))),
                 hr(), plotlyOutput("plot_gantt_assec", height = "600px")
        ),
        
        # --- ONGLET 4 : OPÉRATIONS ---
        tabPanel("Opérations : Pêche & Vidange",
                 br(), h4("Calendrier des opérations"),
                 fluidRow(column(4, dateRangeInput("viz_dates_operations", "Plage :", start = "2010-01-01", end = "2025-12-31", width = "100%"))),
                 hr(), plotlyOutput("plot_operations", height = "600px")
        ),
        
        # --- ONGLET 5 : RÉSEAU HYDROGRAPHIQUE (CASCADE) ---
        tabPanel("Topologie de la Cascade",
                 br(), h4("Visualisation du sens d'écoulement des vidanges"),
                 p("Cette carte illustre les connexions physiques entre les étangs. Les lignes pointillées rouges montrent vers quel exutoire (étang aval) l'eau se dirige."),
                 hr(), 
                 leafletOutput("map_cascade", height = "700px")
        ),
        # --- ONGLET 6 : RUISSELLEMENT (ÉVÉNEMENTIEL) ---
        tabPanel("Indicateurs de Ruissellement",
                 br(),
                 h4("Analyse Événementielle du Ruissellement (Modèles vs Terrain)"),
                 p("Ce nuage de points compare le Coefficient de Ruissellement (CR) entre la réalité (sonde) et vos deux scénarios, uniquement lors des jours d'orage."),
                 
                 # Choix de la période ET du lissage dynamique
                 fluidRow(
                   column(4, dateRangeInput("dates_ruissellement", "Filtrer la période d'analyse :", 
                                            start = "2010-01-01", end = "2025-12-31", width = "100%")),
                   column(4, numericInput("lissage_jours", "Fenêtre de calcul (jours cumulés) :", 
                                          value = 2, min = 1, max = 10, step = 1, width = "100%"))
                 ),
                 
                 hr(),
                 # Le fameux nuage de points
                 plotlyOutput("plot_cr_journalier", height = "500px"),
                 
                 hr(),
                 h4("Bilan du Ruissellement sur la période sélectionnée"),
                 # Le tableau récapitulatif
                 DTOutput("table_cr_resume")
        )
      )
    )
  )
)

# =======================================================
# 2. SERVEUR (LOGIQUE DE LECTURE ET RENDU)
# =======================================================
server <- function(input, output, session) {
  
  # --- 1. Scan et chargement des fichiers .rds ---
  observe({
    fichiers <- list.files(pattern = "\\.rds$")
    if (length(fichiers) > 0) {
      updateSelectInput(session, "fichier_rds_1", choices = fichiers, selected = fichiers[1])
      updateSelectInput(session, "fichier_rds_2", choices = fichiers, selected = ifelse(length(fichiers) > 1, fichiers[2], fichiers[1]))
    }
  })
  
  # Remplacement des "calculs" par la lecture instantanée
  get_active_sim <- reactive({
    req(input$fichier_rds_1)
    readRDS(input$fichier_rds_1)
  })
  
  get_alt_sim <- reactive({
    req(input$fichier_rds_2)
    readRDS(input$fichier_rds_2)
  })
  
  # Mise à jour de la liste des étangs
  observe({
    req(get_active_sim())
    etangs <- names(get_active_sim()$liste_finale)
    updateSelectInput(session, "etang_choisi", choices = etangs)
    updateSelectInput(session, "etang_superpose_ex", choices = c("Aucun", etangs))
    updateSelectInput(session, "grid_etang", choices = etangs)
  })
  
  # --- Chargement géométrie pour la carte (simulation fictive du st_read si tu l'as dans importation) ---
  etgs_shape <- reactive({
    # Assure-toi que ce fichier correspond bien à ta couche spatiale chargée dans ton projet
    tryCatch(st_read("data/shape/Etangs_Chalamont.shp", quiet = TRUE), error = function(e) NULL)
  })
  
  # =======================================================
  # REPRISE DE TES OUTPUTS GRAPHIQUES EXACTS
  # =======================================================
  
  output$ui_zoom_dates <- renderUI({
    req(get_active_sim()$liste_finale)
    df <- get_active_sim()$liste_finale[[1]] 
    dateRangeInput("viz_dates", "Plage de visualisation :", start = min(df$dat), end = max(df$dat), min = min(df$dat), max = max(df$dat))
  })
  
  output$ui_zoom_dates_exutoire <- renderUI({
    req(get_active_sim()$exutoire_data)
    df <- get_active_sim()$exutoire_data 
    dateRangeInput("viz_dates_exutoire", "Plage de visualisation :", start = min(df$dat), end = max(df$dat), min = min(df$dat), max = max(df$dat))
  })
  
  output$plot_calibration <- renderPlotly({
    req(get_active_sim()$liste_finale, input$etang_choisi, input$var_plot)
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
      if(!is.null(df_alt)) {
        df_alt <- df_alt %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
      }
    }
    
    req(input$time_step)
    if (input$time_step != "day") {
      lisser_df <- function(d){
        d %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step)) %>%
          group_by(dat) %>% summarise(BF = mean(BF, na.rm = TRUE), Vsortant = sum(Vsortant, na.rm = TRUE), RR = sum(RR, na.rm = TRUE), Volume_R = sum(Volume_R, na.rm = TRUE), Vp_etp = sum(Vp_etp, na.rm = TRUE), Vol_Vidange_Jour = sum(Vol_Vidange_Jour, na.rm = TRUE), Vmax = first(Vmax), .groups = "drop")
      }
      df_plot <- lisser_df(df_plot)
      if(!is.null(df_alt)) { df_alt <- lisser_df(df_alt) }
    }
    
    terrain_dispo <- FALSE
    if (input$show_terrain) {
      df_terrain <- load_terrain(nom_etang)
      if (!is.null(df_terrain)) {
        if (input$time_step != "day") {
          df_terrain <- df_terrain %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step)) %>% group_by(dat) %>% summarise(Volume_Reel = mean(Volume_Reel, na.rm = TRUE), .groups = "drop")
        }
        df_plot <- df_plot %>% left_join(df_terrain, by = "dat")
        terrain_dispo <- TRUE
      }
    }
    
    p <- ggplot(df_plot, aes(x = dat)) + theme_minimal()
    label_main <- "Scénario Principal"
    label_alt <- "Scénario 2 (Comparaison)"
    
    if (var_select == "BF") {
      p <- p + geom_line(aes(y = BF, color = label_main), linewidth = 0.8) + geom_hline(yintercept = df_plot$Vmax[1], color = "black", linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = df_plot$Vmax[1] * 0.80, color = "red", linetype = "solid", linewidth = 1) 
      
      if (!is.null(df_alt)) { p <- p + geom_line(data = df_alt, aes(y = BF, color = label_alt), linetype = "dashed", linewidth = 0.8) }
      if (terrain_dispo && "Volume_Reel" %in% names(df_plot)) p <- p + geom_line(aes(y = Volume_Reel, color = "Sonde (Terrain)"), linetype = "solid", linewidth = 1) 
      
      my_colors <- setNames(c("#2c3e50", "#27ae60", "#e67e22"), c(label_main, label_alt, "Sonde (Terrain)"))
      p <- p + scale_color_manual(values = my_colors) + labs(title = paste("Volume quotidien -", nom_etang), subtitle = "Rouge: Seuil 80%", y = "Volume (m³)", x = "Date", color = "Légende")
      
    } else if (var_select == "RR") {
      p <- p + geom_col(aes(y = RR, fill = label_main), alpha = 0.7)
      if(!is.null(df_alt)) p <- p + geom_col(data = df_alt, aes(y = RR, fill = label_alt), alpha = 0.4, position = "identity")
      my_fills <- setNames(c("#3498db", "#9b59b6"), c(label_main, label_alt))
      p <- p + scale_fill_manual(values = my_fills) + labs(title = paste("Pluviométrie -", nom_etang), y = "Pluie (mm/période)", x = "Date", fill="Source")
      
    } else {
      p <- p + geom_line(aes_string(y = var_select, color = shQuote(label_main)), linewidth = 0.8)
      if(!is.null(df_alt)) p <- p + geom_line(data=df_alt, aes_string(y = var_select, color = shQuote(label_alt)), linetype="dashed", linewidth=0.8)
      my_colors <- setNames(c("#16a085", "#8e44ad"), c(label_main, label_alt))
      p <- p + scale_color_manual(values = my_colors) + labs(title = paste("Analyse de :", var_select, "-", nom_etang), y = "Valeur", x = "Date", color="Source")
    }
    
    ggplotly(p, dynamicTicks = TRUE) %>% layout(hovermode = "x unified", xaxis = list(rangeslider = list(type = "date")))
  })
  
  output$kpi_table <- renderTable({
    req(get_active_sim()$liste_finale, input$etang_choisi) 
    df <- get_active_sim()$liste_finale[[input$etang_choisi]]
    req(df)
    if (!is.null(input$viz_dates)) df <- df %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
    v_max <- df$Vmax[1] 
    indics <- c("Volume Maximum Théorique (Vmax)", "Volume Maximum Atteint (sur la période)", "Nombre de jours à sec", "Nombre de jours >= 80% de remplissage", "Nombre de jours <= 15% de remplissage", "Taux de remplissage moyen")
    valeurs <- c(paste(format(round(v_max, 0), big.mark = " "), "m³"), paste(format(round(max(df$BF, na.rm = TRUE), 0), big.mark = " "), "m³"), paste(sum(df$BF <= 1, na.rm = TRUE), "jours"), paste(sum(df$BF >= 0.80 * v_max, na.rm = TRUE), "jours"), paste(sum(df$BF <= 0.15 * v_max, na.rm = TRUE), "jours"), paste(round(mean(df$BF / v_max, na.rm = TRUE) * 100, 1), "%"))
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
    if(nrow(df_ex) == 0) return(plot_ly() %>% layout(title = "En attente de données..."))
    if (!is.null(input$viz_dates_exutoire)) df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
    req(input$time_step_exutoire)
    if (input$time_step_exutoire != "day") df_ex <- df_ex %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>% group_by(dat) %>% summarise(Volume_Riviere = sum(Volume_Riviere, na.rm = TRUE), .groups = "drop")
    p <- ggplot() + theme_minimal() + geom_area(data = df_ex, aes(x = dat, y = Volume_Riviere, fill = "Débit Exutoire Total"), alpha = 0.2) + geom_line(data = df_ex, aes(x = dat, y = Volume_Riviere, color = "Débit Exutoire Total"), linewidth = 1)
    if (!is.null(input$etang_superpose_ex) && input$etang_superpose_ex != "Aucun") {
      df_etang <- get_active_sim()$liste_finale[[input$etang_superpose_ex]]
      if (!is.null(df_etang)) {
        if (!is.null(input$viz_dates_exutoire)) df_etang <- df_etang %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
        if (input$time_step_exutoire != "day") df_etang <- df_etang %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>% group_by(dat) %>% summarise(BF = mean(BF, na.rm = TRUE), .groups = "drop")
        p <- p + geom_line(data = df_etang, aes(x = dat, y = BF, color = "Volume Stocké Étang (BF)"), linewidth = 1.2)
      }
    }
    p <- p + scale_color_manual(values = c("Volume Stocké Étang (BF)" = "#3498db", "Débit Exutoire Total" = "darkred")) + scale_fill_manual(values = c("Débit Exutoire Total" = "darkred")) + scale_y_continuous(labels = scales::comma_format(big.mark = " ")) + labs(title = "Comparaison Volume de l'Étang (BF) vs Débit de l'Exutoire", y = "Volume (m³)", x = "Date", color = "Légende", fill = "Zone")
    ggplotly(p, dynamicTicks = TRUE) %>% layout(hovermode = "x unified", xaxis = list(rangeslider = list(type = "date")))
  })
  
  output$table_stats_exutoire <- renderTable({
    req(get_active_sim()$exutoire_data, get_active_sim()$liste_finale)
    df_ex <- get_active_sim()$exutoire_data
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_exutoire)) { df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]); df_all <- df_all %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]) }
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
      "Indicateur_Global_du_Bassin" = c("Capacité maximale de stockage de toute la chaîne", paste("Volume total d'eau retenu le", format(date_fin, "%d/%m/%Y")), "Taux de remplissage global de la chaîne à cette date", paste("Nombre d'étangs pleins le", format(date_fin, "%d/%m/%Y")), "Volume total apporté par le ruissellement (Pluie) sur la période", "Volume total définitivement évacué par l'exutoire final", "Nombre de jours où l'exutoire final a coulé (> 0 m³)", "Volume moyen relâché par l'exutoire final par jour"),
      "Valeur" = c(paste(format(round(capa_max_totale, 0), big.mark = " "), "m³"), paste(format(round(vol_final_stocke, 0), big.mark = " "), "m³"), paste(round(taux_remplissage_final, 1), "%"), paste(nb_etangs_pleins, "étang(s) sur", nb_total_etangs), paste(format(round(apport_total_pluie, 0), big.mark = " "), "m³"), paste(format(round(vol_total_sorti, 0), big.mark = " "), "m³"), paste(format(jours_sup_0, big.mark = " "), "jours"), paste(format(round(moy_jour, 0), big.mark = " "), "m³"))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  output$plot_bilan_pie <- renderPlotly({
    req(get_active_sim()$exutoire_data, get_active_sim()$liste_finale)
    df_ex <- get_active_sim()$exutoire_data
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_exutoire)) { df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]); df_all <- df_all %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]) }
    apport_ruissellement <- sum(df_all$Volume_R, na.rm = TRUE)
    apport_pluie_directe <- sum(df_all$Evap_Reelle[df_all$Evap_Reelle > 0], na.rm = TRUE)
    df_gains <- data.frame(Categorie = c("Ruissellement du Bassin Versant", "Pluie directe sur les Étangs"), Volume = c(apport_ruissellement, apport_pluie_directe))
    perte_evap <- sum(abs(df_all$Evap_Reelle[df_all$Evap_Reelle < 0]), na.rm = TRUE)
    sortie_riviere <- sum(df_ex$Volume_Riviere, na.rm = TRUE)
    df_pertes <- data.frame(Categorie = c("Évaporation", "Évacuée à la Rivière (Exutoire)"), Volume = c(perte_evap, sortie_riviere))
    plot_ly() %>%
      add_pie(data = df_gains, labels = ~Categorie, values = ~Volume, name = "Entrées", textinfo = 'percent', hoverinfo = 'label+text+percent', text = ~paste(format(round(Volume, 0), big.mark = " "), "m³"), marker = list(colors = c("#2980b9", "#7fb3d5")), domain = list(x = c(0, 0.45), y = c(0, 1)), title = list(text = "<b>APPORTS D'EAU</b><br>(Gains)", font = list(size = 14))) %>%
      add_pie(data = df_pertes, labels = ~Categorie, values = ~Volume, name = "Sorties", textinfo = 'percent', hoverinfo = 'label+text+percent', text = ~paste(format(round(Volume, 0), big.mark = " "), "m³"), marker = list(colors = c("#e74c3c", "#2c3e50")), domain = list(x = c(0.55, 1), y = c(0, 1)), title = list(text = "<b>PERTES D'EAU</b><br>(Sorties)", font = list(size = 14))) %>%
      layout(title = list(text = "<b>Bilan des Flux du Réseau (Entrées vs Sorties)</b>", font = list(size = 18)), showlegend = TRUE, margin = list(t = 80, b = 20, l = 10, r = 10))
  })
  
  output$plot_gantt_assec <- renderPlotly({
    req(get_active_sim()$liste_finale)
    pond_names <- names(get_active_sim()$liste_finale)
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_gantt)) df_all <- df_all %>% filter(dat >= input$viz_dates_gantt[1] & dat <= input$viz_dates_gantt[2])
    gestion_periods <- df_all %>% arrange(NOM, dat) %>% group_by(NOM) %>% mutate(Statut = ifelse(is.na(Statut_Simu), "Evolage", Statut_Simu), changement = coalesce(Statut != lag(Statut), FALSE), period_id = cumsum(changement)) %>% group_by(NOM, period_id, Statut) %>% summarise(debut = min(dat), fin = max(dat), duree = as.numeric(fin - debut) + 1, .groups = "drop") %>% filter(duree > 0)
    if(nrow(gestion_periods) == 0) return(NULL)
    gestion_periods$NOM <- factor(gestion_periods$NOM, levels = rev(pond_names)) 
    p <- ggplot(gestion_periods, aes(x = debut, xend = fin, y = NOM, yend = NOM, color = Statut, text = paste("Statut :", Statut, "<br>Du", format(debut, "%d/%m/%Y"), "au", format(fin, "%d/%m/%Y"), "<br>Durée :", duree, "jours"))) + geom_segment(linewidth = 1.5) + scale_color_manual(values = c("Assec" = "#e67e22", "Evolage" = "#3498db")) + theme_minimal() + labs(title = "Calendrier de Gestion (Assec & Évolage)", x = "Date", y = "Étangs (Amont en haut -> Aval en bas)", color = "Statut") + theme(panel.grid.minor.y = element_blank(), axis.text.y = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest") %>% config(displayModeBar = TRUE)
  })
  
  output$plot_operations <- renderPlotly({
    req(get_active_sim()$liste_finale)
    pond_names <- names(get_active_sim()$liste_finale)
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_operations)) df_all <- df_all %>% filter(dat >= input$viz_dates_operations[1] & dat <= input$viz_dates_operations[2])
    df_points_op <- df_all %>% mutate(Operation = case_when(peche == "oui" ~ "Pêche", Vol_Vidange_Jour > 0 ~ "Vidange", TRUE ~ "Rien")) %>% filter(Operation %in% c("Pêche", "Vidange")) %>% mutate(Texte_Survol = paste0("<b>Date :</b> ", format(dat, "%d/%m/%Y"), "<br>", "<b>Action :</b> ", Operation, "<br>", "<b>Eau évacuée :</b> ", format(round(Vsortant, 0), big.mark = " "), " m³")) %>% select(NOM, dat, Operation, Texte_Survol)
    if(nrow(df_points_op) == 0) return(NULL)
    df_points_op$NOM <- factor(df_points_op$NOM, levels = rev(pond_names))
    df_points_op$Operation <- factor(df_points_op$Operation, levels = c("Vidange", "Pêche"))
    p <- ggplot(df_points_op, aes(x = dat, y = NOM, color = Operation, text = Texte_Survol)) + geom_point(shape = 16, size = 1.5) + scale_color_manual(values = c("Vidange" = "#f39c12", "Pêche"   = "#c0392b")) + theme_minimal() + labs(title = "Opérations Journalières (Vidanges et Pêches)", x = "Date", y = "Étangs (Amont -> Aval)", color = "Opération") + theme(panel.grid.minor.y = element_blank(), axis.text.y = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest") %>% config(displayModeBar = TRUE)
  })
  
  # =======================================================
  # ONGLET 5 : CARTE TOPOLOGIQUE DE LA CASCADE
  # =======================================================
  
  output$map_cascade <- renderLeaflet({
    # On vérifie que le Shapefile est bien chargé
    req(etgs_shape())
    shp <- etgs_shape()
    
    # On vérifie que tab_etg_base (qui contient la colonne Exutoire_1) existe bien
    req(exists("tab_etg_base"))
    
    # 1. Jointure : On ajoute l'info de l'Exutoire à la géométrie
    shp_data <- shp %>% 
      left_join(tab_etg_base %>% select(NOM, Exutoire_1), by = "NOM")
    
    # 2. Calcul des centres géométriques (Centroïdes) de chaque étang
    cents <- suppressWarnings(st_centroid(shp_data))
    coords <- st_coordinates(cents)
    cents_df <- data.frame(
      NOM = shp_data$NOM, 
      lng = coords[,1], 
      lat = coords[,2], 
      Exutoire_1 = shp_data$Exutoire_1
    )
    
    # 3. Création de la carte de base avec les polygones bleus
    map <- leaflet(shp_data) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = "#3498db", fillOpacity = 0.5, color = "#2980b9", weight = 2,
                  label = ~paste0("Étang : ", NOM, " | Se vide dans : ", coalesce(Exutoire_1, "Sortie Réseau")))
    
    # 4. Tracé des connexions (Les flèches/lignes de vidange)
    for(i in 1:nrow(cents_df)) {
      src <- cents_df[i, ]
      dest_name <- src$Exutoire_1
      
      # Si l'étang a un exutoire identifié (qui n'est pas la rivière finale)
      if(!is.na(dest_name) && dest_name != "OUTPUT" && dest_name %in% cents_df$NOM) {
        dest <- cents_df[cents_df$NOM == dest_name, ]
        
        # On trace une ligne pointillée rouge entre le centre de A et le centre de B
        map <- map %>% addPolylines(
          lng = c(src$lng, dest$lng), 
          lat = c(src$lat, dest$lat),
          color = "#e74c3c", weight = 3, opacity = 0.9, dashArray = "5, 5",
          label = paste(src$NOM, "➔", dest$NOM)
        )
      }
    }
    
    # 5. Ajout de petits points sur les centres pour un rendu propre
    map <- map %>% addCircleMarkers(
      data = cents_df, lng = ~lng, lat = ~lat, 
      radius = 4, color = "#2c3e50", stroke = FALSE, fillOpacity = 1,
      label = ~NOM
    )
    
    return(map)
  })
  # =======================================================
  # ONGLET 6 : ANALYSE DU RUISSELLEMENT (GRAPHIQUE + TABLEAU)
  # =======================================================
  
  # 1. Base de données réactive dédiée au ruissellement
  data_ruissellement <- reactive({
    req(get_active_sim()$liste_finale, input$etang_choisi, input$lissage_jours)
    nom_etang <- input$etang_choisi
    k_jours <- input$lissage_jours # <-- Lecture du paramètre dynamique
    
    df1 <- get_active_sim()$liste_finale[[nom_etang]]
    df2 <- get_alt_sim()$liste_finale[[nom_etang]]
    
    # Récupération de la sonde
    df_terr <- load_terrain(nom_etang)
    if (!is.null(df_terr) && nrow(df_terr) > 0) {
      df_terr <- df_terr %>% select(dat, Volume_Reel_Sonde = Volume_Reel)
    } else {
      df_terr <- tibble(dat = as.Date(character()), Volume_Reel_Sonde = numeric())
    }
    
    infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
    req(nrow(infos_etg) > 0)
    surface_eau <- infos_etg$SURFACE_eau
    surface_bv_terre <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
    
    # Calcul des volumes bruts puis lissage dynamique
    df_analyse <- df1 %>%
      left_join(df_terr, by = "dat") %>%
      mutate(
        Vol_R_Mod1_Brut = Volume_R,
        Ecart_Jours = as.numeric(dat - lag(dat, 1)),
        Delta_V_Reel = ifelse(Ecart_Jours == 1, Volume_Reel_Sonde - lag(Volume_Reel_Sonde, 1), NA),
        Volume_Meteo_Direct = P_ETP * surface_eau * 10,
        Volume_Residuel_Brut = Delta_V_Reel - Volume_Meteo_Direct,
        Vol_Pluie_Sur_Terre_Brut = RR * surface_bv_terre * 10
      ) %>%
      # --- LISSAGE DYNAMIQUE (1 à 5 jours) ---
      mutate(
        RR_lisse = rollsum(RR, k = k_jours, fill = NA, align = "right"),
        Vol_Pluie_Sur_Terre_lisse = rollsum(Vol_Pluie_Sur_Terre_Brut, k = k_jours, fill = NA, align = "right"),
        Volume_Residuel_lisse = rollsum(Volume_Residuel_Brut, k = k_jours, fill = NA, align = "right"),
        Vol_R_Mod1_lisse = rollsum(Vol_R_Mod1_Brut, k = k_jours, fill = NA, align = "right"),
        
        # Calcul des nouveaux CR sur la fenêtre choisie
        Pseudo_CR_Terrain = (Volume_Residuel_lisse / Vol_Pluie_Sur_Terre_lisse) * 100,
        CR_Mod1_Pct = (Vol_R_Mod1_lisse / Vol_Pluie_Sur_Terre_lisse) * 100,
        
        # Filtre de validation sur la pluie lissée
        Est_Orage_Valide = (RR_lisse >= 5 & Vidange == "non" & peche == "non" & Pseudo_CR_Terrain > 0 & !is.na(Pseudo_CR_Terrain))
      ) %>% 
      select(dat, RR = RR_lisse, Est_Orage_Valide, Vol_Pluie_Sur_Terre = Vol_Pluie_Sur_Terre_lisse, Pseudo_CR_Terrain, CR_Mod1_Pct, Vol_R_Mod1 = Vol_R_Mod1_lisse)
    
    # Ajout et lissage du Scénario 2 s'il existe
    if (!is.null(df2)) {
      df2_sub <- df2 %>% 
        select(dat, Vol_R_Mod2_Brut = Volume_R) %>% 
        mutate(Vol_R_Mod2_lisse = rollsum(Vol_R_Mod2_Brut, k = k_jours, fill = NA, align = "right"))
      
      df_analyse <- df_analyse %>% 
        left_join(df2_sub, by = "dat") %>%
        mutate(
          CR_Mod2_Pct = (Vol_R_Mod2_lisse / Vol_Pluie_Sur_Terre) * 100,
          Vol_R_Mod2 = Vol_R_Mod2_lisse
        )
    } else {
      df_analyse <- df_analyse %>% mutate(CR_Mod2_Pct = NA, Vol_R_Mod2 = NA)
    }
    
    return(df_analyse)
  })
  
  # 2. Le graphique (Nuage de points)
  output$plot_cr_journalier <- renderPlotly({
    df <- data_ruissellement()
    req(input$dates_ruissellement, input$lissage_jours)
    
    # Filtrage par les dates sélectionnées
    df <- df %>% filter(dat >= input$dates_ruissellement[1] & dat <= input$dates_ruissellement[2])
    # Filtrage uniquement sur les orages
    df <- df %>% filter(Est_Orage_Valide == TRUE)
    
    if(nrow(df) == 0) return(plot_ly() %>% layout(title = paste("Aucun orage valide sur cette période (fenêtre de", input$lissage_jours, "jours).")))
    
    p <- ggplot(df, aes(x = dat)) +
      # Points du Terrain (en Vert)
      geom_point(aes(y = Pseudo_CR_Terrain, color = "Sonde (Terrain)", text = paste("Date:", dat, "<br>Pluie (", input$lissage_jours, "j):", round(RR,1), "mm<br>CR Terrain:", round(Pseudo_CR_Terrain,1), "%")), size = 3, alpha = 0.8) +
      # Points du Scénario 1 (en Bleu)
      geom_point(aes(y = CR_Mod1_Pct, color = "Scénario 1 (Base)", text = paste("Date:", dat, "<br>Pluie (", input$lissage_jours, "j):", round(RR,1), "mm<br>CR Modèle 1:", round(CR_Mod1_Pct,1), "%")), size = 2, shape = 17, alpha = 0.8)
    
    # Points du Scénario 2 (en Rouge)
    if(any(!is.na(df$CR_Mod2_Pct))) {
      p <- p + geom_point(aes(y = CR_Mod2_Pct, color = "Scénario 2 (Comparaison)", text = paste("Date:", dat, "<br>Pluie (", input$lissage_jours, "j):", round(RR,1), "mm<br>CR Modèle 2:", round(CR_Mod2_Pct,1), "%")), size = 2, shape = 15, alpha = 0.8)
    }
    
    p <- p + theme_minimal() + 
      labs(title = paste("Comparaison des taux de ruissellement (fenêtre de", input$lissage_jours, "jours)"), x = "Date", y = "Coefficient de Ruissellement (%)", color = "Légende") +
      scale_color_manual(values = c("Sonde (Terrain)" = "#27ae60", "Scénario 1 (Base)" = "#2980b9", "Scénario 2 (Comparaison)" = "#e74c3c"))
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
  })
  
  # 3. Le tableau de synthèse interactif
  output$table_cr_resume <- renderDT({
    df <- data_ruissellement()
    req(input$dates_ruissellement)
    
    # Application du filtre de dates
    df <- df %>% filter(dat >= input$dates_ruissellement[1] & dat <= input$dates_ruissellement[2])
    df_orages <- df %>% filter(Est_Orage_Valide == TRUE)
    
    nb_orages <- nrow(df_orages)
    pluie_totale_bv = sum(df$Vol_Pluie_Sur_Terre, na.rm = TRUE)
    
    res <- tibble(
      `Source des données` = c("Terrain (Sonde)", "Scénario 1 (Base)", "Scénario 2 (Comparaison)"),
      `Nb Orages Analysés` = c(nb_orages, nb_orages, nb_orages),
      `CR Événementiel Moyen (%)` = c(
        round(mean(df_orages$Pseudo_CR_Terrain, na.rm=TRUE), 1),
        round(mean(df_orages$CR_Mod1_Pct, na.rm=TRUE), 1),
        round(mean(df_orages$CR_Mod2_Pct, na.rm=TRUE), 1)
      ),
      `Biais Moyen (Modèle vs Terrain) (%)` = c(
        NA, # Pas de biais pour le terrain avec lui-même
        round(mean(df_orages$CR_Mod1_Pct - df_orages$Pseudo_CR_Terrain, na.rm=TRUE), 1),
        round(mean(df_orages$CR_Mod2_Pct - df_orages$Pseudo_CR_Terrain, na.rm=TRUE), 1)
      ),
      `Volume Ruisselé Total Période (m³)` = c(
        NA, # Incalculable proprement avec l'évaporation
        round(sum(df$Vol_R_Mod1, na.rm=TRUE)),
        round(sum(df$Vol_R_Mod2, na.rm=TRUE))
      ),
      `CR Global Période (%)` = c(
        NA, 
        round((sum(df$Vol_R_Mod1, na.rm=TRUE) / pluie_totale_bv) * 100, 1),
        round((sum(df$Vol_R_Mod2, na.rm=TRUE) / pluie_totale_bv) * 100, 1)
      )
    )
    
    datatable(res, 
              options = list(dom = 't', paging = FALSE, scrollX = TRUE), 
              rownames = FALSE, 
              class = 'cell-border stripe')
  })
  # --- ONGLET 7 : GRILLE 3x3 ---
  output$plot_grid <- renderPlotly({
    # Nécessite que tu aies chargé un environnement ou un fichier avec grid_res
    # Si grid_res n'existe pas dans la mémoire R, le graphique ne s'affichera pas.
    req(exists("grid_res")) 
    req(grid_res$df)
    
    df <- grid_res$df %>% filter(NOM == input$grid_etang)
    req(nrow(df) > 0)
    
    if (!is.null(input$grid_dates)) {
      df <- df %>% filter(dat >= input$grid_dates[1] & dat <= input$grid_dates[2])
    }
    
    terrain_dispo <- FALSE
    df_terrain <- load_terrain(input$grid_etang)
    if (!is.null(df_terrain)) {
      df_terrain <- df_terrain %>% filter(dat >= input$grid_dates[1] & dat <= input$grid_dates[2])
      terrain_dispo <- TRUE
    }
    
    p <- ggplot(df, aes(x = dat)) +
      geom_hline(aes(yintercept = Vmax), color = "black", linetype = "dashed", alpha = 0.6) +
      geom_line(aes(y = BF, color = "Modèle (Grille)"), linewidth = 0.8) 
    
    if(terrain_dispo) {
      p <- p + geom_line(data = df_terrain, aes(y = Volume_Reel, color = "Sonde (Terrain)"), linewidth = 0.8)
    }
    
    p <- p + facet_grid(Lambda_factor ~ Pant_factor) + 
      scale_color_manual(values = c("Modèle (Grille)" = "#2980b9", "Sonde (Terrain)" = "#27ae60")) +
      theme_bw() +
      labs(title = paste("Matrice d'expérimentation -", input$grid_etang),
           x = "Date", y = "Volume (m³)", color = "Légende") +
      theme(
        strip.background = element_rect(fill = "#ecf0f1", color = "#bdc3c7"),
        strip.text = element_text(face = "bold", size = 11)
      )
    
    ggplotly(p, dynamicTicks = TRUE) %>% layout(hovermode = "x unified")
  })
}

# =======================================================
# LANCEMENT DE L'APPLICATION
# =======================================================
shinyApp(ui = ui, server = server)
