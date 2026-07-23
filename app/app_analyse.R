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
library(stringr)

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
  
  # CSS personnalisé pour des cartes KPI au look moderne
  tags$head(
    tags$style(HTML("
      .kpi-card { background-color: #f8f9fa; border-left: 5px solid #2c3e50; border-radius: 5px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .kpi-title { font-size: 14px; color: #7f8c8d; text-transform: uppercase; font-weight: bold; }
      .kpi-value { font-size: 24px; color: #2c3e50; font-weight: bold; margin-top: 5px; }
      .kpi-desc { font-size: 12px; color: #95a5a6; margin-top: 5px; font-style: italic; }
      .kpi-card.blue { border-left-color: #3498db; }
      .kpi-card.green { border-left-color: #27ae60; }
      .kpi-card.orange { border-left-color: #e67e22; }
      .kpi-card.red { border-left-color: #e74c3c; }
      .kpi-card.purple { border-left-color: #9b59b6; }
      .kpi-card.dark { border-left-color: #34495e; }
      .info-box { background-color: #e8f4f8; border: 1px solid #b6d4fe; border-radius: 5px; padding: 10px; margin-bottom: 15px; color: #084298; font-weight: bold; text-align: center; }
      .legend-box { background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 12px; font-size: 12px; color: #495057; margin-top: 15px; }
      .legend-box p { margin-bottom: 6px; line-height: 1.3; }
      .legend-box b { color: #2c3e50; }
      .section-title { margin-top: 30px; margin-bottom: 15px; font-weight: bold; color: #2c3e50; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }
    "))
  ),
  
  titlePanel("Plateforme de Simulation Hydrologique - Dombes"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$h4("Sélection des Scénarios"),
      p("Comparaison instantanée des modélisations pré-calculées."),
      hr(),
      
      selectInput("dossier_1", "📂 Dossier Principal :", choices = NULL),
      selectInput("fichier_rds_1", "📄 Scénario Principal :", choices = NULL),
      
      hr(),
      selectInput("dossier_2", "📂 Dossier Comparaison :", choices = NULL),
      selectInput("fichier_rds_2", "📄 Scénario Comparaison :", choices = NULL),
      
      hr(),
      dateRangeInput("dates", "Période de simulation globale :", 
                     start = "2026-01-01", end = "2070-12-31"),
      
      hr(),
      tags$h4("Filtre Spatial"),
      selectInput("etang_choisi", "Cibler un étang :", choices = NULL),
      
      tags$div(class = "legend-box",
               tags$h5("Légende des Modèles", style="margin-top: 0px; font-weight: bold; color: #2c3e50;"),
               tags$p(tags$b("CNRM-CM5 ALADIN63 :"), " Modéré"),
               tags$p(tags$b("MPI-ESM REMO2009 :"), " Scénario intermédiaire"),
               tags$p(tags$b("IPSL-CM5A WRF381P :"), " Hiver très pluvieux, Été humide"),
               tags$p(tags$b("IPSL-CM5A RCA4 :"), " Hiver très humide, Été extrême"),
               tags$p(tags$b("HadGEM2 RegCM4-6 :"), " Très chaud, sécheresse modérée"),
               tags$p(tags$b("HadGEM2 CCLM4-8-17 :"), " Extrême (Le plus chaud/sec en été)")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        
        tabPanel("Analyse Détaillée (Étang)", 
                 br(),
                 fluidRow(
                   column(4, selectInput("var_plot", "Variable à analyser :", 
                                         choices = c("Volume stocké (BF) [m³]" = "BF", "Volume déversé (Vsortant) [m³]" = "Vsortant", "Pluviométrie (RR) [mm]" = "RR", "Ruissellement capté (Volume_R) [m³]" = "Volume_R", "Bilan Pluie-Évapo (Vp_etp) [m³]" = "Vp_etp"), width = "100%")),
                   column(4, selectInput("time_step", "Lissage temporel :", choices = c("Journalier" = "day", "Hebdomadaire" = "week", "Mensuel" = "month", "Annuel" = "year"), width = "100%")),
                   column(4, uiOutput("ui_zoom_dates"), checkboxInput("show_comparison", "Afficher la courbe de comparaison", value = TRUE))
                 ),
                 plotlyOutput("plot_calibration", height = "500px"),
                 hr(), tags$h4("Indicateurs Clés de l'Étang"), uiOutput("kpi_cards_etang")
        ),
        
        tabPanel("Exutoire Final", 
                 br(), 
                 fluidRow(
                   column(4, selectInput("time_step_exutoire", "Pas de temps :", choices = c("Journalier" = "day", "Hebdomadaire" = "week", "Mensuel" = "month", "Annuel" = "year"), width = "100%")),
                   column(4, uiOutput("ui_zoom_dates_exutoire")),
                   column(4, selectInput("etang_superpose_ex", "Superposer avec l'étang :", choices = NULL, width = "100%"))
                 ),
                 plotlyOutput("plot_exutoire", height = "450px"),
                 hr(),
                 fluidRow(
                   column(5, tags$h4("Bilan des Flux"), plotlyOutput("plot_bilan_pie", height = "350px")),
                   column(7, tags$h4("Métriques du Réseau"), uiOutput("kpi_cards_exutoire"))
                 )
        ),
        
        tabPanel("Calendrier de Gestion",
                 br(),
                 fluidRow(column(4, dateRangeInput("viz_dates_gestion", "Plage temporelle :", start = "2026-01-01", end = "2035-12-31", width = "100%"))),
                 plotlyOutput("plot_gantt_assec", height = "400px"), hr(), plotlyOutput("plot_operations", height = "400px")
        ),
        
        tabPanel("Bilan Chronologique Global",
                 br(),
                 fluidRow(
                   column(4, dateRangeInput("dates_bilan_global", "Période d'analyse :", start = "2026-01-01", end = "2040-12-31", width = "100%")),
                   column(4, selectInput("time_step_bilan", "Lissage temporel :", choices = c("Journalier" = "day", "Hebdomadaire" = "week", "Mensuel" = "month", "Annuel" = "year"), width = "100%"))
                 ),
                 plotlyOutput("plot_bilan_chrono", height = "650px")
        ),
        
        tabPanel("Bilan Annuel Piscicole",
                 br(),
                 fluidRow(
                   column(4, numericInput("annee_piscicole", "Année de début du cycle :", value = 2026, min = 2010, max = 2070, step = 1, width = "100%")),
                   column(8, uiOutput("box_info_cycle"))
                 ),
                 hr(),
                 plotlyOutput("plot_bilan_piscicole_pies", height = "500px")
        ),
        
        # --- NOUVEL ONGLET : VULNÉRABILITÉ & CLIMAT ---
        tabPanel("Vulnérabilité & Climat",
                 br(),
                 p("Analyse avancée des 11 indicateurs de vulnérabilité (moyennes calculées sur la période globale sélectionnée)."),
                 uiOutput("vulnerabilite_ui")
        ),
        
        tabPanel("Palmarès & Robustesse",
                 br(),
                 p("Analyse comparative des fichiers CSV générés sur les événements d'orage."),
                 plotOutput("plot_robustesse_box", height = "700px"),
                 hr(),
                 tags$h4("Classement Définitif des Paramétrages"),
                 DTOutput("table_robustesse")
        )
      )
    )
  )
)

# =======================================================
# 2. SERVEUR (LOGIQUE DE LECTURE ET RENDU)
# =======================================================
server <- function(input, output, session) {
  
  tous_les_rds <- reactive({ list.files(pattern = "\\.rds$", recursive = TRUE) })
  
  observe({
    fichiers <- tous_les_rds()
    if (length(fichiers) > 0) {
      dossiers <- unique(dirname(fichiers))
      noms_dossiers <- setNames(dossiers, ifelse(dossiers == ".", "Dossier Racine", dossiers))
      updateSelectInput(session, "dossier_1", choices = noms_dossiers, selected = dossiers[1])
      updateSelectInput(session, "dossier_2", choices = c("Aucun" = "Aucun", noms_dossiers), selected = "Aucun")
    }
  })
  
  observeEvent(input$dossier_1, {
    req(input$dossier_1)
    fichiers <- tous_les_rds()
    fichiers_filtres <- fichiers[dirname(fichiers) == input$dossier_1]
    noms_fichiers <- setNames(fichiers_filtres, basename(fichiers_filtres))
    updateSelectInput(session, "fichier_rds_1", choices = noms_fichiers)
  })
  
  observeEvent(input$dossier_2, {
    req(input$dossier_2)
    if(input$dossier_2 == "Aucun") {
      updateSelectInput(session, "fichier_rds_2", choices = c("Aucun" = "Aucun"))
    } else {
      fichiers <- tous_les_rds()
      fichiers_filtres <- fichiers[dirname(fichiers) == input$dossier_2]
      noms_fichiers <- c("Aucun" = "Aucun", setNames(fichiers_filtres, basename(fichiers_filtres)))
      updateSelectInput(session, "fichier_rds_2", choices = noms_fichiers)
    }
  })
  
  get_active_sim <- reactive({ req(input$fichier_rds_1); readRDS(input$fichier_rds_1) })
  get_alt_sim <- reactive({ 
    if(input$fichier_rds_2 == "Aucun" || is.null(input$fichier_rds_2)) return(NULL)
    readRDS(input$fichier_rds_2) 
  })
  
  observe({
    req(get_active_sim())
    etangs <- names(get_active_sim()$liste_finale)
    updateSelectInput(session, "etang_choisi", choices = etangs)
    updateSelectInput(session, "etang_superpose_ex", choices = c("Aucun", etangs))
  })
  
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
    if(input$show_comparison && !is.null(get_alt_sim())) { df_alt <- get_alt_sim()$liste_finale[[nom_etang]] }
    
    if (!is.null(input$viz_dates)) {
      df_plot <- df_plot %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
      if(!is.null(df_alt)) { df_alt <- df_alt %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2]) }
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
    
    p <- ggplot(df_plot, aes(x = dat)) + theme_minimal()
    label_main <- "Scénario Principal"
    label_alt <- "Scénario Comparaison"
    
    if (var_select == "BF") {
      p <- p + geom_line(aes(y = BF, color = label_main), linewidth = 0.8) + geom_hline(yintercept = df_plot$Vmax[1], color = "black", linetype = "dotted", alpha = 0.5)
      if (!is.null(df_alt)) { p <- p + geom_line(data = df_alt, aes(y = BF, color = label_alt), linetype = "dashed", linewidth = 0.8) }
      p <- p + scale_color_manual(values = setNames(c("#2c3e50", "#e74c3c"), c(label_main, label_alt))) + labs(title = paste("Volume quotidien -", nom_etang), y = "Volume (m³)", x = "", color = "")
    } else if (var_select == "RR") {
      p <- p + geom_col(aes(y = RR, fill = label_main), alpha = 0.7)
      if(!is.null(df_alt)) p <- p + geom_col(data = df_alt, aes(y = RR, fill = label_alt), alpha = 0.4, position = "identity")
      p <- p + scale_fill_manual(values = setNames(c("#3498db", "#e74c3c"), c(label_main, label_alt))) + labs(title = paste("Pluviométrie -", nom_etang), y = "Pluie (mm)", x = "", fill="")
    } else {
      p <- p + geom_line(aes_string(y = var_select, color = shQuote(label_main)), linewidth = 0.8)
      if(!is.null(df_alt)) p <- p + geom_line(data=df_alt, aes_string(y = var_select, color = shQuote(label_alt)), linetype="dashed", linewidth=0.8)
      p <- p + scale_color_manual(values = setNames(c("#16a085", "#e74c3c"), c(label_main, label_alt))) + labs(title = paste("Analyse :", var_select), y = "Valeur", x = "", color="")
    }
    suppressWarnings(ggplotly(p, dynamicTicks = TRUE) %>% layout(hovermode = "x unified", xaxis = list(rangeslider = list(type = "date")), legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)))
  })
  
  output$kpi_cards_etang <- renderUI({
    req(get_active_sim()$liste_finale, input$etang_choisi) 
    df <- get_active_sim()$liste_finale[[input$etang_choisi]]
    if (!is.null(input$viz_dates)) df <- df %>% filter(dat >= input$viz_dates[1] & dat <= input$viz_dates[2])
    v_max <- df$Vmax[1] 
    fluidRow(
      column(3, div(class = "kpi-card blue", div(class = "kpi-title", "Volume Maximum (Vmax)"), div(class = "kpi-value", paste(format(round(v_max, 0), big.mark = " "), "m³")))),
      column(3, div(class = "kpi-card green", div(class = "kpi-title", "Volume Max Atteint"), div(class = "kpi-value", paste(format(round(max(df$BF, na.rm = TRUE), 0), big.mark = " "), "m³")))),
      column(3, div(class = "kpi-card orange", div(class = "kpi-title", "Jours à sec"), div(class = "kpi-value", paste(sum(df$BF <= 1, na.rm = TRUE), "j")))),
      column(3, div(class = "kpi-card red", div(class = "kpi-title", "Remplissage Moyen"), div(class = "kpi-value", paste(round(mean(df$BF / v_max, na.rm = TRUE) * 100, 1), "%"))))
    )
  })
  
  output$plot_exutoire <- renderPlotly({
    req(get_active_sim()$exutoire_data)
    df_ex <- get_active_sim()$exutoire_data
    if(nrow(df_ex) == 0) return(plot_ly() %>% layout(title = "En attente..."))
    if (!is.null(input$viz_dates_exutoire)) df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
    req(input$time_step_exutoire)
    if (input$time_step_exutoire != "day") df_ex <- df_ex %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>% group_by(dat) %>% summarise(Volume_Riviere = sum(Volume_Riviere, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot() + theme_minimal() + geom_area(data = df_ex, aes(x = dat, y = Volume_Riviere, fill = "Débit Exutoire"), alpha = 0.3) + geom_line(data = df_ex, aes(x = dat, y = Volume_Riviere, color = "Débit Exutoire"), linewidth = 1)
    if (!is.null(input$etang_superpose_ex) && input$etang_superpose_ex != "Aucun") {
      df_etang <- get_active_sim()$liste_finale[[input$etang_superpose_ex]]
      if (!is.null(df_etang)) {
        if (!is.null(input$viz_dates_exutoire)) df_etang <- df_etang %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2])
        if (input$time_step_exutoire != "day") df_etang <- df_etang %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step_exutoire)) %>% group_by(dat) %>% summarise(BF = mean(BF, na.rm = TRUE), .groups = "drop")
        p <- p + geom_line(data = df_etang, aes(x = dat, y = BF, color = "Volume Étang (BF)"), linewidth = 1.2)
      }
    }
    p <- p + scale_color_manual(values = c("Volume Étang (BF)" = "#3498db", "Débit Exutoire" = "darkred")) + scale_fill_manual(values = c("Débit Exutoire" = "darkred")) + scale_y_continuous(labels = scales::comma_format(big.mark = " ")) + labs(title = "", y = "Volume (m³)", x = "", color = "", fill = "")
    suppressWarnings(ggplotly(p, dynamicTicks = TRUE) %>% layout(hovermode = "x unified", legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)))
  })
  
  output$kpi_cards_exutoire <- renderUI({
    req(get_active_sim()$exutoire_data, get_active_sim()$liste_finale)
    df_ex <- get_active_sim()$exutoire_data
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_exutoire)) { df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]); df_all <- df_all %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]) }
    vol_total_sorti <- sum(df_ex$Volume_Riviere, na.rm = TRUE)
    apport_total_pluie <- sum(df_all$Volume_R, na.rm = TRUE)
    capa_max_totale <- sum(sapply(get_active_sim()$liste_finale, function(x) x$Vmax[1]))
    jours_ecoulement <- sum(df_ex$Volume_Riviere > 0, na.rm = TRUE)
    div(
      fluidRow(
        column(6, div(class = "kpi-card blue", div(class = "kpi-title", "Volume Total Évacué"), div(class = "kpi-value", paste(format(round(vol_total_sorti, 0), big.mark = " "), "m³")))),
        column(6, div(class = "kpi-card green", div(class = "kpi-title", "Apport Pluie (Ruissellement)"), div(class = "kpi-value", paste(format(round(apport_total_pluie, 0), big.mark = " "), "m³"))))
      ),
      fluidRow(
        column(6, div(class = "kpi-card orange", div(class = "kpi-title", "Capacité Réseau Totale"), div(class = "kpi-value", paste(format(round(capa_max_totale, 0), big.mark = " "), "m³")))),
        column(6, div(class = "kpi-card red", div(class = "kpi-title", "Jours avec Écoulement"), div(class = "kpi-value", paste(format(jours_ecoulement, big.mark = " "), "j"))))
      )
    )
  })
  
  output$plot_bilan_pie <- renderPlotly({
    req(get_active_sim()$exutoire_data, get_active_sim()$liste_finale)
    df_ex <- get_active_sim()$exutoire_data
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_exutoire)) { df_ex <- df_ex %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]); df_all <- df_all %>% filter(dat >= input$viz_dates_exutoire[1] & dat <= input$viz_dates_exutoire[2]) }
    df_gains <- data.frame(Categorie = c("Ruissellement BV", "Pluie directe"), Volume = c(sum(df_all$Volume_R, na.rm = TRUE), sum(df_all$Evap_Reelle[df_all$Evap_Reelle > 0], na.rm = TRUE)))
    df_pertes <- data.frame(Categorie = c("Évaporation", "Évacué Rivière"), Volume = c(sum(abs(df_all$Evap_Reelle[df_all$Evap_Reelle < 0]), na.rm = TRUE), sum(df_ex$Volume_Riviere, na.rm = TRUE)))
    plot_ly() %>%
      add_pie(data = df_gains, labels = ~Categorie, values = ~Volume, name = "Entrées", textinfo = 'percent', hoverinfo = 'label+text+percent', marker = list(colors = c("#2980b9", "#7fb3d5")), domain = list(x = c(0, 0.45), y = c(0, 1))) %>%
      add_pie(data = df_pertes, labels = ~Categorie, values = ~Volume, name = "Sorties", textinfo = 'percent', hoverinfo = 'label+text+percent', marker = list(colors = c("#e74c3c", "#2c3e50")), domain = list(x = c(0.55, 1), y = c(0, 1))) %>%
      layout(showlegend = FALSE, margin = list(t = 20, b = 20, l = 0, r = 0))
  })
  
  output$plot_gantt_assec <- renderPlotly({
    req(get_active_sim()$liste_finale)
    pond_names <- names(get_active_sim()$liste_finale)
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_gestion)) df_all <- df_all %>% filter(dat >= input$viz_dates_gestion[1] & dat <= input$viz_dates_gestion[2])
    gestion_periods <- df_all %>% arrange(NOM, dat) %>% group_by(NOM) %>% mutate(Statut = ifelse(is.na(Statut_Simu), "Evolage", Statut_Simu), changement = coalesce(Statut != lag(Statut), FALSE), period_id = cumsum(changement)) %>% group_by(NOM, period_id, Statut) %>% summarise(debut = min(dat), fin = max(dat), duree = as.numeric(fin - debut) + 1, .groups = "drop") %>% filter(duree > 0)
    if(nrow(gestion_periods) == 0) return(NULL)
    gestion_periods$NOM <- factor(gestion_periods$NOM, levels = rev(pond_names)) 
    p <- ggplot(gestion_periods, aes(x = debut, xend = fin, y = NOM, yend = NOM, color = Statut, text = paste(Statut, "<br>Du", format(debut, "%d/%m/%Y"), "au", format(fin, "%d/%m/%Y")))) + geom_segment(linewidth = 3) + scale_color_manual(values = c("Assec" = "#e67e22", "Evolage" = "#3498db")) + theme_minimal() + labs(x = "", y = "", color = "") + theme(panel.grid.minor.y = element_blank(), axis.text.y = element_text(face = "bold"))
    suppressWarnings(ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest", legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1)))
  })
  
  output$plot_operations <- renderPlotly({
    req(get_active_sim()$liste_finale)
    pond_names <- names(get_active_sim()$liste_finale)
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$viz_dates_gestion)) df_all <- df_all %>% filter(dat >= input$viz_dates_gestion[1] & dat <= input$viz_dates_gestion[2])
    df_points_op <- df_all %>% mutate(Operation = case_when(peche == "oui" ~ "Pêche", Vol_Vidange_Jour > 0 ~ "Vidange", TRUE ~ "Rien")) %>% filter(Operation %in% c("Pêche", "Vidange")) %>% mutate(Texte_Survol = paste0(format(dat, "%d/%m/%Y"), "<br>", Operation, "<br>", format(round(Vsortant, 0), big.mark = " "), " m³ évacués")) %>% select(NOM, dat, Operation, Texte_Survol)
    if(nrow(df_points_op) == 0) return(NULL)
    df_points_op$NOM <- factor(df_points_op$NOM, levels = rev(pond_names))
    p <- ggplot(df_points_op, aes(x = dat, y = NOM, color = Operation, text = Texte_Survol)) + geom_point(shape = 16, size = 2) + scale_color_manual(values = c("Vidange" = "#f39c12", "Pêche" = "#c0392b")) + theme_minimal() + labs(x = "", y = "", color = "") + theme(panel.grid.minor.y = element_blank(), axis.text.y = element_blank())
    suppressWarnings(ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest", legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1)))
  })
  
  output$plot_bilan_chrono <- renderPlotly({
    req(get_active_sim()$liste_finale)
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    if (!is.null(input$dates_bilan_global)) df_all <- df_all %>% filter(dat >= input$dates_bilan_global[1] & dat <= input$dates_bilan_global[2])
    df_chrono <- df_all %>% group_by(dat) %>% summarise(Stock_Estime = sum(BF, na.rm = TRUE), Vmax_Tout_Etang = sum(Vmax, na.rm = TRUE), Besoin_Evolage = sum(ifelse(is.na(Statut_Simu) | Statut_Simu != "Assec", Vmax, 0), na.rm = TRUE), Surface_Totale_BV = sum(Surface_BV, na.rm = TRUE), RR = mean(RR, na.rm = TRUE), .groups = "drop")
    req(input$time_step_bilan)
    if (input$time_step_bilan != "day") {
      df_chrono <- df_chrono %>% mutate(dat = lubridate::floor_date(dat, unit = input$time_step_bilan)) %>% group_by(dat) %>% summarise(Stock_Estime = mean(Stock_Estime, na.rm = TRUE), Vmax_Tout_Etang = mean(Vmax_Tout_Etang, na.rm = TRUE), Besoin_Evolage = mean(Besoin_Evolage, na.rm = TRUE), Surface_Totale_BV = mean(Surface_Totale_BV, na.rm = TRUE), RR = sum(RR, na.rm = TRUE), .groups = "drop")
    }
    df_chrono <- df_chrono %>% mutate(Stock_mm = Stock_Estime / (Surface_Totale_BV * 10), Vmax_mm = Vmax_Tout_Etang / (Surface_Totale_BV * 10), Besoin_mm = Besoin_Evolage / (Surface_Totale_BV * 10))
    p <- ggplot(df_chrono, aes(x = dat)) +
      geom_col(aes(y = RR, fill = "Pluviométrie (mm)"), alpha = 0.25) +
      geom_line(aes(y = Vmax_mm, color = "Volume max tout étangs"), linewidth = 1) +
      geom_line(aes(y = Besoin_mm, color = "Besoin évolage"), linewidth = 1) +
      geom_line(aes(y = Stock_mm, color = "Stock estimé"), linewidth = 1.2, alpha = 0.8) +
      scale_color_manual(values = c("Volume max tout étangs" = "#e67e22", "Besoin évolage" = "#2c3e50", "Stock estimé" = "#85c1e9")) +
      scale_fill_manual(values = c("Pluviométrie (mm)" = "#3498db")) +
      theme_minimal() + labs(x = "", y = "Bilan en mm", color = "", fill = "")
    suppressWarnings(ggplotly(p, dynamicTicks = TRUE) %>% layout(hovermode = "x unified", xaxis = list(rangeslider = list(type = "date")), legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)))
  })
  
  calcul_bilan_annuel <- reactive({
    req(get_active_sim()$liste_finale, input$etang_choisi, input$annee_piscicole)
    nom_etang <- input$etang_choisi
    annee_debut <- input$annee_piscicole
    df_complet <- get_active_sim()$liste_finale[[nom_etang]]
    jours_vidange_N <- df_complet %>% filter(year(dat) == annee_debut, Vol_Vidange_Jour > 0)
    date_deb <- if (nrow(jours_vidange_N) > 0) max(jours_vidange_N$dat) else as.Date(paste0(annee_debut, "-10-01"))
    jours_vidange_N1 <- df_complet %>% filter(year(dat) == (annee_debut + 1), Vol_Vidange_Jour > 0)
    date_fin <- if (nrow(jours_vidange_N1) > 0) max(jours_vidange_N1$dat) else as.Date(paste0(annee_debut + 1, "-09-30"))
    if (as.numeric(date_fin - date_deb) > 400) { date_fin <- date_deb + 365 }
    infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
    surf_eau <- ifelse(nrow(infos_etg) > 0, infos_etg$SURFACE_eau, 0)
    df_annee <- df_complet %>%
      filter(dat >= date_deb & dat <= date_fin) %>%
      mutate(
        Pluie_Directe_m3 = RR * surf_eau * 10,
        Apport_Amont = replace_na(Vamont, 0),             
        Evaporation  = replace_na(Evap_Reelle, 0),        
        Fuites       = replace_na(Fuite_Reelle, 0),       
        Surverse_Out = replace_na(Vsortant, 0),           
        Vidange      = replace_na(Vol_Vidange_Jour, 0)    
      )
    list(df_annee = df_annee, date_deb = date_deb, date_fin = date_fin)
  })
  
  output$box_info_cycle <- renderUI({
    res <- calcul_bilan_annuel()
    HTML(paste0("<div class='info-box'>Cycle analysé automatiquement : du <b>", format(res$date_deb, "%d/%m/%Y"), "</b> au <b>", format(res$date_fin, "%d/%m/%Y"), "</b> (", as.numeric(res$date_fin - res$date_deb), " jours)</div>"))
  })
  
  output$plot_bilan_piscicole_pies <- renderPlotly({
    res <- calcul_bilan_annuel()
    df_annee <- res$df_annee
    df_entrees <- data.frame(
      Categorie = c("Pluie directe", "Ruissellement BV", "Amont"),
      Volume = c(sum(df_annee$Pluie_Directe_m3, na.rm=TRUE), sum(df_annee$Volume_R, na.rm=TRUE), sum(df_annee$Apport_Amont, na.rm=TRUE))
    )
    df_sorties <- data.frame(
      Categorie = c("Evaporation", "Fuite", "Surverse", "Vidange"),
      Volume = c(sum(df_annee$Evaporation, na.rm=TRUE), sum(df_annee$Fuites, na.rm=TRUE), sum(df_annee$Surverse_Out, na.rm=TRUE), sum(df_annee$Vidange, na.rm=TRUE))
    )
    plot_ly() %>%
      add_pie(data = df_entrees, labels = ~Categorie, values = ~Volume, name = "Entrées", textinfo = 'percent', hoverinfo = 'label+text+percent', marker = list(colors = c("#3498db", "#2ecc71", "#9b59b6")), domain = list(x = c(0, 0.45), y = c(0, 1)), title = list(text = paste0("<b>ENTRÉES</b><br>", round(sum(df_entrees$Volume)/1000, 1), " dam³"))) %>%
      add_pie(data = df_sorties, labels = ~Categorie, values = ~Volume, name = "Sorties", textinfo = 'percent', hoverinfo = 'label+text+percent', marker = list(colors = c("#f39c12", "#7f8c8d", "#e74c3c", "#34495e")), domain = list(x = c(0.55, 1), y = c(0, 1)), title = list(text = paste0("<b>SORTIES</b><br>", round(sum(df_sorties$Volume)/1000, 1), " dam³"))) %>%
      layout(showlegend = TRUE, margin = list(t = 20, b = 20, l = 0, r = 0))
  })
  
  # --- CALCUL DES INDICATEURS DE VULNÉRABILITÉ ---
  vulnerabilite_data <- reactive({
    req(get_active_sim()$liste_finale, input$etang_choisi)
    nom_etang <- input$etang_choisi
    df <- get_active_sim()$liste_finale[[nom_etang]]
    df_ex <- get_active_sim()$exutoire_data
    df_all <- bind_rows(get_active_sim()$liste_finale, .id = "NOM")
    
    if (!is.null(input$dates)) {
      df <- df %>% filter(dat >= input$dates[1] & dat <= input$dates[2])
      df_ex <- df_ex %>% filter(dat >= input$dates[1] & dat <= input$dates[2])
      df_all <- df_all %>% filter(dat >= input$dates[1] & dat <= input$dates[2])
    }
    
    vmax_etang <- df$Vmax[1]
    vmax_total_reseau <- sum(sapply(get_active_sim()$liste_finale, function(x) x$Vmax[1]))
    
    df <- df %>%
      mutate(
        annee = year(dat),
        mois = month(dat),
        Saison_Hydro = if_else(mois >= 10 & day(dat) >= 15 | mois > 10, annee + 1, annee)
      )
    
    # 1. Pression Climatique Pure
    df_hiver <- df %>% filter(mois %in% c(10,11,12,1,2,3)) %>%
      group_by(Saison_Hydro) %>% summarise(Recharge = sum(RR, na.rm=TRUE) - sum(ETP_grille, na.rm=TRUE))
    recharge_moy = mean(df_hiver$Recharge, na.rm=TRUE)
    
    df_ete <- df %>% filter(mois %in% c(6,7,8,9)) %>%
      group_by(annee) %>%
      summarise(Max_Sec = { rl <- rle(RR < 1); if(any(rl$values)) max(rl$lengths[rl$values]) else 0 })
    seq_seche_moy = mean(df_ete$Max_Sec, na.rm=TRUE)
    
    df_aridite <- df %>% group_by(annee) %>%
      summarise(P = sum(RR, na.rm=TRUE), E = sum(ETP_grille, na.rm=TRUE)) %>%
      mutate(Indice = P / E)
    aridite_moy = mean(df_aridite$Indice, na.rm=TRUE)
    
    # 2. Vulnérabilité Hydrologique
    df_avril <- df %>% filter(mois == 4 & day(dat) == 1) %>% mutate(Taux = BF / Vmax)
    secu_printemps = mean(df_avril$Taux, na.rm=TRUE) * 100
    
    df_def <- df %>% group_by(annee) %>% summarise(Jours_Def = sum(BF < 0.3 * Vmax, na.rm=TRUE))
    defaillance_moy = mean(df_def$Jours_Def, na.rm=TRUE)
    
    df_fill <- df %>% group_by(Saison_Hydro) %>% arrange(dat) %>%
      summarise(
        Date_Debut = min(dat[mois == 10 & day(dat) == 15], na.rm=TRUE),
        Date_80 = min(dat[BF >= 0.8 * Vmax & dat >= Date_Debut], na.rm=TRUE)
      ) %>%
      filter(!is.infinite(Date_Debut) & !is.infinite(Date_80)) %>%
      mutate(Jours = as.numeric(Date_80 - Date_Debut))
    time_to_fill_moy = mean(df_fill$Jours, na.rm=TRUE)
    
    # 3. Vulnérabilité Fonctionnelle
    vol_r_tot <- sum(df$Volume_R, na.rm=TRUE)
    vol_pluie_tot <- sum(df$RR * (df$Surface_BV - df$SURFACE_eau) * 10, na.rm=TRUE)
    rendement_bv = (vol_r_tot / vol_pluie_tot) * 100
    
    df_ex_annee <- df_ex %>% mutate(annee = year(dat)) %>% group_by(annee) %>% summarise(V_ex = sum(Volume_Riviere, na.rm=TRUE))
    gaspillage_moyen_annuel = mean(df_ex_annee$V_ex / vmax_total_reseau, na.rm=TRUE) * 100
    
    df_rupture <- df %>% group_by(annee) %>% summarise(Jours_Rupture = sum(Vsortant == 0 & Vol_Vidange_Jour == 0 & BF < Vmax, na.rm=TRUE))
    rupture_moy = mean(df_rupture$Jours_Rupture, na.rm=TRUE)
    
    # 4. Vulnérabilité Agronomique
    df_evap <- df %>% group_by(annee) %>% summarise(Vol_Evap = sum(abs(Evap_Reelle[Evap_Reelle < 0]), na.rm=TRUE))
    poids_evap = mean(df_evap$Vol_Evap / vmax_etang, na.rm=TRUE)
    
    annees_assec <- df %>% filter(Statut_Simu == "Assec") %>% pull(annee) %>% unique()
    if (length(annees_assec) > 0) {
      annees_suivantes <- annees_assec + 1
      reussites <- df %>% filter(annee %in% annees_suivantes & mois == 4 & day(dat) == 1) %>%
        summarise(Succes = sum(BF >= 0.7 * Vmax, na.rm=TRUE), Total = n())
      proba_assec = if(reussites$Total > 0) (reussites$Succes / reussites$Total) * 100 else NA
    } else {
      proba_assec = NA
    }
    
    list(
      recharge_moy = recharge_moy, seq_seche_moy = seq_seche_moy, aridite_moy = aridite_moy,
      secu_printemps = secu_printemps, defaillance_moy = defaillance_moy, time_to_fill_moy = time_to_fill_moy,
      rendement_bv = rendement_bv, gaspillage_moyen_annuel = gaspillage_moyen_annuel, rupture_moy = rupture_moy,
      poids_evap = poids_evap, proba_assec = proba_assec
    )
  })
  
  output$vulnerabilite_ui <- renderUI({
    res <- vulnerabilite_data()
    
    div(
      div(class="section-title", "1. Pression Climatique Pure (Analyse des intrants)"),
      fluidRow(
        column(4, div(class = "kpi-card blue", div(class = "kpi-title", "Déficit Recharge Hivernale"), div(class = "kpi-value", paste(round(res$recharge_moy, 1), "mm")), div(class="kpi-desc", "Bilan Pluie - ETP d'octobre à mars."))),
        column(4, div(class = "kpi-card orange", div(class = "kpi-title", "Séquences Sèches Estivales"), div(class = "kpi-value", paste(round(res$seq_seche_moy, 0), "jours consécutifs")), div(class="kpi-desc", "Pluie < 1mm (Juin à Septembre)."))),
        column(4, div(class = "kpi-card red", div(class = "kpi-title", "Indice d'Aridité (P / ETP)"), div(class = "kpi-value", round(res$aridite_moy, 2)), div(class="kpi-desc", "Glissement vers un climat semi-aride.")))
      ),
      
      div(class="section-title", "2. Vulnérabilité Hydrologique (États des stocks)"),
      fluidRow(
        column(4, div(class = "kpi-card green", div(class = "kpi-title", "Sécurisation Printanière"), div(class = "kpi-value", paste(round(res$secu_printemps, 1), "%")), div(class="kpi-desc", "Remplissage mesuré le 1er avril."))),
        column(4, div(class = "kpi-card red", div(class = "kpi-title", "Défaillance Estivale"), div(class = "kpi-value", paste(round(res$defaillance_moy, 0), "jours/an")), div(class="kpi-desc", "Volume en dessous de 30% (Survie)."))),
        column(4, div(class = "kpi-card purple", div(class = "kpi-title", "Time-to-Fill (Récupération)"), div(class = "kpi-value", paste(round(res$time_to_fill_moy, 0), "jours")), div(class="kpi-desc", "Temps pour atteindre 80% depuis l'automne.")))
      ),
      
      div(class="section-title", "3. Vulnérabilité Fonctionnelle (Le réseau en cascade)"),
      fluidRow(
        column(4, div(class = "kpi-card blue", div(class = "kpi-title", "Rendement du Bassin Versant"), div(class = "kpi-value", paste(round(res$rendement_bv, 1), "%")), div(class="kpi-desc", "Part de la pluie convertie en ruissellement."))),
        column(4, div(class = "kpi-card dark", div(class = "kpi-title", "Taux de Gaspillage Exutoire"), div(class = "kpi-value", paste(round(res$gaspillage_moyen_annuel, 1), "%")), div(class="kpi-desc", "Volume perdu annuellement vs Capacité totale."))),
        column(4, div(class = "kpi-card orange", div(class = "kpi-title", "Indice de Rupture de Continuité"), div(class = "kpi-value", paste(round(res$rupture_moy, 0), "jours/an")), div(class="kpi-desc", "Temps sans aucune surverse vers l'aval.")))
      ),
      
      div(class="section-title", "4. Vulnérabilité Agronomique et Métier"),
      fluidRow(
        column(6, div(class = "kpi-card orange", div(class = "kpi-title", "Poids de l'Évaporation Pure"), div(class = "kpi-value", paste(round(res$poids_evap, 2), "x Vmax")), div(class="kpi-desc", "Volume total évaporé par an rapporté au Vmax."))),
        column(6, div(class = "kpi-card green", div(class = "kpi-title", "Probabilité de Succès post-Assec"), div(class = "kpi-value", ifelse(is.na(res$proba_assec), "Aucun Assec", paste(round(res$proba_assec, 1), "%"))), div(class="kpi-desc", "Chances d'atteindre 70% de remplissage l'année N+1.")))
      )
    )
  })
  
  # --- DONNÉES PALMARÈS & ROBUSTESSE ---
  data_robustesse <- reactive({
    liste_fichiers <- list.files(pattern = "^Analyse_.*\\.csv$")
    req(length(liste_fichiers) > 0)
    
    df_global_box <- list()
    compteur <- 1
    
    for (fichier in liste_fichiers) {
      df_temp <- read.csv(fichier, sep = ";", dec = ",")
      if (nrow(df_temp) > 0) {
        df_temp <- df_temp %>% mutate(hypothese = str_replace(hypothese, "\\./", "/"))
        nom_evenement <- paste0(df_temp$Etang[1], " [", df_temp$Date_Debut_Orage[1], "]")
        df_temp$Evenement <- nom_evenement
        col_rmse <- grep("^RMSE_INRAE_", colnames(df_temp), value = TRUE)
        if (length(col_rmse) > 0) {
          df_temp$RMSE_INRAE <- df_temp[[col_rmse[1]]]
          unite_detectee <- str_extract(col_rmse[1], "(?<=_)[a-zA-Z0-9]+$")
          df_temp$Unite <- unite_detectee
          df_propre <- df_temp %>% select(Evenement, Etang, hypothese, RMSE_INRAE, Unite)
          df_global_box[[compteur]] <- df_propre
          compteur <- compteur + 1
        }
      }
    }
    df_graphique <- bind_rows(df_global_box)
    
    top_12_global <- df_graphique %>% group_by(Evenement) %>% slice_min(order_by = RMSE_INRAE, n = 12, with_ties = FALSE) %>% ungroup()
    palmares_absolu <- top_12_global %>% group_by(hypothese) %>% summarise(Nombre_Apparitions_Top_12 = n())
    departage <- df_graphique %>% group_by(hypothese) %>% summarise(RMSE_Moyen = round(mean(RMSE_INRAE, na.rm = TRUE), 1)) %>% inner_join(palmares_absolu, by = "hypothese") %>% arrange(desc(Nombre_Apparitions_Top_12), RMSE_Moyen)
    
    list(brut = df_graphique, departage = departage, unite = df_graphique$Unite[1])
  })
  
  output$plot_robustesse_box <- renderPlot({
    res <- data_robustesse()
    df_graphique <- res$brut
    req(nrow(df_graphique) > 0)
    unite_finale <- res$unite
    
    g_meta_box <- ggplot(df_graphique, aes(x = RMSE_INRAE , y = reorder(Evenement, RMSE_INRAE, FUN = median), fill = Etang)) +
      geom_boxplot(alpha = 0.75, outlier.alpha = 0.3, color = "#2c3e50", lwd = 0.6) +
      coord_flip() + 
      scale_fill_brewer(palette = "Set2") + 
      theme_minimal() +
      labs(
        title = "Analyse Comparative de la Performance du Modèle",
        subtitle = "Classement des événements (du moins précis au plus précis)",
        x = paste0("Erreur RMSE (", unite_finale, ")"),
        y = "Événement d'Étude",
        fill = "Étang analysé"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#1a252f"),
        plot.subtitle = element_text(size = 11, color = "#555555"),
        axis.text.x = element_text(face = "bold", size = 10, color = "#2c3e50", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_blank(), 
        legend.position = "bottom",
        legend.box.background = element_rect(color = "lightgrey", linewidth = 0.5)
      )
    
    g_meta_box
  })
  
  output$table_robustesse <- renderDT({
    res <- data_robustesse()
    datatable(res$departage %>% rename(`Modèle (Coef/RU/Beta)` = hypothese, `Apparitions Top 12` = Nombre_Apparitions_Top_12, `Erreur Moyenne Globale` = RMSE_Moyen), 
              options = list(pageLength = 10, dom = 'tip'), rownames = FALSE, class = 'cell-border stripe')
  })
}

shinyApp(ui = ui, server = server)