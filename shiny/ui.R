library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)
library(sf)

# =======================================================
# INTERFACE UTILISATEUR (UI)
# =======================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Plateforme de Simulation Hydrologique - Étangs de la Dombes"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$h4("Configuration de la Simulation"),
      hr(),
      
      fileInput("file_os", "Occupation des Sols (.csv)", accept = ".csv"),
      fileInput("file_etg", "Caractéristiques Étangs (.csv)", accept = ".csv"),
      fileInput("file_assec", "Données ASSEC (.csv)", accept = ".csv"),
      fileInput("file_vidange", "Dates de Vidange (.csv)", accept = ".csv"),
      
      hr(),
      numericInput("bv_code", "Code du Bassin Versant (SAFRAN) :", value = 2, min = 1),
      selectInput("source_meteo", "Source Météo Principale :",
                  choices = c("Météo France (SAFRAN)" = "safran",
                              "Capteurs Locaux" = "capteurs")),
      
      dateRangeInput("dates", "Période de simulation :", 
                     start = "2010-01-01", end = "2025-12-31"),
      
      hr(),
      actionButton("run_sim", "Lancer la Simulation (Base)", 
                   class = "btn-primary", style = "width: 100%"),
      
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
                          checkboxInput("show_alt_meteo", "Comparer avec l'autre modèle Météo", value = FALSE))
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
        
        # --- ONGLET 5 : CARTE ---
        tabPanel("Carte Dynamique",
                 br(), h4("Évolution spatiale du bassin versant"),
                 fluidRow(
                   column(5, uiOutput("ui_calendrier_carte"), uiOutput("ui_slider_carte")),
                   column(5, offset = 1,
                          radioButtons("color_carte", "Couleur de fond :", choices = c("Taux de remplissage" = "volume", "État du sol (CN)" = "etat_sol"), selected = "volume"),
                          checkboxGroupInput("overlay_carte", "Calques :", choices = c("Masquer les étangs en Assec" = "statut", "Afficher le Ruissellement" = "ruissellement"), selected = c("statut", "ruissellement")))
                 ),
                 hr(), leafletOutput("map_etangs", height = "700px")
        ),
        
        # --- NOUVEL ONGLET 6 : EXPÉRIMENTATION (LA GRILLE 3x3) ---
        tabPanel("Plan d'Expérimentation (Grille 3x3)",
                 br(),
                 h4("Matrice d'analyse de sensibilité : Lambda vs Jours Pant"),
                 p("Exécutez 9 simulations croisées pour l'étang sélectionné afin d'étudier le comportement hydrologique global."),
                 
                 fluidRow(
                   column(3, selectInput("grid_etang", "Choisir l'étang cible :", choices = NULL)),
                   column(3, selectInput("grid_source", "Source Météo :", choices = c("Météo France (SAFRAN)" = "safran", "Capteurs Locaux" = "capteurs"))),
                   column(3, dateRangeInput("grid_dates", "Fenêtre d'observation :", start = "2010-01-01", end = "2025-12-31")),
                   column(3, actionButton("run_grid", "Lancer la Grille (9 Modèles)", class = "btn-warning", style = "width: 100%; margin-top: 25px;"))
                 ),
                 hr(),
                 plotlyOutput("plot_grid", height = "800px")
        ),
        
        # --- ONGLET 7 : LOG ---
        tabPanel("Journal d'exécution", verbatimTextOutput("console_log"))
      )
    )
  )
)