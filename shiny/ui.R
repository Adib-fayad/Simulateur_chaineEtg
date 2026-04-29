library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)
library(sf)

# =======================================================
# INTERFACE UTILISATEUR (UI)
# =======================================================
# Définition de la mise en page globale de l'application
ui <- fluidPage(
  # Application d'un thème Bootstrap prédéfini pour l'aspect visuel
  theme = shinytheme("flatly"),
  
  # Titre principal affiché en haut de la page
  titlePanel("Plateforme de Simulation Hydrologique - Étangs de la Dombes"),
  
  # Organisation de la page en deux colonnes : barre latérale et panneau principal
  sidebarLayout(
    
    # Configuration du panneau latéral (entrées et paramètres)
    sidebarPanel(
      width = 3,
      tags$h4("Configuration de la Simulation"),
      hr(),
      
      # Modules de téléchargement des fichiers sources au format CSV
      fileInput("file_os", "Occupation des Sols (.csv)", accept = ".csv"),
      fileInput("file_etg", "Caractéristiques Étangs (.csv)", accept = ".csv"),
      fileInput("file_assec", "Données ASSEC (.csv)", accept = ".csv"),
      fileInput("file_vidange", "Dates de Vidange (.csv)", accept = ".csv"),
      
      hr(),
      # Paramétrage des variables de calcul liées à la source de données
      numericInput("bv_code", "Code du Bassin Versant (SAFRAN) :", value = 2, min = 1),
      
      # Choix de la source de données météo pour la simulation principale
      selectInput("source_meteo", "Source Météo Principale :",
                  choices = c("Météo France (SAFRAN)" = "safran",
                              "Capteurs Locaux" = "capteurs")),
      
      # Sélection de la fenêtre temporelle pour la simulation hydrologique
      dateRangeInput("dates", "Période de simulation :", 
                     start = "2010-01-01", end = "2025-12-31"),
      
      hr(),
      # Bouton de déclenchement du script de calcul situé côté serveur
      actionButton("run_sim", "Lancer la Simulation", 
                   class = "btn-primary", style = "width: 100%"),
      
      hr(),
      tags$h4("Visualisation"),
      # Choix de l'étang
      selectInput("etang_choisi", "Choisir un étang à afficher :", choices = NULL)
    ),
    
    # Configuration du panneau principal (affichage des résultats)
    mainPanel(
      width = 9,
      # Organisation des résultats en onglets distincts
      tabsetPanel(
        
        # Premier onglet dédié aux résultats graphiques et statistiques
        tabPanel("Graphiques de Calibration", 
                 br(),
                 
                 # Disposition en ligne pour les menus de contrôle des graphiques
                 fluidRow(
                   column(3, 
                          selectInput("var_plot", "Variable à analyser :", 
                                      choices = c(
                                        "Volume stocké (BF) [m³]" = "BF",
                                        "Volume déversé (Vsortant) [m³]" = "Vsortant",
                                        "Pluviométrie (RR) [mm]" = "RR",
                                        "Ruissellement capté (Volume_R) [m³]" = "Volume_R",
                                        "Bilan Pluie-Évapo (Vp_etp) [m³]" = "Vp_etp",
                                        "Volume forcé par Vidange [m³]" = "Vol_Vidange_Jour"
                                      ), 
                                      width = "100%") 
                   ),
                   column(3,
                          selectInput("time_step", "Pas de temps (Lissage) :", 
                                      choices = c(
                                        "Journalier (Brut)" = "day",
                                        "Hebdomadaire (7 jours)" = "week",
                                        "Mensuel" = "month",
                                        "Annuel" = "year"
                                      ), 
                                      width = "100%")
                   ),
                   column(3,
                          uiOutput("ui_zoom_dates")
                   ),
                   column(3,
                          br(), 
                          checkboxInput("show_terrain", "Superposer les données Terrain", value = FALSE),
                          checkboxInput("show_alt_meteo", "Comparer avec l'autre modèle Météo", value = FALSE)
                   )
                 ),
                 # Sortie graphique interactive générée via la librairie Plotly
                 plotlyOutput("plot_calibration", height = "500px"),
                 
                 hr(),
                 # Section dédiée aux indicateurs statistiques récapitulatifs (KPI)
                 tags$h4("Indicateurs Clés de l'Étang"),
                 tableOutput("kpi_table")
        ),
        #NOUVEL ONGLET : EXUTOIRE FINAL
        tabPanel("Exutoire Final", 
                 br(),
                 h4("Débit global en sortie de bassin versant"),
                 p("Ce graphique cumule l'ensemble des volumes d'eau rejetés vers la rivière par les étangs."),
                 
                 fluidRow(
                   column(3,
                          selectInput("time_step_exutoire", "Pas de temps (Lissage) :", 
                                      choices = c("Journalier" = "day", "Hebdomadaire" = "week", "Mensuel" = "month", "Annuel" = "year"), 
                                      width = "100%")
                   ),
                   column(3,
                          uiOutput("ui_zoom_dates_exutoire")
                   ),
                   column(3,
                          selectInput("etang_superpose_ex", "Comparer avec l'étang :", 
                                      choices = NULL, 
                                      width = "100%")
                   ),
                   column(3,
                          dateInput("date_stat", "Date d'analyse (Étangs pleins) :", 
                                    value = "2015-01-01", width = "100%")
                   )
                 ),
                 
                 hr(),
                 plotlyOutput("plot_exutoire", height = "500px"),
                 
                 hr(),
                 #Zone pour le tableau des statistiques 
                 tags$h4("Statistiques Globales du Réseau"),
                 plotlyOutput("plot_bilan_pie", height = "400px"),
                 tableOutput("table_stats_exutoire")
        ),
        tabPanel("Chronologie des Assecs prévus",
                 br(),
                 h4("Diagramme de Gantt des périodes de mise à sec planifiées"),
                 p("Ce graphique illustre les assecs prévus par les exploitants (selon les calendriers déclarés), classés de l'Amont (en haut) vers l'Aval (en bas)."),
                 
                 fluidRow(
                   column(4,
                          dateRangeInput("viz_dates_gantt", "Plage de visualisation :", 
                                         start = "2010-01-01", end = "2025-12-31", width = "100%")
                   )
                 ),
                 
                 hr(),
                 plotlyOutput("plot_gantt_assec", height = "600px")
        ),
        tabPanel("Opérations : Pêche & Vidange",
                 br(),
                 h4("Calendrier des opérations de vidange et de pêche"),
                 p("Ce graphique isole uniquement les périodes d'ouverture des vannes (Vidange) et les jours d'intervention (Pêche) pour chaque étang."),
                 
                 fluidRow(
                   column(4,
                          dateRangeInput("viz_dates_operations", "Plage de visualisation :", 
                                         start = "2010-01-01", end = "2025-12-31", width = "100%")
                   )
                 ),
                 
                 hr(),
                 plotlyOutput("plot_operations", height = "600px")
        ),
        tabPanel("Carte Dynamique",
                 br(),
                 h4("Évolution spatiale du bassin versant"),
                 
                 fluidRow(
                   column(5,
                          # Le calendrier pour choisir facilement
                          uiOutput("ui_calendrier_carte"),
                          # Le slider caché en dessous pour garder le bouton d'animation "Play"
                          uiOutput("ui_slider_carte")
                   ),
                   column(5, offset = 1,
                          # 1. Choix de la couleur principale des polygones
                          radioButtons("color_carte", "1. Couleur de fond des étangs/BV :",
                                       choices = c("Taux de remplissage en eau" = "volume",
                                                   "État de saturation du sol (Curve Number)" = "etat_sol"),
                                       selected = "volume"),
                          
                          # 2. Choix des calques à superposer par-dessus
                          checkboxGroupInput("overlay_carte", "2. Calques supplémentaires :",
                                             choices = c("Masquer les étangs en Assec (Couvre en orange)" = "statut",
                                                         "Afficher le Ruissellement du jour (Bulles)" = "ruissellement"),
                                             selected = c("statut", "ruissellement"))
                   )
                 ),
                 hr(),
                 leafletOutput("map_etangs", height = "700px")
        ),
        # Second onglet pour le suivi technique du processus de simulation
        tabPanel("Journal d'exécution", 
                 # Affichage du flux textuel généré pendant les calculs
                 verbatimTextOutput("console_log"))
        
      )
    )
  )
)