library(shiny)
library(shinythemes)
library(plotly)

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
      # Paramétrage des variables de calcul liées à la source de données SAFRAN
      numericInput("bv_code", "Code du Bassin Versant (SAFRAN) :", value = 2, min = 1),
      # Sélection de la fenêtre temporelle pour la simulation hydrologique
      dateRangeInput("dates", "Période de simulation :", 
                     start = "2010-01-01", end = "2025-12-31"),
      
      hr(),
      # Bouton de déclenchement du script de calcul situé côté serveur
      actionButton("run_sim", "Lancer la Simulation", 
                   class = "btn-primary", style = "width: 100%"),
      
      hr(),
      tags$h4("Visualisation"),
      # Menu de sélection de l'unité hydrographique (étang) à analyser
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
                   column(4, 
                          # Sélection de la variable physique à projeter sur l'axe des ordonnées
                          selectInput("var_plot", "Sélectionner la variable à analyser :", 
                                      choices = c(
                                        "Volume stocké (BF) [m³]" = "BF",
                                        "Volume déversé vers l'aval (Vsortant) [m³]" = "Vsortant",
                                        "Pluviométrie (RR) [mm]" = "RR",
                                        "Ruissellement capté (Volume_R) [m³]" = "Volume_R",
                                        "Bilan Pluie-Évapo (Vp_etp) [m³]" = "Vp_etp",
                                        "Volume forcé par Vidange [m³]" = "Vol_Vidange_Jour"
                                      ), 
                                      width = "100%") 
                   ),
                   column(4,
                          # Sortie dynamique pour le réglage précis de la plage temporelle affichée
                          uiOutput("ui_zoom_dates")
                   ),
                   column(4,
                          br(), 
                          # Option d'affichage des données d'observation pour comparaison modèle/mesures
                          checkboxInput("show_terrain", "Superposer les données Terrain (Sonde)", value = FALSE)
                   )
                 ),
                 
                 # Sortie graphique interactive générée via la librairie Plotly
                 plotlyOutput("plot_calibration", height = "500px"),
                 
                 hr(),
                 # Section dédiée aux indicateurs statistiques récapitulatifs (KPI)
                 tags$h4("Indicateurs Clés de l'Étang"),
                 tableOutput("kpi_table")
        ),
        
        # Second onglet pour le suivi technique du processus de simulation
        tabPanel("Journal d'exécution", 
                 # Affichage du flux textuel généré pendant les calculs
                 verbatimTextOutput("console_log"))
      )
    )
  )
)