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
      
      # Entrées de fichiers par l'utilisateur
      fileInput("file_os", "Occupation des Sols (.csv)", accept = ".csv"),
      fileInput("file_etg", "Caractéristiques Étangs (.csv)", accept = ".csv"),
      fileInput("file_assec", "Données ASSEC (.csv)", accept = ".csv"),
      fileInput("file_vidange", "Dates de Vidange (.csv)", accept = ".csv"),
      
      hr(),
      # Paramètres de calcul
      numericInput("bv_code", "Code du Bassin Versant (SAFRAN) :", value = 2, min = 1),
      dateRangeInput("dates", "Période de simulation :", 
                     start = "2010-01-01", end = "2025-12-31"),
      
      hr(),
      actionButton("run_sim", "Lancer la Simulation", 
                   class = "btn-primary", style = "width: 100%"),
      
      hr(),
      tags$h4("Visualisation"),
      selectInput("etang_choisi", "Choisir un étang à afficher :", choices = NULL)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Graphiques de Simulation", 
                 plotOutput("plot_volume")),
        tabPanel("Journal d'exécution", 
                 verbatimTextOutput("console_log"))
      )
    )
  )
)