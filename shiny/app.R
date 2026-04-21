library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)
library(plotly) 

# Chargement des fonctions de calcul
source("fonctions.R")

# Chargement de l'interface et du serveur
source("ui.R")
source("server.R")

# Lancement de l'application
shinyApp(ui = ui, server = server)