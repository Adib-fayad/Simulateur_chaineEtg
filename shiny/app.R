library(shiny)
library(shinythemes)
library(tidyverse)
library(arrow)
library(lubridate)
library(zoo)
library(igraph)

# Chargement des fonctions de calcul
source("fonctions.R")

# Chargement de l'interface et du serveur
source("ui.R")
source("server.R")

# Lancement de l'application
shinyApp(ui = ui, server = server)