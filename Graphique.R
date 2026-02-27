library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
source("fonctions_V3.R")

###Graphe pluvio

par(mfrow = c(1, 2))
barplot(tapply(pluvio$RR,as.factor(pluvio$an),FUN = "sum"),main="Precipitation")
barplot(tapply(pluvio$P_ETP,as.factor(pluvio$an),FUN = "sum"),main="Bilan P-ETP")

head(pluvio)


####Graphe chane d'etangs
df_tous_les_etangs <- bind_rows(liste_etangs)

# On lance le super-graphique à facettes !
ggplot(df_tous_les_etangs, aes(x = dat)) +
  
  # La ligne bleue pour le volume calculé (BF)
  geom_line(aes(y = BF), color = "blue", size = 0.5) +
  
  # On découpe le graphique par étang (NOM)
  facet_wrap(~ NOM, scales = "free_y") +
  
  # LA SOLUTION EST ICI : On force l'affichage en vrais nombres avec un espace pour les milliers
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  
  # On rend ça joli
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8, face = "bold"), # Nom des étangs en gras
    axis.text.x = element_text(angle = 45, hjust = 1)   # On penche les dates pour la lisibilité
  ) +
  labs(
    title = "Bilan hydrologique complet : Volume de tous les étangs",
    x = "Date", 
    y = "Volume de l'étang (m³)"
  )

#

dev.new()
un_etang <- liste_etangs[["RIOUX"]]

ggplot(un_etang, aes(x = dat, y = BF)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = paste("Évolution du volume -", un_etang$NOM[1]),
       x = "Date", y = "Volume de l'étang (m³)")


#