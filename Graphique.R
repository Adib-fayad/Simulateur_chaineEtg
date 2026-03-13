library(tidyverse)
library(zoo)
library(igraph)
library(scales) 
library(ggraph)
source("fonctions.R")

###Graphe pluvio

par(mfrow = c(1, 2))
barplot(tapply(pluvio$RR,as.factor(pluvio$an),FUN = "sum"),main="Precipitation")
barplot(tapply(pluvio$P_ETP,as.factor(pluvio$an),FUN = "sum"),main="Bilan P-ETP")

head(pluvio)

dev.new()

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
un_etang <- liste_etangs[["PARADIS"]]

ggplot(un_etang, aes(x = dat, y = BF)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = paste("Évolution du volume -", un_etang$NOM[1]),
       x = "Date", y = "Volume de l'étang (m³)")+ scale_x_date(date_breaks = "1 month", date_labels = "%B")


# 1. On extrait l'étang ET on filtre directement sur l'année 2022
un_etang_2022 <- liste_etangs[["VIEUX"]] %>%
  filter(format(dat, "%Y") == c(2015,2016,2017,2018))
#2021,2022,2023,2024
# 2. On dessine le graphique avec ce nouveau tableau
ggplot(un_etang_2022, aes(x = dat, y = BF)) +
  geom_line(color = "blue", size = 1) +  # (Note : R te dira peut-être d'utiliser 'linewidth = 1' à la place de 'size' selon ta version)
  theme_minimal() +
  labs(title = paste("Évolution du volume en 2022 -", un_etang_2022$NOM[1]),
       x = "Date", y = "Volume de l'étang (m³)")+ scale_x_date(date_breaks = "1 month", date_labels = "%B")

#


# Remplace "NOM_ETANG_VIDE" par un étang qui pose problème
diagnostic <- liste_etangs[["PARADIS"]]

# On va tracer le volume (BF) et l'évaporation/pluie (Vp_etp) pour comprendre
ggplot(diagnostic, aes(x = dat)) +
  geom_line(aes(y = BF, color = "Volume de l'étang (BF)"), size = 1) +
  theme_minimal() +
  labs(title = paste("Diagnostic de la panne d'eau -", diagnostic$NOM[1]),
       x = "Date", y = "Volume (m3)")



un_etang_zoom <- liste_etangs[["VIEUX"]] %>%
  filter(dat >= as.Date("2022-11-08") & dat <= as.Date("2022-11-22")) %>% select(, -Position,-CNI)
un_etang_zoom
# On donne la colonne BF pour la hauteur, et la colonne dat pour le texte en dessous
barplot(
  height = un_etang_zoom$Vamont, 
  names.arg = un_etang_zoom$dat, 
  col = "steelblue",           # Une jolie couleur
  main = "Volume de l'étang",  # Le titre
  ylab = "Volume (m3)",        # Le nom de l'axe vertical
  las = 2                      # L'astuce magique : las = 2 tourne les dates à la verticale !
)

