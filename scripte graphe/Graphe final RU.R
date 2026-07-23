# ==============================================================================
# COMPARAISON GLOBALE DES 15 ANALYSES (Boxplots)
# ==============================================================================

library(tidyverse)
library(ggplot2)

# Ouverture de la fenêtre graphique
dev.new(width = 12, height = 8) 

# ------------------------------------------------------------------------------
# 1. DÉTECTION DES FICHIERS
# ------------------------------------------------------------------------------
liste_fichiers <- list.files(pattern = "^Analyse_.*\\.csv$")
print(paste(length(liste_fichiers), "fichiers d'analyse détectés."))

# ------------------------------------------------------------------------------
# 2. CHARGEMENT ET FUSION
# ------------------------------------------------------------------------------
df_global_box <- list()
compteur <- 1

for (fichier in liste_fichiers) {
  df_temp <- read.csv(fichier, sep = ";", dec = ",")
  
  if (nrow(df_temp) > 0) {
    df_temp <- df_temp %>% mutate(hypothese = str_replace(hypothese, "\\./", "/"))
    
    nom_evenement <- paste0(df_temp$Etang[1], " [", df_temp$Date_Debut_Orage[1], "]")
    df_temp$Evenement <- nom_evenement
    
    # Détection de l'unité (m3 ou mm)
    col_rmse <- grep("^RMSE_INRAE_", colnames(df_temp), value = TRUE)
    
    if (length(col_rmse) > 0) {
      df_temp$RMSE_INRAE <- df_temp[[col_rmse[1]]]
      
      unite_detectee <- str_extract(col_rmse[1], "(?<=_)[a-zA-Z0-9]+$")
      df_temp$Unite <- unite_detectee
      
      df_propre <- df_temp %>% select(Evenement, Etang, RMSE_INRAE, Unite)
      
      df_global_box[[compteur]] <- df_propre
      compteur <- compteur + 1
    }
  }
}

df_graphique <- bind_rows(df_global_box)
unite_finale <- df_graphique$Unite[1]

# ------------------------------------------------------------------------------
# 3. GÉNÉRATION DU GRAPHIQUE COMPARATIF
# ------------------------------------------------------------------------------
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
    legend.box.background = element_rect(color = "lightgrey", size = 0.5)
  )

print(g_meta_box)


# ==============================================================================
# EXTRACTION DU TOP 12 DES MEILLEURS SCÉNARIOS PAR ÉVÉNEMENT
# ==============================================================================

library(tidyverse)
library(stringr)

# 1. Lister les fichiers d'analyse
liste_fichiers <- list.files(pattern = "^Analyse_.*\\.csv$")
print(paste("Traitement de", length(liste_fichiers), "fichiers en cours..."))

# 2. Lecture et compilation
df_tous_orages <- map_df(liste_fichiers, function(fichier) {
  df <- read.csv(fichier, sep = ";", dec = ",")
  
  if(nrow(df) > 0) {
    col_rmse <- grep("^RMSE_INRAE_", colnames(df), value = TRUE)[1]
    
    df %>%
      mutate(
        RMSE = .data[[col_rmse]],
        Evenement = paste0(Etang, " (", Date_Debut_Orage, ")"),
        hypothese = str_replace(hypothese, "\\./", "/")
      ) %>%
      select(Evenement, Etang, Date_Debut_Orage, hypothese, RMSE)
  }
})

# 3. Extraction du Top 12 par événement
top_12_global <- df_tous_orages %>%
  group_by(Evenement) %>%
  slice_min(order_by = RMSE, n = 12, with_ties = FALSE) %>%
  ungroup()

# 4. Sauvegarde des résultats
write.table(top_12_global, "Palmares_Top12_Par_Evenement.csv", sep = ";", row.names = FALSE, dec = ",")
print("Export terminé : Palmares_Top12_Par_Evenement.csv")



# ==============================================================================
# COMPTAGE ET DEPARTAGE DES SCENARIOS (PALMARÈS ABSOLU)
# ==============================================================================

# 1. Création du palmarès (On compte le nombre d'apparitions dans le Top 12)
palmares_absolu <- top_12_global %>%
  group_by(hypothese) %>%
  summarise(Nombre_Apparitions_Top_12 = n()) %>%
  arrange(desc(Nombre_Apparitions_Top_12))

# 2. Départage par l'erreur moyenne globale
departage <- df_tous_orages %>%
  group_by(hypothese) %>%
  summarise(RMSE_Moyen_Global = round(mean(RMSE, na.rm = TRUE), 1)) %>%
  inner_join(palmares_absolu, by = "hypothese") %>%
  # On trie d'abord par le plus grand nombre d'apparitions, puis par le RMSE le plus bas
  arrange(desc(Nombre_Apparitions_Top_12), RMSE_Moyen_Global)

# 3. Affichage du grand gagnant et sauvegarde
print("🏆 CLASSEMENT FINAL DES SCÉNARIOS LES PLUS ROBUSTES :")
print(head(departage, 10)) # Affiche le Top 10 dans la console

write.table(departage, "Classement_Final_Robustesse.csv", sep = ";", row.names = FALSE, dec = ",")
print("Export terminé : Classement_Final_Robustesse.csv")




# =========================================================
# DEPARTAGE DES SCENARIOS PAR L'ERREUR MOYENNE
# =========================================================

departage <- df_tous_orages %>%
  group_by(hypothese) %>%
  summarise(RMSE_Moyen_Global = round(mean(RMSE, na.rm = TRUE), 1)) %>%
  inner_join(palmares_absolu, by = "hypothese") %>%
  arrange(desc(Nombre_Apparitions_Top_12), RMSE_Moyen_Global)

print("CLASSEMENT DÉFINITIF (FILTRE >= 4 APPARITIONS) :")
print(departage %>% 
        filter(Nombre_Apparitions_Top_12 >= 4))


# ==============================================================================
# BILAN HYDROLOGIQUE ANNUEL PISCICOLE
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork) 
library(ggrepel)

dev.new(width = 12, height = 6)

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE 
# ------------------------------------------------------------------------------
nom_etang <- "CORVEYZIEUX" 
annee_debut <- 2024 
"Resultats_MonteCarlo_RDS/CHALAMONT_ISOLE/Simu_Isole_Base_1.rds"

chemin_fichier_opti <- file.path(DOSSIER_RDS, "Simu_Base.rds")

# ------------------------------------------------------------------------------
# 2. CHARGEMENT ET DÉTECTION DE LA PÉRIODE DE GESTION
# ------------------------------------------------------------------------------
simu_inrae <- readRDS(chemin_fichier_opti)
infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)

df_complet <- simu_inrae$liste_finale[[nom_etang]]

# --- Étape A : Détection de la fin de la vidange (Année N) ---
jours_vidange_N <- df_complet %>%
  filter(year(dat) == annee_debut, Vol_Vidange_Jour > 0)

if (nrow(jours_vidange_N) > 0) {
  date_deb <- max(jours_vidange_N$dat)
} else {
  date_deb <- as.Date(paste0(annee_debut, "-10-01"))
  print("Information : Aucune vidange détectée (Année N). Application de la date par défaut (01-10).")
}

# --- Étape B : Détection de la fin de la vidange (Année N+1) ---
jours_vidange_N1 <- df_complet %>%
  filter(year(dat) == (annee_debut + 1), Vol_Vidange_Jour > 0)

if (nrow(jours_vidange_N1) > 0) {
  date_fin <- max(jours_vidange_N1$dat)
} else {
  date_fin <- as.Date(paste0(annee_debut + 1, "-09-30"))
}

# --- Étape C : Plafonnement temporel ---
duree_cycle <- as.numeric(date_fin - date_deb)

if (duree_cycle > 400) {
  date_fin <- date_deb + 365
  print(paste("Alerte : Durée du cycle (", duree_cycle, "jours) supérieure au seuil. Coupure à 400 jours."))
}

print(paste("Période analysée : du", date_deb, "au", date_fin, "(", as.numeric(date_fin - date_deb), "jours )"))

# --- Étape D : Découpage des données ---
df_annee <- df_complet %>%
  filter(dat >= date_deb & dat <= date_fin) %>%
  mutate(
    Pluie_Directe_m3 = RR * infos_etg$SURFACE_eau * 10,
    Apport_Amont = replace_na(Vamont, 0),             
    Evaporation  = replace_na(Evap_Reelle, 0),        
    Fuites       = replace_na(Fuite_Reelle, 0),       
    Surverse_Out = replace_na(Vsortant, 0),           
    Vidange      = replace_na(Vol_Vidange_Jour, 0)    
  )

# ------------------------------------------------------------------------------
# 3. CALCUL DES VOLUMES ET POURCENTAGES
# ------------------------------------------------------------------------------
df_entrees <- data.frame(
  Source = factor(c("Pluie directe", "Ruissellement", "Eau qui arrive par l'amont"), 
                  levels = c("Pluie directe", "Ruissellement", "Eau qui arrive par l'amont")),
  Volume = c(sum(df_annee$Pluie_Directe_m3, na.rm=TRUE), 
             sum(df_annee$Volume_R, na.rm=TRUE), 
             sum(df_annee$Apport_Amont, na.rm=TRUE))
) %>%
  mutate(
    Pourcentage = Volume / sum(Volume) * 100,
    Label = ifelse(Volume > 0, paste0(round(Pourcentage, 1), "%\n(", round(Volume/1000, 1), " dam3)"), "")
  )

df_sorties <- data.frame(
  Destination = factor(c("Evaporation", "Fuite", "Surverse", "Vidange"),
                       levels = c("Evaporation", "Fuite", "Surverse", "Vidange")),
  Volume = c(sum(df_annee$Evaporation, na.rm=TRUE), 
             sum(df_annee$Fuites, na.rm=TRUE), 
             sum(df_annee$Surverse_Out, na.rm=TRUE), 
             sum(df_annee$Vidange, na.rm=TRUE))
) %>%
  mutate(
    Pourcentage = Volume / sum(Volume) * 100,
    Label = ifelse(Volume > 0, paste0(round(Pourcentage, 1), "%\n(", round(Volume/1000, 1), " dam3)"), "")
  )

# ------------------------------------------------------------------------------
# 4. GÉNÉRATION DES CAMEMBERTS
# ------------------------------------------------------------------------------
theme_camembert <- theme_void() + theme(legend.position = "right", plot.title = element_text(face="bold", hjust=0.5, size=14))

# Graphique Entrées
g_entrees <- ggplot(df_entrees, aes(x = "", y = Volume, fill = Source)) +
  geom_bar(width = 1, stat = "identity", color = "white", size = 1) +
  coord_polar("y", start = 0) +
  geom_label_repel(aes(label = Label), 
                   position = position_stack(vjust = 0.5), 
                   color = "black", fontface = "bold", size = 4, 
                   show.legend = FALSE, box.padding = 0.5) +
  scale_fill_manual(values = c("Pluie directe" = "#3498db", "Ruissellement" = "#2ecc71", "Eau qui arrive par l'amont" = "#9b59b6")) +
  labs(title = paste("EAU ENTRANTE (Total:", round(sum(df_entrees$Volume)/1000, 1), "dam3)"), fill = "Origine") +
  theme_camembert

# Graphique Sorties
g_sorties <- ggplot(df_sorties, aes(x = "", y = Volume, fill = Destination)) +
  geom_bar(width = 1, stat = "identity", color = "white", size = 1) +
  coord_polar("y", start = 0) +
  geom_label_repel(aes(label = Label), 
                   position = position_stack(vjust = 0.5), 
                   color = "black", fontface = "bold", size = 4, 
                   show.legend = FALSE, box.padding = 0.5) +
  scale_fill_manual(values = c("Evaporation" = "#f39c12", "Fuite" = "#7f8c8d", "Surverse" = "#e74c3c", "Vidange" = "#34495e")) +
  labs(title = paste("EAU SORTANTE (Total:", round(sum(df_sorties$Volume)/1000, 1), "dam3)"), fill = "Destination") +
  theme_camembert

# ------------------------------------------------------------------------------
# 5. ASSEMBLAGE FINAL
# ------------------------------------------------------------------------------
graphique_final <- g_entrees + g_sorties + 
  plot_annotation(
    title = paste("Bilan Hydrologique de l'année piscicole -", nom_etang),
    subtitle = paste("Période :", date_deb, "au", date_fin),
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 14, hjust = 0.5))
  )

print(graphique_final)














