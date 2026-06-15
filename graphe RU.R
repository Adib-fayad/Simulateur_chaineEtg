# ==============================================================================
# BATTERIE D'ANALYSE GLOBALE DES PERFORMANCES DU MODÈLE INRAE
# ==============================================================================

library(tidyverse)
library(ggplot2)

# Ouvre une fenêtre externe pour une meilleure visualisation
dev.new() 

# ------------------------------------------------------------------------------
# 1. CHARGEMENT ET NETTOYAGE CENTRALISÉ DES DONNÉES
# ------------------------------------------------------------------------------
fichier_csv <- "Analyse_ROUE_RONZUEL_2025-05-28.csv"

df_analyses <- read.csv(fichier_csv, sep = ";", dec = ",") %>%
  filter(!is.na(RMSE_INRAE_m3)) %>%
  # Séparation de l'hypothèse en 3 paramètres distincts
  separate(hypothese, into = c("Coef", "RU", "Beta"), sep = "/", remove = FALSE) %>%
  mutate(
    Coef_num = as.numeric(Coef),
    RU_num = as.numeric(RU),
    Beta_num = as.numeric(Beta),
    # Versions en facteurs pour les couleurs et les catégories des graphiques
    Coef_fac = as.factor(Coef_num),
    RU_fac = as.factor(RU_num),
    Beta_fac = as.factor(Beta_num)
  ) %>%
  arrange(RMSE_INRAE_m3)

rmse_reference_cn <- df_analyses$RMSE_CN_m3[1]
biais_reference_cn <- df_analyses$Biais_Global_CN[1]
meilleur_modele <- df_analyses[1, ]

print("✅ Données chargées. Génération de la batterie de graphiques...")

# ------------------------------------------------------------------------------
# GRAPHIQUE 1 : LE PALMARÈS (Bar Chart)
# ------------------------------------------------------------------------------
g_top <- df_analyses %>%
  head(15) %>%
  ggplot(aes(x = reorder(hypothese, -RMSE_INRAE_m3), y = RMSE_INRAE_m3, fill = RMSE_INRAE_m3)) +
  geom_col(color = "black", alpha = 0.85) +
  geom_hline(yintercept = rmse_reference_cn, color = "#e74c3c", linetype = "dashed", size = 1.2) +
  geom_text(aes(label = round(RMSE_INRAE_m3, 0)), hjust = 1.2, color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "mako") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "1. Palmarès des Paramétrages (Top 15)",
    subtitle = paste("Ligne rouge = Modèle SCS-CN (", round(rmse_reference_cn,0), "m3) | Le modèle INRAE améliore nettement la précision."),
    x = "Combinaison (Coef / RU / Beta)",
    y = "Erreur RMSE (m3)"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# GRAPHIQUE 2 : BIAIS/ERREUR (Facettes Beta + Taille RU + Couleur Coef)
# ------------------------------------------------------------------------------
g_nuage <- ggplot(df_analyses, aes(x = Biais_Global_INRAE, y = RMSE_INRAE_m3)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) + 
  geom_point(aes(color = Coef_fac, size = RU_num), alpha = 0.8) +
  geom_point(data = data.frame(Biais_Global_INRAE = biais_reference_cn, RMSE_INRAE_m3 = rmse_reference_cn), 
             color = "#e74c3c", size = 5, shape = 17) + 
  facet_wrap(~ paste("Beta =", Beta_fac)) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "2. Analyse Biais vs Erreur (Séparée par paramètre Beta)",
    subtitle = "Triangle rouge = Modèle SCS-CN de base",
    x = "Biais Global (m3)  [ <-- Sous-estime  |  Surestime --> ]",
    y = "Erreur RMSE (m3) [ Plus c'est bas, mieux c'est ]",
    color = "Coefficient",
    size = "Réserve Utile (RU)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    strip.background = element_rect(fill = "#2c3e50", color = NA),
    legend.position = "right" 
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 3 : SENSIBILITÉ INDIVIDUELLE (Boxplots séparés)
# ------------------------------------------------------------------------------
df_long <- df_analyses %>%
  select(RMSE_INRAE_m3, Beta = Beta_fac, RU = RU_fac, Coef = Coef_fac) %>%
  pivot_longer(cols = c(Beta, RU, Coef), names_to = "Parametre", values_to = "Valeur") %>%
  mutate(Parametre = factor(Parametre, levels = c("Beta", "RU", "Coef")))

g_box <- ggplot(df_long, aes(x = Valeur, y = RMSE_INRAE_m3, fill = Parametre)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, color = "#2c3e50") +
  geom_hline(yintercept = rmse_reference_cn, color = "#e74c3c", linetype = "dashed", size = 1) +
  facet_wrap(~Parametre, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "3. Impact et Sensibilité de chaque paramètre",
    subtitle = "Chaque encadré isole l'impact d'un seul paramètre sur l'erreur (Ligne rouge = SCS-CN)",
    x = "Valeur testée",
    y = "Dispersion de l'erreur RMSE (m3)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 13, color = "white"),
    strip.background = element_rect(fill = "#34495e", color = NA)
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 4 : CARTOGRAPHIE DES ZONES OPTIMALES (Heatmap)
# ------------------------------------------------------------------------------
df_heatmap <- df_analyses %>%
  group_by(Beta_fac, Coef_fac) %>%
  summarise(RMSE_moyen = mean(RMSE_INRAE_m3), .groups = "drop")

g_heat <- ggplot(df_heatmap, aes(x = Beta_fac, y = Coef_fac, fill = RMSE_moyen)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(RMSE_moyen, 0)), color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_minimal() +
  labs(
    title = "4. Cartographie des interactions (Beta vs Coef)",
    subtitle = "Couleurs sombres = Fortes erreurs | Couleurs claires (jaune) = Zone optimale de calibrage",
    x = "Paramètre Beta (Forme de la vidange du réservoir)",
    y = "Coefficient de transfert (Coef)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# AFFICHAGE FINAL ET BILAN
# ------------------------------------------------------------------------------
print(g_top)
print(g_nuage)
print(g_box)
print(g_heat)

print("=======================================================================")
print(paste("🏆 LE GAGNANT ABSOLU EST :", meilleur_modele$hypothese))
print(paste("RMSE :", round(meilleur_modele$RMSE_INRAE_m3, 0), "m3 | Biais :", round(meilleur_modele$Biais_Global_INRAE, 0), "m3"))
print("=======================================================================")















# ==============================================================================
# ANALYSE CROISÉE (ROBUSTESSE) MULTI-ÉVÉNEMENTS - MODÈLE INRAE
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(stringr)

# Ouvre une fenêtre externe pour une meilleure visualisation
dev.new() 

# ------------------------------------------------------------------------------
# 1. IMPORTATION ET FUSION DES ÉVÉNEMENTS
# ------------------------------------------------------------------------------
# Renseigne ici le nom exact de tes 3 fichiers CSV générés
fichiers_csv <- c(
  "Analyse_ROUE_RONZUEL_2025-05-28.csv",
  "Analyse_ROUE_RONZUEL_2025-08-26.csv",
  "Analyse_ROUE_RONZUEL_2025-09-20.csv",
  "Analyse_GRAND_ETANG_LA_ROUE_2025-05-31.csv",
  "Analyse_GRAND_ETANG_LA_ROUE_2025-09-20.csv",
  "Analyse_GRAND_ETANG_LA_ROUE_2025-10-17.csv",
  "Analyse_REMONDET_NORD_2023-10-18.csv",
  "Analyse_REMONDET_NORD_2025-10-29.csv",
  "Analyse_REMONDET_NORD_2025-11-12.csv"
  
  
)

# On lit, on empile (bind_rows) et on nettoie le point potentiel dans l'hypothèse
df_compile <- lapply(fichiers_csv, function(f) read.csv(f, sep = ";", dec = ",")) %>%
  bind_rows() %>%
  filter(!is.na(RMSE_INRAE_m3)) %>%
  mutate(hypothese = str_replace(hypothese, "\\./", "/"))

# ------------------------------------------------------------------------------
# 2. LE MOTEUR D'ANALYSE CROISÉE (Agrégation des performances)
# ------------------------------------------------------------------------------
df_croise <- df_compile %>%
  group_by(hypothese) %>%
  summarise(
    # La vraie performance globale : la moyenne des erreurs sur tous les orages
    RMSE_Moyen = mean(RMSE_INRAE_m3, na.rm = TRUE),
    
    # Le biais avec signe (pour voir si globalement ça surestime ou sous-estime)
    Biais_Moyen_Signe = mean(Biais_Global_INRAE, na.rm = TRUE),
    
    # L'erreur volumique réelle moyenne (sans que les + et - s'annulent)
    Biais_Absolu_Moyen = mean(abs(Biais_Global_INRAE), na.rm = TRUE),
    
    # Pour comparer avec l'ancien modèle sur les mêmes dates
    RMSE_CN_Moyen = mean(RMSE_CN_m3, na.rm = TRUE) 
  ) %>%
  ungroup() %>%
  # Découpage des paramètres pour pouvoir les mettre en graphique
  separate(hypothese, into = c("Coef", "RU", "Beta"), sep = "/", remove = FALSE) %>%
  mutate(
    Coef_num = as.numeric(Coef),
    RU_num = as.numeric(RU),
    Beta_num = as.numeric(Beta),
    Coef_fac = as.factor(Coef_num),
    RU_fac = as.factor(RU_num),
    Beta_fac = as.factor(Beta_num)
  ) %>%
  # On trie pour avoir le meilleur modèle (plus petit RMSE Moyen) en haut
  arrange(RMSE_Moyen)

# Récupération des repères du meilleur modèle
rmse_ref_croise <- df_croise$RMSE_CN_Moyen[1]
meilleur_modele <- df_croise[1, ]

print("✅ Fichiers croisés avec succès. Génération de la batterie de graphiques de robustesse...")

# ------------------------------------------------------------------------------
# GRAPHIQUE 1 : LE PALMARÈS DE ROBUSTESSE
# ------------------------------------------------------------------------------
g_top <- df_croise %>%
  head(15) %>%
  ggplot(aes(x = reorder(hypothese, -RMSE_Moyen), y = RMSE_Moyen, fill = RMSE_Moyen)) +
  geom_col(color = "black", alpha = 0.85) +
  geom_hline(yintercept = rmse_ref_croise, color = "#e74c3c", linetype = "dashed", size = 1.2) +
  geom_text(aes(label = round(RMSE_Moyen, 0)), hjust = 1.2, color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "mako") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "1. Les 15 Modèles les plus robustes (Moyenne sur 3 événements)",
    subtitle = paste("Ligne rouge = SCS-CN Moyen (", round(rmse_ref_croise,0), "m3)"),
    x = "Combinaison (Coef / RU / Beta)",
    y = "Erreur RMSE Moyenne (m3)"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# GRAPHIQUE 2 : COMPROMIS BIAIS/ERREUR GLOBAL
# ------------------------------------------------------------------------------
g_nuage <- ggplot(df_croise, aes(x = Biais_Moyen_Signe, y = RMSE_Moyen)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) + 
  geom_point(aes(color = Coef_fac, size = RU_num), alpha = 0.8) +
  facet_wrap(~ paste("Beta =", Beta_fac)) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "2. Comportement Multicritères Moyen (Séparé par Beta)",
    subtitle = "Positionnement des scénarios croisés sur les 3 orages",
    x = "Biais Moyen (m3)  [ <-- Sous-estime  |  Surestime --> ]",
    y = "Erreur RMSE Moyenne (m3)",
    color = "Coefficient",
    size = "Réserve Utile (RU)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    strip.background = element_rect(fill = "#2c3e50", color = NA)
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 3 : SENSIBILITÉ CROISÉE (Qu'est-ce qui influence la robustesse ?)
# ------------------------------------------------------------------------------
df_long <- df_croise %>%
  select(RMSE_Moyen, Beta = Beta_fac, RU = RU_fac, Coef = Coef_fac) %>%
  pivot_longer(cols = c(Beta, RU, Coef), names_to = "Parametre", values_to = "Valeur") %>%
  mutate(Parametre = factor(Parametre, levels = c("Beta", "RU", "Coef")))

g_box <- ggplot(df_long, aes(x = Valeur, y = RMSE_Moyen, fill = Parametre)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, color = "#2c3e50") +
  geom_hline(yintercept = rmse_ref_croise, color = "#e74c3c", linetype = "dashed", size = 1) +
  facet_wrap(~Parametre, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "3. Impact des paramètres sur la robustesse globale",
    x = "Valeur testée",
    y = "Dispersion du RMSE Moyen (m3)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 13, color = "white"),
    strip.background = element_rect(fill = "#34495e", color = NA)
  )

# ------------------------------------------------------------------------------
# GRAPHIQUE 4 : CARTOGRAPHIE DES ZONES OPTIMALES GLOBALES
# ------------------------------------------------------------------------------
df_heatmap <- df_croise %>%
  group_by(Beta_fac, Coef_fac) %>%
  summarise(RMSE_Lisse = mean(RMSE_Moyen), .groups = "drop")

g_heat <- ggplot(df_heatmap, aes(x = Beta_fac, y = Coef_fac, fill = RMSE_Lisse)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(RMSE_Lisse, 0)), color = "white", fontface = "bold") +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_minimal() +
  labs(
    title = "4. Cartographie du couple Beta/Coef idéal multi-événements",
    subtitle = "Zone claire = Paramétrage le plus stable sur les 3 orages",
    x = "Paramètre Beta",
    y = "Coefficient de transfert"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

# ------------------------------------------------------------------------------
# AFFICHAGE ET VERDICT FINAL
# ------------------------------------------------------------------------------
print(g_top)
print(g_nuage)
print(g_box)
print(g_heat)

print("=======================================================================")
print("🏆 VERDICT DE L'ANALYSE CROISÉE (Paramétrage le plus robuste) :")
print(paste("Hypothèse gagnante :", meilleur_modele$hypothese))
print("-----------------------------------------------------------------------")
print(paste("-> RMSE Moyen :", round(meilleur_modele$RMSE_Moyen, 0), "m3"))
print(paste("-> Erreur volumique moyenne (Biais Absolu) :", round(meilleur_modele$Biais_Absolu_Moyen, 0), "m3"))
print(paste("-> Biais net :", round(meilleur_modele$Biais_Moyen_Signe, 0), "m3"))
print("=======================================================================")




# ==============================================================================
# COMPARAISON GLOBALE DES 15 ANALYSES (Méta-Analyse par Boxplots)
# ==============================================================================

library(tidyverse)
library(ggplot2)

# Ouvre une grande fenêtre externe pour tout voir sans écraser les textes
dev.new(width = 12, height = 8) 

# ------------------------------------------------------------------------------
# 1. DÉTECTION AUTOMATIQUE DES FICHIERS
# ------------------------------------------------------------------------------
# Le script cherche TOUS tes fichiers CSV qui commencent par "Analyse_"
liste_fichiers <- list.files(pattern = "^Analyse_.*\\.csv$")

print(paste("🔎", length(liste_fichiers), "fichiers d'analyse trouvés dans ton dossier."))

# ------------------------------------------------------------------------------
# 2. CHARGEMENT ET FUSION SÉCURISÉE
# ------------------------------------------------------------------------------
df_global_box <- list()
compteur <- 1

for (fichier in liste_fichiers) {
  # Lecture du CSV (s'adapte aux virgules et points-virgules)
  df_temp <- read.csv(fichier, sep = ";", dec = ",")
  
  if (nrow(df_temp) > 0) {
    # On nettoie l'éventuel point dans l'hypothèse au cas où
    df_temp <- df_temp %>% mutate(hypothese = str_replace(hypothese, "\\./", "/"))
    
    # On crée une étiquette propre pour l'événement (ex: "ROUE RONZUEL (2025-05-28)")
    nom_evenement <- paste0(df_temp$Etang[1], " [", df_temp$Date_Debut_Orage[1], "]")
    df_temp$Evenement <- nom_evenement
    
    # SÉCURITÉ UNITÉ : Le code détecte tout seul si ton fichier est en m3 ou en mm !
    col_rmse <- grep("^RMSE_INRAE_", colnames(df_temp), value = TRUE)
    
    if (length(col_rmse) > 0) {
      df_temp$RMSE_INRAE <- df_temp[[col_rmse[1]]]
      
      # On extrait l'unité (m3 ou mm) pour l'afficher proprement sur l'axe
      unite_detectee <- str_extract(col_rmse[1], "(?<=_)[a-zA-Z0-9]+$")
      df_temp$Unite <- unite_detectee
      
      # On ne garde que le strict nécessaire pour alléger la mémoire
      df_propre <- df_temp %>% select(Evenement, Etang, RMSE_INRAE, Unite)
      
      df_global_box[[compteur]] <- df_propre
      compteur <- compteur + 1
    }
  }
}

# Fusion de tes 15 fichiers en un seul grand tableau de calcul
df_graphique <- bind_rows(df_global_box)
unite_finale <- df_graphique$Unite[1]

# ------------------------------------------------------------------------------
# 3. GÉNÉRATION DU GRAPHIQUE COMPARATIF COMPACT (Solution Biais)
# ------------------------------------------------------------------------------
g_meta_box <- ggplot(df_graphique, aes(x = RMSE_INRAE , y = reorder(Evenement, RMSE_INRAE, FUN = median), fill = Etang)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3, color = "#2c3e50", lwd = 0.6) +
  
  # On garde le flip pour mettre les boîtes debout
  coord_flip() + 
  
  scale_fill_brewer(palette = "Set2") + 
  
  theme_minimal() +
  labs(
    title = "Analyse Comparative de la Performance du Modèle",
    subtitle = "Classement des événements du plus précis (à droite) au moins précis (à gauche)",
    x = paste0("Erreur RMSE (", unite_finale, ")"),
    y = "Événement d'Étude",
    fill = "Étang analysé"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a252f"),
    plot.subtitle = element_text(size = 11, color = "#555555"),
    
    # L'ASTUCE EST ICI : on incline le texte à 45 degrés et on l'aligne (hjust=1)
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

# 1. Lister tous les fichiers d'analyse
liste_fichiers <- list.files(pattern = "^Analyse_.*\\.csv$")

print(paste("Traitement de", length(liste_fichiers), "fichiers en cours..."))

# 2. Lecture et compilation intelligente
df_tous_orages <- map_df(liste_fichiers, function(fichier) {
  df <- read.csv(fichier, sep = ";", dec = ",")
  
  if(nrow(df) > 0) {
    # On détecte la colonne RMSE (qu'elle soit en m3 ou en mm)
    col_rmse <- grep("^RMSE_INRAE_", colnames(df), value = TRUE)[1]
    
    df %>%
      mutate(
        RMSE = .data[[col_rmse]], # On standardise le nom de la colonne
        Evenement = paste0(Etang, " (", Date_Debut_Orage, ")"),
        hypothese = str_replace(hypothese, "\\./", "/") # Nettoyage au cas où
      ) %>%
      select(Evenement, Etang, Date_Debut_Orage, hypothese, RMSE)
  }
})

# 3. LE FILTRE MAGIQUE : Extraction du Top 12 par événement
top_12_global <- df_tous_orages %>%
  group_by(Evenement) %>%
  # On trie par RMSE et on garde les 12 plus petits pour CHAQUE événement
  slice_min(order_by = RMSE, n = 12, with_ties = FALSE) %>%
  ungroup()

# 4. Sauvegarde du résultat dans un fichier propre
write.table(top_12_global, "Palmares_Top12_Par_Evenement.csv", sep = ";", row.names = FALSE, dec = ",")

print("✅ SUCCÈS ! Le fichier 'Palmares_Top12_Par_Evenement.csv' a été créé.")

# =========================================================
# QUEL EST LE SCÉNARIO LE PLUS SOUVENT DANS LE TOP 12 ?
# =========================================================
palmares_absolu <- top_12_global %>%
  count(hypothese, name = "Nombre_Apparitions_Top_12") %>%
  arrange(desc(Nombre_Apparitions_Top_12))

print("🏆 LE CLASSEMENT DE ROBUSTESSE (Qui apparaît le plus souvent dans le Top 12 ?) :")
print(head(palmares_absolu, 10))





# ==============================================================================
# BILAN HYDROLOGIQUE ANNUEL PISCICOLE (Version Sécurisée & Diagnostic)
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork) 

dev.new(width = 12, height = 6)

# ------------------------------------------------------------------------------
# 1. PARAMÉTRAGE 
# ------------------------------------------------------------------------------
# ATTENTION À L'ORTHOGRAPHE EXACTE : 
nom_etang <- "GRAND ETANG LA ROUE" # Corrigé ici (vérifie que c'est bien ça dans ta base)
annee_debut <- 2023 

chemin_fichier_opti <- "Banque_Simulations_Globales/Simu_INRAE_Beta1_RU175_Coef0.30.rds"

# ------------------------------------------------------------------------------
# 2. CHARGEMENT ET DÉTECTION DYNAMIQUE DE LA PÉRIODE DE GESTION
# ------------------------------------------------------------------------------
simu_inrae <- readRDS(chemin_fichier_opti)
infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)

# On extrait tout le tableau de l'étang pour faire nos recherches
df_complet <- simu_inrae$liste_finale[[nom_etang]]

# --- Étape A : Détection de la fin de la vidange (Année N) ---
jours_vidange_N <- df_complet %>%
  filter(year(dat) == annee_debut, Vol_Vidange_Jour > 0)

if (nrow(jours_vidange_N) > 0) {
  # S'il y a eu une vidange, la date de début est le TOUT DERNIER jour de cette vidange
  date_deb <- max(jours_vidange_N$dat)
} else {
  # Sécurité : S'il n'y a eu aucune vidange (ex: maintien en eau), on prend le 1er Octobre
  date_deb <- as.Date(paste0(annee_debut, "-10-01"))
  print("⚠️ Info : Aucune vidange détectée l'année N. Utilisation du 01-10 par défaut.")
}

# --- Étape B : Détection de la fin de la vidange (Année N+1) ---
jours_vidange_N1 <- df_complet %>%
  filter(year(dat) == (annee_debut + 1), Vol_Vidange_Jour > 0)

if (nrow(jours_vidange_N1) > 0) {
  date_fin <- max(jours_vidange_N1$dat)
} else {
  date_fin <- as.Date(paste0(annee_debut + 1, "-09-30"))
}

# --- Étape C : Application stricte de la contrainte des 365 jours ---
duree_cycle <- as.numeric(date_fin - date_deb)

if (duree_cycle > 400) {
  date_fin <- date_deb + 365
  print(paste("⚠️ Plafonnement activé : La période faisait", duree_cycle, "jours. Coupée à 400 jours."))
}

print("=====================================================")
print(paste("🎯 Période dynamique validée : du", date_deb, "au", date_fin))
print(paste("⏳ Durée analysée :", as.numeric(date_fin - date_deb), "jours"))
print("=====================================================")

# --- Étape D : Découpage du tableau final ---
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

# N'oublie pas d'ajouter cette ligne tout en haut de ton script avec les autres library !
library(ggrepel)

# ------------------------------------------------------------------------------
# 4. GÉNÉRATION DES CAMEMBERTS (AVEC GGREPEL POUR ÉVITER LES CHEVAUCHEMENTS)
# ------------------------------------------------------------------------------
theme_camembert <- theme_void() + theme(legend.position = "right", plot.title = element_text(face="bold", hjust=0.5, size=14))

# Graphique ARRIVÉES
g_entrees <- ggplot(df_entrees, aes(x = "", y = Volume, fill = Source)) +
  geom_bar(width = 1, stat = "identity", color = "white", size = 1) +
  coord_polar("y", start = 0) +
  
  # LA MAGIE EST ICI : geom_label_repel repousse les textes pour qu'ils ne se touchent jamais
  geom_label_repel(aes(label = Label), 
                   position = position_stack(vjust = 0.5), 
                   color = "black", fontface = "bold", size = 4, 
                   show.legend = FALSE, box.padding = 0.5) +
  
  scale_fill_manual(values = c("Pluie directe" = "#3498db", "Ruissellement" = "#2ecc71", "Eau qui arrive par l'amont" = "#9b59b6")) +
  labs(title = paste("EAU QUI ARRIVE (Total:", round(sum(df_entrees$Volume)/1000, 1), "dam3)"), fill = "Origine") +
  theme_camembert

# Graphique SORTIES / PERTES
g_sorties <- ggplot(df_sorties, aes(x = "", y = Volume, fill = Destination)) +
  geom_bar(width = 1, stat = "identity", color = "white", size = 1) +
  coord_polar("y", start = 0) +
  
  # LA MÊME CHOSE ICI
  geom_label_repel(aes(label = Label), 
                   position = position_stack(vjust = 0.5), 
                   color = "black", fontface = "bold", size = 4, 
                   show.legend = FALSE, box.padding = 0.5) +
  
  scale_fill_manual(values = c("Evaporation" = "#f39c12", "Fuite" = "#7f8c8d", "Surverse" = "#e74c3c", "Vidange" = "#34495e")) +
  labs(title = paste("EAU QUI SORT (Total:", round(sum(df_sorties$Volume)/1000, 1), "dam3)"), fill = "Destination") +
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


