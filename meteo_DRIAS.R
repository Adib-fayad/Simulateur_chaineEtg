library(tidyverse)
library(lubridate)

# 1. Chemin vers ton fichier texte DRIAS téléchargé
fichier_drias <- "data/meteo/DRIAS_NorESM1_REMO2015.txt"

# 2. CORRECTION : On utilise read_delim en précisant que le séparateur est un point-virgule (;)
print("Lecture du fichier brut DRIAS...")
drias_brut <- read_delim(fichier_drias, delim = ";", comment = "#", col_names = FALSE, show_col_types = FALSE)

# 3. On force les noms de colonnes
colnames(drias_brut) <- c("DATE", "LAMBX", "LAMBY", "tasminAdjust", "tasmaxAdjust", "tasAdjust",  "prtotAdjust", "prsnAdjust", "evspsblpotAdjust")

# 4. Traduction et CORRECTION DES UNITÉS (Vital pour le simulateur)
meteo_traduite <- drias_brut %>%
  mutate(
    DATE = ymd(DATE),               
    # DRIAS donne des kg/m2/s. On multiplie par 86400 (secondes par jour) pour avoir des mm/jour
    PRELIQ = as.numeric(prtotAdjust) * 86400,
    ETP = as.numeric(evspsblpotAdjust) * 86400,
    Tmoy= as.numeric(tasAdjust) -273.15,
    Tmax= as.numeric(tasmaxAdjust) -273.15,
    Tmin= as.numeric(tasminAdjust) -273.15
    
  ) %>%
  # On garde uniquement nos 5 colonnes de travail
  select(DATE, LAMBX, LAMBY, PRELIQ, ETP,Tmoy,Tmax,Tmin)

# 5. On exporte le fichier
chemin_sortie <- "data/meteo/Drias_RCP.csv"
write.csv2(meteo_traduite, chemin_sortie, row.names = FALSE)

print(paste("✅ Succès ! Fichier découpé, unités converties en mm/jour, et enregistré ici :", chemin_sortie))






# Chargement des librairies
library(tidyverse)
library(igraph)

# 1. Chargement dynamique du fichier ASSEC
chemin_assec_origine <- "data/Chalamont/ASSEC_Final.csv" # Modifie le chemin si besoin
print(paste("Lecture du fichier d'origine :", chemin_assec_origine))

df_base <- read.csv(chemin_assec_origine, sep = ";", stringsAsFactors = FALSE)

# Nettoyage des valeurs vides en "Evolage"
df_base[is.na(df_base) | df_base == ""] <- "Evolage"

# 2. Construction de l'arbre topologique (Réseau d'étangs)
liens <- df_base %>% 
  select(NOM, Exutoire_1) %>% 
  filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT" & Exutoire_1 != "")
g <- graph_from_data_frame(liens, directed = TRUE)

# Calcul de la "profondeur" de chaque étang dans la cascade
profondeurs <- distances(g, mode = "in")
profondeur_max_par_etang <- apply(profondeurs, 1, function(x) max(x[!is.infinite(x)]))

# 3. Création des dataframes de projection
df_opt <- df_base
df_alea <- df_base

# Boucle temporelle de 2025 à 2050
annees_futures <- 2025:2050
set.seed(42)

for (annee in annees_futures) {
  nom_col <- paste0("Assec", annee)
  
  # --- SCÉNARIO OPTIMISÉ ---
  assec_opt <- c()
  for (etang in df_opt$NOM) {
    if (etang %in% names(profondeur_max_par_etang)) {
      prof <- profondeur_max_par_etang[etang]
    } else {
      prof <- 0 
    }
    
    rotation_id <- prof %% 5
    annee_cycle <- (annee - 2025) %% 5
    
    if (annee_cycle == rotation_id) {
      assec_opt <- c(assec_opt, "Assec")
    } else {
      assec_opt <- c(assec_opt, "Evolage")
    }
  }
  df_opt[[nom_col]] <- assec_opt
  
  # --- SCÉNARIO ALÉATOIRE ---
  df_alea[[nom_col]] <- sample(c("Evolage", "Assec"), size = nrow(df_alea), prob = c(0.8, 0.2), replace = TRUE)
}

# 4. NOUVEAU : Nettoyage pour ne garder que 2025 à 2050
colonnes_a_garder <- c("OBJECTID", "NOM", "Exutoire_1", paste0("Assec", 2025:2050))

df_opt_final <- df_opt %>% select(all_of(colonnes_a_garder))
df_alea_final <- df_alea %>% select(all_of(colonnes_a_garder))

# 5. Sauvegarde des fichiers finaux
write.table(df_opt_final, "data/Chalamont/ASSEC_Optimise.csv", sep = ";", row.names = FALSE, quote = FALSE)
write.table(df_alea_final, "data/Chalamont/ASSEC_Aleatoire.csv", sep = ";", row.names = FALSE, quote = FALSE)

print("✅ Fichiers ASSEC_Optimise.csv et ASSEC_Aleatoire.csv générés avec succès ! (Uniquement de 2025 à 2050)")







# ==============================================================================
# SCRIPT D'ANALYSE DE SENSIBILITE GLOBALE (INRAE vs SCS-CN)
# Objectif : Comparer les performances des modeles sur les etangs instrumentes
# ==============================================================================

library(tidyverse)
library(lubridate)
library(ggplot2)

# =========================================================
# 1. PARAMETRAGE DES FICHIERS ET DES ETANGS
# =========================================================
# Indique ici les deux simulations que tu souhaites faire s'affronter
chemin_simu_inrae <- "archive simul/Modele_INRAE_Base_beta_5_RU_200_Coef_0.4_20260608.rds"
chemin_simu_cn    <- "archive simul/L0.2_Pant5_CN_base_20260519.rds"

# Liste stricte des etangs (en MAJUSCULES pour correspondre a la base de donnees)
etangs_suivis <- c("REMONDET NORD", "CARRONNIER", "CORVEYZIEUX", "FOUR", 
                   "GRAND ETANG LA ROUE", "GRAND RONZUEL", "LIGNIERE", 
                   "POLLET", "ROUE RONZUEL")

# Chargement des donnees pre-calculees
simu_inrae <- readRDS(chemin_simu_inrae)
simu_cn    <- readRDS(chemin_simu_cn)

# Initialisation du tableau de synthese
df_bilan_global <- data.frame()

print("Demarrage de l'analyse comparative globale...")

# =========================================================
# 2. BOUCLE D'ANALYSE SUR CHAQUE ETANG
# =========================================================
for (nom_etang in etangs_suivis) {
  
  # Verification de la presence de l'etang dans les simulations
  if (!(nom_etang %in% names(simu_inrae$liste_finale))) {
    print(paste("Ignorer :", nom_etang, "(Introuvable dans les donnees simulees)"))
    next
  }
  
  # Recuperation des parametres physiques
  infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
  surface_terre_ha <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
  
  # Extraction des donnees des modeles
  df_inrae <- simu_inrae$liste_finale[[nom_etang]] %>% select(dat, RR, Vol_INRAE = Volume_R)
  df_cn    <- simu_cn$liste_finale[[nom_etang]] %>% select(dat, Vol_CN = Volume_R)
  
  # Extraction de la Sonde Terrain
  df_sonde <- load_terrain(nom_etang) %>%
    select(dat, Volume_Reel) %>%
    mutate(Delta_Vol = Volume_Reel - lag(Volume_Reel, 1))
  
  # Fusion et Filtre de Purete Stricte
  analyse_ruiss <- df_inrae %>%
    left_join(df_cn, by = "dat") %>%
    left_join(df_sonde, by = "dat") %>%
    mutate(
      Pluie_Totale_BV_m3 = RR * surface_terre_ha * 10,
      Pluie_Directe_m3 = RR * infos_etg$SURFACE_eau * 10,
      Vol_Reel_Sonde_Brut = Delta_Vol - Pluie_Directe_m3,
      
      Vol_Reel_Sonde_Pur = case_when(
        RR > 5 & 
          Delta_Vol > 0 & 
          Vol_Reel_Sonde_Brut > 0 & 
          Vol_Reel_Sonde_Brut < (Pluie_Totale_BV_m3 * 0.90) ~ Vol_Reel_Sonde_Brut,
        TRUE ~ NA_real_
      )
    ) %>%
    drop_na(Vol_Reel_Sonde_Pur)
  
  nb_orages <- nrow(analyse_ruiss)
  
  # Securite : on ne juge pas un modele sur moins de 5 orages
  if (nb_orages < 5) {
    print(paste("Ignorer :", nom_etang, "(Seulement", nb_orages, "orages valides)"))
    next
  }
  
  # Calcul des metriques de performance
  rmse_inrae <- sqrt(mean((analyse_ruiss$Vol_INRAE - analyse_ruiss$Vol_Reel_Sonde_Pur)^2))
  rmse_cn    <- sqrt(mean((analyse_ruiss$Vol_CN - analyse_ruiss$Vol_Reel_Sonde_Pur)^2))
  
  biais_inrae <- mean(analyse_ruiss$Vol_INRAE - analyse_ruiss$Vol_Reel_Sonde_Pur)
  biais_cn    <- mean(analyse_ruiss$Vol_CN - analyse_ruiss$Vol_Reel_Sonde_Pur)
  
  vol_tot_reel  <- sum(analyse_ruiss$Vol_Reel_Sonde_Pur)
  err_vol_inrae <- (sum(analyse_ruiss$Vol_INRAE) - vol_tot_reel) / vol_tot_reel * 100
  err_vol_cn    <- (sum(analyse_ruiss$Vol_CN) - vol_tot_reel) / vol_tot_reel * 100
  
  # Ajout au tableau final
  df_bilan_global <- rbind(df_bilan_global, data.frame(
    Etang = nom_etang,
    Nb_Orages_Analyses = nb_orages,
    RMSE_INRAE = round(rmse_inrae, 0),
    RMSE_CN = round(rmse_cn, 0),
    Biais_INRAE = round(biais_inrae, 0),
    Biais_CN = round(biais_cn, 0),
    Err_Vol_INRAE_pct = round(err_vol_inrae, 1),
    Err_Vol_CN_pct = round(err_vol_cn, 1)
  ))
}

# =========================================================
# 3. AFFICHAGE DU TABLEAU SYNTHETIQUE
# =========================================================
print("======================================================")
print("             BILAN COMPARATIF FINAL                   ")
print("======================================================")
print(df_bilan_global)

# =========================================================
# 4. GENERATION DES GRAPHIQUES POUR LA SOUTENANCE
# =========================================================

# Graphique 1 : Comparaison de la precision (RMSE)
# Le RMSE le plus bas est le meilleur
df_graph_rmse <- df_bilan_global %>%
  select(Etang, RMSE_INRAE, RMSE_CN) %>%
  pivot_longer(cols = c(RMSE_INRAE, RMSE_CN), names_to = "Modele", values_to = "RMSE") %>%
  mutate(Modele = ifelse(Modele == "RMSE_INRAE", "INRAE (Continu)", "SCS-CN (Evenementiel)"))

g1 <- ggplot(df_graph_rmse, aes(x = Etang, y = RMSE, fill = Modele)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("INRAE (Continu)" = "#2980b9", "SCS-CN (Evenementiel)" = "#e74c3c")) +
  theme_minimal() +
  labs(title = "Comparaison de l'Erreur Absolue (RMSE) par Etang",
       subtitle = "Plus la barre est basse, plus le modele est precis",
       y = "RMSE (m3)", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))

print(g1)

# Graphique 2 : Comparaison de l'Erreur Volumetrique Globale (%)
# La valeur la plus proche de 0 est la meilleure
df_graph_vol <- df_bilan_global %>%
  select(Etang, Err_Vol_INRAE_pct, Err_Vol_CN_pct) %>%
  pivot_longer(cols = c(Err_Vol_INRAE_pct, Err_Vol_CN_pct), names_to = "Modele", values_to = "Erreur_Pct") %>%
  mutate(Modele = ifelse(Modele == "Err_Vol_INRAE_pct", "INRAE (Continu)", "SCS-CN (Evenementiel)"))

g2 <- ggplot(df_graph_vol, aes(x = Etang, y = Erreur_Pct, fill = Modele)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = c("INRAE (Continu)" = "#27ae60", "SCS-CN (Evenementiel)" = "#8e44ad")) +
  theme_minimal() +
  labs(title = "Erreur d'estimation du Volume Total Ruissele (%)",
       subtitle = "Plus la barre est proche de 0, meilleur est le bilan de masse",
       y = "Erreur par rapport a la realite (%)", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))

print(g2)
