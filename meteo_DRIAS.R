# library(tidyverse)
# library(lubridate)
# 
# # 1. Chemin vers ton fichier texte DRIAS téléchargé
# fichier_drias <- "data/meteo/DRIAS_NorESM1_REMO2015.txt"
# 
# # 2. CORRECTION : On utilise read_delim en précisant que le séparateur est un point-virgule (;)
# print("Lecture du fichier brut DRIAS...")
# drias_brut <- read_delim(fichier_drias, delim = ";", comment = "#", col_names = FALSE, show_col_types = FALSE)
# 
# # 3. On force les noms de colonnes
# colnames(drias_brut) <- c("DATE", "LAMBX", "LAMBY", "tasminAdjust", "tasmaxAdjust", "tasAdjust",  "prtotAdjust", "prsnAdjust", "evspsblpotAdjust")
# 
# # 4. Traduction et CORRECTION DES UNITÉS (Vital pour le simulateur)
# meteo_traduite <- drias_brut %>%
#   mutate(
#     DATE = ymd(DATE),               
#     # DRIAS donne des kg/m2/s. On multiplie par 86400 (secondes par jour) pour avoir des mm/jour
#     PRELIQ = as.numeric(prtotAdjust) * 86400,
#     ETP = as.numeric(evspsblpotAdjust) * 86400,
#     Tmoy= as.numeric(tasAdjust) -273.15,
#     Tmax= as.numeric(tasmaxAdjust) -273.15,
#     Tmin= as.numeric(tasminAdjust) -273.15
#     
#   ) %>%
#   # On garde uniquement nos 5 colonnes de travail
#   select(DATE, LAMBX, LAMBY, PRELIQ, ETP,Tmoy,Tmax,Tmin)
# 
# # 5. On exporte le fichier
# chemin_sortie <- "data/meteo/Drias_RCP.csv"
# write.csv2(meteo_traduite, chemin_sortie, row.names = FALSE)
# 
# print(paste("✅ Succès ! Fichier découpé, unités converties en mm/jour, et enregistré ici :", chemin_sortie))
# 
# 
# 
# 
# 
# 
# # Chargement des librairies
# library(tidyverse)
# library(igraph)
# 
# # 1. Chargement dynamique du fichier ASSEC
# chemin_assec_origine <- "data/Chalamont/ASSEC_Final.csv" # Modifie le chemin si besoin
# print(paste("Lecture du fichier d'origine :", chemin_assec_origine))
# 
# df_base <- read.csv(chemin_assec_origine, sep = ";", stringsAsFactors = FALSE)
# 
# # Nettoyage des valeurs vides en "Evolage"
# df_base[is.na(df_base) | df_base == ""] <- "Evolage"
# 
# # 2. Construction de l'arbre topologique (Réseau d'étangs)
# liens <- df_base %>% 
#   select(NOM, Exutoire_1) %>% 
#   filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT" & Exutoire_1 != "")
# g <- graph_from_data_frame(liens, directed = TRUE)
# 
# # Calcul de la "profondeur" de chaque étang dans la cascade
# profondeurs <- distances(g, mode = "in")
# profondeur_max_par_etang <- apply(profondeurs, 1, function(x) max(x[!is.infinite(x)]))
# 
# # 3. Création des dataframes de projection
# df_opt <- df_base
# df_alea <- df_base
# 
# # Boucle temporelle de 2025 à 2050
# annees_futures <- 2025:2050
# set.seed(42)
# 
# for (annee in annees_futures) {
#   nom_col <- paste0("Assec", annee)
#   
#   # --- SCÉNARIO OPTIMISÉ ---
#   assec_opt <- c()
#   for (etang in df_opt$NOM) {
#     if (etang %in% names(profondeur_max_par_etang)) {
#       prof <- profondeur_max_par_etang[etang]
#     } else {
#       prof <- 0 
#     }
#     
#     rotation_id <- prof %% 5
#     annee_cycle <- (annee - 2025) %% 5
#     
#     if (annee_cycle == rotation_id) {
#       assec_opt <- c(assec_opt, "Assec")
#     } else {
#       assec_opt <- c(assec_opt, "Evolage")
#     }
#   }
#   df_opt[[nom_col]] <- assec_opt
#   
#   # --- SCÉNARIO ALÉATOIRE ---
#   df_alea[[nom_col]] <- sample(c("Evolage", "Assec"), size = nrow(df_alea), prob = c(0.8, 0.2), replace = TRUE)
# }
# 
# # 4. NOUVEAU : Nettoyage pour ne garder que 2025 à 2050
# colonnes_a_garder <- c("OBJECTID", "NOM", "Exutoire_1", paste0("Assec", 2025:2050))
# 
# df_opt_final <- df_opt %>% select(all_of(colonnes_a_garder))
# df_alea_final <- df_alea %>% select(all_of(colonnes_a_garder))
# 
# # 5. Sauvegarde des fichiers finaux
# write.table(df_opt_final, "data/Chalamont/ASSEC_Optimise.csv", sep = ";", row.names = FALSE, quote = FALSE)
# write.table(df_alea_final, "data/Chalamont/ASSEC_Aleatoire.csv", sep = ";", row.names = FALSE, quote = FALSE)
# 
# print("✅ Fichiers ASSEC_Optimise.csv et ASSEC_Aleatoire.csv générés avec succès ! (Uniquement de 2025 à 2050)")
# 
# 

library(tidyverse)
library(lubridate)
library(ggplot2)

# =========================================================
# 1. PARAMÉTRAGE STRICT DE L'ÉVÉNEMENT
# =========================================================
nom_etang <- "ROUE RONZUEL" 
date_debut_orage <- as.Date("2025-09-20")
date_fin_orage   <- as.Date("2025-09-25") 

# Chargement des simulations
chemin_simu_inrae <- "Analyse R3_RU_175_Coef_0.35_20260611.rds"
chemin_simu_cn    <- "archive simul/L0.2_Pant5_CN_base_20260519.rds"

simu_inrae <- readRDS(chemin_simu_inrae)
simu_cn    <- readRDS(chemin_simu_cn)

# =========================================================
# 2. EXTRACTION ET BILAN DE MASSE ROBUSTE
# =========================================================
infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
surface_terre_ha <- infos_etg$Surface_BV - infos_etg$SURFACE_eau

# 1. Extraction pure sans calculs croisés
df_inrae <- simu_inrae$liste_finale[[nom_etang]] %>% select(dat, RR, Vol_INRAE = Volume_R)
df_cn    <- simu_cn$liste_finale[[nom_etang]] %>% select(dat, Vol_CN = Volume_R)

df_sonde <- load_terrain(nom_etang) %>%
  select(dat, Volume_Reel) %>%
  mutate(Delta_Vol = Volume_Reel - lag(Volume_Reel, 1))

# 2. Fusion, Filtrage, PUIS Calculs
analyse_event <- df_inrae %>%
  left_join(df_cn, by = "dat") %>%
  left_join(df_sonde, by = "dat") %>%
  filter(dat >= date_debut_orage & dat <= date_fin_orage) %>%
  mutate(
    Pluie_Directe_m3 = RR * infos_etg$SURFACE_eau * 10,
    Ruissellement_Sonde_Brut = Delta_Vol - Pluie_Directe_m3,
    Ruissellement_Sonde_Brut = ifelse(Ruissellement_Sonde_Brut < 0, 0, Ruissellement_Sonde_Brut),
    Ruissellement_Sonde_Brut = replace_na(Ruissellement_Sonde_Brut, 0)
  )

# =========================================================
# 3. CALCUL DES MÉTRIQUES (RMSE ET BIAIS)
# =========================================================
vol_tot_pluie <- sum(analyse_event$RR * surface_terre_ha * 10, na.rm = TRUE)
vol_tot_inrae <- sum(analyse_event$Vol_INRAE, na.rm = TRUE)
vol_tot_cn    <- sum(analyse_event$Vol_CN, na.rm = TRUE)
vol_tot_reel  <- sum(analyse_event$Ruissellement_Sonde_Brut, na.rm = TRUE)

rmse_inrae <- sqrt(mean((analyse_event$Vol_INRAE - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))
rmse_cn    <- sqrt(mean((analyse_event$Vol_CN - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))

print("======================================================")
print(paste("BILAN HYDRIQUE :", nom_etang, "du", date_debut_orage, "au", date_fin_orage))
print("======================================================")
print(paste("Pluie tombée sur le BV :", round(vol_tot_pluie, 0), "m3"))
print(paste("VÉRITÉ TERRAIN (Sonde stable) :", round(vol_tot_reel, 0), "m3"))
print("------------------------------------------------------")
print(paste("Simul INRAE :", round(vol_tot_inrae, 0), "m3 ( Biais :", round(vol_tot_inrae-vol_tot_reel, 0), "m3 | RMSE :", round(rmse_inrae, 0), "m3 )"))
print(paste("Simul SCS-CN :", round(vol_tot_cn, 0), "m3 ( Biais :", round(vol_tot_cn-vol_tot_reel, 0), "m3 | RMSE :", round(rmse_cn, 0), "m3 )"))

# =========================================================
# 4. GÉNÉRATION DE L'HYDROGRAMME
# =========================================================
df_graph <- analyse_event %>%
  select(dat, Verite_Terrain = Ruissellement_Sonde_Brut, INRAE = Vol_INRAE, SCS_CN = Vol_CN) %>%
  pivot_longer(cols = c(Verite_Terrain, INRAE, SCS_CN), names_to = "Modele", values_to = "Volume_m3")

g_event <- ggplot() +
  geom_bar(data = analyse_event, aes(x = dat, y = RR * (max(c(analyse_event$Vol_INRAE, analyse_event$Ruissellement_Sonde_Brut), na.rm=TRUE) / max(analyse_event$RR, na.rm=TRUE))), 
           stat = "identity", fill = "#3498db", alpha = 0.4) +
  geom_line(data = df_graph, aes(x = dat, y = Volume_m3, color = Modele, linetype = Modele), size = 1.2) +
  scale_color_manual(values = c("Verite_Terrain" = "black", "INRAE" = "#27ae60", "SCS_CN" = "#e74c3c")) +
  scale_linetype_manual(values = c("Verite_Terrain" = "solid", "INRAE" = "dashed", "SCS_CN" = "dotted")) +
  theme_minimal() +
  labs(
    title = paste("Hydrogramme -", nom_etang),
    subtitle = "Ruissellement observé vs Modélisé",
    x = "Date",
    y = "Volume de ruissellement (m3)"
  ) +
  theme(legend.position = "bottom")

print(g_event)

# =========================================================
# 5. SAUVEGARDE AUTOMATIQUE 
# =========================================================
ligne_resultat <- data.frame(
  Date_Analyse = format(Sys.time(), "%Y-%m-%d %H:%M"),
  hypothese = "0.35/175/10",
  Etang = nom_etang,
  Date_Debut_Orage = date_debut_orage,
  Date_Fin_Orage = date_fin_orage,
  Pluie_Total_BV_m3 = round(vol_tot_pluie, 0),
  Ruissellement_Reel_Sonde_m3 = round(vol_tot_reel, 0),
  Volume_Calcule_INRAE_m3 = round(vol_tot_inrae, 0),
  Volume_Calcule_CN_m3 = round(vol_tot_cn, 0),
  Biais_Global_INRAE = round(vol_tot_inrae - vol_tot_reel, 0),
  Biais_Global_CN = round(vol_tot_cn - vol_tot_reel, 0),
  RMSE_INRAE_m3 = round(rmse_inrae, 0),
  RMSE_CN_m3 = round(rmse_cn, 0)
)


# Nom du fichier de registre global
fichier_registre <- paste0("Registre_Analyses_", nom_etang , ".csv")

# Logique d'écriture intelligente : 
# Si le fichier existe, on ajoute la ligne en dessous (append). Sinon, on le crée.
if (file.exists(fichier_registre)) {
  write.table(ligne_resultat, file = fichier_registre, sep = ";", 
              col.names = FALSE, row.names = FALSE, append = TRUE, dec = ",")
  print(paste("✅ Résultats ajoutés avec succès au fichier :", fichier_registre))
} else {
  write.table(ligne_resultat, file = fichier_registre, sep = ";", 
              col.names = TRUE, row.names = FALSE, dec = ",")
  print(paste("✅ Nouveau registre créé :", fichier_registre))
}




# ==============================================================================
# SCRIPT D'OPTIMISATION AUTOMATIQUE DES PARAMÈTRES INRAE
# Objectif : Tester toutes les combinaisons pour minimiser le RMSE
# ==============================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)

# Note : Le script importation.R doit avoir été exécuté en amont.
# Assure-toi que la fonction run_hydrological_model() est bien chargée dans ton environnement.

# =========================================================
# 1. PARAMÉTRAGE DE L'ÉVÉNEMENT ET DES BOUCLES
# =========================================================
nom_etang <- "ROUE RONZUEL" 
date_debut_orage <- as.Date("2025-09-20")
date_fin_orage   <- as.Date("2025-09-25") 

# Définition des plages de paramètres à tester
valeurs_beta <- 1:5
valeurs_ru <- c(150, 175, 200, 225, 250)
valeurs_coef <- c(0.2, 0.3, 0.35, 0.4, 0.5)

# Fichier de référence SCS-CN (constant)
chemin_simu_cn <- "archive simul/L0.2_Pant5_CN_base_20260519.rds"
simu_cn <- readRDS(chemin_simu_cn)

# Nom du fichier de registre (spécifique à l'optimisation)
fichier_registre <- paste0("Optimisation_Parametres_", nom_etang , ".csv")

print("Démarrage de la boucle d'optimisation... Cela peut prendre du temps.")

# =========================================================
# 2. LA BOUCLE D'OPTIMISATION (Force Brute)
# =========================================================

for (beta in valeurs_beta) {
  for (ru in valeurs_ru) {
    for (coef in valeurs_coef) {
      
      # -----------------------------------------------------
      # Étape A : Exécution de la simulation avec les paramètres courants
      # -----------------------------------------------------
      cat(sprintf("\n--- Test en cours : Beta=%d | RU=%d | Coef=%.2f ---\n", beta, ru, coef))
      
      simu_inrae <- run_hydrological_model(
        pluvio_data = pluvio_base,
        tab_etg_data = tab_etg_base,  
        RU_defaut = ru, 
        beta_val = beta,     
        C_transfert = coef
      )
      
      # -----------------------------------------------------
      # Étape B : Extraction et Bilan de Masse sur l'événement
      # -----------------------------------------------------
      infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
      surface_terre_ha <- infos_etg$Surface_BV - infos_etg$SURFACE_eau
      
      df_inrae <- simu_inrae$liste_finale[[nom_etang]] %>% select(dat, RR, Vol_INRAE = Volume_R)
      df_cn    <- simu_cn$liste_finale[[nom_etang]] %>% select(dat, Vol_CN = Volume_R)
      
      df_sonde <- load_terrain(nom_etang) %>%
        select(dat, Volume_Reel) %>%
        mutate(Delta_Vol = Volume_Reel - lag(Volume_Reel, 1))
      
      analyse_event <- df_inrae %>%
        left_join(df_cn, by = "dat") %>%
        left_join(df_sonde, by = "dat") %>%
        filter(dat >= date_debut_orage & dat <= date_fin_orage) %>%
        mutate(
          Pluie_Directe_m3 = RR * infos_etg$SURFACE_eau * 10,
          Ruissellement_Sonde_Brut = Delta_Vol - Pluie_Directe_m3,
          Ruissellement_Sonde_Brut = ifelse(Ruissellement_Sonde_Brut < 0, 0, Ruissellement_Sonde_Brut),
          Ruissellement_Sonde_Brut = replace_na(Ruissellement_Sonde_Brut, 0)
        )
      
      # -----------------------------------------------------
      # Étape C : Calcul des métriques
      # -----------------------------------------------------
      vol_tot_pluie <- sum(analyse_event$RR * surface_terre_ha * 10, na.rm = TRUE)
      vol_tot_inrae <- sum(analyse_event$Vol_INRAE, na.rm = TRUE)
      vol_tot_cn    <- sum(analyse_event$Vol_CN, na.rm = TRUE)
      vol_tot_reel  <- sum(analyse_event$Ruissellement_Sonde_Brut, na.rm = TRUE)
      
      rmse_inrae <- sqrt(mean((analyse_event$Vol_INRAE - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))
      rmse_cn    <- sqrt(mean((analyse_event$Vol_CN - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))
      
      # Création de la chaîne d'hypothèse dynamique
      hypo_str <- paste0(coef, "/", ru, "/", beta)
      
      # -----------------------------------------------------
      # Étape D : Écriture dans le fichier CSV
      # -----------------------------------------------------
      ligne_resultat <- data.frame(
        Date_Analyse = format(Sys.time(), "%Y-%m-%d %H:%M"),
        hypothese = hypo_str,
        Etang = nom_etang,
        Date_Debut_Orage = date_debut_orage,
        Date_Fin_Orage = date_fin_orage,
        Pluie_Total_BV_m3 = round(vol_tot_pluie, 0),
        Ruissellement_Reel_Sonde_m3 = round(vol_tot_reel, 0),
        Volume_Calcule_INRAE_m3 = round(vol_tot_inrae, 0),
        Volume_Calcule_CN_m3 = round(vol_tot_cn, 0),
        Biais_Global_INRAE = round(vol_tot_inrae - vol_tot_reel, 0),
        Biais_Global_CN = round(vol_tot_cn - vol_tot_reel, 0),
        RMSE_INRAE_m3 = round(rmse_inrae, 0),
        RMSE_CN_m3 = round(rmse_cn, 0)
      )
      
      if (file.exists(fichier_registre)) {
        write.table(ligne_resultat, file = fichier_registre, sep = ";", 
                    col.names = FALSE, row.names = FALSE, append = TRUE, dec = ",")
      } else {
        write.table(ligne_resultat, file = fichier_registre, sep = ";", 
                    col.names = TRUE, row.names = FALSE, dec = ",")
      }
      
    } # Fin boucle Coef
  } # Fin boucle RU
} # Fin boucle Beta

print(paste("✅ Optimisation terminée ! Le registre final est disponible :", fichier_registre))








# ==============================================================================
# SCRIPT DE GÉNÉRATION MASSIVE DES SCÉNARIOS GLOBAUX (Data Lake)
# Objectif : Calculer et sauvegarder TOUTES les combinaisons en .rds
# ==============================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)

# Note : Le script importation.R et la fonction run_hydrological_model() 
# doivent être chargés dans l'environnement.

# =========================================================
# 1. PARAMÉTRAGE DES BOUCLES (125 combinaisons)
# =========================================================
valeurs_beta <- 1:5
valeurs_ru <- c(150, 175, 200, 225, 250)
valeurs_coef <- c(0.2, 0.3, 0.35, 0.4, 0.5)

# Création du dossier de stockage pour ne pas polluer ton espace
dossier_sauvegarde <- "Banque_Simulations_Globales"
if (!dir.exists(dossier_sauvegarde)) {
  dir.create(dossier_sauvegarde)
  cat(paste("\n📁 Dossier créé :", dossier_sauvegarde, "\n"))
}

print("Démarrage de la production massive des fichiers .rds...")

# =========================================================
# 2. LA BOUCLE DE PRODUCTION PURE (Zéro analyse)
# =========================================================

compteur <- 1
total_simu <- length(valeurs_beta) * length(valeurs_ru) * length(valeurs_coef)

for (beta in valeurs_beta) {
  for (ru in valeurs_ru) {
    for (coef in valeurs_coef) {
      
      cat(sprintf("\n[%d/%d] Calcul en cours : Beta=%d | RU=%d | Coef=%.2f...\n", 
                  compteur, total_simu, beta, ru, coef))
      
      # 1. Exécution du modèle pour TOUTE la chaîne et TOUTES les dates
      simu_globale <- run_hydrological_model(
        pluvio_data = pluvio_base,
        tab_etg_data = tab_etg_base,  
        RU_defaut = ru, 
        beta_val = beta,     
        C_transfert = coef
      )
      
      # 2. Création du nom de fichier propre
      nom_fichier_rds <- sprintf("%s/Simu_INRAE_Beta%d_RU%d_Coef%.2f.rds", 
                                 dossier_sauvegarde, beta, ru, coef)
      
      # 3. Sauvegarde immédiate sur le disque
      saveRDS(simu_globale, file = nom_fichier_rds)
      cat(paste("✅ Sauvegardé sous :", nom_fichier_rds, "\n"))
      
      compteur <- compteur + 1
      
    } # Fin boucle Coef
  } # Fin boucle RU
} # Fin boucle Beta

print("=======================================================================")
print(paste("🚀 PRODUCTION TERMINÉE ! Tes", total_simu, "fichiers .rds t'attendent dans le dossier :", dossier_sauvegarde))
print("=======================================================================")







# ==============================================================================
# SCRIPT DE GÉNÉRATION MASSIVE EN PARALLÈLE (Calcul Multicœur)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(igraph)
library(doParallel) # <-- Le package magique pour activer les cœurs de ton PC
library(foreach)    # <-- Le package pour remplacer les boucles for classiques

# Note : Le script importation.R et tes fonctions (run_hydrological_model, Bfinal) 
# doivent être chargés dans ton environnement avant de lancer ça.

# =========================================================
# 1. PARAMÉTRAGE DES SCÉNARIOS
# =========================================================
valeurs_beta <- 1:5
valeurs_ru <- c(150, 175, 200, 225, 250)
valeurs_coef <- c(0.2, 0.3, 0.35, 0.4, 0.5)

dossier_sauvegarde <- "Banque_Simulations_Globales"
if (!dir.exists(dossier_sauvegarde)) {
  dir.create(dossier_sauvegarde)
}

# On crée un tableau qui contient toutes les combinaisons possibles (125 lignes)
grille_parametres <- expand.grid(beta = valeurs_beta, ru = valeurs_ru, coef = valeurs_coef)

# =========================================================
# 2. ALLUMAGE DES MOTEURS (Préparation du Parallélisme)
# =========================================================

nb_coeurs_total <- detectCores()
nb_coeurs_alloues <- max(1, nb_coeurs_total - 2) 

print(paste("Démarrage du calcul multicœur sur", nb_coeurs_alloues, "cœurs simultanément !"))

# L'astuce est ici : outfile = "" force les cœurs virtuels à écrire dans ta console
cl <- makeCluster(nb_coeurs_alloues, outfile = "")
registerDoParallel(cl)

# =========================================================
# 3. LA BOUCLE PARALLÈLE (%dopar%)
# =========================================================

foreach(i = 1:nrow(grille_parametres), 
        .packages = c("tidyverse", "lubridate", "zoo", "igraph"),
        .export = c("run_hydrological_model", "Bfinal", "pluvio_base", "tab_etg_base", "dossier_sauvegarde")) %dopar% {
          
          beta_actuel <- grille_parametres$beta[i]
          ru_actuel <- grille_parametres$ru[i]
          coef_actuel <- grille_parametres$coef[i]
          
          # --- LE MESSAGE DE SUIVI ---
          # Chaque cœur va imprimer ce message dans ta console quand il commence un calcul
          cat(sprintf("[%s] Calcul en cours : Beta=%d | RU=%d | Coef=%.2f\n", 
                      format(Sys.time(), "%H:%M:%S"), beta_actuel, ru_actuel, coef_actuel))
          
          simu_globale <- run_hydrological_model(
            pluvio_data = pluvio_base,
            tab_etg_data = tab_etg_base,  
            RU_defaut = ru_actuel, 
            beta_val = beta_actuel,     
            C_transfert = coef_actuel
          )
          
          nom_fichier_rds <- sprintf("%s/Simu_INRAE_Beta%d_RU%d_Coef%.2f.rds", 
                                     dossier_sauvegarde, beta_actuel, ru_actuel, coef_actuel)
          
          saveRDS(simu_globale, file = nom_fichier_rds)
          
          # Message de confirmation de sauvegarde
          cat(sprintf("   -> Terminé et sauvegardé : Beta=%d | RU=%d | Coef=%.2f\n", 
                      beta_actuel, ru_actuel, coef_actuel))
        }

# =========================================================
# 4. EXTINCTION DES MOTEURS
# =========================================================
# Il est obligatoire d'arrêter le cluster à la fin pour libérer la mémoire de ton PC
stopCluster(cl)

print("=======================================================================")
print("🚀 PRODUCTION PARALLÈLE TERMINÉE AVEC SUCCÈS !")
print("=======================================================================")






# ==============================================================================
# SCRIPT D'ÉVALUATION DES 125 MODÈLES SUR UN ÉVÉNEMENT CIBLE
# Objectif : Créer un registre CSV spécifique à un étang et une date
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr) # Très utile pour extraire du texte des noms de fichiers

# =========================================================
# 1. PARAMÉTRAGE STRICT DE L'ÉVÉNEMENT (À modifier à chaque fois)
# =========================================================
nom_etang <- "REMONDET NORD" 
date_debut_orage <- as.Date("2023-10-18")
date_fin_orage   <- as.Date("2023-11-30") 

dossier_rds <- "Banque_Simulations_Globales" # Le dossier contenant tes 125 fichiers

# Nom du fichier CSV final généré automatiquement
nom_csv_sortie <- paste0("Analyse_", gsub(" ", "_", nom_etang), "_", date_debut_orage, ".csv")

print(paste("Démarrage de l'analyse... Génération prévue de :", nom_csv_sortie))

# =========================================================
# 2. PRÉPARATION DES DONNÉES COMMUNES (Pour gagner du temps)
# =========================================================
# On charge SCS-CN une seule fois
chemin_simu_cn <- "archive simul/L0.2_Pant5_CN_base_20260519.rds"
simu_cn <- readRDS(chemin_simu_cn)
df_cn <- simu_cn$liste_finale[[nom_etang]] %>% select(dat, Vol_CN = Volume_R)

# On prépare les infos de l'étang et la sonde une seule fois
infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
surface_terre_ha <- infos_etg$Surface_BV - infos_etg$SURFACE_eau

df_sonde <- load_terrain(nom_etang) %>%
  select(dat, Volume_Reel) %>%
  mutate(Delta_Vol = Volume_Reel - lag(Volume_Reel, 1))

# Liste qui va stocker les 125 résultats
resultats_event <- list()

# On récupère la liste exacte de tes 125 fichiers .rds
liste_fichiers <- list.files(path = dossier_rds, pattern = "\\.rds$", full.names = TRUE)

# =========================================================
# 3. LA BOUCLE D'ANALYSE SUR LES 125 SCÉNARIOS
# =========================================================
compteur <- 1

for (fichier in liste_fichiers) {
  
  # A. Extraction des paramètres depuis le nom du fichier
  # LA CORRECTION : On efface le ".rds" à la fin du nom pour qu'il ne pollue pas la recherche
  nom_base <- str_remove(basename(fichier), "\\.rds$") 
  
  beta_val <- str_extract(nom_base, "(?<=Beta)\\d+")
  ru_val   <- str_extract(nom_base, "(?<=RU)\\d+")
  coef_val <- str_extract(nom_base, "(?<=Coef)[0-9.]+")
  hypo_str <- paste0(coef_val, "/", ru_val, "/", beta_val)
  
  cat(sprintf("\rAnalyse du fichier %d/%d : %s", compteur, length(liste_fichiers), hypo_str))
  
  # B. Chargement du modèle INRAE
  simu_inrae <- readRDS(fichier)
  df_inrae <- simu_inrae$liste_finale[[nom_etang]] %>% select(dat, RR, Vol_INRAE = Volume_R)
  
  # C. Bilan de masse
  analyse_event <- df_inrae %>%
    left_join(df_cn, by = "dat") %>%
    left_join(df_sonde, by = "dat") %>%
    filter(dat >= date_debut_orage & dat <= date_fin_orage) %>%
    mutate(
      Pluie_Directe_m3 = RR * infos_etg$SURFACE_eau * 10,
      Ruissellement_Sonde_Brut = Delta_Vol - Pluie_Directe_m3,
      Ruissellement_Sonde_Brut = ifelse(Ruissellement_Sonde_Brut < 0, 0, Ruissellement_Sonde_Brut),
      Ruissellement_Sonde_Brut = replace_na(Ruissellement_Sonde_Brut, 0)
    )
  
  # D. Calcul des métriques
  vol_tot_pluie <- sum(analyse_event$RR * surface_terre_ha * 10, na.rm = TRUE)
  vol_tot_inrae <- sum(analyse_event$Vol_INRAE, na.rm = TRUE)
  vol_tot_cn    <- sum(analyse_event$Vol_CN, na.rm = TRUE)
  vol_tot_reel  <- sum(analyse_event$Ruissellement_Sonde_Brut, na.rm = TRUE)
  
  rmse_inrae <- sqrt(mean((analyse_event$Vol_INRAE - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))
  rmse_cn    <- sqrt(mean((analyse_event$Vol_CN - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))
  
  # E. Enregistrement de la ligne
  resultats_event[[compteur]] <- data.frame(
    Date_Analyse = format(Sys.time(), "%Y-%m-%d %H:%M"),
    hypothese = hypo_str,
    Etang = nom_etang,
    Date_Debut_Orage = date_debut_orage,
    Date_Fin_Orage = date_fin_orage,
    Pluie_Total_BV_m3 = round(vol_tot_pluie, 0),
    Ruissellement_Reel_Sonde_m3 = round(vol_tot_reel, 0),
    Volume_Calcule_INRAE_m3 = round(vol_tot_inrae, 0),
    Volume_Calcule_CN_m3 = round(vol_tot_cn, 0),
    Biais_Global_INRAE = round(vol_tot_inrae - vol_tot_reel, 0),
    Biais_Global_CN = round(vol_tot_cn - vol_tot_reel, 0),
    RMSE_INRAE_m3 = round(rmse_inrae, 0),
    RMSE_CN_m3 = round(rmse_cn, 0)
  )
  
  compteur <- compteur + 1
}

# =========================================================
# 4. EXPORTATION FINALE DU REGISTRE CSV
# =========================================================
# On fusionne les 125 lignes en un seul grand tableau
df_final <- bind_rows(resultats_event)

# On écrit le CSV d'un seul coup (beaucoup plus rapide et propre)
write.table(df_final, file = nom_csv_sortie, sep = ";", row.names = FALSE, dec = ",")

cat("\n\n======================================================\n")
print(paste("✅ SUCCÈS ! Les 125 modèles ont été analysés pour l'événement."))
print(paste("Fichier sauvegardé :", nom_csv_sortie))
cat("======================================================\n")





# ==============================================================================
# SCRIPT D'ÉVALUATION DES 125 MODÈLES SUR UN ÉVÉNEMENT CIBLE
# Objectif : Créer un registre CSV spécifique à un étang et une date
# ==============================================================================

library(tidyverse)
library(lubridate)
library(stringr) # Très utile pour extraire du texte des noms de fichiers

# =========================================================
# 1. PARAMÉTRAGE STRICT DE L'ÉVÉNEMENT (À modifier à chaque fois)
# =========================================================
nom_etang <- "REMONDET NORD" 
date_debut_orage <- as.Date("2025-10-29")
date_fin_orage   <- as.Date("2025-11-05") 

dossier_rds <- "Banque_Simulations_Globales" # Le dossier contenant tes 125 fichiers

# Nom du fichier CSV final généré automatiquement
nom_csv_sortie <- paste0("Analyse_", gsub(" ", "_", nom_etang), "_", date_debut_orage, ".csv")

print(paste("Démarrage de l'analyse... Génération prévue de :", nom_csv_sortie))

# =========================================================
# 2. PRÉPARATION DES DONNÉES COMMUNES 
# =========================================================
chemin_simu_cn <- "archive simul/L0.2_Pant5_CN_base_20260519.rds"
simu_cn <- readRDS(chemin_simu_cn)

# CORRECTION : On additionne Ruissellement Local + Apport Amont
df_cn <- simu_cn$liste_finale[[nom_etang]] %>% 
  mutate(Vol_CN = Volume_R + Vamont) %>% 
  select(dat, Vol_CN)

infos_etg <- tab_etg_base %>% filter(NOM == nom_etang) %>% head(1)
surface_terre_ha <- infos_etg$Surface_BV - infos_etg$SURFACE_eau

df_sonde <- load_terrain(nom_etang) %>%
  select(dat, Volume_Reel) %>%
  mutate(Delta_Vol = Volume_Reel - lag(Volume_Reel, 1))

# Liste qui va stocker les 125 résultats
resultats_event <- list()

# On récupère la liste exacte de tes 125 fichiers .rds
liste_fichiers <- list.files(path = dossier_rds, pattern = "\\.rds$", full.names = TRUE)

# =========================================================
# 3. LA BOUCLE D'ANALYSE SUR LES 125 SCÉNARIOS
# =========================================================
compteur <- 1

for (fichier in liste_fichiers) {
  
  # A. Extraction des paramètres depuis le nom du fichier
  # LA CORRECTION : On efface le ".rds" à la fin du nom pour qu'il ne pollue pas la recherche
  nom_base <- str_remove(basename(fichier), "\\.rds$") 
  
  beta_val <- str_extract(nom_base, "(?<=Beta)\\d+")
  ru_val   <- str_extract(nom_base, "(?<=RU)\\d+")
  coef_val <- str_extract(nom_base, "(?<=Coef)[0-9.]+")
  hypo_str <- paste0(coef_val, "/", ru_val, "/", beta_val)
  
  cat(sprintf("\rAnalyse du fichier %d/%d : %s", compteur, length(liste_fichiers), hypo_str))
  # B. Chargement du modèle INRAE
  simu_inrae <- readRDS(fichier)
  
  # CORRECTION : La variable Vol_INRAE devient la somme du ruissellement et de la cascade
  df_inrae <- simu_inrae$liste_finale[[nom_etang]] %>% 
    mutate(Vol_INRAE = Volume_R + Vamont) %>% 
    select(dat, RR, Vol_INRAE)
  
  # C. Bilan de masse
  analyse_event <- df_inrae %>%
    left_join(df_cn, by = "dat") %>%
    left_join(df_sonde, by = "dat") %>%
    filter(dat >= date_debut_orage & dat <= date_fin_orage) %>%
    mutate(
      Pluie_Directe_m3 = RR * infos_etg$SURFACE_eau * 10,
      Ruissellement_Sonde_Brut = Delta_Vol - Pluie_Directe_m3,
      Ruissellement_Sonde_Brut = ifelse(Ruissellement_Sonde_Brut < 0, 0, Ruissellement_Sonde_Brut),
      Ruissellement_Sonde_Brut = replace_na(Ruissellement_Sonde_Brut, 0)
    )
  
  # D. Calcul des métriques
  vol_tot_pluie <- sum(analyse_event$RR * surface_terre_ha * 10, na.rm = TRUE)
  vol_tot_inrae <- sum(analyse_event$Vol_INRAE, na.rm = TRUE)
  vol_tot_cn    <- sum(analyse_event$Vol_CN, na.rm = TRUE)
  vol_tot_reel  <- sum(analyse_event$Ruissellement_Sonde_Brut, na.rm = TRUE)
  
  rmse_inrae <- sqrt(mean((analyse_event$Vol_INRAE - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))
  rmse_cn    <- sqrt(mean((analyse_event$Vol_CN - analyse_event$Ruissellement_Sonde_Brut)^2, na.rm = TRUE))
  
  # E. Enregistrement de la ligne
  resultats_event[[compteur]] <- data.frame(
    Date_Analyse = format(Sys.time(), "%Y-%m-%d %H:%M"),
    hypothese = hypo_str,
    Etang = nom_etang,
    Date_Debut_Orage = date_debut_orage,
    Date_Fin_Orage = date_fin_orage,
    Pluie_Total_BV_m3 = round(vol_tot_pluie, 0),
    Ruissellement_Reel_Sonde_m3 = round(vol_tot_reel, 0),
    Volume_Calcule_INRAE_m3 = round(vol_tot_inrae, 0),
    Volume_Calcule_CN_m3 = round(vol_tot_cn, 0),
    Biais_Global_INRAE = round(vol_tot_inrae - vol_tot_reel, 0),
    Biais_Global_CN = round(vol_tot_cn - vol_tot_reel, 0),
    RMSE_INRAE_m3 = round(rmse_inrae, 0),
    RMSE_CN_m3 = round(rmse_cn, 0)
  )
  
  compteur <- compteur + 1
}

# =========================================================
# 4. EXPORTATION FINALE DU REGISTRE CSV
# =========================================================
# On fusionne les 125 lignes en un seul grand tableau
df_final <- bind_rows(resultats_event)

# On écrit le CSV d'un seul coup (beaucoup plus rapide et propre)
write.table(df_final, file = nom_csv_sortie, sep = ";", row.names = FALSE, dec = ",")

cat("\n\n======================================================\n")
print(paste("✅ SUCCÈS ! Les 125 modèles ont été analysés pour l'événement."))
print(paste("Fichier sauvegardé :", nom_csv_sortie))
cat("======================================================\n")










