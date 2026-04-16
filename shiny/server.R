# =======================================================
# SERVEUR (LOGIQUE DE CALCUL)
# =======================================================
server <- function(input, output, session) {
  
  # Stockage des données calculées
  data_res <- reactiveValues(liste_finale = NULL, log = "En attente d'exécution...", pond_names = NULL)
  
  observeEvent(input$run_sim, {
    req(input$file_os, input$file_etg, input$file_assec, input$file_vidange)
    
    # Message immédiat qui bloque l'attente
    id_chargement <- showNotification("Lecture des fichiers en cours, veuillez patienter...", 
                                      duration = NULL, closeButton = FALSE, type = "message")
    
    data_res$log <- "Initialisation des calculs...\n"
    
    tryCatch({
      # -------------------------------------------------------
      # PARTIE 1 : PRÉPARATION (OS, Étangs, Vidanges)
      # -------------------------------------------------------
      
      os_data <- read.csv2(input$file_os$datapath, dec = ".", sep = ",") %>%
        select(ClasseOS = 1, Etang = 2, Surface = 3) %>%
        filter(ClasseOS < 23)
      
      tab_cn <- read.csv2("OS_CN.csv", sep = ";", encoding = "latin1")
      
      os_complet <- os_data %>%
        left_join(tab_cn, by = c("ClasseOS" = "Code_OS"))
      
      cnetg <- os_complet %>%
        group_by(Etang) %>%
        summarise(
          Surface_BV = round(sum(Surface) / 10000, 1),
          CNII = round(sum(CN.sol.D.Fav * Surface) / sum(Surface), 1)
        ) %>%
        mutate(
          CNI   = round(4.2 * CNII / (10 - 0.058 * CNII)),
          CNIII = round(23 * CNII / (10 + 0.13 * CNII))
        )
      
      etg_params <- read.csv2(input$file_etg$datapath, header = TRUE, dec = ",", sep = ";") %>% 
        filter(Chaine_etu == "oui") %>% 
        rename(SURFACE_eau = SURFACE_SI) %>%
        select(-num_range("Assec", 2021:2025))
      
      assec_data <- read.csv2(input$file_assec$datapath, header = TRUE, sep = ";") %>% 
        select(-Exutoire_1, -OBJECTID)
      
      etg_model <- assec_data %>% 
        inner_join(etg_params, by ="NOM") %>%
        mutate(Vmax = SURFACE_eau * Profondeur_m * 10000)
      
      vidange_raw <- read.csv(input$file_vidange$datapath, sep = ",")
      
      tab_etg <- cnetg %>%
        rename(NOM = Etang) %>%
        inner_join(etg_model, by = "NOM") %>% 
        select(-any_of("Vidange")) %>%
        left_join(vidange_raw %>% select(-Exutoire_1, -OBJECTID), by = "NOM") %>%
        mutate(
          jours_vidange = ceiling(SURFACE_eau),
          across(
            .cols = starts_with("peche"),                 
            .fns = ~ as.Date(.x) - jours_vidange,         
            .names = "{gsub('peche', 'Vidange', .col)}"   
          )
        )
      
      # -------------------------------------------------------
      # PARTIE 2 : MÉTÉO SAFRAN 
      # -------------------------------------------------------
      data_res$log <- paste0(data_res$log, "Extraction des données SAFRAN...\n")
      
      fichiers_meteo <- c(
        "meteo/SAFRAN/QUOT_SIM2_2010-2019.CSV", 
        "meteo/SAFRAN/QUOT_SIM2_previous-2020-202602.csv"
      )
      
      coordonnees <- read.csv("meteo/SAFRAN/centro_BV.csv", header = TRUE, sep = ",") %>% 
        filter(CODE == input$bv_code)
      
      X_ref <- coordonnees$LAMBX[1]
      Y_ref <- coordonnees$LAMBY[1]
      
      fenetre <- 40 
      
      meteo_all <- bind_rows(
        open_dataset(fichiers_meteo[1], format = "csv", delimiter = ";") %>%
          filter(LAMBX >= (X_ref - fenetre) & LAMBX <= (X_ref + fenetre) & 
                   LAMBY >= (Y_ref - fenetre) & LAMBY <= (Y_ref + fenetre)) %>% collect(),
        open_dataset(fichiers_meteo[2], format = "csv", delimiter = ";") %>%
          filter(LAMBX >= (X_ref - fenetre) & LAMBX <= (X_ref + fenetre) & 
                   LAMBY >= (Y_ref - fenetre) & LAMBY <= (Y_ref + fenetre)) %>% collect()
      )
      
      maille_proche <- meteo_all %>%
        select(LAMBX, LAMBY) %>% distinct() %>%
        mutate(dist = sqrt((LAMBX - X_ref)^2 + (LAMBY - Y_ref)^2)) %>% 
        arrange(dist) %>% head(1)
      
      pluvio <- meteo_all %>%
        filter(LAMBX == maille_proche$LAMBX & LAMBY == maille_proche$LAMBY) %>%
        rename(RR = PRELIQ) %>%
        mutate(
          dat = as.Date(as.character(DATE), format="%Y%m%d"),
          RR = as.numeric(as.character(RR)),
          ETP_grille = as.numeric(as.character(ETP)),
          P_ETP = RR - ETP_grille
        ) %>%
        filter(between(dat, input$dates[1], input$dates[2])) %>%
        arrange(dat)
      
      # -------------------------------------------------------
      # PARTIE 3 : SIMULATION HYDROLOGIQUE GLOBALE
      # -------------------------------------------------------
      data_res$log <- paste0(data_res$log, "Préparation des tables de calcul...\n")
      
      pluvio_calc <- pluvio %>%
        arrange(dat) %>%
        mutate(
          cumul_5j = rollsum(RR, k = 5, fill = NA, align = "right"),
          Pant = lag(cumul_5j, n = 1, default = 0)
        ) %>%
        mutate(Pant = replace_na(Pant, 0))
      
      table_dates <- tab_etg %>%
        select(NOM, starts_with("Assec"), starts_with("Vidange"), starts_with("peche")) %>%
        pivot_longer(
          cols = -NOM,
          names_to = c(".value", "annee"),
          names_pattern = "([A-Za-z]+)(\\d{4})"
        ) %>%
        arrange(NOM, annee) %>%
        group_by(NOM) %>%
        mutate(Assec_Futur = coalesce(lead(assec), assec)) %>%
        ungroup()
      
      df_bilan <- tab_etg %>%
        select(NOM, Surface_BV, SURFACE_eau, Vmax, CNI, CNII, CNIII, Exutoire_1) %>%
        cross_join(pluvio_calc) %>%
        mutate(annee = format(dat, "%Y")) %>%
        left_join(table_dates, by = c("NOM", "annee")) %>%
        mutate(
          CN_jour = calculer_cn_du_jour(Pant, CNI, CNII, CNIII, dat),
          CR = case_when(
            RR > 0 ~ (ruisselement(RR, CN_jour)) / RR,
            TRUE ~ 0
          ),
          Volume_R = CR * RR * (Surface_BV - SURFACE_eau) * 10,
          VFuite = round(0.1 * 3600 * 24) / 1000,
          Vp_etp = P_ETP * SURFACE_eau * 10,
          Vidange = if_else(dat == as.Date(Vidange), "oui", "non", missing = "non"),
          peche   = if_else(dat == as.Date(peche), "oui", "non", missing = "non"),
          Vamont = 0,
          BF = case_when(
            dat == input$dates[1] & assec == "Evolage" ~ Vmax / 2, 
            TRUE ~ 0
          )
        ) %>% 
        group_by(NOM, annee) %>%
        mutate(
          Statut_Simu = case_when(
            peche == "oui" | Vidange == "oui" ~ "Evolage",
            cumany(peche == "oui") ~ Assec_Futur,
            TRUE ~ assec
          )
        ) %>% 
        ungroup() %>%
        select(-annee, -assec, -Assec_Futur)
      
      liste_etangs <- df_bilan %>% split(.$NOM)
      
      liens <- df_bilan %>% 
        select(NOM, Exutoire_1) %>% 
        filter(!is.na(Exutoire_1) & Exutoire_1 != "OUTPUT") %>%
        distinct()
      
      reseau <- graph_from_data_frame(liens, directed = TRUE)
      ordre_topologique <- names(topo_sort(reseau, mode = "out"))
      
      Volume_Total_Exutoire_BV <- data.frame(
        dat = liste_etangs[[1]]$dat, 
        Volume_Riviere = rep(0, nrow(liste_etangs[[1]])) 
      )
      
      # --- BOUCLE DE CALCUL AVEC BARRE DE PROGRESSION ---
      withProgress(message = 'Simulation hydrologique en cours...', value = 0, {
        
        nombre_total_etangs <- length(ordre_topologique)
        compteur <- 0
        
        for (nom_etang in ordre_topologique) {
          compteur <- compteur + 1
          incProgress(1 / nombre_total_etangs, 
                      detail = paste("Étang :", nom_etang, "(", compteur, "/", nombre_total_etangs, ")"))
          
          etangs_calcule <- liste_etangs[[nom_etang]]
          Stockage_Vamont <- numeric(nrow(etangs_calcule))
          etangs_calcule$Vol_Vidange_Jour <- 0
          
          lignes_vidange <- which(etangs_calcule$Vidange == "oui")
          lignes_peche <- which(etangs_calcule$peche == "oui")
          
          if (length(lignes_vidange) > 0) {
            for (t0 in lignes_vidange) {
              peches_futures <- lignes_peche[lignes_peche > t0]
              if (length(peches_futures) > 0) {
                tfin <- peches_futures[1]
                delta_T <- tfin - t0
                if (delta_T > 0) {
                  Volume_Etang <- etangs_calcule$Vmax[t0]
                  Surface_ha <- etangs_calcule$SURFACE_eau[t0] 
                  Vol_1ha_jour <- Volume_Etang / Surface_ha
                  jours_pour_vider <- ceiling(Surface_ha)
                  jours_effectifs <- min(delta_T, jours_pour_vider)
                  jours_actifs <- t0:(t0 + jours_effectifs - 1)
                  etangs_calcule$Vol_Vidange_Jour[jours_actifs] <- Vol_1ha_jour
                }
              }
            }
          }
          
          for (jour in 2:nrow(etangs_calcule)) {
            statut_du_jour <- etangs_calcule$Statut_Simu[jour]
            
            resultat <- Bfinal(
              Vmax = etangs_calcule$Vmax[jour],
              BF = etangs_calcule$BF[jour-1] ,
              Vp_etp = etangs_calcule$Vp_etp[jour], 
              Volume_R = etangs_calcule$Volume_R[jour],
              Vamont = etangs_calcule$Vamont[jour],
              VFuite = etangs_calcule$VFuite[jour], 
              Statut_Assec = statut_du_jour,
              Volume_Vidange_Jour = etangs_calcule$Vol_Vidange_Jour[jour],
              Peche_Jour = etangs_calcule$peche[jour]
            )
            
            etangs_calcule$BF[jour] = resultat$BF
            Stockage_Vamont[jour] = resultat$Vsortant
          }
          
          liste_etangs[[nom_etang]] <- etangs_calcule
          
          exutoire <- etangs_calcule$Exutoire_1[1]
          if (!is.na(exutoire)) {
            if (exutoire != "OUTPUT") {
              liste_etangs[[exutoire]]$Vamont <- liste_etangs[[exutoire]]$Vamont + Stockage_Vamont
            } else {
              Volume_Total_Exutoire_BV$Volume_Riviere <- Volume_Total_Exutoire_BV$Volume_Riviere + Stockage_Vamont
            }
          }
        }
      })
      
      data_res$liste_finale <- liste_etangs
      data_res$pond_names <- ordre_topologique
      data_res$log <- paste0(data_res$log, "Simulation terminée avec succès.\n")
      
      # On injecte les noms des étangs dans la liste déroulante
      updateSelectInput(session, "etang_choisi", choices = data_res$pond_names)
      
      removeNotification(id_chargement)
      showNotification("Simulation terminée avec succès !", type = "message", duration = 5)
      
    }, error = function(e) {
      removeNotification(id_chargement)
      data_res$log <- paste0(data_res$log, "Erreur lors de l'exécution : ", e$message)
      showNotification("Une erreur s'est produite (voir le journal).", type = "error")
    })
  })
  
  # Affichage dynamique du graphique selon l'étang sélectionné
  output$plot_volume <- renderPlot({
    req(data_res$liste_finale)
    req(input$etang_choisi) 
    
    nom_etang <- input$etang_choisi
    df_plot <- data_res$liste_finale[[nom_etang]]
    
    ggplot(df_plot, aes(x = dat, y = BF)) +
      geom_line(color = "blue", linewidth = 1) +
      theme_minimal() +
      labs(
        title = paste("Volume de l'étang :", nom_etang), 
        x = "Date", 
        y = "Volume stocké (m³)"
      )
  })
  
  output$console_log <- renderText({ data_res$log })
}