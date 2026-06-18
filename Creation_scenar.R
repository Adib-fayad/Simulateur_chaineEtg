# ==============================================================================
# ETAPE 1 : CALENDRIER OPTIMISE (ZERO PERTE D'EAU)
# ==============================================================================

library(tidyverse)
library(lubridate)

# PARAMETRES A MODIFIER POUR CHAQUE NOUVEAU BASSIN
fichier_entree <- "Ordre_BV.csv"
fichier_sortie <- "Vidange_Peche_Opti_BV.csv"

# 1. Chargement et nettoyage
df_base <- read.csv(fichier_entree, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
colnames(df_base)[1:3] <- c("NOM", "Duree", "Exutoire_1")

df_base <- df_base %>% filter(NOM != "" & !is.na(NOM))
df_base$NOM <- trimws(toupper(df_base$NOM))
df_base$Exutoire_1 <- trimws(toupper(df_base$Exutoire_1))
df_base$Duree <- as.numeric(df_base$Duree)

# 2. Initialisation des décalages
exutoires_finaux <- setdiff(df_base$Exutoire_1, df_base$NOM)

df_base$Offset_Vidange <- NA
df_base$Offset_Peche <- NA

df_base <- df_base %>%
  mutate(
    Offset_Vidange = ifelse(Exutoire_1 %in% exutoires_finaux, 0, NA),
    Offset_Peche = ifelse(Exutoire_1 %in% exutoires_finaux, Duree, NA)
  )

# 3. Calcul de la cascade amont-aval
while(any(is.na(df_base$Offset_Vidange))) {
  for (i in 1:nrow(df_base)) {
    if (is.na(df_base$Offset_Vidange[i])) {
      exutoire_cible <- df_base$Exutoire_1[i]
      index_aval <- which(df_base$NOM == exutoire_cible)
      
      if (length(index_aval) > 0 && !is.na(df_base$Offset_Peche[index_aval])) {
        df_base$Offset_Vidange[i] <- df_base$Offset_Peche[index_aval]
        df_base$Offset_Peche[i] <- df_base$Offset_Vidange[i] + df_base$Duree[i]
      }
    }
  }
}

# 4. Generation du calendrier 2026-2070
annees_sim <- 2026:2070
df_final <- df_base %>% select(NOM, Exutoire_1)

for (annee in annees_sim) {
  date_depart <- as.Date(paste0(annee, "-10-15"))
  col_v <- paste0("Vidange", annee)
  col_p <- paste0("peche", annee)
  
  df_final[[col_v]] <- format(date_depart + df_base$Offset_Vidange, "%Y-%m-%d")
  df_final[[col_p]] <- format(date_depart + df_base$Offset_Peche, "%Y-%m-%d")
}

write.csv2(df_final, fichier_sortie, row.names = FALSE, quote = FALSE)
print(paste("Etape 1 terminee : Fichier", fichier_sortie, "genere."))






# ==============================================================================
# ETAPE 2 : MATRICE DES ASSECS OPTIMISES (SYNCHRONISATION AVAL-AMONT)
# ==============================================================================

library(tidyverse)

# PARAMETRES A MODIFIER
fichier_entree <- "Ordre_BV.csv"
fichier_sortie <- "ASSEC_Opti_BV.csv"

# 1. Chargement et nettoyage
df_base <- read.csv(fichier_entree, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
colnames(df_base)[1:3] <- c("NOM", "Duree", "Exutoire_1")
df_base <- df_base %>% filter(NOM != "" & !is.na(NOM))
df_base$NOM <- trimws(toupper(df_base$NOM))
df_base$Exutoire_1 <- trimws(toupper(df_base$Exutoire_1))

# 2. Hierarchisation topologique
df_base$Niveau <- NA
exutoires_finaux <- setdiff(df_base$Exutoire_1, df_base$NOM) 
df_base$Niveau[df_base$Exutoire_1 %in% exutoires_finaux] <- 1

niveau_actuel <- 1
while(any(is.na(df_base$Niveau))) {
  etangs_n <- df_base$NOM[df_base$Niveau == niveau_actuel]
  df_base$Niveau[df_base$Exutoire_1 %in% etangs_n] <- niveau_actuel + 1
  niveau_actuel <- niveau_actuel + 1
}

df_base <- df_base %>% arrange(Niveau)

# 3. Parametrage de la simulation
etangs <- df_base$NOM
n_etangs <- length(etangs)
annees_sim <- 2026:2070

set.seed(42) 
df_base$Compteur_Eau <- sample(0:3, n_etangs, replace = TRUE)
matrice_assec <- data.frame(NOM = etangs)

# 4. Moteur de calcul
for (annee in annees_sim) {
  assec_annee <- logical(n_etangs)
  
  for (i in 1:nrow(df_base)) {
    exutoire <- df_base$Exutoire_1[i]
    compteur <- df_base$Compteur_Eau[i]
    
    exut_en_assec <- FALSE
    if (exutoire %in% df_base$NOM) {
      exut_idx <- which(df_base$NOM == exutoire)
      exut_en_assec <- assec_annee[exut_idx]
    }
    
    if (compteur >= 5) {
      assec_annee[i] <- TRUE
    } else if (compteur >= 3 && exut_en_assec) {
      assec_annee[i] <- TRUE
    } else if (compteur >= 3 && df_base$Niveau[i] == 1 && runif(1) < 0.3) {
      assec_annee[i] <- TRUE
    } else if (compteur >= 3 && runif(1) < 0.1) {
      assec_annee[i] <- TRUE
    } else {
      assec_annee[i] <- FALSE
    }
  }
  
  df_base$Compteur_Eau <- ifelse(assec_annee, 0, df_base$Compteur_Eau + 1)
  matrice_assec[[as.character(annee)]] <- ifelse(assec_annee, "Assec", "Evolage")
}

matrice_assec <- matrice_assec %>% arrange(NOM)
write.csv2(matrice_assec, fichier_sortie, row.names = FALSE, quote = FALSE)
print(paste("Etape 2 terminee : Fichier", fichier_sortie, "genere."))







# ==============================================================================
# ETAPE 3 : RECALIBRAGE FINAL (DECALAGE SI ASSEC N+1)
# ==============================================================================

library(tidyverse)
library(lubridate)

# PARAMETRES A MODIFIER
fichier_dates <- "data/Chalamont_opti/Vidange/Vidange_Peche.csv"
fichier_assecs <- "data/Chalamont_opti/Vidange/ASSEC_Final.csv"
fichier_sortie <- "Calendrier_Final_BV.csv"

# 1. Chargement et standardisation
df_vidange <- read.csv(fichier_dates, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
df_assec <- read.csv(fichier_assecs, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)

df_vidange$NOM <- trimws(toupper(df_vidange$NOM))
df_assec$NOM <- trimws(toupper(df_assec$NOM))
if("Exutoire_1" %in% colnames(df_vidange)) df_vidange$Exutoire_1 <- trimws(toupper(df_vidange$Exutoire_1))

# 2. Conversion format long
df_vidange_long <- df_vidange %>%
  pivot_longer(
    cols = matches("^(Vidange|peche)\\d{4}$"),
    names_to = c(".value", "Annee"),
    names_pattern = "(Vidange|peche)(\\d{4})"
  ) %>%
  mutate(
    Annee = as.numeric(Annee),
    Vidange = as.Date(Vidange),
    peche = as.Date(peche),
    Duree_Vidange = as.numeric(peche - Vidange)
  )

df_assec_long <- df_assec %>%
  pivot_longer(
    cols = -any_of(c("OBJECTID", "NOM", "Exutoire_1", "Niveau", "Compteur_Eau")), 
    names_to = "Nom_Colonne",
    values_to = "Statut_Assec"
  ) %>%
  mutate(
    Annee_Assec = as.numeric(str_extract(Nom_Colonne, "\\d{4}")),
    Est_En_Assec = grepl("(?i)assec", Statut_Assec)
  ) %>%
  filter(!is.na(Annee_Assec))

# 3. Detection anticipation et jointure
df_anticipation <- df_assec_long %>%
  mutate(Annee_Cible = Annee_Assec - 1) %>%
  select(NOM, Annee = Annee_Cible, Assec_Annee_Suivante = Est_En_Assec)

df_complet <- df_vidange_long %>%
  left_join(df_anticipation, by = c("NOM", "Annee")) %>%
  mutate(Assec_Annee_Suivante = replace_na(Assec_Annee_Suivante, FALSE))

# Verification
nb_decalages <- sum(df_complet$Assec_Annee_Suivante, na.rm = TRUE)
print(paste("Diagnostic :", nb_decalages, "decalages a operer detectes."))

# 4. Decalage des dates
df_corrige <- df_complet %>%
  group_by(Exutoire_1, Annee) %>% 
  mutate(
    Date_Max_Chaine = max(peche, na.rm = TRUE),
    Nouvelle_Peche = if_else(Assec_Annee_Suivante == TRUE, Date_Max_Chaine + days(7), peche),
    Nouvelle_Vidange = Nouvelle_Peche - days(Duree_Vidange)
  ) %>%
  ungroup() %>%
  group_by(Exutoire_1, Annee, Assec_Annee_Suivante) %>%
  arrange(Nouvelle_Vidange) %>%
  mutate(
    Decalage = if_else(Assec_Annee_Suivante == TRUE, (row_number() - 1) * 7, 0),
    Date_Peche_Definitive = Nouvelle_Peche + days(Decalage),
    Date_Vidange_Definitive = Nouvelle_Vidange + days(Decalage)
  ) %>%
  ungroup()

# 5. Exportation au format INRAE
df_final <- df_corrige %>%
  select(any_of(c("OBJECTID", "NOM", "Exutoire_1")), Annee, Date_Vidange_Definitive, Date_Peche_Definitive) %>%
  pivot_wider(
    names_from = Annee,
    values_from = c(Date_Vidange_Definitive, Date_Peche_Definitive),
    names_glue = "{.value}{Annee}"
  ) %>%
  rename_with(~ str_replace(., "Date_Vidange_Definitive", "Vidange")) %>%
  rename_with(~ str_replace(., "Date_Peche_Definitive", "peche"))

annees_sim <- sort(unique(df_complet$Annee))
ordre_colonnes <- c(
  intersect(c("OBJECTID", "NOM", "Exutoire_1"), colnames(df_final)),
  as.vector(outer(c("Vidange", "peche"), annees_sim, paste0))
)
df_final <- df_final %>% select(any_of(ordre_colonnes))

write.csv2(df_final, fichier_sortie, row.names = FALSE, quote = FALSE)
print(paste("Etape 3 terminee : Fichier final", fichier_sortie, "genere avec succes."))







