library(tidyverse)
library(ggplot2)
library(scales)
dev.new()
# ====================================================================
# 1. PRÉPARATION DES DONNÉES 
# ====================================================================
nom_etang_cible <- "CARRONNIER" 

df_bilan_jour <- liste_etangs[[nom_etang_cible]] %>%
  filter(between(dat, as.Date("2024-01-01"), as.Date("2024-12-31"))) %>% 
  mutate(
    V_prov = lag(BF, default = 0) + Vp_etp + Volume_R + Vamont - VFuite,
    Trop_Plein_Exc = case_when(
      Statut_Simu == "Assec" ~ 0,
      V_prov > Vmax ~ -(V_prov - Vmax), 
      TRUE ~ 0
    ),
    Fuite_Infil  = -VFuite,
    
    Vidange = -Vol_Vidange_Jour
  ) %>%
  select(dat, Vp_etp, Volume_R, Vamont, Fuite_Infil, Trop_Plein_Exc, Vidange)

df_bilan_long <- df_bilan_jour %>%
  pivot_longer(
    cols = -dat, 
    names_to = "Flux",
    values_to = "Volume_m3"
  ) %>%
  mutate(
    Flux = factor(Flux, levels = c(
      "Vp_etp", "Volume_R", "Vamont", 
      "Fuite_Infil", "Trop_Plein_Exc", "Vidange"
    ))
  )

# ====================================================================
# 2. CRÉATION DU GRAPHIQUE MULTI-PANNEAUX
# ====================================================================
couleurs_flux <- c(
  "Vp_etp"         = "purple", 
  "Volume_R"       = "darkblue",
  "Vamont"         = "cyan3",
  "Fuite_Infil"    = "saddlebrown",
  "Trop_Plein_Exc" = "grey50",
  "Vidange"        = "red"
)

g_multi <- ggplot(df_bilan_long, aes(x = dat, y = Volume_m3, color = Flux)) +
  geom_line(size = 0.8) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
  scale_color_manual(values = couleurs_flux) +
  
  # Un panneau par Flux, chacun son échelle Y libre
  facet_wrap(~ Flux, scales = "free_y", ncol = 2) +
  scale_x_date(date_labels = "%d %b", date_breaks = "month") +
  
  scale_y_continuous(labels = scales::comma_format(big.mark = " ", decimal.mark = ",")) +
  
  labs(
    title = paste("Décomposition des flux hydrologiques :", nom_etang_cible, "(2024)"),
    subtitle = "Attention : L'échelle de l'axe vertical (Y) est différente pour chaque graphique",
    x = "Mois",
    y = "Volume journalier (m³)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none", 
    strip.text = element_text(size = 11, face = "bold"), 
    panel.border = element_rect(color = "grey80", fill = NA) 
  )

# Affichage
print(g_multi)