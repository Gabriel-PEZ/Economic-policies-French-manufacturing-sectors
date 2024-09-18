rm(list = ls())
setwd("C:/Users/gabri/OneDrive/Bureau/LDD/LDD3/S2/Projet")


library(ggplot2)
library(ggimage)
library(ggthemes)
library(gtExtras)
library(dplyr)
library(grid)
library(tidyr)
library(webshot)
library(ggrepel)
library(htmlwidgets)
library(regclass)
library(corrr)
library(broom)
library(gt)

GHP_France <- read.csv("GHP_France.csv", sep = ',', header = T)
df <- read.csv("Données projet-bi.csv", sep = ',', header = T)


icon_path <- "C:/Users/gabri/OneDrive/Bureau/LDD/LDD3/S2/Projet/Logo_COP_21_Paris_2015.png"

image_height_start <- max(GHP_France$GHP) * 0.85
offset_for_arrow_start <- max(GHP_France$GHP) * 0.05 

#FIGURE 1 : Evolution des émissions par habitant de GES de 1995 à 2021 :

g <- ggplot(GHP_France, aes(x = Annee, y = GHP)) +#Je stocke le graphique au lieu de l'afficher directement car il est lourd
  geom_line(aes(color = GHP), size = 2, linetype = "solid") +
  geom_point(aes(color = GHP), size = 5, shape = 21, fill = "#ffffff88") +
  geom_image(x = 2018, y = image_height_start, image = icon_path, size = 0.2, asp = 1) +
  geom_segment(aes(x = 2018, y = image_height_start + offset_for_arrow_start, 
                   xend = 2015, yend = GHP_France[GHP_France$Annee == 2015, "GHP"]),
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "last", angle = 25), 
               color = "firebrick", linetype = "solid", size = 2, lineend = "round") + 
  scale_color_gradient(low = "#6baed6", high = "#08306b") +
  labs(
    title = "Évolution des émissions par habitant de GES de 1995 à 2021",
    x = "Année",
    y = "Émissions (tonnes par habitant)",
    caption = "Source : Données fournies par la NASA"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 14, hjust = 1, face = "italic"),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.grid.major = element_line(color = "#e3e3e3"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  geom_smooth(method = "lm", color = "red", size = 1.5, se = FALSE)

print(g)

#Je crée un nouveau dataframe qui me servira pour la construction des graphiques concernant les statistiques
#descriptives

df_graph <- subset(df, select = c(CO2, FBCF, Investissements_economie_denergie,
                                  Investissements_renouvellement,
                                  Investissements_nouvellestechniques,
                                  Investissements_nouveauxproduits,
                                  Brevets_Manufacturier,
                                  Taxe_Initiale))

#Je renomme les colonnes afin d'améliorer la compréhension des données.
df_graph <- df_graph %>%
  rename(
    "CO2" = CO2,
    "FBCF" = FBCF,
    "Economie d'énergie" = Investissements_economie_denergie,
    "Renouvellement" = Investissements_renouvellement,
    "Nouvelles techniques" = Investissements_nouvellestechniques,
    "Nouveaux produits" = Investissements_nouveauxproduits,
    "Brevets" = Brevets_Manufacturier,
    "Taxe (initiale)" = Taxe_Initiale
  )

df_long <- df_graph %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

#Je choisis une palette de couleurs pour les différentes boîtes à moustaches de mes variables
custom_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#f781bf")


#FIGURE 3 : Boxplot des différentes variables

ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  geom_text(aes(label = round(median(Value), 1)), stat = "summary", fun.y = median, vjust = -0.5) +
  labs(title = "Résumé des Variables",
       x = "Variable",
       y = "Valeur") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none",  
    panel.background = element_rect(fill = "#f5f5f2"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = custom_palette)


#FIGURE 4 : Distribution des variables principales

df_graph %>%
  gt_plt_summary()

#Je crée un thème pour l'esthétique de mon prochain graphique
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9)
    )
}


#FIGURE 5 : Evolution du CO2, de la FBCF et des investissements verts dans le secteur manufacturier entre 1990 et 2021

ggplot(df, aes(x = Annee)) +
  geom_line(aes(y = FBCF/74.5752, color = "FBCF"), size = 1, linetype = "solid") +
  geom_line(aes(y = CO2/124.9, color = "CO2"), size = 1, linetype = "solid") +
  geom_line(aes(y = Investissements_renouvellement/24.5, color = "Renouvellements"), size = 1, linetype = "solid") +
  geom_line(aes(y = Investissements_nouveauxproduits/15.2, color = "Nouveaux produits"), size = 1, linetype = "solid") +
  geom_line(aes(y = Investissements_nouvellestechniques/7.9, color = "Nouvelles techniques"), size = 1, linetype = "solid") +
  geom_line(aes(y = Investissements_economie_denergie/4.6, color = "Economie d'énergie"), size = 1, linetype = "dashed") +
  geom_point(aes(y = FBCF/74.5752, color = "FBCF"), size = 2) +
  geom_point(aes(y = CO2/124.9, color = "CO2"), size = 2) +
  geom_point(aes(y = Investissements_renouvellement/24.5, color = "Renouvellements"), size = 2) +
  geom_point(aes(y = Investissements_nouveauxproduits/15.2, color = "Nouveaux produits"), size = 2) +
  geom_point(aes(y = Investissements_nouvellestechniques/7.9, color = "Nouvelles techniques"), size = 2) +
  geom_point(aes(y = Investissements_economie_denergie/4.6, color = "Economie d'énergie"), size = 2) +
  labs(title = "Évolution du CO2, de la FBCF et des investissements dans le secteur manufacturier entre 1990 et 2021",
       x = "Année",
       y = "Valeurs normalisées (base 2000 = 1)",
       caption="Source : INSEE & CITEPA",
       color = "Variables") +
  scale_color_manual(values = c("CO2" = "#1f78b4", 
                                "Renouvellements" = "#fb9a99", 
                                "Nouvelles techniques" = "#e31a1c", 
                                "Economie d'énergie" = "#33a02c",
                                "Nouveaux produits" = "#0D253F",
                                "FBCF" = "#FFA500")) +
  theme_custom() +
  theme(plot.title = element_text(size = 15), 
        legend.position = "bottom", 
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 11), 
        panel.background = element_rect(fill = "transparent"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dotted"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.y = element_text(vjust = 0.5))


#FIGURE 6 : Relation entre les brevets verts et le CO2

ggplot(df, aes(x = Brevets_Manufacturier, y = CO2, label = Annee, color = as.factor(Annee))) +
  geom_point(alpha = 0.8, size = 4) +  
  scale_color_viridis_d(option = "D", begin = 0.3, end = 0.9, guide = FALSE) + 
  geom_smooth(method = "lm", color = "darkred", size = 1.5, fill = "lightcoral", alpha = 0.4, se = TRUE) +
  geom_label_repel(size = 5, box.padding = 0.5, point.padding = 0.7,   
                   max.overlaps = 10, segment.color = 'grey50') +
  labs(x = "% Brevets verts secteur manufacturier", y = "CO2 (en mégatonnes d'équivalent CO2)",
       title = "Relation entre les brevets verts et le CO2",
       caption = "Source : CITEPA & OCDE") +
  theme_light(base_size = 14) +  
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill=NA, size=1))


#FIGURE 7 : Relation entre le logarithme de la taxe initiale et le CO2

ggplot(df[seq(25, 32, 1),], aes(x=log(Taxe_Initiale), y=CO2, color=factor(Annee), label=Annee)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_viridis_d(begin = 0.3, end = 0.9, guide = FALSE) +  
  geom_smooth(method="lm", color="darkred", size = 1.5, fill = "lightcoral", alpha = 0.4, se = TRUE) +
  geom_label_repel(size = 5, box.padding = 0.5, point.padding = 0.7,   
                   max.overlaps = 10, segment.color = 'grey50') +
  labs(x="Log(Taxe Initiale)", y="CO2",
       title="Relation entre le logarithme de la taxe initiale et le CO2",
       caption="Source : OCDE & CITEPA") +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill=NA, size=1))


#Je crée un nouveau dataframe en changeant le nom des variables pour améliorer la compréhension
DF <- read.csv("Données.csv", sep = ',', header = T)
df_reg <- DF %>%
  rename(
    "CO2" = CO2,
    "FBCF" = FBCF,
    "Economie_énergie" = Investissements_economie_denergie,
    "Renouvellement" = Investissements_renouvellement,
    "Nouvelles_techniques" = Investissements_nouvellestechniques,
    "Nouveaux_produits" = Investissements_nouveauxproduits,
    "Brevets" = Brevets_Manufacturier,
    "Marchandes" = marchandes,
    "Non_marchandes" = non_marchandes,
  )

#1ere régression
summary(reg_pol <- lm(CO2 ~ Marchandes + Non_marchandes + Subvention, df_reg))

#2e régression
summary(reg_instru <- lm(CO2 ~ Brevets + log(1 + Taxe_Initiale) + Souffre, df_reg))

#3e régression
summary(reg_inv <- lm(CO2 ~ Economie_énergie + Renouvellement
                      + Nouvelles_techniques + Nouveaux_produits 
                      + FBCF, df_reg))

#4e régression
summary(reg_EE <- lm(Economie_énergie ~ Marchandes + Non_marchandes + 
                      Subvention, df_reg))



#Résumé de la première régression avec les intervalles de confiance également :

reg_summary_pol <- tidy(reg_pol, conf.int = TRUE, conf.level = 0.95)
model_stats <- glance(reg_pol)
tableau_final <- reg_summary_pol %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "Marchandes" ~ "Marchandes",
      term == "Non_marchandes" ~ "Non marchandes",
      term == "Subvention" ~ "Subvention",
      TRUE ~ term
    ),
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 4),
    p.value = round(p.value, 4),
    conf.low = round(conf.low, 4),
    conf.high = round(conf.high, 4)
  ) %>%
  gt(rowname_col = "term") %>%
  cols_label(
    estimate = "Estimation",
    std.error = "Erreur Standard",
    statistic = "Statistique T",
    p.value = "P-valeur",
    conf.low = "Limite Inférieure",
    conf.high = "Limite Supérieure"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value, conf.low, conf.high),
    decimals = 4
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray95"),
      cell_borders(sides = "top", color = "gray80", style = "solid", weight = px(2))
    ),
    locations = cells_body(columns = TRUE)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  tab_source_note(
    source_note = sprintf(
      "Les limites inférieure et supérieure correspondent aux bornes d'un intervalle de confiance à 95%%. R² = %.4f, R² ajusté = %.4f, Statistique F = %.4f, RSE = %.4f", 
      model_stats$r.squared, model_stats$adj.r.squared, 
      model_stats$statistic, model_stats$sigma)
  )
print(tableau_final)



#Résumé de la seconde régression avec les intervalles de confiance également :

reg_summary_instru <- tidy(reg_instru, conf.int = TRUE, conf.level = 0.95)
model_stats_instru <- glance(reg_instru)
tableau_final_instru <- reg_summary_instru %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "Brevets" ~ "Brevets",
      term == "log(1 + Taxe_Initiale)" ~ "log(1 + TI)",
      term == "Souffre" ~ "Soufre",
      TRUE ~ term
    ),
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 4),
    p.value = round(p.value, 4),
    conf.low = round(conf.low, 4),
    conf.high = round(conf.high, 4)
  ) %>%
  gt(rowname_col = "term") %>%
  cols_label(
    estimate = "Estimation",
    std.error = "Erreur Standard",
    statistic = "Statistique T",
    p.value = "P-valeur",
    conf.low = "Limite Inférieure",
    conf.high = "Limite Supérieure"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value, conf.low, conf.high),
    decimals = 4
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray95"),
      cell_borders(sides = "top", color = "gray80", style = "solid", weight = px(2))
    ),
    locations = cells_body(columns = TRUE)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  tab_source_note(
    source_note = sprintf(
      "Les limites inférieure et supérieure correspondent aux bornes d'un intervalle de confiance à 95%%. R² = %.4f, R² ajusté = %.4f, Statistique F = %.4f, RSE = %.4f", 
      model_stats_instru$r.squared, model_stats_instru$adj.r.squared, 
      model_stats_instru$statistic, model_stats_instru$sigma)
  )
print(tableau_final_instru)


#Résumé de la troisième régression avec les intervalles de confiance également :

reg_summary_inv <- tidy(reg_inv, conf.int = TRUE, conf.level = 0.95)
model_stats_inv <- glance(reg_inv)
tableau_final_inv <- reg_summary_inv %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "Economie_énergie" ~ "EE",
      term == "Renouvellement" ~ "R",
      term == "Nouvelles_techniques" ~ "NT",
      term == "Nouveaux_produits" ~ "NP",
      term == "FBCF" ~ "FBCF",
      TRUE ~ term
    ),
    estimate = round(estimate, 6),
    std.error = round(std.error, 6),
    statistic = round(statistic, 6),
    p.value = round(p.value, 6),
    conf.low = round(conf.low, 6),
    conf.high = round(conf.high, 6)
  ) %>%
  gt(rowname_col = "term") %>%
  cols_label(
    estimate = "Estimation",
    std.error = "Erreur Standard",
    statistic = "Statistique T",
    p.value = "P-valeur",
    conf.low = "Limite Inférieure",
    conf.high = "Limite Supérieure"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value, conf.low, conf.high),
    decimals = 4
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray95"),
      cell_borders(sides = "top", color = "gray80", style = "solid", weight = px(2))
    ),
    locations = cells_body(columns = TRUE)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  tab_source_note(
    source_note = sprintf(
      "Les limites inférieure et supérieure correspondent aux bornes d'un intervalle de confiance à 95%%. R² = %.4f, R² ajusté = %.4f, Statistique F = %.4f, RSE = %.4f", 
      model_stats_inv$r.squared, model_stats_inv$adj.r.squared, 
      model_stats_inv$statistic, model_stats_inv$sigma)
  )
print(tableau_final_inv)



#Résumé de la dernière régression avec les intervalles de confiance également :

reg_summary_EE <- tidy(reg_EE, conf.int = TRUE, conf.level = 0.95)
model_stats_EE <- glance(reg_EE)
tableau_final_EE <- reg_summary_EE %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "Marchandes" ~ "Marchandes",
      term == "Non_marchandes" ~ "Non marchandes",
      term == "Subvention" ~ "Subvention",
      TRUE ~ term
    ),
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 4),
    p.value = round(p.value, 4),
    conf.low = round(conf.low, 4),
    conf.high = round(conf.high, 4)
  ) %>%
  gt(rowname_col = "term") %>%
  cols_label(
    estimate = "Estimation",
    std.error = "Erreur Standard",
    statistic = "Statistique T",
    p.value = "P-valeur",
    conf.low = "Limite Inférieure",
    conf.high = "Limite Supérieure"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value, conf.low, conf.high),
    decimals = 4
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray95"),
      cell_borders(sides = "top", color = "gray80", style = "solid", weight = px(2))
    ),
    locations = cells_body(columns = TRUE)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  tab_source_note(
    source_note = sprintf(
      "Les limites inférieure et supérieure correspondent aux bornes d'un intervalle de confiance à 95%%. R² = %.4f, R² ajusté = %.4f, Statistique F = %.4f, RSE = %.4f", 
      model_stats_EE$r.squared, model_stats_EE$adj.r.squared, 
      model_stats_EE$statistic, model_stats_EE$sigma)
  )
print(tableau_final_EE)

