# Import des librairies
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Import de la base de données
data_cap = read_xlsx("Base de données.xlsx")
PIB <- read_xlsx("Base de données.xlsx", sheet = 6)
taux_fbcf <- read_xlsx("Base de données.xlsx", sheet = 7)
croissance_pib <- read_xlsx("Base de données.xlsx", sheet = 8)
categories <- read_xlsx("Base de données.xlsx", sheet = 2)

# FBCF par annee
fbcf <- data_cap %>%
  group_by(annee) %>%
  summarise(Valeur = sum(Volume, na.rm = TRUE)/1000)

pib_fbcf <- bind_rows(
  fbcf %>% mutate(serie = "FBCF"),
  PIB %>% mutate(serie = "PIB"))

# Evolution de la formation brute de capital fixe
graphic1 <- ggplot(pib_fbcf, aes(x = annee, y = Valeur, color = serie, group = serie)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = unique(fbcf$annee))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("#4C6C8C", "#9B8AA6")) +
  labs(
    x = "Année", 
    y = "PIB et FBCF en volume en milliards de Franc CFA"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )
ggsave("results/FBCF_PIB_par_annee.png", plot = graphic1, width = 20, height = 12, units = "cm", dpi = 300)

# Evolution du taux de FBCF
fbcf_croissance <- bind_rows(
  taux_fbcf %>% mutate(serie = "Taux de FBCF"),
  croissance_pib %>% mutate(serie = "Croissance du PIB")
)

graphic2 <- ggplot(fbcf_croissance, aes(x = annee, y = Valeur, color = serie, group = serie)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = unique(fbcf_croissance$annee))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("#4C6C8C", "#9B8AA6")) +
  labs(
    x = "Année", 
    y = "Croissance du PIB et taux de FBCF"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )
ggsave("results/FBCF_croissance.png", plot = graphic2, width = 20, height = 12, units = "cm", dpi = 300)

# Faire l'évolution par catégorie d'actifs
fbcf_cat <- data_cap %>%
  left_join(categories %>% select(id_produit, categories), by = "id_produit")

fbcf_cat <- fbcf_cat %>%
  group_by(categories, annee) %>%
  summarise(Valeur = sum(Volume, na.rm = TRUE)/1000)

# Evolution
graphic3 <- ggplot(fbcf_cat, aes(x = annee, y = Valeur, colour = categories)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = unique(fbcf_cat$annee)) +
  scale_color_manual(values = c(
    "#4C6C8C",
    "#7A9E7E",
    "#C97B63",
    "#9B8AA6",
    "#B08C5F",
    "#D48FA3"
  )) +
  labs(
    x = "Année",
    y = "FBCF en milliards de Francs CFA constants",
    color = "Catégorie d'actif"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )
ggsave("results/FBCF_categories.png", plot = graphic3, width = 20, height = 12, units = "cm", dpi = 300)

# Evolution de la FBCF seule
fbcf_evolution <- ggplot(fbcf, aes(x = annee, y = Valeur)) +
  geom_line(color = "#4C6C8C", linewidth = 1) +
  geom_point(color = "#4C6C8C", size = 2) +
  geom_hline(yintercept = 0.065, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = unique(fbcf$annee)) +
  labs(
    x = "Année",
    y = "FBCF en milliards de Francs CFA constants"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )
ggsave("results/FBCF.png", plot = fbcf_evolution, width = 20, height = 12, units = "cm", dpi = 300)
