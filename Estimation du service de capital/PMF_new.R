# Import des librairies
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(tidyr)


# Import des bases
croissance <- read.csv("croissance.csv") %>% select(-X, -PIB, -lag_PIB)
facteurs <- read.csv("apport_facteurs_new.csv")

# Jointure des bases
base_pmf <- croissance %>%
  left_join(facteurs, by = "annee")

# Calcul de la PMF
pmf <- base_pmf %>%
  mutate(log_PMF = log_croissance_PIB - apport_facteurs,
         PMF = 100*log_PMF,
         capital = 100*apport_capital,
         travail = 100*apport_travail,
         croissance = 100*log_croissance_PIB) %>%
  select(annee, PMF, capital, travail, croissance)


pmf_long <- pmf %>%
  pivot_longer(cols = c(PMF, capital, travail, croissance),
               names_to = "composante", values_to = "valeur") %>%
  mutate(
    annee = factor(annee),
    composante = factor(composante,
                        levels = c("PMF", "capital", "travail", "croissance"),
                        labels = c("PMF", "Capital", "Travail", "Croissance"))
  ) %>%
  filter((annee != "2015") & (annee != "2016"))

# Graphique à barres groupées
pmf_plot <- ggplot(pmf_long, aes(x = annee, y = valeur, fill = composante)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "PMF" = "#4C6C8C",
    "Capital" = "#7A9E7E",
    "Travail" = "#C97B63",
    "Croissance" = "#9B8AA6"
  )) +
  labs(
    x = "Année",
    y = "Contributions en pourcentage",
    fill = "Composantes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )
ggsave("results/decomposition_croissance.png", plot = pmf_plot, width = 20, height = 12, units = "cm", dpi = 300)

# PMFet croissance
pmf_croissance <- read_xlsx("Base de données.xlsx", sheet = 9)

pmf_croissance_long <- pivot_longer(pmf_croissance, cols = c(croissance, PMF),
                        names_to = "variable", values_to = "valeur")

croissance_pmf_plot <- ggplot(pmf_croissance_long, aes(x = annee, y = valeur, color = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = pmf_croissance$annee) +
  scale_y_continuous(
    labels = function(x) x * 100
  ) +
  scale_color_manual(values = c("croissance" = "#4C6C8C", "PMF" = "#7A9E7E")) +
  labs(x = "Année", y = "Valeur en %", color = NULL) +
  theme_minimal()
ggsave("results/croissance_x_pmf.png", plot = croissance_pmf_plot, width = 20, height = 12, units = "cm", dpi = 300)
