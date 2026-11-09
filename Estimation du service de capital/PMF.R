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
facteurs <- read.csv("apport_facteurs.csv")

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
  )

# Graphique à barres groupées
pmf_plot <- graphic_pmf <- ggplot(pmf_long, aes(x = annee, y = valeur, fill = composante)) +
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

pmf_growth <- pmf %>%
  select(annee, PMF) %>%
  mutate(growth = (exp(PMF/100)-1)*100)
