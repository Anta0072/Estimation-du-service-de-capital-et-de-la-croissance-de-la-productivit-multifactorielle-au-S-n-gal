# Import des librairies
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Import de la base
travail <- read_xlsx("travail.xlsx", sheet = 2)

# Agrégation du travail entre 2019 et 2021
travail <- travail %>%
  group_by(annee) %>%
  mutate(weight = salaires/sum(salaires, na.rm = TRUE)) %>%
  ungroup()

travail <- travail %>%
  arrange(secteur, annee) %>%
  group_by(secteur) %>%
  mutate(travail_lag = lag(travail),
         weight_lag = lag(weight)) %>%
  ungroup()

work_flow <- travail %>%
  group_by(annee) %>%
  summarise(log_croissance_travail = sum(0.5*(weight+weight_lag)*log(travail/travail_lag), na.rm = TRUE),
            croissance_travail = exp(log_croissance_travail)-1,
            total_salaires = sum(salaires, na.rm = TRUE))

work_flow_plot <- ggplot(work_flow, aes(x = annee, y = croissance_travail)) +
  geom_line(color = "#4C6C8C", linewidth = 1) +
  geom_point(color = "#4C6C8C", size = 2) +
  geom_hline(yintercept = 0.065, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = unique(work_flow$annee)) +
  labs(
    x = "Année",
    y = "Croissance des services du travail"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )
ggsave("results/services_travail.png", plot = work_flow_plot, width = 20, height = 12, units = "cm", dpi = 300)


write.csv(work_flow, "intrant_travail.csv")
