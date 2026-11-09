# Import des librairies
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Import des bases
travail <- read.csv("intrant_travail_new.csv") %>% select(-X)
capital_services <- read.csv("capital_services.csv") %>% select(-X)

# Jointure des bases
facteurs <- travail %>% 
  left_join(capital_services %>% select(-Valeur), by = "annee")

facteurs <- facteurs %>%
  mutate(poids_capital = total_services / (total_services + total_salaires),
         poids_travail = total_salaires / (total_services + total_salaires),
         poids_capital_lag = lag(poids_capital),
         poids_travail_lag = lag(poids_travail),
         apport_capital = 0.5*(poids_capital+poids_capital_lag)*log_croissance_services_cap,
         apport_travail = 0.5*(poids_travail+poids_travail_lag)*log_croissance_travail,
         apport_facteurs = apport_capital + apport_travail)
write.csv(facteurs, "apport_facteurs_new.csv")
