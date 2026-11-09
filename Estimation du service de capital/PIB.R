# Import des librairies
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Import de la base
pib <- read_xlsx("PIB.xlsx")
croissance_PIB <- read_xlsx("Base de données.xlsx", sheet = 8)
capital_services_growth <- read.csv("capital_services.csv") 

pib <- pib %>%
  mutate(lag_PIB = lag(PIB),
         log_croissance_PIB = log(PIB/lag_PIB)) %>%
  filter(annee!=2019)
write.csv(pib, "croissance.csv")
