# Import des librairies
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Import de la base de données
data_cap = read_xlsx("serie FBCF.xlsx")

# Calcul du taux de dépréciation par actif
data_cap_dep <- data_cap %>%
  mutate(
    amortissement = case_when(
      substr(id_produit, 1, 1) == "A" ~ delta_betail,
      substr(id_produit, 1, 1) == "F" ~ 0.91 / Duree_moy,
      substr(id_produit, 1, 1) == "C11" ~ 1.65 / Duree_moy,
      substr(id_produit, 1, 1) == "C12" ~ 1.65 / Duree_moy,
      TRUE ~ 2 / Duree_moy
    )
  ) %>%
  filter(annee != 2014)

# Estimation du stock productif
stock_prod <- data_cap_dep %>%
  arrange(id_produit, Libelle_produit, annee) %>%
  group_by(id_produit) %>%
  group_modify(~{
    actifs <- .x
    id <- .y$id_produit
    annees <- actifs$annee
    invests <- actifs$Volume
    libelle <- actifs$Libelle_produit
    delta <- actifs$amortissement[1]
    n <- length(invests)
    g <- rep(NA, n)
    stock <- rep(NA, n)
    
    for (t in seq_len(n)) {
      
      if (t==1) {
        g_calc <- switch(as.character(id),
                         "M00030000" = (invests[2]/invests[1]) - 1,
                         "F00030000" = (invests[2]/invests[1]) - 1,
                         "J00020000" = 0,
                         "C04030000" = 0,
                         (invests[7]/invests[1])^(1/6) - 1)
        g <- rep(g_calc, n)
        stock[t] = invests[t]/(g_calc+delta)
      }
      
      if (t>1) {
        stock[t] <- (1-delta)*stock[t-1] + invests[t]
      }
    }
    data.frame(annee = annees, Actif = libelle, Volume = invests, amortissement = delta, growth = g, Stock_productif = stock)
  }) %>%
  ungroup()

# Agrégation du stock net de capital
stock_net <- stock_prod %>%
  left_join(data_cap_dep %>% select(id_produit, annee, Indice_de_prix), by = c("id_produit", "annee")) %>%
  mutate(stock_net = Indice_de_prix * Stock_productif) %>%
  select(id_produit, annee, Actif, stock_net, Stock_productif)

categories <- read_xlsx("serie FBCF.xlsx", sheet = 2)

valeur_totale_stock_net <- stock_net %>% 
  left_join(categories %>% select(id_produit, categories), by = "id_produit") %>%
  group_by(categories, annee) %>%
  summarise(stock_net_total = sum(stock_net, na.rm = TRUE)/1000)
write.csv(valeur_totale_stock_net, "stock_net_categories.csv")

# Evolution de la valeur du stock net par catégorie d'actifs
stock_net_valeur <- ggplot(valeur_totale_stock_net, aes(x = annee, y = stock_net_total, colour = categories)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = unique(valeur_totale_stock_net$annee)) +
  scale_color_manual(values = c(
    "#4C6C8C",
    "#7A9E7E",
    "#C97B63",
    "#9B8AA6",
    "#B08C5F"
  )) +  
  labs(
    x = "Année",
    y = "Valeur du stock net par catégorie d'actifs en milliards de Franc CFA",
    color = "Catégorie d'actif"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
ggsave("results2/stock_net_val.png", plot = stock_net_valeur, width = 20, height = 12, units = "cm", dpi = 300)

# Indice de volume du stock net de capital
stock_net_cat <- stock_net %>% 
  left_join(categories %>% select(id_produit, categories), by = "id_produit") %>%
  group_by(categories, annee) %>%
  mutate(poids_cat = stock_net/sum(stock_net, na.rm = TRUE))

stock_net_cat <- stock_net_cat %>%
  arrange(categories, id_produit, annee) %>%
  group_by(categories, id_produit) %>%
  mutate(
    log_growth = log(Stock_productif / lag(Stock_productif)),
    poids_cat_lag  = lag(poids_cat)
  ) %>%
  ungroup()

cat_net_stock <- stock_net_cat %>%
  group_by(categories, annee) %>%
  summarize(
    log_croissance = sum(0.5*(poids_cat + poids_cat_lag)*log_growth, na.rm = TRUE),
    Valeur = (exp(log_croissance) - 1),
    .groups = "drop"
  ) %>%
  filter(annee != 2015)

# Croissance du stock net de capital par catégorie d'actif
stock_net_growth_cat <- ggplot(cat_net_stock, aes(x = annee, y = Valeur, color = categories)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = unique(cat_net_stock$annee)) +
  scale_color_manual(values = c(
    "#4C6C8C",
    "#7A9E7E",
    "#C97B63",
    "#9B8AA6",
    "#B08C5F"
  )) +  
  labs(
    x = "Année",
    y = "Taux de croissance annuel",
    color = "Catégorie d'actif"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
ggsave("results2/croissance_stock_net_categories.png", plot = stock_net_growth_cat, width = 20, height = 12, units = "cm", dpi = 300)

# Agrégation pour toute l'économie
# Valeur du stock net de capital pour toute l'économie
wealth <- stock_net %>%
  group_by(annee) %>%
  summarise(wealth = sum(stock_net, na.rm = TRUE)/1000)
write.csv(wealth, "stock_net_economy.csv")

wealth_plot <- ggplot(wealth, aes(x = annee, y = wealth)) +
  geom_line(color = "#4C6C8C", linewidth = 1) +
  geom_point(color = "#4C6C8C", size = 2) +
  geom_hline(yintercept = 20000, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = unique(wealth$annee)) +
  labs(
    x = "Année",
    y = "Valeur du stock net en milliards de FCFA"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold")
  )
ggsave("results2/valeur_stock_net_senegal.png", plot = wealth_plot, width = 20, height = 12, units = "cm", dpi = 300)


stock_net_tot <- stock_net %>%
  group_by(annee) %>%
  mutate(poids_tot = stock_net/sum(stock_net, na.rm = TRUE))

stock_net_tot <- stock_net_tot %>%
  arrange(id_produit, annee) %>%
  group_by(id_produit) %>%
  mutate(
    lag_stock = lag(Stock_productif),
    log_growth = log(Stock_productif / lag_stock),
    poids_tot_lag  = lag(poids_tot)
  ) %>%
  ungroup()

economy_net_stock <- stock_net_tot %>%
  group_by(annee) %>%
  summarize(
    log_croissance = sum(0.5*(poids_tot + poids_tot_lag)*log_growth, na.rm = TRUE),
    Valeur = (exp(log_croissance) - 1),
    .groups = "drop" 
  ) %>%
  filter(annee!="2015")

# Croissance du stock de richesse au Sénégal entre 2016 et 2021
stock_net_growth_economy <- ggplot(economy_net_stock, aes(x = annee, y = Valeur)) +
  geom_line(color = "#4C6C8C", linewidth = 1) +
  geom_point(color = "#4C6C8C", size = 2) +
  geom_hline(yintercept = 0.065, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    breaks = seq(0.06, 0.08, by = 0.01),
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(breaks = unique(economy_net_stock$annee)) +
  labs(
    x = "Année",
    y = "Taux de croissance annuel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold")
  )
ggsave("results2/croissance_stock_net_senegal.png", plot = stock_net_growth_economy, width = 20, height = 12, units = "cm", dpi = 300)

# Prix de location du capital
# Rendement endogène
ebe <- read_xlsx("Base de données.xlsx", sheet = 3)

prix_loc <- stock_prod %>%
  left_join(data_cap_dep %>% select(id_produit, annee, Indice_de_prix, amortissement), by = c("id_produit", "annee")) %>%
  left_join(ebe, by = "annee") %>%
  arrange(id_produit, annee) %>%
  group_by(id_produit) %>%
  mutate(index_lag = ifelse(annee==2015, 1, lag(Indice_de_prix))) %>%
  rename(amortissement = amortissement.x)

prix_loc <- prix_loc %>%
  arrange(id_produit, annee) %>%
  group_by(id_produit) %>%
  mutate(gain_cap = (Indice_de_prix/index_lag) - 1) %>%
  ungroup()

impots <- read_xlsx("serie FBCF.xlsx", sheet = 1) %>%
  group_by(annee) %>%
  summarise(Impots = sum(Impots_Valeur, na.rm = TRUE))

prix_loc_OCDE <- prix_loc %>%
  left_join(impots, by = "annee")

prix_loc_OCDE <- prix_loc_OCDE %>%
  arrange(id_produit, annee) %>%
  group_by(annee) %>%
  mutate(rental = (EBE + Impots - sum(amortissement*Indice_de_prix*Stock_productif, na.rm = TRUE))/sum(index_lag*Stock_productif, na.rm = TRUE))

prix_loc_OCDE <- prix_loc_OCDE %>%
  mutate(prix_location = ifelse(id_produit=="M00030000", Indice_de_prix*(rental + amortissement),
                                index_lag*rental + Indice_de_prix*amortissement))

# Services de capital par actifs
prix_loc_OCDE <- prix_loc_OCDE %>%
  arrange(id_produit, annee) %>%
  group_by(id_produit, annee) %>%
  mutate(cap_services = prix_location*Stock_productif)

# Valeur des services du capital par catégories d'actifs
cap_services_total_value <- prix_loc_OCDE %>% 
  left_join(categories %>% select(id_produit, categories), by = "id_produit") %>%
  group_by(categories, annee) %>%
  summarise(capital_services = sum(cap_services, na.rm = TRUE)/1000) %>%
  filter((annee != 2015))

# Evolution de la valeur des services du capital par catégories d'actifs
capital_services_value <- ggplot(cap_services_total_value, aes(x = annee, y = capital_services, colour = categories)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(breaks = unique(cap_services_total_value$annee)) +
  scale_color_manual(values = c(
    "#4C6C8C",
    "#7A9E7E",
    "#C97B63",
    "#9B8AA6",
    "#B08C5F"
  )) +  
  labs(
    x = "Année",
    y = "Valeur des services du capital par catégorie d'actifs",
    color = "Catégorie d'actif"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
ggsave("results2/cap_services_val.png", plot = capital_services_value, width = 20, height = 12, units = "cm", dpi = 300)

# Agrégation par catégorie d'actifs
cap_services_cat <- prix_loc_OCDE %>% 
  left_join(categories %>% select(id_produit, categories), by = "id_produit") %>%
  group_by(categories, annee) %>%
  mutate(poids_cat = cap_services/sum(cap_services, na.rm = TRUE))

cap_services_cat <- cap_services_cat %>%
  arrange(categories, id_produit, annee) %>%
  group_by(categories, id_produit) %>%
  mutate(
    log_growth = log(Stock_productif / lag(Stock_productif)),
    poids_cat_lag  = lag(poids_cat)
  ) %>%
  ungroup()

cat_cap_services <- cap_services_cat %>%
  group_by(categories, annee) %>%
  summarize(
    log_croissance = sum(0.5*(poids_cat + poids_cat_lag)*log_growth, na.rm = TRUE),
    Valeur = (exp(log_croissance) - 1),
    .groups = "drop"
  ) %>%
  filter((annee != 2015))

cap_services_growth_cat <- ggplot(cat_cap_services, aes(x = annee, y = Valeur, color = categories)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = unique(cat_cap_services$annee)) +
  scale_color_manual(values = c(
    "#4C6C8C",
    "#7A9E7E",
    "#C97B63",
    "#9B8AA6",
    "#B08C5F"
  )) +  
  labs(
    x = "Année",
    y = "Taux de croissance annuel",
    color = "Catégorie d'actif"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
ggsave("results2/croissance_services_capital_categories.png", plot = cap_services_growth_cat, width = 20, height = 12, units = "cm", dpi = 300)


# Agrégation pour toute l'économie
cap_services_tot <- prix_loc_OCDE %>%
  group_by(annee) %>%
  mutate(poids_serv_tot = cap_services/sum(cap_services, na.rm = TRUE))

cap_services_tot <- cap_services_tot %>%
  arrange(id_produit, annee) %>%
  group_by(id_produit) %>%
  mutate(
    log_growth = log(Stock_productif / lag(Stock_productif)),
    poids_tot_lag  = lag(poids_serv_tot)
  ) %>%
  ungroup()

economy_cap_services <- cap_services_tot %>%
  group_by(annee) %>%
  summarize(
    log_croissance_services_cap = sum(0.5*(poids_serv_tot + poids_tot_lag)*log_growth, na.rm = TRUE),
    Valeur = (exp(log_croissance_services_cap) - 1),
    total_services = sum(cap_services, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter((annee != 2015))

write.csv(economy_cap_services, "capital_services.csv")

# Représentation graphique
economy_cap <- bind_rows(
  economy_cap_services %>% select(annee, Valeur) %>% mutate(serie = "Services du capital"),
  economy_net_stock %>% select(annee, Valeur) %>% mutate(serie = "Stock net") %>% filter(annee != 2015)
)

cap_services_growth_economy <- ggplot(economy_cap, aes(x = annee, y = Valeur, color = serie, group = serie)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.065, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = unique(economy_cap$annee)) +
  scale_color_manual(values = c("#4C6C8C", "#9B8AA6")) +
  labs(
    x = "Année",
    y = "Taux de croissance annuel",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold")
  )
ggsave("results2/croissance_services_capital_senegal.png", plot = cap_services_growth_economy, width = 20, height = 12, units = "cm", dpi = 300)


