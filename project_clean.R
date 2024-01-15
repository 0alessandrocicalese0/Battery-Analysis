#! ------------------- Leggiamo il Dataset ------------------- 
library(tidyverse)
library(dplyr)
library(ggplot2)

data <- readRDS("battery_clean.rds")
names(data)[names(data) == "Gruppo"] <- "Gruppi_vecchi"
data$Gruppo <- dense_rank(data$Gruppi_vecchi)
data <- subset(data, select = -Diversi) 

# Aggiungiamo la colonna degli indici e dei secondi per ogni gruppo

data <- data %>%
  mutate(Indice = seq(1, n())) %>%
  select(Timestamp, Indice, everything())

data <- data %>%
  group_by(Gruppo) %>%
  mutate(second = as.numeric(Timestamp - min(Timestamp), units = "secs")) %>%
  ungroup() %>%
  select(Timestamp, second, everything())

#! ------------------- Provo qualche plot ------------------- 
library(gridExtra)

# Funzione per generare un singolo ggplot per un gruppo
generate_plot <- function(group) {
  group_data <- data[data$Gruppo == group, ]
  ggplot(data = group_data, aes(x = second, y = HMI_IBatt_C2)) +
    geom_line() +
    geom_hline(yintercept = 26.25, linetype = "dashed", color = "red") + 
    labs(title = paste("Group", group), x = "Time", y = "Ampere") +
    theme_minimal()  # Personalizza il tema se necessario
}

# Lista per memorizzare i ggplot
plot_list <- list()

# Creazione di ggplot per ciascun gruppo
for (group in 1:20) {
  group_data <- data[data$Gruppo == group, ]
  plot_list[[length(plot_list) + 1]] <- generate_plot(group_data)
}

# Organizza i ggplot in una griglia
grid.arrange(grobs = plot_list, ncol = 10)  # Modifica il numero di colonne se necessario

generate_plot(1)
