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

generate_plot_I <- function(group) {
  group_data <- data[data$Gruppo == group, ]
  
  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = HMI_IBatt_C2, color = "C2"), linetype = "solid") +
    geom_line(aes(y = HMI_IBatt_C4, color = "C4"), linetype = "solid") +
    geom_line(aes(y = HMI_IBatt_C5, color = "C5"), linetype = "solid") +
    geom_line(aes(y = HMI_IBatt_C7, color = "C7"), linetype = "solid") +
    geom_hline(yintercept = 26.25, linetype = "dashed", color = "red") +
    labs(title = paste("Gruppo", group), x = "Time (s)", y = "Ampere") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery")
}

generate_plot_V <- function(group) { 
  group_data <- data[data$Gruppo == group, ]     
  
  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = HMI_VBatt_C2, color = "C2"), linetype = "solid") +
    geom_line(aes(y = HMI_VBatt_C4, color = "C4"), linetype = "solid") +
    geom_line(aes(y = HMI_VBatt_C5, color = "C5"), linetype = "solid") +
    geom_line(aes(y = HMI_VBatt_C7, color = "C7"), linetype = "solid") +
    geom_hline(yintercept = 25.25, linetype = "dashed", color = "red") +
    labs(title = paste("Group", group), x = "Time(s)", y = "Voltage") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery") 
}

# Lista per memorizzare i ggplot
plot_list <- list()

# Creazione di ggplot per ciascun gruppo
for (group in 1:20) {
  plot_list[[length(plot_list) + 1]] <- generate_plot_I(group)
}

# Organizza i ggplot in una griglia
grid.arrange(grobs = plot_list, ncol = 10)  # Modifica il numero di colonne se necessario


#! ---------------- Segnalazione primo valore con Voltaggio > 25.25 V in ogni gruppo --------------
 
# Inizializza la colonna Segnalazione a 0
data$Segnalazione <- 0
 
# Ciclo per ogni gruppo
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Trova l'indice della prima corrispondenza nel gruppo specifico
  indice_prima_corrispondenza <- which(data$HMI_VBatt_C2 > 25.25 & data$Gruppo == group)[1]
  data$Segnalazione[indice_prima_corrispondenza] <- 1
}

#! ---------------- Aggiungiamo la colonna mese e la colonna stagione --------------

data <- data %>%
  mutate(mese = format(Timestamp, "%m"))
 
data <- data %>%
  mutate(stagione = case_when(
    between(as.numeric(mese), 3, 4) | between(as.numeric(mese), 9, 10) ~ "primavera/autunno",
    between(as.numeric(mese), 5, 8) ~ "estate",
    between(as.numeric(mese), 11, 12) | between(as.numeric(mese), 1, 2) ~ "inverno",
    TRUE ~ NA_character_
  ))

#! ---------------- Calcolo e  aggiunta colonna Wattora --------------
# Inizializza una colonna per i Wattora

data$Wattora <- 0
 
# Ciclo per ogni gruppo
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  
  # Trova l'indice del primo e dell'ultimo valore negativo in HMI_IBatt_C2
  ind_max <- tail(which(subset_gruppo$HMI_IBatt_C2 < 0), 1)
  ind_min <- head(which(subset_gruppo$HMI_IBatt_C2 < 0), 1)

  # Calcola la durata della fase in ore
  durata_fase <- (subset_gruppo$second[ind_max] - subset_gruppo$second[ind_min]) / 3600

  # Calcola i Wattora per il gruppo
  wattora_gruppo <- -sum(subset_gruppo$HMI_IBatt_C2[ind_min:ind_max] * subset_gruppo$HMI_VBatt_C2[ind_min:ind_max]) * durata_fase
  
  # Assegna il risultato alla colonna Wattora
  data$Wattora[data$Gruppo == group] <- wattora_gruppo
}

