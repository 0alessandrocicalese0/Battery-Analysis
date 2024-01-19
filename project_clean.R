#! ---------------- Leggiamo il Dataset ------------------- 
library(tidyverse)
library(dplyr)
library(ggplot2)

#load("my_workspace_projects_clean.RData")
save.image(file = "my_workspace_project_clean.RData")

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

#! ---------------- Calcolo e  aggiunta colonna Durata della scarica --------------

# Calcola la durata della fase in secondi per la fase di scarica
data$Durata_scarica <-0
for (group in seq(0, max(data$Gruppo) - 1)) {
   # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  ind_min_scarica <- head(which(subset_gruppo$HMI_IBatt_C2 < 0), 1)
  ind_max_scarica <- tail(which(subset_gruppo$HMI_IBatt_C2 < 0), 1)
  durata_della_scarica <- (subset_gruppo$second[ind_max_scarica] - subset_gruppo$second[ind_min_scarica]) 
  data$Durata_scarica[data$Gruppo == group & data$ID_C2 == -1]  <- durata_della_scarica
}

#! ---------------- Calcolo e  aggiunta colonna Amperora --------------

#* C2
data$Ah_C2 <- 0
# Ciclo for sui gruppi
for (group in seq(0, max(data$Gruppo) - 1)) {
  
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  # Calcola la capacità_erogata
  capacita_erogata_C2 <- cumsum(subset_gruppo$HMI_IBatt_C2) * subset_gruppo$Durata_scarica/3600
  #cat(capacita_erogata_C2,"\n")
  # Aggiorna la colonna Ah per la fase di scarica
  data$Ah_C2[data$Gruppo == group & data$ID_C2 == -1] <- capacita_erogata_C2
}  


#* C4
data$Ah_C4 <- 0
# Ciclo for sui gruppi
for (group in seq(0, max(data$Gruppo) - 1)) {
  
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
    # Calcola la capacità_erogata
  capacita_erogata_C4 <- cumsum(subset_gruppo$HMI_IBatt_C4) * subset_gruppo$Durata_scarica/3600
  #cat(capacita_erogata_C4,"\n")
  # Aggiorna la colonna Ah per la fase di scarica
  data$Ah_C4[data$Gruppo == group & data$ID_C4 == -1] <- capacita_erogata_C4
}  

#* C5
data$Ah_C5 <- 0
# Ciclo for sui gruppi
for (group in seq(0, max(data$Gruppo) - 1)) {
  
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  # Calcola la capacità_erogata
  capacita_erogata_C5 <- cumsum(subset_gruppo$HMI_IBatt_C5) * subset_gruppo$Durata_scarica/3600
  #cat(capacita_erogata_C2,"\n")
  # Aggiorna la colonna Ah per la fase di scarica
  data$Ah_C5[data$Gruppo == group & data$ID_C5 == -1] <- capacita_erogata_C5
}  

#* C7
data$Ah_C7 <- 0
# Ciclo for sui gruppi
for (group in seq(0, max(data$Gruppo) - 1)) {
  
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  # Calcola la capacità_erogata
  capacita_erogata_C7 <- cumsum(subset_gruppo$HMI_IBatt_C7) * subset_gruppo$Durata_scarica/3600
  #cat(capacita_erogata_C7,"\n")
  # Aggiorna la colonna Ah per la fase di scarica
  data$Ah_C7[data$Gruppo == group & data$ID_C7 == -1] <- capacita_erogata_C7
}  

#! ---------------- Calcolo e  aggiunta colonna Wattora --------------
# Inizializza una colonna per i Wattora

#* C2
data$Wattora_C2 <- 0
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
  data$Wattora_C2[data$Gruppo == group] <- wattora_gruppo
}

#* C4
data$Wattora_C4 <- 0
# Ciclo per ogni gruppo
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  
  # Trova l'indice del primo e dell'ultimo valore negativo in HMI_IBatt_C2
  ind_max <- tail(which(subset_gruppo$HMI_IBatt_C4 < 0), 1)
  ind_min <- head(which(subset_gruppo$HMI_IBatt_C4 < 0), 1)
  
  # Calcola la durata della fase in ore
  durata_fase <- (subset_gruppo$second[ind_max] - subset_gruppo$second[ind_min]) / 3600
  
  # Calcola i Wattora per il gruppo
  wattora_gruppo <- -sum(subset_gruppo$HMI_IBatt_C4[ind_min:ind_max] * subset_gruppo$HMI_VBatt_C4[ind_min:ind_max]) * durata_fase
  
  # Assegna il risultato alla colonna Wattora
  data$Wattora_C4[data$Gruppo == group] <- wattora_gruppo
}

#* C5
data$Wattora_C5 <- 0
# Ciclo per ogni gruppo
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  
  # Trova l'indice del primo e dell'ultimo valore negativo in HMI_IBatt_C2
  ind_max <- tail(which(subset_gruppo$HMI_IBatt_C5 < 0), 1)
  ind_min <- head(which(subset_gruppo$HMI_IBatt_C5 < 0), 1)
  
  # Calcola la durata della fase in ore
  durata_fase <- (subset_gruppo$second[ind_max] - subset_gruppo$second[ind_min]) / 3600
  
  # Calcola i Wattora per il gruppo
  wattora_gruppo <- -sum(subset_gruppo$HMI_IBatt_C5[ind_min:ind_max] * subset_gruppo$HMI_VBatt_C5[ind_min:ind_max]) * durata_fase
  
  # Assegna il risultato alla colonna Wattora
  data$Wattora_C5[data$Gruppo == group] <- wattora_gruppo
}

#* C7
data$Wattora_C7 <- 0
# Ciclo per ogni gruppo
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  
  # Trova l'indice del primo e dell'ultimo valore negativo in HMI_IBatt_C2
  ind_max <- tail(which(subset_gruppo$HMI_IBatt_C7 < 0), 1)
  ind_min <- head(which(subset_gruppo$HMI_IBatt_C7 < 0), 1)
  
  # Calcola la durata della fase in ore
  durata_fase <- (subset_gruppo$second[ind_max] - subset_gruppo$second[ind_min]) / 3600
  
  # Calcola i Wattora per il gruppo
  wattora_gruppo <- -sum(subset_gruppo$HMI_IBatt_C7[ind_min:ind_max] * subset_gruppo$HMI_VBatt_C7[ind_min:ind_max]) * durata_fase
  
  # Assegna il risultato alla colonna Wattora
  data$Wattora_C7[data$Gruppo == group] <- wattora_gruppo
}

#! ---------------- Proviamo qualche plot ------------------- 
library(gridExtra)

generate_plot_I <- function(group) {
  group_data <- data[data$Gruppo == group, ]
  
  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = HMI_IBatt_C2, color = "C2"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_IBatt_C4, color = "C4"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_IBatt_C5, color = "C5"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_IBatt_C7, color = "C7"), linetype = "solid", linewidth=1) +
    geom_hline(yintercept = 26.25, linetype = "dashed", color = "red") +
    labs(title = paste("Group", group), x = "Time (s)", y = "Ampere") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery")
}

generate_plot_V <- function(group) { 
  group_data <- data[data$Gruppo == group, ]     
  
  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = HMI_VBatt_C2, color = "C2"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_VBatt_C4, color = "C4"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_VBatt_C5, color = "C5"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_VBatt_C7, color = "C7"), linetype = "solid", linewidth=1) +
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


#! ---------------- Creazione tabella per gruppi Wattora C2 -----------
df_C2 <- data.frame(
  Gruppo = integer(1519),    
  I_iniziale = numeric(1519), 
  I_media = numeric(1519),    
  I_finale = numeric(1519),    
  V_iniziale = numeric(1519), 
  V_media = numeric(1519),   
  V_finale = numeric(1519),
  Durata_scarica = numeric(1519),
  Wattora = numeric(1519),
  Stagione = character(1519),
  #POC_ID = integer(1519)
)

#Riempiamo iterativamente il nuovo dataset

#Gruppo
df_C2$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  subset_gruppo <- data[data$Gruppo == group, ]
  
  #I iniziale finale e media
  df_C2$I_finale[group] <- tail(subset_gruppo$HMI_IBatt_C2[subset_gruppo$HMI_IBatt_C2 < 0], 1)
  df_C2$I_iniziale[group]<- head(subset_gruppo$HMI_IBatt_C2[subset_gruppo$HMI_IBatt_C2 < 0], 1)
  df_C2$I_media[group] <- mean(subset_gruppo$HMI_IBatt_C2[subset_gruppo$HMI_IBatt_C2 < 0])
  
  #V iniziale finale e media
  df_C2$V_finale[group] <- tail(subset_gruppo$HMI_VBatt_C2[subset_gruppo$HMI_IBatt_C2 < 0], 1)
  df_C2$V_iniziale[group]<- head(subset_gruppo$HMI_VBatt_C2[subset_gruppo$HMI_IBatt_C2 < 0], 1)
  df_C2$V_media[group] <- mean(subset_gruppo$HMI_VBatt_C2[subset_gruppo$HMI_IBatt_C2 < 0])
  
  # Durata scarica
  df_C2$Durata_scarica[group] <- subset_gruppo$Durata_scarica[1]
  
  # Stagione
  df_C2$Stagione[group] <- subset_gruppo$stagione[1]
  
  # Wattora
  df_C2$Wattora[group] <- subset_gruppo$Wattora_C2[1]
  
}

#! ---------------- Creazione tabella per gruppi Amperora C2 ----------
df_C2_Ah <- df_C2
df_C2_Ah <- subset(df_C2_Ah, select = -Wattora)
df_C2_Ah$Amperora <- 0

for (group in seq(1, max(data$Gruppo) - 1)) {
  subset_gruppo <- data[data$Gruppo == group, ]
  # Amperora
  df_C2_Ah$Amperora[group] <- subset_gruppo$Ah_C2[1]
}  
