#! ---------------- Leggiamo il Dataset ------------------- 
library(tidyverse)
library(dplyr)
library(ggplot2)

 
load("workspace/my_workspace_project_clean.RData")
#save.image(file = "workspace/my_workspace_project_clean.RData")

data <- readRDS("battery_clean.rds")

# Trova i gruppi in cui almeno una osservazione ha POC_ID != 0
gruppi_con_POC <- unique(data$Gruppo[data$POC_ID != 0])

# Crea il subset e chiamalo "gruppi_POC"
data <- data[data$Gruppo %in% gruppi_con_POC, ]

names(data)[names(data) == "Gruppo"] <- "Gruppi_vecchi"
data$Gruppo <- dense_rank(data$Gruppi_vecchi)
data <- subset(data, select = -c(Diversi, Gruppi_vecchi))

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

#! ---------------- Calcolo e  aggiunta colonna Potenza --------------
data$Potenza_C2 <- data$HMI_IBatt_C2 * data$HMI_VBatt_C2
data$Potenza_C4 <- data$HMI_IBatt_C4 * data$HMI_VBatt_C4
data$Potenza_C5 <- data$HMI_IBatt_C5 * data$HMI_VBatt_C5
data$Potenza_C7 <- data$HMI_IBatt_C7 * data$HMI_VBatt_C7

#! ---------------- Funzioni per generare plot ------------------- 
library(gridExtra)

generate_plot_I <- function(group) {
  group_data <- data[data$Gruppo == group, ]
  
  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = HMI_IBatt_C2, color = "1"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_IBatt_C4, color = "2"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_IBatt_C5, color = "3"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_IBatt_C7, color = "4"), linetype = "solid", linewidth=1) +
    geom_hline(yintercept = 26.25, linetype = "dashed", color = "red") +
    labs(title = paste("Observation", group), x = "Time (s)", y = "Intensity (A)") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery")
}

generate_plot_V <- function(group) { 
  group_data <- data[data$Gruppo == group, ]     
  
  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = HMI_VBatt_C2, color = "1"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_VBatt_C4, color = "2"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_VBatt_C5, color = "3"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = HMI_VBatt_C7, color = "4"), linetype = "solid", linewidth=1) +
    geom_hline(yintercept = 25.25, linetype = "dashed", color = "red") +
    labs(title = paste("Observation", group), x = "Time(s)", y = "Voltage (V)") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery") 
}

generate_plot_P <- function(group) { 
  group_data <- data[data$Gruppo == group, ]     
  
  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = Potenza_C2, color = "1"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = Potenza_C4, color = "2"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = Potenza_C5, color = "3"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = Potenza_C7, color = "4"), linetype = "solid", linewidth=1) +
    #geom_hline(yintercept = 25.25, linetype = "dashed", color = "red") +
    labs(title = paste("Observation", group), x = "Time(s)", y = "Power (W)") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery") 
}
#! ---------------- Creazione tabella per gruppi C2 -----------
#Riempiamo  il nuovo dataset
df_C2 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)
df_C2$Timestamp_iniziale <- character()
#Gruppo
df_C2$Gruppo <- unique(data$Gruppo)


for (group in seq(1, max(data$Gruppo) - 1)) {
  subset_gruppo <- data[data$Gruppo == group & data$HMI_IBatt_C2 < 0, ]
  
  # Timestamp iniziale
  df_C2$Timestamp_iniziale[group] <- as.character(subset_gruppo$Timestamp[1])
  
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
  
  # Mese
  df_C2$Mese[group] <- subset_gruppo$mese[1]
  
  # Stagione
  df_C2$Stagione[group] <- subset_gruppo$stagione[1]
  
  # Wattora
  df_C2$Wattora[group] <- subset_gruppo$Wattora_C2[1]
  
  # Amperora
  df_C2$Amperora[group] <- subset_gruppo$Ah_C2[1]
  
  # POC
  df_C2$POC[group] <- subset_gruppo$POC_ID[subset_gruppo$POC_ID != 0 & subset_gruppo$HMI_IBatt_C2 < 0][1]
  
}  

df_C2$Timestamp_iniziale <- as.POSIXct(df_C2$Timestamp_iniziale)
# Elimina le righe di df_C2 in cui POC è NA
df_C2 <- na.omit(df_C2)

# Imposta la lingua di base su inglese
Sys.setlocale("LC_TIME", "C")

plot(df_C2$Timestamp_iniziale, df_C2$V_iniziale, type = "line", col = "blue", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 1")


plot(df_C2$Timestamp_iniziale, df_C2$V_iniziale, pch = 16, col = "blue", cex = .5,
     xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 1")

df_C2 <- subset(df_C2, select = -Timestamp_iniziale)


#! ---------------- Creazione tabella per gruppi C4 -----------
#Riempiamo il nuovo dataset
df_C4 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)
df_C4$Timestamp_iniziale <- character()

# Gruppo
df_C4$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  subset_gruppo <- data[data$Gruppo == group & data$HMI_IBatt_C4 < 0, ]
  
  # Timestamp iniziale
  df_C4$Timestamp_iniziale[group] <- as.character(subset_gruppo$Timestamp[1])
  
  # I iniziale finale e media
  df_C4$I_finale[group] <- tail(subset_gruppo$HMI_IBatt_C4[subset_gruppo$HMI_IBatt_C4 < 0], 1)
  df_C4$I_iniziale[group] <- head(subset_gruppo$HMI_IBatt_C4[subset_gruppo$HMI_IBatt_C4 < 0], 1)
  df_C4$I_media[group] <- mean(subset_gruppo$HMI_IBatt_C4[subset_gruppo$HMI_IBatt_C4 < 0])
  
  # V iniziale finale e media
  df_C4$V_finale[group] <- tail(subset_gruppo$HMI_VBatt_C4[subset_gruppo$HMI_IBatt_C4 < 0], 1)
  df_C4$V_iniziale[group] <- head(subset_gruppo$HMI_VBatt_C4[subset_gruppo$HMI_IBatt_C4 < 0], 1)
  df_C4$V_media[group] <- mean(subset_gruppo$HMI_VBatt_C4[subset_gruppo$HMI_IBatt_C4 < 0])
  
  # Durata scarica
  df_C4$Durata_scarica[group] <- subset_gruppo$Durata_scarica[1]
  
  # Mese
  df_C4$Mese[group] <- subset_gruppo$mese[1]
  
  # Stagione
  df_C4$Stagione[group] <- subset_gruppo$stagione[1]
  
  # Wattora
  df_C4$Wattora[group] <- subset_gruppo$Wattora_C4[1]
  
  # Amperora
  df_C4$Amperora[group] <- subset_gruppo$Ah_C4[1]
  
  # POC
  df_C4$POC[group] <- subset_gruppo$POC_ID[subset_gruppo$POC_ID != 0 & subset_gruppo$HMI_IBatt_C4 < 0][1]
  
}  

df_C4$Timestamp_iniziale <- as.POSIXct(df_C4$Timestamp_iniziale)
# Elimina le righe di df_C4 in cui POC è NA
df_C4 <- na.omit(df_C4)

# Imposta la lingua di base su inglese
Sys.setlocale("LC_TIME", "C")

# Line Plot - Battery 4
plot(df_C4$Timestamp_iniziale, df_C4$V_iniziale, type = "line", col = "green", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 2")

# Scatter Plot - Battery 4
plot(df_C4$Timestamp_iniziale, df_C4$V_iniziale, pch = 16, col = "green", cex = .5,
     xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 2")

# Rimuovi la colonna Timestamp_iniziale
df_C4 <- subset(df_C4, select = -Timestamp_iniziale)


#! ---------------- Creazione tabella per gruppi C5 -----------
#Riempiamo il nuovo dataset
df_C5 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)
df_C5$Timestamp_iniziale <- character()

# Gruppo
df_C5$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  subset_gruppo <- data[data$Gruppo == group & data$HMI_IBatt_C5 < 0, ]
  
  # Timestamp iniziale
  df_C5$Timestamp_iniziale[group] <- as.character(subset_gruppo$Timestamp[1])
  
  # I iniziale finale e media
  df_C5$I_finale[group] <- tail(subset_gruppo$HMI_IBatt_C5[subset_gruppo$HMI_IBatt_C5 < 0], 1)
  df_C5$I_iniziale[group] <- head(subset_gruppo$HMI_IBatt_C5[subset_gruppo$HMI_IBatt_C5 < 0], 1)
  df_C5$I_media[group] <- mean(subset_gruppo$HMI_IBatt_C5[subset_gruppo$HMI_IBatt_C5 < 0])
  
  # V iniziale finale e media
  df_C5$V_finale[group] <- tail(subset_gruppo$HMI_VBatt_C5[subset_gruppo$HMI_IBatt_C5 < 0], 1)
  df_C5$V_iniziale[group] <- head(subset_gruppo$HMI_VBatt_C5[subset_gruppo$HMI_IBatt_C5 < 0], 1)
  df_C5$V_media[group] <- mean(subset_gruppo$HMI_VBatt_C5[subset_gruppo$HMI_IBatt_C5 < 0])
  
  # Durata scarica
  df_C5$Durata_scarica[group] <- subset_gruppo$Durata_scarica[1]
  
  # Mese
  df_C5$Mese[group] <- subset_gruppo$mese[1]
  
  # Stagione
  df_C5$Stagione[group] <- subset_gruppo$stagione[1]
  
  # Wattora
  df_C5$Wattora[group] <- subset_gruppo$Wattora_C5[1]
  
  # Amperora
  df_C5$Amperora[group] <- subset_gruppo$Ah_C5[1]
  
  # POC
  df_C5$POC[group] <- subset_gruppo$POC_ID[subset_gruppo$POC_ID != 0 & subset_gruppo$HMI_IBatt_C5 < 0][1]
  
}  

df_C5$Timestamp_iniziale <- as.POSIXct(df_C5$Timestamp_iniziale)
# Elimina le righe di df_C5 in cui POC è NA
df_C5 <- na.omit(df_C5)

# Imposta la lingua di base su inglese
Sys.setlocale("LC_TIME", "C")

# Line Plot - Battery C5
plot(df_C5$Timestamp_iniziale, df_C5$V_iniziale, type = "line", col = "orange", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 3")

# Scatter Plot - Battery C5
plot(df_C5$Timestamp_iniziale, df_C5$V_iniziale, pch = 16, col = "orange", cex = .5,
     xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 3")

# Rimuovi la colonna Timestamp_iniziale
df_C5 <- subset(df_C5, select = -Timestamp_iniziale)


#! ---------------- Creazione tabella per gruppi C7 -----------
# Riempiamo il nuovo dataset
df_C7 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)
df_C7$Timestamp_iniziale <- character()

# Gruppo
df_C7$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  subset_gruppo <- data[data$Gruppo == group & data$HMI_IBatt_C7 < 0, ]
  
  # Timestamp iniziale
  df_C7$Timestamp_iniziale[group] <- as.character(subset_gruppo$Timestamp[1])
  
  # I iniziale finale e media
  df_C7$I_finale[group] <- tail(subset_gruppo$HMI_IBatt_C7[subset_gruppo$HMI_IBatt_C7 < 0], 1)
  df_C7$I_iniziale[group] <- head(subset_gruppo$HMI_IBatt_C7[subset_gruppo$HMI_IBatt_C7 < 0], 1)
  df_C7$I_media[group] <- mean(subset_gruppo$HMI_IBatt_C7[subset_gruppo$HMI_IBatt_C7 < 0])
  
  # V iniziale finale e media
  df_C7$V_finale[group] <- tail(subset_gruppo$HMI_VBatt_C7[subset_gruppo$HMI_IBatt_C7 < 0], 1)
  df_C7$V_iniziale[group] <- head(subset_gruppo$HMI_VBatt_C7[subset_gruppo$HMI_IBatt_C7 < 0], 1)
  df_C7$V_media[group] <- mean(subset_gruppo$HMI_VBatt_C7[subset_gruppo$HMI_IBatt_C7 < 0])
  
  # Durata scarica
  df_C7$Durata_scarica[group] <- subset_gruppo$Durata_scarica[1]
  
  # Mese
  df_C7$Mese[group] <- subset_gruppo$mese[1]
  
  # Stagione
  df_C7$Stagione[group] <- subset_gruppo$stagione[1]
  
  # Wattora
  df_C7$Wattora[group] <- subset_gruppo$Wattora_C7[1]
  
  # Amperora
  df_C7$Amperora[group] <- subset_gruppo$Ah_C7[1]
  
  # POC
  df_C7$POC[group] <- subset_gruppo$POC_ID[subset_gruppo$POC_ID != 0 & subset_gruppo$HMI_IBatt_C7 < 0][1]
  
}  

df_C7$Timestamp_iniziale <- as.POSIXct(df_C7$Timestamp_iniziale)
# Elimina le righe di df_C7 in cui POC è NA
df_C7 <- na.omit(df_C7)

# Imposta la lingua di base su inglese
Sys.setlocale("LC_TIME", "C")

# Line Plot - Battery C7
plot(df_C7$Timestamp_iniziale, df_C7$V_iniziale, type = "line", col = "purple", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 4")

# Scatter Plot - Battery C7
plot(df_C7$Timestamp_iniziale, df_C7$V_iniziale, pch = 16, col = "purple", cex = .5,
     xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 4")

# Rimuovi la colonna Timestamp_iniziale
df_C7 <- subset(df_C7, select = -Timestamp_iniziale)


#! ---------------- Divisione dei gruppi rispetto ai POC ----------------

# Creare un vettore per ciascun POC
gruppi_POC_1 <- df_C2$Gruppo[df_C2$POC == "POC_1"]
gruppi_POC_2 <- df_C2$Gruppo[df_C2$POC == "POC_2"]
gruppi_POC_3 <- df_C2$Gruppo[df_C2$POC == "POC_3"]
gruppi_POC_4 <- df_C2$Gruppo[df_C2$POC == "POC_4"]
gruppi_POC_5 <- df_C2$Gruppo[df_C2$POC == "POC_5"]
gruppi_POC_6 <- df_C2$Gruppo[df_C2$POC == "POC_6"]
gruppi_POC_7 <- df_C2$Gruppo[df_C2$POC == "POC_7"]
gruppi_POC_8 <- df_C2$Gruppo[df_C2$POC == "POC_8"]

#Generiamo i plot relativi al solo POC_4
plot_list <- lapply(gruppi_POC_7[1:5], function(gr) generate_plot_V(gr))
grid.arrange(grobs = plot_list, ncol = 5, top = "POC_7")  

#saveRDS(df_C2, "df_C2.rds")


