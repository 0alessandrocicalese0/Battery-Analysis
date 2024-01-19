library(tidyverse)
library(dplyr)
library(ggplot2)

#! ------------------- Leggiamo il Dataset e recuperiamo solo i dati del 2022 -------------------

data <- readRDS("tr_13.rds")
data_POC_ID <- readRDS("tab_tr_13_POC_ID.rds") #colonna contenente i valori di ID per ogni POC
data$POC_ID <- data_POC_ID$POC_ID
data <- subset(data, select = -POC) #eliminazione colonna POC priva di ID

data$Timestamp <- as.POSIXct(data$Timestamp) # Modifichiamo il formato di Timestamp
data  <-  data[format(data$Timestamp, "%Y") == "2022", ]

# Rinomina la colonna relativa alla velocità
names(data)[names(data) == "_VEHICLE_SPEED"] <- "Speed"

data$Indice <- seq(1, dim(data)[1])


#! ------------------- Aggiungiamo le colonne ID in base al segno della corrente -------------------
data$ID_C2  <- ifelse(data$HMI_IBatt_C2 >= 0,   1,  -1)
data$ID_C4  <- ifelse(data$HMI_IBatt_C4 >= 0,   1,  -1)
data$ID_C5  <- ifelse(data$HMI_IBatt_C5 >= 0,   1,  -1)
data$ID_C7  <- ifelse(data$HMI_IBatt_C7 >= 0,   1,  -1)



data$Diversi <- ifelse(rowSums(data[, c("ID_C2", "ID_C4", "ID_C5", "ID_C7")] != data$ID_C2) > 0, 1, 0)

indices_da_copiare <- which(data$Diversi == 1)
for (indice in indices_da_copiare) {
  data[indice, -1] <- data[(indice - 1), -1]
}
sum(data$Diversi)

#! ------------------- Aggiungiamo la colonna Gruppo per ciascuna batteria -------------------

#*C2
data$Derivata_C2  <- c(0, as.numeric(diff(data$HMI_IBatt_C2)) / as.numeric(diff(data$Timestamp)))
PeriodoC2         <- cumsum(c(0, diff(data$Timestamp) > 15))

# Contatore Gruppi
gruppo <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C2[index] < 0 && data$ID_C2[index] != data$ID_C2[index-1]) || PeriodoC2[index] != PeriodoC2[index-1])) {
    gruppo[index] <-  1
  }
  else {
    gruppo[index] <-  0
  }
}
data$Gruppo_C2 <- cumsum(gruppo)
data <- subset(data, select = -Derivata_C2)


#*C4
data$Derivata_C4  <- c(0, as.numeric(diff(data$HMI_IBatt_C4)) / as.numeric(diff(data$Timestamp)))
PeriodoC4         <- cumsum(c(0, diff(data$Timestamp) > 15))

# Contatore Gruppi
gruppo <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C4[index] < 0 && data$ID_C4[index] != data$ID_C4[index-1]) || PeriodoC4[index] != PeriodoC4[index-1])) {
    gruppo[index] <-  1
  }
  else {
    gruppo[index] <-  0
  }
}
data$Gruppo_C4 <- cumsum(gruppo)
data <- subset(data, select = -Derivata_C4)


#*C5
data$Derivata_C5  <- c(0, as.numeric(diff(data$HMI_IBatt_C5)) / as.numeric(diff(data$Timestamp)))
PeriodoC5         <- cumsum(c(0, diff(data$Timestamp) > 15))

# Contatore Gruppi
gruppo <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C5[index] < 0 && data$ID_C5[index] != data$ID_C5[index-1]) || PeriodoC5[index] != PeriodoC5[index-1])) {
    gruppo[index] <-  1
  }
  else {
    gruppo[index] <-  0
  }
}
data$Gruppo_C5 <- cumsum(gruppo)
data <- subset(data, select = -Derivata_C5)


#*C7
data$Derivata_C7  <- c(0, as.numeric(diff(data$HMI_IBatt_C7)) / as.numeric(diff(data$Timestamp)))
PeriodoC7         <- cumsum(c(0, diff(data$Timestamp) > 15))

# Contatore Gruppi
gruppo <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C7[index] < 0 && data$ID_C7[index] != data$ID_C7[index-1]) || PeriodoC7[index] != PeriodoC7[index-1])) {
    gruppo[index] <-  1
  }
  else {
    gruppo[index] <-  0
  }
}
data$Gruppo_C7 <- cumsum(gruppo)
data <- subset(data, select = -Derivata_C7)


# Controlliamo se i gruppi sono gli stessi per tutte le batterie
sum(ifelse(rowSums(data[, c("Gruppo_C2", "Gruppo_C4", "Gruppo_C5", "Gruppo_C7")] != data$Gruppo_C2) > 0, 1, 0))
data <- subset(data, select = -c(Gruppo_C4, Gruppo_C5, Gruppo_C7))
# Rinomina la colonna Gruppo_C2 con il nuovo nome Gruppo
names(data)[which(names(data) == "Gruppo_C2")] <- "Gruppo"


#! ------------------- Seleziono i gruppi buoni ed eporto il nuovo dataset  -------------------

gruppo_buono <- rep(0, max(data$Gruppo) )
for (group in seq(0, max(data$Gruppo)) ) {
  if (min(data[data$Gruppo == group, "HMI_IBatt_C2" ]) < 0 && 
      max(data[data$Gruppo == group, "HMI_IBatt_C2" ]) > 0 ) {
    gruppo_buono [group] <- 1;
  }
}
gruppi_buoni <- which (gruppo_buono == 1)

data_new <- data[data$Gruppo %in% gruppi_buoni, ]

# Save data frame to an RDS file
saveRDS(data_new, "battery_clean.rds")


#! ---------------- Aggiungiamo la colonna di segnalazione, con elementi pari ad 1 in corrispondenza del primo valore di Voltaggio >25.25 V in ogni gruppo --------------
 
# Inizializza la colonna Segnalazione a 0

data$Segnalazione <- 0
 
# Ciclo per ogni gruppo
for (group in seq(0, max(data$Gruppo) - 1)) {
  # Trova l'indice della prima corrispondenza nel gruppo specifico
  indice_prima_corrispondenza <- which(data$HMI_VBatt_C2 > 25.25 & data$ID_C2 == -1 & data$Gruppo == group)[1]
  if (!is.na(indice_prima_corrispondenza)) {
    cat("indice:", indice_prima_corrispondenza, "\n")
    data$Segnalazione[indice_prima_corrispondenza] <- 1
  }
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
for (group in seq(0, max(data$Gruppo) - 1)) {
  # Filtra il dataset per il gruppo specifico
  subset_gruppo <- data[data$Gruppo == group, ]
  # Calcola la durata della fase in ore
  durata_fase <- (max(subset_gruppo$second) - min(subset_gruppo$second)) / 3600
  # Trova l'indice di massimo e di minimo nel gruppo
  ind_max <- which.max(subset_gruppo$HMI_VBatt_C2)
  ind_min <- which.min(subset_gruppo$HMI_VBatt_C2)
  # Calcola i Wattora per il gruppo
  wattora_gruppo <- sum(subset_gruppo$HMI_IBatt_C2[ind_min:ind_max] * subset_gruppo$HMI_VBatt_C2[ind_min:ind_max]) * durata_fase
  #wattora_gruppo <- cumsum(subset_gruppo$HMI_IBatt_C2[ind_min:ind_max] * subset_gruppo$HMI_VBatt_C2[ind_min:ind_max]) * durata_fase  
  # Assegna il risultato alla colonna Wattora
  data$Wattora[subC2$Gruppo == group] <- wattora_gruppo

}
