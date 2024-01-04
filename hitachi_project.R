library(tidyverse)
library(ggplot2)
load("my_workspace.RData")

#* Leggiamo il Dataset e recuperiamo solo i dati del 2022

data <- readRDS("tr_13.rds")
data$Timestamp <- as.POSIXct(data$Timestamp)
data  <-  data[format(data$Timestamp, "%Y") == "2022", ]

# Rinomina la colonna relativa alla velocità
names(data)[names(data) == "_VEHICLE_SPEED"] <- "Speed"

# Visualizziamo la velocità
ggplot(data = data[0:1000,]) +
  geom_line(mapping = aes(x = Timestamp, y = Speed))

#* Studio la Batteria 2
subC2 <- data[, c("Timestamp", "Speed", "HMI_IBatt_C2", "HMI_VBatt_C2")]

# Colonne: ID - Derivata - Fase
subC2$ID        <- ifelse(data$HMI_IBatt_C2 >= 0,   1,  -1)
subC2$Derivata  <- c(0, as.numeric(diff(data$HMI_IBatt_C2)) / as.numeric(diff(data$Timestamp)))
Periodo         <- cumsum(c(0, diff(data$Timestamp) > 15))


#* Contatore Gruppi
gruppo <- rep(0, length(subC2$Timestamp))

for (index in 2:(length(subC2$Timestamp))) {
  if (subC2$Derivata[index] < 0 
      && subC2$ID[index]      !=  subC2$ID[index-1] 
      || Periodo[index] !=  Periodo[index-1]) {
        gruppo[index] <-  1
  }
  else {
    gruppo[index] <-  0
  }
}
subC2$Gruppo <- cumsum(gruppo)
view(subC2[1:2000,])

#//subC2 <- subset(subC2, select = -Periodo)


#* Visualizzo il primo periodo

# Creo la Colonna dei secondi per ogni gruppo
subC2$second <- 0
for (group in seq(0, max(subC2$Gruppo) - 1)) {
  ind_min <- min(which(subC2$Gruppo == group))
  ind_max <- min(which(subC2$Gruppo == group + 1)) - 1

  subC2$second[ind_min:ind_max] <- as.numeric(subC2$Timestamp[ind_min:ind_max]  - min(subC2$Timestamp[ind_min:ind_max] ), units = "secs")
}

# Plotto

ind_min <- min(which(subC2$Gruppo == 1))
ind_max <- min(which(subC2$Gruppo == 2)) - 1

ggplot(data = subC2[ind_min:ind_max,] ) +
  geom_line(mapping = aes(x = second, y = HMI_IBatt_C2)) +
geom_hline(yintercept = 26.25, linetype = "dashed", color = "red") + 
  labs(title = "C2 Current evolution", x = "Elapsed Time(S)", y = "Ampere (I)")

ggplot(data = subC2[ind_min:ind_max,]) +
  geom_line(mapping = aes(x = second, y = HMI_VBatt_C2)) +
  labs(title = "C2 Potential evolution", x = "Elapsed Time(S)", y = "Voltage (V)") 




#! Integrale grafico Ampere per calcolare (A/h)

#* Fase di scarica
# Inizializza un vettore di capacità a zero per ciascun gruppo
capacità <- rep(0, max(subC2$Gruppo))

# Itera su ciascun gruppo
for (group in seq(0, max(subC2$Gruppo) - 1)) {
  
  # Trova gli indici di inizio e fine del gruppo
  ind_min <- min(which(subC2$Gruppo == group))
  ind_max <- min(which(subC2$Gruppo == group + 1)) - 1
  
  # Itera sugli indici all'interno del gruppo
  for (index in seq(ind_min, ind_max)) {
    # Aggiorna la capacità se l'ID è -1
    if (subC2$ID[index] == -1) {
      capacità[group] <- capacità[group] + subC2$HMI_IBatt_C2[index]
    }
  }
  
  # Trova gli indici di inizio e fine del gruppo con ID uguale a -1
  ind_min2 <- min(which(subC2$ID[ind_min:ind_max] == -1))
  ind_max2 <- max(which(subC2$ID[ind_min:ind_max] == -1))
  
  # Calcola la capacità totale del gruppo
  capacità[group] <- capacità[group] * (subC2$second[ind_max2] - subC2$second[ind_min2]) / 3600
  
  # Stampa informazioni sul gruppo e sulla capacità
  print(sprintf("\nSono nel gruppo %d", group))
  print(sprintf('\nLa mia capacità è  %f', capacità[group]))
}
#* Fase di carica
# Inizializza un vettore di capacità a zero per ciascun gruppo
capacità2 <- rep(0, max(subC2$Gruppo))

# Itera su ciascun gruppo
for (group in seq(0, max(subC2$Gruppo) - 1)) {
  
  # Trova gli indici di inizio e fine del gruppo
  ind_min <- min(which(subC2$Gruppo == group))
  ind_max <- min(which(subC2$Gruppo == group + 1)) - 1
  
  # Itera sugli indici all'interno del gruppo
  for (index in seq(ind_min, ind_max)) {
    # Aggiorna la capacità se l'ID è +1 e l'intensità è >= 26.25   
    if (subC2$ID[index] == 1 && subC2$HMI_IBatt_C2[index] >= 26.25) {
      capacità2[group] <- capacità2[group] + subC2$HMI_IBatt_C2[index]
    }
  }
  
  # Trova gli indici di inizio e fine del gruppo con ID uguale a 1 e intensità >= 26.25
  ind_min2 <- min(which(subC2$ID == 1 & subC2$HMI_IBatt_C2 >= 26.25 & subC2$Gruppo == group))
  ind_max2 <- max(which(subC2$ID == 1 & subC2$HMI_IBatt_C2 >= 26.25 & subC2$Gruppo == group))
  
  # Calcola la capacità totale del gruppo
  capacità2[group] <- capacità2[group] * (subC2$second[ind_max2] - subC2$second[ind_min2]) / 3600
  
  # Stampa informazioni sul gruppo e sulla capacità
  print(sprintf("\nSono nel gruppo %d", group))
  print(sprintf('\nLa mia capacità è  %f', capacità2[group]))
}

capacità2[is.na(capacità2)] <- 0






ggplot(subC2[1:2190,], aes(x = Timestamp, y = HMI_IBatt_C2)) +
  geom_line(color = "black") +
  facet_wrap(~Gruppo, scales = "fixed") +
  labs(title = "Intensità C2 per Gruppo", x = "Date", y = "Ampere (I)")

ggplot(subC2[1:2190,], aes(x = Timestamp, y = HMI_VBatt_C2)) +
  geom_line(color = "black") +
  geom_line(mapping = aes(x = Timestamp, y = HMI_VBatt_C2), color = "red") +
  facet_wrap(~Periodo, scales = "fixed") +
  labs(title = "Intensità C2 per Gruppo", x = "Date", y = "Ampere (I)")

ggsave("Intensità_per_gruppo.png", intensità)
ggsave("Voltaggio_per_gruppo.png", voltaggio)

save.image(file = "my_workspace.RData")


