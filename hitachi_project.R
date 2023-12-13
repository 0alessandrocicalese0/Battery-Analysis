library(tidyverse)
library(ggplot2)

data <- readRDS("Projects/tr_13.rds")

head(data)

# Eliminare dati del 2017
data$Timestamp <- as.POSIXct(data$Timestamp)
data  <-  data[format(data$Timestamp, "%Y") == "2022", ]

# Rinomina la colonna relativa alla velocità
names(data)[names(data) == "_VEHICLE_SPEED"] <- "Speed"

ggplot(data = data[0:1000,]) +
  geom_line(mapping = aes(x = Timestamp, y = Speed))





#! Mantenere la history e l'enviroment in modo tale da non dover ricaricare il tutto il tutto

#* Integrale grafico Ampere per calcolare (A/h)
# Creo una tabella contente solo i dati relativi alla Batteria 2
subC2 <- data[, c("Timestamp", "Speed", "HMI_IBatt_C2", "HMI_VBatt_C2")]

# Colonne: ID - Derivata - Fase
subC2$ID       <- ifelse(data$HMI_IBatt_C2 >= 0,   1,  -1)
subC2$Derivata <- c(0, diff(data$HMI_IBatt_C2) /15)
subC2$Fasi     <- as.numeric(c(0, diff(data$Timestamp) > 15))


# Contatore
contatore_scarica <- 0

for (index in 2:(length(data$Timestamp) - 1000000)) {
  if (data$Derivata_C2[index] < 0 
      && subC2$ID[index] != subC2$ID[index-1] 
      && subC2$Fasi_[index] != 1) {
        contatore_scarica <- contatore_scarica + 1
  }
}

# Visualizza il risultato
print(contatore_scarica)



#* Visualizzare un ciclo di carica e scarica 
ggplot(data = data[1:740 ,]) +
  geom_line(mapping = aes(x = Timestamp, y = HMI_IBatt_C2)) +
  geom_hline(yintercept = 8.6, linetype = "dashed", color = "red") + 
  labs(title = "C2 Current evolution", x = "Time", y = "Ampere (I)")

ggplot(data = data[1:740,]) +
geom_line(mapping = aes(x = Timestamp, y = HMI_VBatt_C2)) +
  labs(title = "C2 Potential evolution", x = "date", y = "Voltage (V)") 

ggplot(data = data[1:5000,]) +
  geom_point(mapping = aes(x = Timestamp, y = HMI_IBatt_C2)) +
  geom_hline(yintercept = 8.6, linetype = "dashed", color = "red") + 
  labs(title = "", x = "date", y = "Ampere (I)")

ggplot(data = prova) +
  geom_point(mapping = aes(x = Timestamp, y = HMI_VBatt_C2)) +
  labs(title = "", x = "date", y = "Voltage (V)") 