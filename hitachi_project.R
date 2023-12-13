library(tidyverse)
library(ggplot2)
load("my_workspace.RData")


data <- readRDS("tr_13.rds")

head(subC2)

# Eliminare dati del 2017
data$Timestamp <- as.POSIXct(data$Timestamp)
data  <-  data[format(data$Timestamp, "%Y") == "2022", ]

# Rinomina la colonna relativa alla velocità
names(data)[names(data) == "_VEHICLE_SPEED"] <- "Speed"

ggplot(data = data[0:1000,]) +
  geom_line(mapping = aes(x = Timestamp, y = Speed))


# Creo una tabella contente solo i dati relativi alla Batteria 2
subC2 <- data[, c("Timestamp", "Speed", "HMI_IBatt_C2", "HMI_VBatt_C2")]

# Colonne: ID - Derivata - Fase
subC2$ID       <- ifelse(data$HMI_IBatt_C2 >= 0,   1,  -1)
subC2$Derivata <- c(0, diff(data$HMI_IBatt_C2) /diff(data$Timestamp))
subC2$Periodo  <- cumsum(c(0, diff(data$Timestamp) > 15))


# Contatore Gruppi
gruppo <- rep(0, length(subC2$Timestamp))

for (index in 2:(length(subC2$Timestamp))) {
  if (subC2$Derivata[index] < 0 
      && subC2$ID[index]      !=  subC2$ID[index-1] 
      || subC2$Periodo[index] !=  subC2$Periodo[index-1]) {
        gruppo[index] <-  1
  }
  else {
    gruppo[index] <-  0
  }
}

subC2$Gruppo <- cumsum(gruppo)


#* Visualizzare un ciclo di carica e scarica 
ggplot(data = data[1:740 ,]) +
  geom_line(mapping = aes(x = Timestamp, y = HMI_IBatt_C2)) +
  geom_hline(yintercept = 8.6, linetype = "dashed", color = "red") + 
  labs(title = "C2 Current evolution", x = "Time", y = "Ampere (I)")

ggplot(data = data[1:740,]) +
geom_line(mapping = aes(x = Timestamp, y = HMI_VBatt_C2)) +
  labs(title = "C2 Potential evolution", x = "date", y = "Voltage (V)") 

ggplot(data = subC2[1:5000,]) +
  geom_line(mapping = aes(x = Timestamp, y = HMI_IBatt_C2), color = "black") +
  geom_line(mapping = aes(x = Timestamp, y = Gruppo), color = 'red', size = 2) +
  labs(title = "Primi 5000 dati", x = "Date", y = "Ampere (I)")

ggplot(data = subC2[1:5000,]) +
  geom_line(mapping = aes(x = Timestamp, y = HMI_VBatt_C2), color = "black") +
  labs(title = "Primi 5000 dati", x = "Date", y = "Voltage")


#* Integrale grafico Ampere per calcolare (A/h)
capacità <- rep(0, length(subC2$Timestamp))
for (index in 2:(length(subC2$Timestamp))) {
  capacità[index] <- (subC2$Timestamp[index] - subC2$Timestamp[index-1])*(subC2$HMI_IBatt_C2[index] + subC2$HMI_IBatt_C2[index-1])/2
}
subC2$Capacità <- capacità


intensità <- ggplot(subC2[1:2190,], aes(x = Timestamp, y = HMI_IBatt_C2)) +
  geom_line(color = "black") +
  #geom_line(mapping = aes(x = Timestamp, y = HMI_IBatt_C2), color = "black") +
  facet_wrap(~Gruppo, scales = "fixed") +
  labs(title = "Intensità C2 per Gruppo", x = "Date", y = "Ampere (I)")

voltaggio <- ggplot(subC2[1:2190,], aes(x = Timestamp, y = HMI_VBatt_C2)) +
  geom_line(color = "black") +
  #geom_line(mapping = aes(x = Timestamp, y = HMI_VBatt_C2), color = "red") +
  facet_wrap(~Gruppo, scales = "fixed") +
  labs(title = "Intensità C2 per Gruppo", x = "Date", y = "Ampere (I)")

ggsave("Intensità_per_gruppo.png", intensità)
ggsave("Voltaggio_per_gruppo.png", voltaggio)

save.image(file = "my_workspace.RData")

