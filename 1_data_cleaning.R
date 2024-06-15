library(tidyverse)
library(dplyr)
library(ggplot2)

#! ------------------- Read the Dataset and retrieve only the 2022 data -------------------

data <- readRDS("tr_13.rds")
data_POC_ID <- readRDS("tab_tr_13_POC_ID.rds") # Column containing the ID values for each POC
data$POC_ID <- data_POC_ID$POC_ID
data <- subset(data, select = -POC) # Remove POC column without ID

data$Timestamp <- as.POSIXct(data$Timestamp) # Change the format of Timestamp
data  <-  data[format(data$Timestamp, "%Y") == "2022", ]

# Rename the column related to speed
names(data)[names(data) == "_VEHICLE_SPEED"] <- "Speed"

# Modify the POC_ID column based on the value of Speed
data$POC_ID[data$Speed == 0] <- 0

data$Indice <- seq(1, dim(data)[1])

#! ------------------- Add ID columns based on the sign of the current -------------------
data$ID_C2  <- ifelse(data$HMI_IBatt_C2 >= 0,   1,  -1)
data$ID_C4  <- ifelse(data$HMI_IBatt_C4 >= 0,   1,  -1)
data$ID_C5  <- ifelse(data$HMI_IBatt_C5 >= 0,   1,  -1)
data$ID_C7  <- ifelse(data$HMI_IBatt_C7 >= 0,   1,  -1)

data$Diversi <- ifelse(rowSums(data[, c("ID_C2", "ID_C4", "ID_C5", "ID_C7")] != data$ID_C2) > 0, 1, 0)

data <- data[data$Diversi != 1, ]

sum(data$Diversi)

#! ------------------- Add the Group column for each battery -------------------

#* C2
data$Derivata_C2  <- c(0, as.numeric(diff(data$HMI_IBatt_C2)) / as.numeric(diff(data$Timestamp)))
PeriodoC2         <- cumsum(c(0, diff(data$Timestamp) > 22))

# Group Counter
group <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C2[index] < 0 && data$ID_C2[index] != data$ID_C2[index-1]) || PeriodoC2[index] != PeriodoC2[index-1])) {
    group[index] <-  1
  }
  else {
    group[index] <-  0
  }
}
data$Group_C2 <- cumsum(group)
data <- subset(data, select = -Derivata_C2)

#* C4
data$Derivata_C4  <- c(0, as.numeric(diff(data$HMI_IBatt_C4)) / as.numeric(diff(data$Timestamp)))
PeriodoC4         <- cumsum(c(0, diff(data$Timestamp) > 22))

# Group Counter
group <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C4[index] < 0 && data$ID_C4[index] != data$ID_C4[index-1]) || PeriodoC4[index] != PeriodoC4[index-1])) {
    group[index] <-  1
  }
  else {
    group[index] <-  0
  }
}
data$Group_C4 <- cumsum(group)
data <- subset(data, select = -Derivata_C4)

#* C5
data$Derivata_C5  <- c(0, as.numeric(diff(data$HMI_IBatt_C5)) / as.numeric(diff(data$Timestamp)))
PeriodoC5         <- cumsum(c(0, diff(data$Timestamp) > 22))

# Group Counter
group <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C5[index] < 0 && data$ID_C5[index] != data$ID_C5[index-1]) || PeriodoC5[index] != PeriodoC5[index-1])) {
    group[index] <-  1
  }
  else {
    group[index] <-  0
  }
}
data$Group_C5 <- cumsum(group)
data <- subset(data, select = -Derivata_C5)

#* C7
data$Derivata_C7  <- c(0, as.numeric(diff(data$HMI_IBatt_C7)) / as.numeric(diff(data$Timestamp)))
PeriodoC7         <- cumsum(c(0, diff(data$Timestamp) > 22))

# Group Counter
group <- rep(0, length(data$Timestamp))

for (index in 2:(length(data$Timestamp))) {
  if (data$Diversi[index] != 1
      && ((data$Derivata_C7[index] < 0 && data$ID_C7[index] != data$ID_C7[index-1]) || PeriodoC7[index] != PeriodoC7[index-1])) {
    group[index] <-  1
  }
  else {
    group[index] <-  0
  }
}
data$Group_C7 <- cumsum(group)
data <- subset(data, select = -Derivata_C7)

# Check if the groups are the same for all batteries
sum(ifelse(rowSums(data[, c("Group_C2", "Group_C4", "Group_C5", "Group_C7")] != data$Group_C2) > 0, 1, 0))
data <- subset(data, select = -c(Group_C4, Group_C5, Group_C7))
# Rename the column Group_C2 to the new name Group
names(data)[which(names(data) == "Group_C2")] <- "Group"

#! ------------------- Select the good groups and export the new dataset -------------------

good_group <- rep(0, max(data$Group))
for (group in seq(0, max(data$Group))) {
  if (min(data[data$Group == group, "HMI_IBatt_C2"]) < 0 &&
      max(data[data$Group == group, "HMI_IBatt_C2"]) > 0) {
    good_group[group] <- 1;
  }
}
good_groups <- which(good_group == 1)

data_new <- data[data$Group %in% good_groups, ]

length(unique(data_new$Group))

# Save data frame to an RDS file
saveRDS(data_new, "battery_clean.rds")
