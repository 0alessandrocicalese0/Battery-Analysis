#! ---------------- Read the Dataset -------------------

library(tidyverse)
library(dplyr)
library(ggplot2)

load("workspace/my_workspace_project_clean.RData")
#save.image(file = "workspace/my_workspace_project_clean.RData")

data <- readRDS("battery_clean.rds")

# Find the groups where at least one observation has POC_ID != 0
groups_with_POC <- unique(data$Gruppo[data$POC_ID != 0])

# Create the subset and call it "groups_POC"
data <- data[data$Gruppo %in% groups_with_POC, ]

names(data)[names(data) == "Gruppo"] <- "Old_Groups"
data$Gruppo <- dense_rank(data$Old_Groups)
data <- subset(data, select = -c(Diversi, Old_Groups))

# Add the column of indices and seconds for each group
data <- data %>%
  mutate(Index = seq(1, n())) %>%
  select(Timestamp, Index, everything())

data <- data %>%
  group_by(Gruppo) %>%
  mutate(second = as.numeric(Timestamp - min(Timestamp), units = "secs")) %>%
  ungroup() %>%
  select(Timestamp, second, everything())

#! ---------------- Flag first value with Voltage > 25.5 V in each group --------------

# Initialize the Flag column to 0
data$Flag <- 0

# Loop through each group
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Find the index of the first match in the specific group
  first_match_index <- which(data$HMI_VBatt_C2 > 25.5 & data$Gruppo == group)[1]
  data$Flag[first_match_index] <- 1
}

#! ---------------- Add month column and season column --------------

data <- data %>%
  mutate(month = format(Timestamp, "%m"))

data <- data %>%
  mutate(season = case_when(
    between(as.numeric(month), 4, 5) | between(as.numeric(month), 10, 10) ~ "spring/autumn",
    between(as.numeric(month), 6, 9) ~ "summer",
    between(as.numeric(month), 11, 12) | between(as.numeric(month), 1, 3) ~ "winter",
    TRUE ~ NA_character_
  ))

#! ---------------- Calculate and add Discharge Duration column --------------

# Calculate the duration of the discharge phase in seconds
data$Discharge_Duration <- 0
for (group in seq(0, max(data$Gruppo) - 1)) {
   # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]
  min_discharge_index <- head(which(group_subset$HMI_IBatt_C2 < 0), 1)
  max_discharge_index <- tail(which(group_subset$HMI_IBatt_C2 < 0), 1)
  discharge_duration <- (group_subset$second[max_discharge_index] - group_subset$second[min_discharge_index])
  data$Discharge_Duration[data$Gruppo == group & data$ID_C2 == -1]  <- discharge_duration
}

#! ---------------- Calculate and add Ampere-hour column --------------

#* C2
data$Ah_C2 <- 0
# Loop through the groups
for (group in seq(0, max(data$Gruppo) - 1)) {

  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]
  # Calculate the delivered_capacity
  delivered_capacity_C2 <- cumsum(group_subset$HMI_IBatt_C2) * group_subset$Discharge_Duration / 3600
  #cat(delivered_capacity_C2,"\n")
  # Update the Ah column for the discharge phase
  data$Ah_C2[data$Gruppo == group & data$ID_C2 == -1] <- delivered_capacity_C2
}


#* C4
data$Ah_C4 <- 0
# Loop through the groups
for (group in seq(0, max(data$Gruppo) - 1)) {

  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]
  # Calculate the delivered_capacity
  delivered_capacity_C4 <- cumsum(group_subset$HMI_IBatt_C4) * group_subset$Discharge_Duration / 3600
  #cat(delivered_capacity_C4,"\n")
  # Update the Ah column for the discharge phase
  data$Ah_C4[data$Gruppo == group & data$ID_C4 == -1] <- delivered_capacity_C4
}

#* C5
data$Ah_C5 <- 0
# Loop through the groups
for (group in seq(0, max(data$Gruppo) - 1)) {

  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]
  # Calculate the delivered_capacity
  delivered_capacity_C5 <- cumsum(group_subset$HMI_IBatt_C5) * group_subset$Discharge_Duration / 3600
  #cat(delivered_capacity_C2,"\n")
  # Update the Ah column for the discharge phase
  data$Ah_C5[data$Gruppo == group & data$ID_C5 == -1] <- delivered_capacity_C5
}

#* C7
data$Ah_C7 <- 0
# Loop through the groups
for (group in seq(0, max(data$Gruppo) - 1)) {

  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]
  # Calculate the delivered_capacity
  delivered_capacity_C7 <- cumsum(group_subset$HMI_IBatt_C7) * group_subset$Discharge_Duration / 3600
  #cat(delivered_capacity_C7,"\n")
  # Update the Ah column for the discharge phase
  data$Ah_C7[data$Gruppo == group & data$ID_C7 == -1] <- delivered_capacity_C7
}

#! ---------------- Calculate and add Watt-hour column --------------
# Initialize a column for Watt-hours

#* C2
data$Wattora_C2 <- 0
# Loop through each group
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]

  # Find the index of the first and last negative value in HMI_IBatt_C2
  max_index <- tail(which(group_subset$HMI_IBatt_C2 < 0), 1)
  min_index <- head(which(group_subset$HMI_IBatt_C2 < 0), 1)

  # Calculate the phase duration in hours
  phase_duration <- (group_subset$second[max_index] - group_subset$second[min_index]) / 3600

  # Calculate Watt-hours for the group
  watt_hours_group <- -sum(group_subset$HMI_IBatt_C2[min_index:max_index] * group_subset$HMI_VBatt_C2[min_index:max_index]) * phase_duration

  # Assign the result to the Wattora column
  data$Wattora_C2[data$Gruppo == group] <- watt_hours_group
}

#* C4
data$Wattora_C4 <- 0
# Loop through each group
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]

  # Find the index of the first and last negative value in HMI_IBatt_C2
  max_index <- tail(which(group_subset$HMI_IBatt_C4 < 0), 1)
  min_index <- head(which(group_subset$HMI_IBatt_C4 < 0), 1)

  # Calculate the phase duration in hours
  phase_duration <- (group_subset$second[max_index] - group_subset$second[min_index]) / 3600

  # Calculate Watt-hours for the group
  watt_hours_group <- -sum(group_subset$HMI_IBatt_C4[min_index:max_index] * group_subset$HMI_VBatt_C4[min_index:max_index]) * phase_duration

  # Assign the result to the Wattora column
  data$Wattora_C4[data$Gruppo == group] <- watt_hours_group
}

#* C5
data$Wattora_C5 <- 0
# Loop through each group
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]

  # Find the index of the first and last negative value in HMI_IBatt_C2
  max_index <- tail(which(group_subset$HMI_IBatt_C5 < 0), 1)
  min_index <- head(which(group_subset$HMI_IBatt_C5 < 0), 1)

  # Calculate the phase duration in hours
  phase_duration <- (group_subset$second[max_index] - group_subset$second[min_index]) / 3600

  # Calculate Watt-hours for the group
  watt_hours_group <- -sum(group_subset$HMI_IBatt_C5[min_index:max_index] * group_subset$HMI_VBatt_C5[min_index:max_index]) * phase_duration

  # Assign the result to the Wattora column
  data$Wattora_C5[data$Gruppo == group] <- watt_hours_group
}

#* C7
data$Wattora_C7 <- 0
# Loop through each group
for (group in seq(1, max(data$Gruppo) - 1)) {
  # Filter the dataset for the specific group
  group_subset <- data[data$Gruppo == group, ]

  # Find the index of the first and last negative value in HMI_IBatt_C2
  max_index <- tail(which(group_subset$HMI_IBatt_C7 < 0), 1)
  min_index <- head(which(group_subset$HMI_IBatt_C7 < 0), 1)

  # Calculate the phase duration in hours
  phase_duration <- (group_subset$second[max_index] - group_subset$second[min_index]) / 3600

  # Calculate Watt-hours for the group
  watt_hours_group <- -sum(group_subset$HMI_IBatt_C7[min_index:max_index] * group_subset$HMI_VBatt_C7[min_index:max_index]) * phase_duration

  # Assign the result to the Wattora column
  data$Wattora_C7[data$Gruppo == group] <- watt_hours_group
}

#! ---------------- Calculate and add Power column --------------
data$Power_C2 <- data$HMI_IBatt_C2 * data$HMI_VBatt_C2
data$Power_C4 <- data$HMI_IBatt_C4 * data$HMI_VBatt_C4
data$Power_C5 <- data$HMI_IBatt_C5 * data$HMI_VBatt_C5
data$Power_C7 <- data$HMI_IBatt_C7 * data$HMI_VBatt_C7

#! ---------------- Functions to generate plots -------------------
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
    geom_hline(yintercept = 25.5, linetype = "dashed", color = "red") +
    labs(title = paste("Observation", group), x = "Time(s)", y = "Voltage (V)") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery")
}

generate_plot_P <- function(group) {
  group_data <- data[data$Gruppo == group, ]

  ggplot(data = group_data, aes(x = second)) +
    geom_line(aes(y = Power_C2, color = "1"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = Power_C4, color = "2"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = Power_C5, color = "3"), linetype = "solid", linewidth=1) +
    geom_line(aes(y = Power_C7, color = "4"), linetype = "solid", linewidth=1) +
    #geom_hline(yintercept = 25.25, linetype = "dashed", color = "red") +
    labs(title = paste("Observation", group), x = "Time(s)", y = "Power (W)") +
    scale_color_manual(values = c("blue", "green", "orange", "purple"), name = "Battery")
}
#! ---------------- Create table for C2 groups -----------
# Fill the new dataset
df_C2 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)

# Group
df_C2$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  group_subset <- data[data$Gruppo == group & data$HMI_IBatt_C2 < 0, ]

  # Initial Timestamp
  df_C2$Timestamp_iniziale[group] <- as.character(group_subset$Timestamp[1])

  # Initial, final, and average I
  df_C2$I_finale[group] <- tail(group_subset$HMI_IBatt_C2[group_subset$HMI_IBatt_C2 < 0], 1)
  df_C2$I_iniziale[group] <- head(group_subset$HMI_IBatt_C2[group_subset$HMI_IBatt_C2 < 0], 1)
  df_C2$I_media[group] <- mean(group_subset$HMI_IBatt_C2[group_subset$HMI_IBatt_C2 < 0])

  # Initial, final, and average V
  df_C2$V_finale[group] <- tail(group_subset$HMI_VBatt_C2[group_subset$HMI_IBatt_C2 < 0], 1)
  df_C2$V_iniziale[group] <- head(group_subset$HMI_VBatt_C2[group_subset$HMI_IBatt_C2 < 0], 1)
  df_C2$V_media[group] <- mean(group_subset$HMI_VBatt_C2[group_subset$HMI_IBatt_C2 < 0])

  # Discharge duration
  df_C2$Discharge_Duration[group] <- group_subset$Discharge_Duration[1]

  # Month
  df_C2$Mese[group] <- group_subset$month[1]

  # Season
  df_C2$Stagione[group] <- group_subset$season[1]

  # Watt-hour
  df_C2$Wattora[group] <- group_subset$Wattora_C2[1]

  # Ampere-hour
  df_C2$Amperora[group] <- group_subset$Ah_C2[1]

  # POC
  df_C2$POC[group] <- group_subset$POC_ID[group_subset$POC_ID != 0 & group_subset$HMI_IBatt_C2 < 0][1]
}

df_C2$Timestamp_iniziale <- as.POSIXct(df_C2$Timestamp_iniziale)
# Remove rows from df_C2 where POC is NA
df_C2 <- na.omit(df_C2)
#
# # Set the base language to English
# Sys.setlocale("LC_TIME", "C")
#
# plot(df_C2$Timestamp_iniziale, df_C2$V_iniziale, type = "line", col = "blue", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 1")
#
#
# plot(df_C2$Timestamp_iniziale, df_C2$V_iniziale, pch = 16, col = "blue", cex = .5,
#      xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 1")

#df_C2 <- subset(df_C2, select = -Timestamp_iniziale)


#! ---------------- Create table for C4 groups -----------
# Fill the new dataset
df_C4 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)

# Group
df_C4$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  group_subset <- data[data$Gruppo == group & data$HMI_IBatt_C4 < 0, ]

  # Initial Timestamp
  df_C4$Timestamp_iniziale[group] <- as.character(group_subset$Timestamp[1])

  # Initial, final, and average I
  df_C4$I_finale[group] <- tail(group_subset$HMI_IBatt_C4[group_subset$HMI_IBatt_C4 < 0], 1)
  df_C4$I_iniziale[group] <- head(group_subset$HMI_IBatt_C4[group_subset$HMI_IBatt_C4 < 0], 1)
  df_C4$I_media[group] <- mean(group_subset$HMI_IBatt_C4[group_subset$HMI_IBatt_C4 < 0])

  # Initial, final, and average V
  df_C4$V_finale[group] <- tail(group_subset$HMI_VBatt_C4[group_subset$HMI_IBatt_C4 < 0], 1)
  df_C4$V_iniziale[group] <- head(group_subset$HMI_VBatt_C4[group_subset$HMI_IBatt_C4 < 0], 1)
  df_C4$V_media[group] <- mean(group_subset$HMI_VBatt_C4[group_subset$HMI_IBatt_C4 < 0])

  # Discharge duration
  df_C4$Discharge_Duration[group] <- group_subset$Discharge_Duration[1]

  # Month
  df_C4$Mese[group] <- group_subset$month[1]

  # Season
  df_C4$Stagione[group] <- group_subset$season[1]

  # Watt-hour
  df_C4$Wattora[group] <- group_subset$Wattora_C4[1]

  # Ampere-hour
  df_C4$Amperora[group] <- group_subset$Ah_C4[1]

  # POC
  df_C4$POC[group] <- group_subset$POC_ID[group_subset$POC_ID != 0 & group_subset$HMI_IBatt_C4 < 0][1]
}

df_C4$Timestamp_iniziale <- as.POSIXct(df_C4$Timestamp_iniziale)
# Remove rows from df_C4 where POC is NA
df_C4 <- na.omit(df_C4)

# # Set the base language to English
# Sys.setlocale("LC_TIME", "C")
#
# # Line Plot - Battery 4
# plot(df_C4$Timestamp_iniziale, df_C4$V_iniziale, type = "line", col = "green", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 2")
#
# # Scatter Plot - Battery 4
# plot(df_C4$Timestamp_iniziale, df_C4$V_iniziale, pch = 16, col = "green", cex = .5,
#      xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 2")

# Remove the Timestamp_iniziale column
df_C4 <- subset(df_C4, select = -Timestamp_iniziale)


#! ---------------- Create table for C5 groups -----------
# Fill the new dataset
df_C5 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)

# Group
df_C5$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  group_subset <- data[data$Gruppo == group & data$HMI_IBatt_C5 < 0, ]

  # Initial Timestamp
  df_C5$Timestamp_iniziale[group] <- as.character(group_subset$Timestamp[1])

  # Initial, final, and average I
  df_C5$I_finale[group] <- tail(group_subset$HMI_IBatt_C5[group_subset$HMI_IBatt_C5 < 0], 1)
  df_C5$I_iniziale[group] <- head(group_subset$HMI_IBatt_C5[group_subset$HMI_IBatt_C5 < 0], 1)
  df_C5$I_media[group] <- mean(group_subset$HMI_IBatt_C5[group_subset$HMI_IBatt_C5 < 0])

  # Initial, final, and average V
  df_C5$V_finale[group] <- tail(group_subset$HMI_VBatt_C5[group_subset$HMI_IBatt_C5 < 0], 1)
  df_C5$V_iniziale[group] <- head(group_subset$HMI_VBatt_C5[group_subset$HMI_IBatt_C5 < 0], 1)
  df_C5$V_media[group] <- mean(group_subset$HMI_VBatt_C5[group_subset$HMI_IBatt_C5 < 0])

  # Discharge duration
  df_C5$Discharge_Duration[group] <- group_subset$Discharge_Duration[1]

  # Month
  df_C5$Mese[group] <- group_subset$month[1]

  # Season
  df_C5$Stagione[group] <- group_subset$season[1]

  # Watt-hour
  df_C5$Wattora[group] <- group_subset$Wattora_C5[1]

  # Ampere-hour
  df_C5$Amperora[group] <- group_subset$Ah_C5[1]

  # POC
  df_C5$POC[group] <- group_subset$POC_ID[group_subset$POC_ID != 0 & group_subset$HMI_IBatt_C5 < 0][1]
}

df_C5$Timestamp_iniziale <- as.POSIXct(df_C5$Timestamp_iniziale)
# Remove rows from df_C5 where POC is NA
df_C5 <- na.omit(df_C5)

# # Set the base language to English
# Sys.setlocale("LC_TIME", "C")
#
# # Line Plot - Battery C5
# plot(df_C5$Timestamp_iniziale, df_C5$V_iniziale, type = "line", col = "orange", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 3")
#
# # Scatter Plot - Battery C5
# plot(df_C5$Timestamp_iniziale, df_C5$V_iniziale, pch = 16, col = "orange", cex = .5,
#      xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 3")

# Remove the Timestamp_iniziale column
df_C5 <- subset(df_C5, select = -Timestamp_iniziale)


#! ---------------- Create table for C7 groups -----------
# Fill the new dataset
df_C7 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)

# Group
df_C7$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  group_subset <- data[data$Gruppo == group & data$HMI_IBatt_C7 < 0, ]

  # Initial Timestamp
  df_C7$Timestamp_iniziale[group] <- as.character(group_subset$Timestamp[1])

  # Initial, final, and average I
  df_C7$I_finale[group] <- tail(group_subset$HMI_IBatt_C7[group_subset$HMI_IBatt_C7 < 0], 1)
  df_C7$I_iniziale[group] <- head(group_subset$HMI_IBatt_C7[group_subset$HMI_IBatt_C7 < 0], 1)
  df_C7$I_media[group] <- mean(group_subset$HMI_IBatt_C7[group_subset$HMI_IBatt_C7 < 0])

  # Initial, final, and average V
  df_C7$V_finale[group] <- tail(group_subset$HMI_VBatt_C7[group_subset$HMI_IBatt_C7 < 0], 1)
  df_C7$V_iniziale[group] <- head(group_subset$HMI_VBatt_C7[group_subset$HMI_IBatt_C7 < 0], 1)
  df_C7$V_media[group] <- mean(group_subset$HMI_VBatt_C7[group_subset$HMI_IBatt_C7 < 0])

  # Discharge duration
  df_C7$Discharge_Duration[group] <- group_subset$Discharge_Duration[1]

  # Month
  df_C7$Mese[group] <- group_subset$month[1]

  # Season
  df_C7$Stagione[group] <- group_subset$season[1]

  # Watt-hour
  df_C7$Wattora[group] <- group_subset$Wattora_C7[1]

  # Ampere-hour
  df_C7$Amperora[group] <- group_subset$Ah_C7[1]

  # POC
  df_C7$POC[group] <- group_subset$POC_ID[group_subset$POC_ID != 0 & group_subset$HMI_IBatt_C7 < 0][1]
}

df_C7$Timestamp_iniziale <- as.POSIXct(df_C7$Timestamp_iniziale)
# Remove rows from df_C7 where POC is NA
df_C7 <- na.omit(df_C7)

# # Set the base language to English
# Sys.setlocale("LC_TIME", "C")
#
# # Line Plot - Battery C7
# plot(df_C7$Timestamp_iniziale, df_C7$V_iniziale, type = "line", col = "purple", xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Line Plot - Battery 4")
#
# # Scatter Plot - Battery C7
# plot(df_C7$Timestamp_iniziale, df_C7$V_iniziale, pch = 16, col = "purple", cex = .5,
#      xlab = "Initial Timestamp", ylab = "Initial Voltage", main = "Scatter Plot - Battery 4")

# Remove the Timestamp_iniziale column
df_C7 <- subset(df_C7, select = -Timestamp_iniziale)


#! ---------------- Group division based on POC ----------------

# Create a vector for each POC
# groups_POC_1 <- df_C2$Gruppo[df_C2$POC == "POC_1"]
# groups_POC_2 <- df_C2$Gruppo[df_C2$POC == "POC_2"]
# groups_POC_3 <- df_C2$Gruppo[df_C2$POC == "POC_3"]
# groups_POC_4 <- df_C2$Gruppo[df_C2$POC == "POC_4"]
# groups_POC_5 <- df_C2$Gruppo[df_C2$POC == "POC_5"]
# groups_POC_6 <- df_C2$Gruppo[df_C2$POC == "POC_6"]
# groups_POC_7 <- df_C2$Gruppo[df_C2$POC == "POC_7"]
# groups_POC_8 <- df_C2$Gruppo[df_C2$POC == "POC_8"]

# Generate plots for only POC_4
# plot_list <- lapply(groups_POC_7[1:5], function(gr) generate_plot_V(gr))
# grid.arrange(grobs = plot_list, ncol = 5, top = "POC_7")

saveRDS(df_C2, "df_C2_C7/df_C2.rds")
saveRDS(df_C4, "df_C2_C7/df_C4.rds")
saveRDS(df_C5, "df_C2_C7/df_C5.rds")
saveRDS(df_C7, "df_C2_C7/df_C7.rds")


#! ---------------- Charging phase for C2 -----------
# Fill the new dataset
charging_C2 <- data.frame(Gruppo = unique(data$Gruppo), stringsAsFactors = FALSE)

# Group
charging_C2$Gruppo <- unique(data$Gruppo)

for (group in seq(1, max(data$Gruppo) - 1)) {
  group_subset <- data[data$Gruppo == group & data$HMI_IBatt_C2 > 0, ]

  # Initial Timestamp
  charging_C2$Timestamp_iniziale[group] <- as.character(group_subset$Timestamp[1])

  # Initial, final, and average I
  charging_C2$I_finale[group] <- tail(group_subset$HMI_IBatt_C2[group_subset$HMI_IBatt_C2 > 0], 1)
  charging_C2$I_iniziale[group] <- head(group_subset$HMI_IBatt_C2[group_subset$HMI_IBatt_C2 > 0], 1)
  charging_C2$I_media[group] <- mean(group_subset$HMI_IBatt_C2[group_subset$HMI_IBatt_C2 > 0])

  # Initial, final, and average V
  charging_C2$V_finale[group] <- tail(group_subset$HMI_VBatt_C2[group_subset$HMI_IBatt_C2 > 0], 1)
  charging_C2$V_iniziale[group] <- head(group_subset$HMI_VBatt_C2[group_subset$HMI_IBatt_C2 > 0], 1)
  charging_C2$V_media[group] <- mean(group_subset$HMI_VBatt_C2[group_subset$HMI_IBatt_C2 > 0])

  # Discharge duration
  charging_C2$Discharge_Duration[group] <- group_subset$Discharge_Duration[1]

  # Month
  charging_C2$Mese[group] <- group_subset$month[1]

  # Season
  charging_C2$Stagione[group] <- group_subset$season[1]

  # Watt-hour
  charging_C2$Wattora[group] <- group_subset$Wattora_C2[1]

  # Ampere-hour
  charging_C2$Amperora[group] <- group_subset$Ah_C2[1]
}

saveRDS(charging_C2, "df_C2_C7/charging_C2.rds")
