#! ---------------- Leggiamo il Dataset ------------------- 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(corrplot)

#load("my_workspace_stat.RData")
#save.image(file = "my_workspace_stat.RData")

df_C2 <- readRDS("df_C2.rds")

df_C2 <- df_C2 %>% 
  mutate(Stagione = case_when(
    Stagione == "inverno" ~ 0,
    Stagione == "estate" ~ 1,
    Stagione == "primavera/autunno" ~ 2,
  ))

df_C2 <- df_C2 %>% 
  mutate(POC = case_when(
        grepl("1", POC) ~ 1,
        grepl("2", POC) ~ 2,
        grepl("3", POC) ~ 3,
        grepl("4", POC) ~ 4,
        grepl("5", POC) ~ 5,
        grepl("6", POC) ~ 6,
        grepl("7", POC) ~ 7,
        grepl("8", POC) ~ 8,
        TRUE ~ as.numeric(POC)  # Mantieni i valori originali se non corrispondono a nessuno dei casi sopra
      ))

#! ---------------- PCA e Clustering -----------
corr_matrix <- df_C2 %>% 
  dplyr::select(where(is.numeric)) %>% 
  as.matrix() %>%
  cor()

corrplot(corr_matrix) 

# my_matrix <- df_C2 %>% 
#   mutate(Stagione = case_when(
#     Stagione == "inverno" ~ 0,
#     Stagione == "estate" ~ 1,
#     Stagione == "primavera/autunno" ~ 2,
#   ))  %>% 
#   mutate(POC_ID = case_when(
#     grepl("1", POC_ID) ~ 1,
#     grepl("2", POC_ID) ~ 2,
#     grepl("3", POC_ID) ~ 3,
#     grepl("4", POC_ID) ~ 4,
#     grepl("5", POC_ID) ~ 5,
#     grepl("6", POC_ID) ~ 6,
#     grepl("7", POC_ID) ~ 7,
#     grepl("8", POC_ID) ~ 8,
#     TRUE ~ as.numeric(POC_ID)  # Mantieni i valori originali se non corrispondono a nessuno dei casi sopra
#   ))


pr.out <- prcomp(subset(df_C2, select = -Gruppo), scale = TRUE)
pr.out$rotation[,1]

names(pr.out)
varianza_spiegata <-sum(pr.out$sdev[1:4]^2) / sum(pr.out$sdev^2)  # 90% di varianza spiegata

library(GGally)
df_C2_scaled <- as.data.frame(scale(df_C2))  # Scale the data frame
ggpairs(df_C2_scaled)
plot(pr.out)

par(mfrow=c(1,2)) #for a graph panel
fvs <- pr.out$sdev^2/sum(pr.out$sdev^2) 
plot(
  fvs,
  xlab = " PC ",
  ylab = " Fraction of variance explained",
  main= "scree plot",
  ylim = c(0, 1) ,
  type = 'b') #scree plot

plot(
  cumsum(fvs) ,
  xlab = " PC ",
  ylab = " Cumulative fraction of variance explained", 
  ylim = c(0, 1) ,
  type = 'b')

# df_C2[df_C2$Wattora>700,]
# sum(df_C2$Wattora>700)
# 
# # PCA e Clustering solo ai gruppi riferiti ai POC
