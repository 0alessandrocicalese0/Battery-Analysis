#! ---------------- Leggiamo il Dataset ------------------- 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(corrplot)

load("my_workspace_stat.RData")
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
boxplot(df_C2$Durata_scarica, df_C2$Wattora)

boxplot(Wattora ~ POC, data = df_C2, 
        xlab = "POC", ylab = "Durata_scarica", 
        main = "Boxplot di Durata_scarica in base a Wattora")

# Calcola la media e la deviazione standard di Wattora
mean_wattora <- mean(df_C2$Wattora)
sd_wattora <- sd(df_C2$Wattora)

outlier_limit <- 700
# Identifica gli outlier
outliers <- df_C2$Wattora > outlier_limit | df_C2$Wattora < -outlier_limit

# Stampa il numero di outlier
cat("Numero di outlier:", sum(outliers), "\n")

# Visualizza un boxplot prima dell'eliminazione degli outlier
boxplot(df_C2$Wattora, main = "Boxplot di Wattora (prima dell'eliminazione)")

# Elimina gli outlier dal dataframe
df_C2 <- df_C2[!outliers, ]

# Visualizza un boxplot dopo l'eliminazione degli outlier
boxplot(df_C2$Wattora, main = "Boxplot di Wattora (dopo l'eliminazione)")


#* PCA
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

par(mfrow=c(1,1)) #for a graph panel
biplot(pr.out, scale = 0)
 
biplot( pr.out,
scale = 0,
cex = .6,
col=c(alpha(1,0), "red")
)
abline(h = 0, v = 0, lty = 2)

#* Clustering
set.seed(1)

km.out <- kmeans(df_C2_scaled, 8, nstart = 20) 
km.out$tot.withinss
names(km.out)
km.out$cluster

par(mfrow=c(2,1)) #for a graph panel
plot( factor(km.out$cluster))
plot( df_C2$POC)



#* Regressione sugli Amperora
names(df_C2)
lm.fit <- lm( Amperora ~ . - Gruppo , data = df_C2)

library(car)
vif(lm.fit)

library(leaps)
regfit.full <- regsubsets(Amperora ~ . - Gruppo , data = df_C2) 
reg.summary <- summary(regfit.full)
reg.summary$outmat
