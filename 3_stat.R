#! ---------------- Leggiamo il Dataset ------------------- 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(corrplot)
library(reshape2)

#load("my_workspace_stat.RData")
#save.image(file = "my_workspace_stat.RData")

df_C2 <- readRDS("df_C2.rds")
df_C4 <- readRDS("df_C4.rds")
df_C5 <- readRDS("df_C5.rds")
df_C7 <- readRDS("df_C7.rds")

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

# Convertiamo in variabili numeriche quelle categoriche
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

#! ----------------corr_matrix -------------
corr_matrix <- df_C2 %>% 
  dplyr::select(where(is.numeric)) %>%  
  select(-Gruppo, -Amperora, -Wattora, -POC, -Stagione)%>%
  as.matrix() %>%
  cor()

corrplot(corr_matrix)

#!---------------boxplot--------------

# Calcola la media e la deviazione standard di Wattora
mean_wattora <- mean(df_C2$Wattora)
sd_wattora <- sd(df_C2$Wattora)

outlier_limit <- 1000
# Identifica gli outlier
outliers <- df_C2$Wattora > outlier_limit | df_C2$Wattora < -outlier_limit

# Stampa il numero di outlier
cat("Numero di outlier:", sum(outliers), "\n")


# Visualizza un boxplot con colori diversi
boxplot(Wattora ~ POC, data = df_C2, 
        main = "Boxplot Wattora Stratified by POC (before removing outliers)",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()

# Elimina gli outlier dal dataframe
df_C2 <- df_C2[!outliers, ]

# Visualizza un boxplot dopo l'eliminazione degli outlier
boxplot(Wattora ~ POC, data = df_C2, 
        main = "Boxplot Wattora Stratified by POC (after removing the outliers)",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()



#! ---------------- PCA e Clustering -----------
 
# Rendiamo stagione e poc  variabili di tipo categorico usando "factor"
PCA_strati_POC <- function(poc) {
  dati <- df_C2[df_C2$POC == poc,]
  dati <- subset(dati, select = -c(Gruppo, POC, Stagione, Mese, Wattora, Amperora))
  
  pr.out <- prcomp(dati)#, scale = TRUE)
  biplot(pr.out,
         scale = 1,
         cex = 0.9,
         col = c(alpha(1, 0), "red")
  )
}



df_C2$POC <- as.factor(df_C2$POC)
df_C2$Stagione <- as.factor(df_C2$Stagione)

str(df_C2)



pr.out <- prcomp(subset(df_C2, select = -c(Gruppo, POC, Stagione, Mese, Wattora, Amperora)), scale = TRUE)

pr.out$rotation[,1]

names(pr.out)
varianza_spiegata <-sum(pr.out$sdev[1:4]^2) / sum(pr.out$sdev^2)  # 90% di varianza spiegata

library(GGally)
#df_C2_scaled <- as.data.frame(scale(df_C2))  # Scale the data frame
ggpairs(subset(df_C2, select = -c(Gruppo, POC, Stagione, Mese, Wattora, Amperora)))
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
scale = 1,
cex = .9,
col=c(alpha(1,0), "red")
)
abline(h = 0, v = 0, lty = 2)

#! ---------- Clustering ------------ 
set.seed(1)

km.out <- kmeans(df_C2_scaled, 8, nstart = 20) 
km.out$tot.withinss
names(km.out)
km.out$cluster

par(mfrow=c(2,1)) #for a graph panel
plot( factor(km.out$cluster))
plot( df_C2$POC)

#kmeans sugli score delle prime due PCs
km.out.scores <- kmeans (scores_PC1_2,2, nstart =50)
valori$clusters.k.means.PCA <- km.out.scores$cluster
sum(valori$clusters!=valori$clusters.k.means.PCA)

par(pty="s")
plot(PC2 ~ PC1, data = scores_PC1_2, type = "n", col=clusters(m2), ylim=c(-6,4))
etich = abbreviate(1:valori$ID, minlength = 2)
text(scores_PC1_2$PC1, scores_PC1_2$PC2, labels = etich, col=valori$clusters.k.means.PCA)
abline(parameters(m2)[1:2, 1],lty=3)
abline(parameters(m2)[1:2, 2],lty=3)
abline(h = 0, v = 0,lty=2)


#* Regressione sugli Amperora
names(df_C2)
lm.fit <- lm( Amperora ~ . - Gruppo, data = df_C2)

library(car)
vif(lm.fit)

library(leaps)
regfit.full <- regsubsets(Amperora ~ . - Gruppo , data = df_C2) 
reg.summary <- summary(regfit.full)
reg.summary$outmat

# regressione: volt in carica(regressore) e ampere in carica(risposta)
# prendiamo il residuo stratificato per poc, lo utilizzo come variabile aggiuntiva
# analizzare fase carica col residuo(1, non stratificato per poc; 2, stratificato per poc, 3 non stratificato per poc utilizzando solo 7 e 8)
