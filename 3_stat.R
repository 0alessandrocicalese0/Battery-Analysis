#! ---------------- Leggiamo il Dataset ------------------- 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(corrplot)
library(reshape2)

#load("workspace/my_workspace_stat.RData")
#save.image(file = "workspace/my_workspace_stat.RData")

df_C2 <- readRDS("df_C2_C7/df_C2.rds")
df_C4 <- readRDS("df_C2_C7/df_C4.rds")
df_C5 <- readRDS("df_C2_C7/df_C5.rds")
df_C7 <- readRDS("df_C2_C7/df_C7.rds")
# Abbiamo 957 gruppi

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
df_C2$Mese <- as.numeric(df_C2$Mese)

#! ---------------- Divisione dei gruppi rispetto ai POC ----------------
# gruppi_POC_1 <- df_C2$Gruppo[df_C2$POC == "1"]
# gruppi_POC_2 <- df_C2$Gruppo[df_C2$POC == "2"]
# gruppi_POC_3 <- df_C2$Gruppo[df_C2$POC == "3"]
# gruppi_POC_4 <- df_C2$Gruppo[df_C2$POC == "4"]
# gruppi_POC_5 <- df_C2$Gruppo[df_C2$POC == "5"]
# gruppi_POC_6 <- df_C2$Gruppo[df_C2$POC == "6"]
# gruppi_POC_7 <- df_C2$Gruppo[df_C2$POC == "7"]
# gruppi_POC_8 <- df_C2$Gruppo[df_C2$POC == "8"]

#! ---------------- corr_matrix e matrix plot -------------

# Calcola la matrice di correlazione delle variabili numeriche nel dataframe df_C2
corr_matrix <- df_C2 %>% 
  dplyr::select(where(is.numeric)) %>%  
  # Rimuovi alcune colonne non necessarie per la correlazione
  select(-Gruppo, -Amperora, -Wattora, -POC) %>% #, -Stagione
  as.matrix() %>%
  cor()

# Visualizza la matrice di correlazione utilizzando la funzione corrplot
corrplot(corr_matrix)


library(GGally) # Carica la libreria GGally

# Crea un matrix plot che mostra scatter plots per tutte le coppie di variabili nel subset del dataframe
ggpairs(subset(df_C2, select = -c(Gruppo, Wattora, Amperora)))


#!----------------- Boxplot--------------
# Calcola la media e la deviazione standard di Wattora
#mean_wattora <- mean(df_C2$Wattora)
#sd_wattora <- sd(df_C2$Wattora)

#* Visualizza un boxplot prima di rimuovere gli outlier
boxplot(Wattora ~ POC, data = df_C2, 
        main = "Boxplot Wattora Stratified by POC (before removing outliers)",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()

#* Visualizza un boxplot dopo l'eliminazione degli outlier (Wh>5000)

outlier_limit <- 5000
# Identifica gli outlier
outliers <- df_C2$Wattora > outlier_limit | df_C2$Wattora < -outlier_limit
cat("Numero di outlier:", sum(outliers), "\n")

# Elimina gli outlier dal dataframe
df_C2 <- df_C2[!outliers, ]

# Visualizza il boxplot 
boxplot(Wattora ~ POC, data = df_C2, 
        main = "Boxplot Wattora Stratified by POC (removing observations with Wh>5000)",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()

#* Visualizza un boxplot rimuovere gli outlier (Wh>1000)
#* 
outlier_limit <- 1000
# Identifica gli outlier
outliers <- df_C2$Wattora > outlier_limit | df_C2$Wattora < -outlier_limit
cat("Numero di outlier:", sum(outliers), "\n")

# Elimina gli outlier dal dataframe
df_C2 <- df_C2[!outliers, ] # ci rimangono 930 gruppi

# Visualizza il boxplot
boxplot(Wattora ~ POC, data = df_C2, 
        main = "Boxplot Wattora Stratified by POC (removing observations with Wh>1000)",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()


#! ---------------- PCA  -----------
 
# Rendiamo stagione, mese e poc  variabili di tipo categorico usando "factor"
df_C2$POC <- as.factor(df_C2$POC)
df_C2$Stagione <- as.factor(df_C2$Stagione)
df_C2$Mese <- as.factor(df_C2$Mese)

# Creiamo una funzione per rappresentare i risultati del clustering in base al poc
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

# Eseguiamo la PCA sul subset del dataframe df_C2, escludendo alcune colonne specifiche e standardizzando le variabili
pr.out <- prcomp(subset(df_C2, select = -c(Gruppo, POC, Stagione,
                                           Mese, Wattora, Amperora)), scale = TRUE)

# Estaiamo il vettore di caricamento (rotation) per la prima variabile principale
pr.out$rotation[, 1]

# Visualizziamo i nomi delle variabili nel dataframe df_C2
names(pr.out)

# Calcoliamo la frazione di varianza spiegata dalle prime quattro componenti principali
varianza_spiegata <- sum(pr.out$sdev[1:4]^2) / sum(pr.out$sdev^2)  # 90% di varianza spiegata

# Visualizziamo il plot delle componenti principali
plot(pr.out)

# Crea un panel con due grafici: uno per il plot della frazione di varianza spiegata e uno per il plot della varianza cumulata
par(mfrow=c(1,2))

# Calcola la frazione di varianza spiegata e crea uno scree plot
fvs <- pr.out$sdev^2 / sum(pr.out$sdev^2)
plot(
  fvs,
  xlab = "PC",
  ylab = "Fraction of variance explained",
  main = "Scree plot",
  ylim = c(0, 1),
  type = 'b'
)

# Calcola la varianza cumulata e crea uno scree plot
plot(
  cumsum(fvs),
  xlab = "PC",
  ylab = "Cumulative fraction of variance explained",
  ylim = c(0, 1),
  type = 'b'
)

# Resetta il layout dei grafici a una singola griglia
par(mfrow=c(1,1))

# Crea un biplot delle prime due componenti principali per le variabili I, V, Durata
biplot(pr.out, scale = 1)

# Crea un biplot con personalizzazioni aggiuntive, incluso il colore rosso per le frecce delle variabili
biplot(
  pr.out,
  scale = 1,
  cex = 0.9,
  col = c(alpha(1, 0), "red")
)

#abline(h = 0, v = 0, lty = 2)

#! ---------------- Clustering ------------ 

set.seed(1)# Impostiamo un seme per la riproducibilità

library(cluster)# Carichiamo la libreria 'cluster'

k_seq <- 2:10 # Definiamo una sequenza di possibili valori di K per il clustering
silhouette_vec <- numeric(length(k_seq)) # Inizializziamo un vettore per memorizzare le misure di Silhouette per ogni K
# Iteriamo su diversi valori di K
for (kk in seq_along(k_seq)) {
  X <- subset(df_C2, select = -c(Gruppo, POC, Stagione, Mese, Wattora, Amperora))# Selezioniamo le colonne numeriche del dataframe escludendo alcune colonne specifiche
  km_out <- kmeans(X, centers = k_seq[kk], nstart = 100)   # Eseguiamo il clustering K-means con il valore corrente di K
  cluster_kk <- km_out$cluster  # Otteniamo i cluster assegnati a ciascuna osservazione
  sil <- silhouette(cluster_kk, dist = dist(X)) # Calcoliamo la misura di Silhouette per valutare la coesione e la separazione dei cluster
  silhouette_vec[kk] <- summary(sil)$avg.width  # Memorizziamo la media delle larghezze di Silhouette
}
# Plot della misura di Silhouette in funzione del numero di cluster K
plot(k_seq, silhouette_vec, type = "l", xlab = "K", ylab = "Silhouette (average)", main = "Cluster with POC 1-8") 
points(k_seq[which.max(silhouette_vec)], max(silhouette_vec), col = "red", pch = 16)
grid()

# Ripetiamo il processo considerando solo i POC 7 e 8
k_seq <- 2:10 
silhouette_vec <- numeric(length(k_seq)) 
for (kk in seq_along(k_seq)) {
  ii <- k_seq[kk]
  X <- subset(df_C2, select = -c(Gruppo, Wattora, Amperora))#, POC, Stagione, Mese))
  X <- X[df_C2$POC %in% c(7, 8), ]
  km_out <- kmeans(X, centers = ii, nstart = 100) 
  cluster_kk <- km_out$cluster
  sil <- silhouette(cluster_kk, dist = dist(X)) 
  silhouette_vec[kk] <- summary(sil)$avg.width
}
# Plot della misura di Silhouette in funzione del numero di cluster K per POC 7-8
plot(k_seq, silhouette_vec, type = "l", xlab = "K", ylab = "Silhouette (average)", main = "Cluster with POC 7-8") 
points(k_seq[which.max(silhouette_vec)], max(silhouette_vec), col = "red", pch = 16)
grid()





# Eseguiamo il clustering K-means con K=3 sul dataframe completo
km.out <- kmeans(df_C2, 3, nstart = 100) 

km.out$tot.withinss # Visualizziamo la somma della varianza intra-cluster totale
names(km.out)# Visualizziamo i nomi degli oggetti restituiti dall'output di kmeans
km.out$cluster # Visualizziamo i cluster assegnati alle osservazioni

df_C2$Cluster <- as.factor(km.out$cluster)

library(ggplot2)
# Visualizza un grafico a dispersione per V con colori differenti per i cluster
ggplot(df_C2, aes(x = Mese, y = V_iniziale, color = Cluster)) +
  geom_point() +
  labs(title = "Scatter Plot Cluster - Initial Voltage") +
  xlab("Month") +
  ylab("Initial Voltage") +
  theme_minimal()

# Visualizza un grafico a dispersione per I con colori differenti per i cluster
ggplot(df_C2, aes(x = Mese, y = I_iniziale, color = Cluster)) +
  geom_point() +
  labs(title = "Scatter Plot Cluster - Initial Intensity") +
  xlab("Month") +
  ylab("Initial intensity") +
  theme_minimal()

# 
# # Visualizza un grafico a dispersione con colori differenti per i POC
# ggplot(df_C2, aes(x = Mese, y = V_iniziale, color = POC)) +
#   geom_point() +
#   labs(title = "Scatter Plot POC - Initial Voltage") +
#   xlab("Month") +
#   ylab("Initial Voltage") +
#   theme_minimal()


# Visualizza un grafico a barre che mostra la distribuzione dei cluster
ggplot(df_C2, aes(x = Cluster)) +
  geom_bar() +
  labs(title = "Clusters' Distribution") +
  xlab("Cluster") +
  ylab("Number of Observations") +
  theme_minimal()


# Visualizza un boxplot per ogni cluster
ggplot(df_C2, aes(x = Cluster, y = V_iniziale, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Boxplot for Clusters") +
  xlab("Cluster") +
  ylab("Initial Voltage") +
  theme_minimal()


# Impostiamo il layout a 2 righe e 1 colonna per i prossimi grafici
#par(mfrow=c(2,1))

#plot(factor(km.out$cluster))# Plot della variabile cluster assegnata da K-means

#plot(df_C2$POC)# Plot della variabile POC nel dataframe df_C2

par(pty="s") # Ripristiniamo il layout di default dei grafici




# Eseguiamo il clustering K-means con K=2 sul dataframe contenente solo POC 7 e 8
km.out <- kmeans(X, 2, nstart = 20) 

km.out$tot.withinss # Visualizziamo la somma della varianza intra-cluster totale
names(km.out)# Visualizziamo i nomi degli oggetti restituiti dall'output di kmeans
km.out$cluster # Visualizziamo i cluster assegnati alle osservazioni


# Aggiungi le assegnazioni dei cluster al dataframe

X$Cluster <- as.factor(km.out$cluster)

library(ggplot2)
# Visualizza un grafico a dispersione con colori differenti per i cluster
ggplot(X, aes(x = Stagione, y = V_iniziale, color = Cluster)) +
  geom_point() +
  labs(title = "Scatter Plot Cluster - Initial Voltage") +
  xlab("Season") +
  ylab("Initial Voltage") +
  theme_minimal()

# Visualizza un grafico a dispersione con colori differenti per i POC
# ggplot(X, aes(x = Stagione, y = V_iniziale, color = POC)) +
#   geom_point() +
#   labs(title = "Scatter Plot POC - Initial Voltage") +
#   xlab("Season") +
#   ylab("Initial Voltage") +
#   theme_minimal()


# Visualizza un grafico a barre che mostra la distribuzione dei cluster
ggplot(X, aes(x = Cluster)) +
  geom_bar() +
  labs(title = "Distribuzione dei Cluster") +
  xlab("Cluster") +
  ylab("Number of Observations") +
  theme_minimal()



# Eseguiamo il clustering K-means sugli score delle prime due PC ottenute da un'analisi PCA
km.out.scores <- kmeans(scores_PC1_2, 2, nstart = 50)

# Aggiorniamo la colonna clusters.k.means.PCA nel dataframe valori
valori$clusters.k.means.PCA <- km.out.scores$cluster

# Calcoliamo il numero di osservazioni che differiscono tra i due metodi di clustering
sum(valori$clusters != valori$clusters.k.means.PCA)

# Visualizziamo uno scatter plot delle prime due PC, colorando le osservazioni in base ai cluster assegnati da K-means
plot(PC2 ~ PC1, data = scores_PC1_2, type = "n", col = clusters(m2), ylim = c(-6,4))
etich = abbreviate(1:valori$ID, minlength = 2)
text(scores_PC1_2$PC1, scores_PC1_2$PC2, labels = etich, col = valori$clusters.k.means.PCA)
abline(parameters(m2)[1:2, 1], lty = 3)
abline(parameters(m2)[1:2, 2], lty = 3)
abline(h = 0, v = 0, lty = 2)











#! ---------------- Regressione ------------------
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
