#! ---------------- Read the Dataset -------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(corrplot)
library(reshape2)
#load("workspace/my_workspace_stat.RData")
#save.image(file = "workspace/my_workspace_stat.RData")
setwd("~/Documents/Magistrale/Statistical Lab/Hitachi_train_batteries")


df_C2 <- readRDS("df_C2_C7/df_C2.rds")
df_C4 <- readRDS("df_C2_C7/df_C4.rds")
df_C5 <- readRDS("df_C2_C7/df_C5.rds")
df_C7 <- readRDS("df_C2_C7/df_C7.rds")
# We have 957 groups

# Convert categorical variables to numeric
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
    TRUE ~ as.numeric(POC)  # Keep the original values if they don't match any of the above cases
  ))
df_C2$Mese <- as.numeric(df_C2$Mese)

#* Group division based on POC
# gruppi_POC_1 <- df_C2$Gruppo[df_C2$POC == "1"]
# gruppi_POC_2 <- df_C2$Gruppo[df_C2$POC == "2"]
# gruppi_POC_3 <- df_C2$Gruppo[df_C2$POC == "3"]
# gruppi_POC_4 <- df_C2$Gruppo[df_C2$POC == "4"]
# gruppi_POC_5 <- df_C2$Gruppo[df_C2$POC == "5"]
# gruppi_POC_6 <- df_C2$Gruppo[df_C2$POC == "6"]
# gruppi_POC_7 <- df_C2$Gruppo[df_C2$POC == "7"]
# gruppi_POC_8 <- df_C2$Gruppo[df_C2$POC == "8"]

#* corr_matrix and matrix plot

# Calculate the correlation matrix of the numerical variables in the df_C2 dataframe
corr_matrix <- df_C2 %>%
  dplyr::select(where(is.numeric)) %>%
  # Remove some columns not necessary for the correlation
  select(-Gruppo, -Amperora, -Wattora, -POC) %>% #, -Stagione
  as.matrix() %>%
  cor()

# Visualize the correlation matrix using the corrplot function
corrplot(corr_matrix)

library(GGally) # Load the GGally library

# Create a matrix plot showing scatter plots for all pairs of variables in the dataframe subset
ggpairs(subset(df_C2, select = -c(Gruppo, Wattora, Amperora)))
#! ----------------- Boxplot -----------------
# Calculate the mean and standard deviation of Wattora
#mean_wattora <- mean(df_C2$Wattora)
#sd_wattora <- sd(df_C2$Wattora)

#* Visualize a boxplot before removing outliers
boxplot(Wattora ~ POC, data = df_C2,
        main = "Boxplot Wattora Stratified by POC",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()

#* Visualize a boxplot after removing outliers (Wh>5000)

outlier_limit <- 5000
# Identify the outliers
outliers <- df_C2$Wattora > outlier_limit | df_C2$Wattora < -outlier_limit
cat("Number of outliers:", sum(outliers), "\n")

# Remove the outliers from the dataframe
df_C2 <- df_C2[!outliers, ]

# Visualize the boxplot
boxplot(Wattora ~ POC, data = df_C2,
        main = "Boxplot Wattora Stratified by POC (removing observations with Wh>5000)",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()

#* Visualize a boxplot after removing outliers (Wh>1000)
outlier_limit <- 1000
# Identify the outliers
outliers <- df_C2$Wattora > outlier_limit | df_C2$Wattora < -outlier_limit
cat("Number of outliers:", sum(outliers), "\n")

# Remove the outliers from the dataframe
df_C2 <- df_C2[!outliers, ] # we are left with 930 groups

# Visualize the boxplot
boxplot(Wattora ~ POC, data = df_C2,
        main = "Boxplot Wattora Stratified by POC",
        xlab = "POC",
        col = rainbow(length(unique(df_C2$POC))))
grid()
#! ----------------- PCA  -----------------
 library(plotly)
# Make season, month, and poc categorical variables using "factor"
df_C2$POC <- as.factor(df_C2$POC)
df_C2$Stagione <- as.factor(df_C2$Stagione)
df_C2$Mese <- as.factor(df_C2$Mese)

# Create a function to represent clustering results based on poc

# Perform PCA on the subset of the df_C2 dataframe, excluding some specific columns and standardizing the variables
pr.out <- prcomp(subset(df_C2, select = -c(Timestamp_iniziale,Gruppo, POC, Stagione,
                                           Mese, Wattora, Amperora)), scale = TRUE)

# Extract the loading vector (rotation) for the first principal component
pr.out$rotation[, 1]

# Display the names of the variables in the df_C2 dataframe
names(pr.out)

# Calculate the fraction of variance explained by the first four principal components
varianza_spiegata <- sum(pr.out$sdev[1:3]^2) / sum(pr.out$sdev^2)  # 90% explained variance

# Display the principal components plot
plot(pr.out)

# Create a panel with two plots: one for the plot of the fraction of explained variance and one for the plot of cumulative variance
par(mfrow=c(1,2))

# Calculate the fraction of explained variance and create a scree plot
fvs <- pr.out$sdev^2 / sum(pr.out$sdev^2)
plot(
  fvs,
  xlab = "PC",
  ylab = "Fraction of variance explained",
  main = "Scree plot",
  ylim = c(0, 1),
  type = 'b'
)

# Calculate the cumulative variance and create a scree plot
plot(
  cumsum(fvs),
  xlab = "PC",
  ylab = "Cumulative fraction of variance explained",
  ylim = c(0, 1),
  type = 'b'
)

# Reset the plot layout to a single grid
par(mfrow=c(1,1))

# Create a biplot with additional customizations, including red color for the variable arrows
biplot(
  pr.out,
  scale = 1,
  cex = 0.9,
  col = c("blue", "red")
)

biplot(
  pr.out$rotation[,c(1,3)],
  scale = 1,
  cex = 0.9,
  col = c("blue", "red")
)

biplot(
  pr.out$rotation[,c(1,3)],
  scale = 1,
  cex = 0.9,
  col = c("blue", "red")
)

biplot(
  pr.out,
  scale = 1,
  cex = 0.9,
  col = c("blue", "red"),
  choices = c(1, 3)
)
#abline(h = 0, v = 0, lty = 2)

# # Create the 3D plot with plotly
plot_3d <- plot_ly(df_C2, x = ~pr.out$x[,1], y = ~pr.out$x[,2], z = ~pr.out$x[,3],
color = ~as.factor(Cluster), type = "scatter3d",size = 1) %>%
 layout(scene = list(title = "3D Scatter Plot - Initial Intensity vs. Voltage",
                    xaxis = list(title = "Timestamp"),
                    yaxis = list(title = "Initial Intensity"),
                    zaxis = list(title = "Initial Voltage")))

#
c11()
# # Display the plotly plot

plot_3d
#3D plot
# Perform PCA on the subset of the df_C2 dataframe, excluding some specific columns and standardizing the variables
pr.out <- prcomp(subset(df_C2, select = -c(Timestamp_iniziale,Gruppo, POC, Stagione,
                                           Mese, Wattora, Amperora)), scale = TRUE)

pr.out$Cluster <- df_C2$Cluster

library(rgl)
plot3d(pr.out$x[,1:3], col = pr.out$Cluster)

coords <- NULL
for (i in 1:nrow(pr.out$rotation)) {
  coords <- rbind(coords, rbind(c(0,0,0),pr.out$rotation[i,1:3]))
}

lines3d(10*coords, col="red", lwd=4)
text3d(10.2*pr.out$rotation[,1:3], texts=rownames(pr.out$rotation), col="red")
rgl.snapshot('PCA_3D.png', fmt = 'png')


#! ----------------- Clustering -----------------

set.seed(1)  # Set a seed for reproducibility
library(cluster)

k_seq <- 2:10
silhouette_vec <- numeric(length(k_seq))

for (kk in seq_along(k_seq)) {
  X <- subset(df_C2, select = -c(Gruppo, Timestamp_iniziale,POC, Stagione, Mese, Wattora, Amperora))

  km_out <- kmeans(X, centers = k_seq[kk], nstart = 100)
  cluster_kk <- km_out$cluster  # Obtain the clusters assigned to each observation
  sil <- silhouette(cluster_kk, dist = dist(X)) # Calculate the Silhouette measure
  silhouette_vec[kk] <- summary(sil)$avg.width
}

# Plot the Silhouette measure as a function of the number of clusters K
plot(k_seq, silhouette_vec, type = "l", xlab = "K", ylab = "Silhouette (average)", main = "Cluster with POC 1-8")
points(k_seq[which.max(silhouette_vec)], max(silhouette_vec), col = "red", pch = 16)
grid()

# Repeat the process considering only POCs 7 and 8

# Perform K-means clustering with K=3 on the complete dataframe
km.out <- kmeans(subset(df_C2, select = -c(Timestamp_iniziale)), 3, nstart = 100)

km.out$tot.withinss # Display the total within-cluster variance
names(km.out) # Display the names of the objects returned by the kmeans output
km.out$cluster # Display the clusters assigned to the observations

df_C2$Cluster <- as.factor(km.out$cluster)

library(ggplot2)
# Display a scatter plot for V with different colors for clusters
ggplot(df_C2, aes(x = Timestamp_iniziale, y = V_iniziale, color = Cluster)) +
  geom_point(cex=1) +
  geom_hline(yintercept = 25.5, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot Cluster - Initial Voltage") +
  xlab("Month") +
  ylab("Initial Voltage") +
  theme_minimal()

# Display a scatter plot for I with different colors for clusters
ggplot(df_C2, aes(x = Timestamp_iniziale, y = I_iniziale, color = Cluster)) +
  geom_line() +
  labs(title = "Scatter Plot Cluster - Initial Intensity") +
  xlab("Month") +
  ylab("Initial intensity") +
  theme_minimal()

install.packages("plotly")
library(plotly)

# Create the 3D plot with plotly
plot_3d <- plot_ly(df_C2, x = ~Timestamp_iniziale, y = ~I_iniziale, z = ~V_iniziale,
                   color = ~as.factor(Cluster), type = "scatter3d",size = 1) %>%
  layout(scene = list(title = "3D Scatter Plot - Initial Intensity vs. Voltage",
                      xaxis = list(title = "Timestamp"),
                      yaxis = list(title = "Initial Intensity"),
                      zaxis = list(title = "Initial Voltage")))

# Display the plotly plot
plot_3d

# # Display a scatter plot with different colors for POC
# ggplot(df_C2, aes(x = Mese, y = V_iniziale, color = POC)) +
#   geom_point() +
#   labs(title = "Scatter Plot POC - Initial Voltage") +
#   xlab("Month") +
#   ylab("Initial Voltage") +
#   theme_minimal()

# Display a bar plot showing the distribution of clusters
ggplot(df_C2, aes(x = Cluster)) +
  geom_bar() +
  labs(title = "Clusters' Distribution") +
  xlab("Cluster") +
  ylab("Number of Observations") +
  theme_minimal()

# Display a boxplot for each cluster
ggplot(df_C2, aes(x = Cluster, y = V_iniziale, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Boxplot for Clusters") +
  xlab("Cluster") +
  ylab("Initial Voltage") +
  theme_minimal()

# Set the layout to 2 rows and 1 column for the next plots
#par(mfrow=c(2,1))

#plot(factor(km.out$cluster))# Plot the cluster variable assigned by K-means

#plot(df_C2$POC)# Plot the POC variable in the df_C2 dataframe

par(pty="s") # Restore the default plot layout

# Perform K-means clustering with K=2 on the dataframe containing only POCs 7 and 8
km.out <- kmeans(X, 2, nstart = 20)

km.out$tot.withinss # Display the total within-cluster variance
names(km.out) # Display the names of the objects returned by the kmeans output
km.out$cluster # Display the clusters assigned to the observations

# Add the cluster assignments to the dataframe
X$Cluster <- as.factor(km.out$cluster)

# Display a scatter plot with different colors for clusters
ggplot(X, aes(x = Stagione, y = V_iniziale, color = Cluster)) +
  geom_point() +
  labs(title = "Scatter Plot Cluster - Initial Voltage") +
  xlab("Season") +
  ylab("Initial Voltage") +
  theme_minimal()

# Display a scatter plot with different colors for POC
# ggplot(X, aes(x = Stagione, y = V_iniziale, color = POC)) +
#   geom_point() +
#   labs(title = "Scatter Plot POC - Initial Voltage") +
#   xlab("Season") +
#   ylab("Initial Voltage") +
#   theme_minimal()

# Display a bar plot showing the distribution of clusters
ggplot(X, aes(x = Cluster)) +
  geom_bar() +
  labs(title = "Distribution of Clusters") +
  xlab("Cluster") +
  ylab("Number of Observations") +
  theme_minimal()

# Perform K-means clustering on the scores of the first two PCs obtained from a PCA analysis
km.out.scores <- kmeans(scores_PC1_2, 2, nstart = 50)

# Update the clusters.k.means.PCA column in the valori dataframe
valori$clusters.k.means.PCA <- km.out.scores$cluster

# Calculate the number of observations that differ between the two clustering methods
sum(valori$clusters != valori$clusters.k.means.PCA)

# Display a scatter plot of the first two PCs, coloring the observations based on the clusters assigned by K-means
plot(PC2 ~ PC1, data = scores_PC1_2, type = "n", col = clusters(m2), ylim = c(-6,4))
etich = abbreviate(1:valori$ID, minlength = 2)
text(scores_PC1_2$PC1, scores_PC1_2$PC2, labels = etich, col = valori$clusters.k.means.PCA)
abline(parameters(m2)[1:2, 1], lty = 3)
abline(parameters(m2)[1:2, 2], lty = 3)
abline(h = 0, v = 0, lty = 2)

# #kmeans on the scores of the first two PCs
# km.out.scores <- kmeans (scores_PC1_2, 2, nstart =50)
# valori$clusters.k.means.PCA <- km.out.scores$cluster
# sum(valori$clusters!=valori$clusters.k.means.PCA)
#
# par(pty="s")
# plot(PC2 ~ PC1, data = scores_PC1_2, type = "n", col=clusters(m2), ylim=c(-6,4))
# etich = abbreviate(1:valori$ID, minlength = 2)
# text(scores_PC1_2$PC1, scores_PC1_2$PC2, labels = etich, col=valori$clusters.k.means.PCA)
# abline(parameters(m2)[1:2, 1],lty=3)
# abline(parameters(m2)[1:2, 2],lty=3)
# abline(h = 0, v = 0,lty=2)
