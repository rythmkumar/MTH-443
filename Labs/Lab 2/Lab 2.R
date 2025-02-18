
#########
#Load libraries
library('corrr')
library(ggcorrplot)
library("FactoMineR")
library("factoextra")
library(readr)
library(knitr)
library(dplyr)
library(tidyr)
library(hopkins)
library(cluster)
library(flexclust)
library(fpc)
library(ClusterR)
library(clusterSim)
library(smacof)
library(gridExtra)
library(MASS)
library(paran)
library(BBmisc)
library(stats)
library(rworldmap)
library(ClustGeo)

#########
#Load datasets
eco_dev_data <- read_csv("~/Desktop/MTH443/Labs/Lab 2/eco_dev_data.csv")
eco_dev_data <- as.data.frame(eco_dev_data)
row.names(eco_dev_data) <- eco_dev_data[,1]
eco_dev_data <- eco_dev_data[,-1]

eur_protein_consump <- read_csv("~/Desktop/MTH443/Labs/Lab 2/eur_protein_consump.csv")
eur_protein_consump <- as.data.frame(eur_protein_consump)
row.names(eur_protein_consump) <- eur_protein_consump[,1]
eur_protein_consump <- eur_protein_consump[,-1]

cars_data <- read_csv("~/Desktop/MTH443/Labs/Lab 2/cars_data.csv")
cars_data <- as.data.frame(cars_data)
row.names(cars_data) <- cars_data[,1]
cars_data <- cars_data[,-1]

PS_bank_fin_ratio <- read_csv("~/Desktop/MTH443/Labs/Lab 2/PS_bank_fin_ratio.csv")
PS_bank_fin_ratio <- as.data.frame(PS_bank_fin_ratio)
row.names(PS_bank_fin_ratio) <- PS_bank_fin_ratio[,1]
PS_bank_fin_ratio <- PS_bank_fin_ratio[,-1]


#########
#Apply PCA
eco_dev_data.pca <- prcomp(eco_dev_data)
eur_protein_consump.pca <- prcomp(eur_protein_consump)
cars_data.pca <- prcomp(cars_data)
PS_bank_fin_ratio.pca <- prcomp(PS_bank_fin_ratio)

###Method 2
eco_dev_data.pca <- princomp(eco_dev_data)
eur_protein_consump.pca <- princomp(eur_protein_consump)
cars_data.pca <- princomp(cars_data)
PS_bank_fin_ratio.pca <- princomp(PS_bank_fin_ratio)

#########
#Look at summary
summary(eco_dev_data.pca)
summary(eur_protein_consump.pca)
summary(cars_data.pca)
summary(PS_bank_fin_ratio.pca)

#########
#Check individual variance percentages
eco_dev_data.std_variance <- as.vector(eco_dev_data.pca[["sdev"]]/sum(eco_dev_data.pca[["sdev"]]))
eur_protein_consump.std_variance <- as.vector(eur_protein_consump.pca[["sdev"]]/sum(eur_protein_consump.pca[["sdev"]]))
cars_data.std_variance <- as.vector(cars_data.pca[["sdev"]]/sum(cars_data.pca[["sdev"]]))
PS_bank_fin_ratio.std_variance <- as.vector(PS_bank_fin_ratio.pca[["sdev"]]/sum(PS_bank_fin_ratio.pca[["sdev"]]))

##########
#Get scree plots
qplot(c(1:length(eco_dev_data.std_variance)), eco_dev_data.std_variance) + 
  geom_col() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

qplot(c(1:length(eur_protein_consump.std_variance)), eur_protein_consump.std_variance) + 
  geom_col() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

qplot(c(1:length(cars_data.std_variance)), cars_data.std_variance) + 
  geom_col() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

qplot(c(1:length(PS_bank_fin_ratio.std_variance)), PS_bank_fin_ratio.std_variance) + 
  geom_col() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

##Method 2
fviz_eig(eco_dev_data.pca, addlabels = TRUE)
fviz_eig(cars_data.pca, addlabels = TRUE)
fviz_eig(eur_protein_consump.pca, addlabels = TRUE)
fviz_eig(PS_bank_fin_ratio.pca, addlabels = TRUE)

##Method 3
fviz_nbclust(PS_bank_fin_ratio.pca$x, FUNcluster=kmeans)
fviz_nbclust(eco_dev_data.pca$x, FUNcluster=kmeans)
fviz_nbclust(eur_protein_consump.pca$x, FUNcluster=kmeans)
fviz_nbclust(cars_data.pca$x, FUNcluster=kmeans)

##########
#Identify multidimensional outliers
fviz_pca_var(eco_dev_data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_var(eur_protein_consump.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_var(cars_data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_var(PS_bank_fin_ratio.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_biplot(cars_data.pca)
fviz_pca_biplot(PS_bank_fin_ratio.pca)
fviz_pca_biplot(eur_protein_consump.pca)
fviz_pca_biplot(eco_dev_data.pca)

###########
#Identify clusters
eclust(eco_dev_data.pca$x, "kmeans", hc_metric="eucliden",k=6)
eclust(eur_protein_consump.pca$x, "kmeans", hc_metric="eucliden",k=2)
eclust(PS_bank_fin_ratio.pca$x, "kmeans", hc_metric="eucliden",k=2)
eclust(cars_data.pca$x, "kmeans", hc_metric="eucliden",k=2)

##Method 2
dm<-dist(eco_dev_data.pca$x) 
hc<-hclust(dm, method="complete")
plot(hc, hang=-1) 
rect.hclust(hc, k=6, border="red")

dm<-dist(eur_protein_consump.pca$x) 
hc<-hclust(dm, method="complete")
plot(hc, hang=-1) 
rect.hclust(hc, k=6, border="red")

dm<-dist(PS_bank_fin_ratio.pca$x) 
hc<-hclust(dm, method="complete")
plot(hc, hang=-1) 
rect.hclust(hc, k=6, border="red")

dm<-dist(cars_data.pca$x) 
hc<-hclust(dm, method="complete")
plot(hc, hang=-1) 
rect.hclust(hc, k=6, border="red")





