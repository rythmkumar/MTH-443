
#########
#Load libraries
library('corrr')
library(ggcorrplot)
library("FactoMineR")
library("factoextra")

########
# Read Data
cyber_crime_2020 <- read_csv("cyber_crime_2020.csv")
cyber_crime_2020 <- as.data.frame(cyber_crime_2020)
row.names(cyber_crime_2020) <- cyber_crime_2020[,1]
cyber_crime_2020 <- cyber_crime_2020[,-1]

#########
#Apply PCA
cyber_crime.pca <- prcomp(cyber_crime_2020)
summary(cyber_crime.pca)

########
# Percentage of the variance as shown by the given component
cyber_crime.std_variance <- as.vector(cyber_crime.pca[["sdev"]]/sum(cyber_crime.pca[["sdev"]]))

#Identify the outlier in the variable
fviz_pca_var(cyber_crime.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

#Identify the outlier in the individuals
fviz_pca_ind(cyber_crime.pca)

#For exact outliers I use mahalanobis distance, and i works with another PCA function
results <- PCA(cyber_crime_2020, scale = TRUE)
pca_coordinates <- results$ind$coord
mahalanobis_dist <- mahalanobis(pca_coordinates, colMeans(pca_coordinates), cov(pca_coordinates))
cutoff <- qchisq(0.975, df = ncol(pca_coordinates))
outliers <- which(mahalanobis_dist > cutoff)

#Plotting the scree
qplot(c(1:length(cyber_crime.std_variance)), cyber_crime.std_variance) + 
  geom_col() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#Observing the knee in the scree plot
fviz_eig(cyber_crime.pca, addlabels = TRUE)

#optimal number of clusters for kmeans
fviz_nbclust(cyber_crime.pca$x, FUNcluster=kmeans)

#Hierarchical clustering
dist_matrix <- dist(cyber_crime.pca$x, method = "euclidean")

par(mfrow = c(1, 2))

hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete, hang=-1, main = "Complete Linkage Dendrogram") 
rect.hclust(hc_complete, k=4, border="red") 
hc_complete_labels <- cutree(hc_complete, k = 4)

hc_single <- hclust(dist_matrix, method = "single")
plot(hc_single, hang=-1, main = "Single Linkage Dendrogram") 
rect.hclust(hc_single, k=4, border="red") 
hc_single_labels <- cutree(hc_single, k = 4)

#Kmeans cluster
km<-eclust(cyber_crime.pca$x, "kmeans", hc_metric="eucliden",k=4)
results <- list()
for (i in 1:4) {
  kmeans_result <- kmeans(cyber_crime_2020, centers = 4, nstart = 1, iter.max = 100, algorithm = "Lloyd")
  results[[i]] <- list(labels = kmeans_result$cluster, inertia = kmeans_result$tot.withinss)
}
best_result <- which.min(sapply(results, function(x) x$inertia))
best_labels <- results[[best_result]]$labels

#Across labels similarity
cluster_purity <- function(true_labels, cluster_labels) {
  confusion_matrix <- table(true_labels, cluster_labels)
  cluster_purity <- sum(apply(confusion_matrix, 2, max)) / sum(confusion_matrix)
  return(cluster_purity)
}
hierarchical_purity <- cluster_purity(hc_single_labels, hc_complete_labels)
kmeans_hierarchical_purity <- cluster_purity(hc_complete_labels, best_labels)




