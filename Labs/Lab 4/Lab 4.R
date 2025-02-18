library(cluster) 
library(ggplot2) 
library(dendextend) 
library(dplyr)  
library(tidyr)  

#############
#Question 1
#############

Wine_data <- read_csv("Wine_data.csv")

# #####
# Clustering using hierarchical clustering
Wine_data_wo_type <- Wine_data[,-1]
dist_matrix <- dist(Wine_data_wo_type, method = "euclidean")

hc_complete <- hclust(dist_matrix, method = "complete")
hc_single <- hclust(dist_matrix, method = "single")

par(mfrow = c(1, 2))
plot(hc_complete, main = "Complete Linkage Dendrogram", xlab = "", sub = "")
plot(hc_single, main = "Single Linkage Dendrogram", xlab = "", sub = "")

########
#Clustering using kmeans
k <- 3
n_initial <- 5

results <- list()
for (i in 1:n_initial) {
  kmeans_result <- kmeans(Wine_data_wo_type, centers = k, nstart = 1, iter.max = 100, algorithm = "Lloyd")
  results[[i]] <- list(labels = kmeans_result$cluster, inertia = kmeans_result$tot.withinss)
}

best_result <- which.min(sapply(results, function(x) x$inertia))
best_labels <- results[[best_result]]$labels
best_inertia <- results[[best_result]]$inertia

print(paste("Best K-means Inertia:", best_inertia))

#########
#Within cluster scatter
calculate_inertia <- function(data, cluster_labels) {
  unique_labels <- unique(cluster_labels)
  inertia <- 0
  for (label in unique_labels) {
    cluster_points <- data[cluster_labels == label, ]
    centroid <- colMeans(cluster_points)
    inertia <- inertia + sum(rowSums((cluster_points - centroid)^2))
  }
  return(inertia)
}

hierarchical_labels <- cutree(hc_complete, k = k)
hierarchical_inertia <- calculate_inertia(Wine_data_wo_type, hierarchical_labels)

print(paste("Hierarchical Inertia:", hierarchical_inertia))
print(paste("K-means Inertia:", best_inertia))

###########
#Validating clustering with given classification
cluster_purity <- function(true_labels, cluster_labels) {
  confusion_matrix <- table(true_labels, cluster_labels)
  cluster_purity <- sum(apply(confusion_matrix, 2, max)) / sum(confusion_matrix)
  return(cluster_purity)
}
hierarchical_purity <- cluster_purity(Wine_data$Type, hierarchical_labels)
kmeans_purity <- cluster_purity(Wine_data$Type, best_labels)

print(paste("Hierarchical Clustering Purity:", hierarchical_purity))
print(paste("K-means Clustering Purity:", kmeans_purity))


#############
#Question 2
#############
CrimesOnWomenData <- read_csv("CrimesOnWomenData.csv")

data_for_clustering <- CrimesOnWomenData %>%
  dplyr::select(-State, -Year)
dist_matrix <- dist(data_for_clustering, method = "euclidean")
hc_average <- hclust(dist_matrix, method = "average")
hc_single <- hclust(dist_matrix, method = "single")

par(mfrow = c(1, 2))
plot(hc_average, main = "Average Linkage Dendrogram", xlab = "", sub = "")
plot(hc_single, main = "Single Linkage Dendrogram", xlab = "", sub = "")

k <- 5
n_initial <- 5

results <- list()
for (i in 1:n_initial) {
  kmeans_result <- kmeans(data_for_clustering, centers = k, nstart = 1, iter.max = 100, algorithm = "Lloyd")
  results[[i]] <- list(labels = kmeans_result$cluster, inertia = kmeans_result$tot.withinss)
}

best_result <- which.min(sapply(results, function(x) x$inertia))
best_labels <- results[[best_result]]$labels
best_inertia <- results[[best_result]]$inertia

print(paste("Best K-means Inertia:", best_inertia))

cut_clusters_average <- cutree(hc_average, k = k)
cut_clusters_single <- cutree(hc_single, k = k)

calculate_inertia <- function(data, cluster_labels) {
  unique_labels <- unique(cluster_labels)
  inertia <- 0
  for (label in unique_labels) {
    cluster_points <- data[cluster_labels == label, ]
    centroid <- colMeans(cluster_points)
    inertia <- inertia + sum(rowSums((cluster_points - centroid)^2))
  }
  return(inertia)
}

hierarchical_inertia_average <- calculate_inertia(data_for_clustering, cut_clusters_average)
hierarchical_inertia_single <- calculate_inertia(data_for_clustering, cut_clusters_single)

kmeans_inertia <- best_inertia

print(paste("Hierarchical Average Inertia:", hierarchical_inertia_average))
print(paste("Hierarchical Single Inertia:", hierarchical_inertia_single))
print(paste("K-means Inertia:", kmeans_inertia))

calculate_between_scatter <- function(data, cluster_labels) {
  overall_mean <- colMeans(data)
  unique_labels <- unique(cluster_labels)
  between_scatter <- 0
  for (label in unique_labels) {
    cluster_points <- data[cluster_labels == label, ]
    cluster_mean <- colMeans(cluster_points)
    n <- nrow(cluster_points)
    between_scatter <- between_scatter + n * sum((cluster_mean - overall_mean)^2)
  }
  return(between_scatter)
}

hierarchical_between_scatter_average <- calculate_between_scatter(data_for_clustering, cut_clusters_average)
hierarchical_between_scatter_single <- calculate_between_scatter(data_for_clustering, cut_clusters_single)
kmeans_between_scatter <- calculate_between_scatter(data_for_clustering, best_labels)

print(paste("Hierarchical Average Between-Cluster Scatter:", hierarchical_between_scatter_average))
print(paste("Hierarchical Single Between-Cluster Scatter:", hierarchical_between_scatter_single))
print(paste("K-means Between-Cluster Scatter:", kmeans_between_scatter))

data_with_clusters <- CrimesOnWomenData %>%
  mutate(Kmeans_Cluster = best_labels) %>%
  group_by(Year, Kmeans_Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

ggplot(data_with_clusters, aes(x = Year, y = Kmeans_Cluster, color = factor(Kmeans_Cluster))) +
  geom_line() +
  labs(title = "K-means Clusters Over Time", x = "Year", y = "Cluster", color = "Cluster") +
  theme_minimal()
