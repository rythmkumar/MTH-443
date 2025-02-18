#########
# Question 1
#########
eco_dev_data <- read_csv("~/Desktop/MTH443/Labs/Lab 3/eco_dev_data.csv")
eco_dev_data <- as.data.frame(eco_dev_data)
row.names(eco_dev_data) <- eco_dev_data$Country
eco_dev_data <- eco_dev_data[,-1]

results <- PCA(eco_dev_data, scale = TRUE)


#######
# Remove outliers
#######
fviz_pca_ind(results, geom = "point", label = "none")
pca_coordinates <- results$ind$coord
mahalanobis_dist <- mahalanobis(pca_coordinates, colMeans(pca_coordinates), cov(pca_coordinates))
cutoff <- qchisq(0.975, df = ncol(pca_coordinates))
outliers <- which(mahalanobis_dist > cutoff)
cleaned_data <- eco_dev_data[-outliers, ]
pca_cleaned <- PCA(cleaned_data, scale = TRUE)
order(pca_cleaned$ind$coord[, 1], decreasing = TRUE)
pca_cleaned$ind$coord[order(pca_cleaned$ind$coord[, 1], decreasing = TRUE), 1]

######
#Identify feature clusters using PCA
######

pca_var <- results$var$coord
fviz_pca_var(results, col.var = "contrib")
var_clustering <- hclust(dist(pca_var))
plot(var_clustering, labels = colnames(eco_dev_data))
cluster_cut <- cutree(var_clustering, k = 3)
cluster_cut

#########
#Check MVN of feature vector
mvn_result <- mvn(cleaned_data, mvnTest = "mardia")
mvn_result$multivariateNormality
mvnPlot <- mvn(cleaned_data, mvnTest = "mardia", multivariatePlot = "qq")

###########
# Question 2
###########

PS_bank_fin_ratio <- read_csv("~/Desktop/MTH443/Labs/Lab 3/PS_bank_fin_ratio.csv")
PS_bank_fin_ratio <- as.data.frame(PS_bank_fin_ratio)
row.names(PS_bank_fin_ratio) <- PS_bank_fin_ratio[,1]
PS_bank_fin_ratio <- PS_bank_fin_ratio[,-1]

results <- PCA(PS_bank_fin_ratio, scale = TRUE)


#######
# Remove outliers
#######
fviz_pca_ind(results, geom = "point", label = "none")
pca_coordinates <- results$ind$coord
mahalanobis_dist <- mahalanobis(pca_coordinates, colMeans(pca_coordinates), cov(pca_coordinates))
cutoff <- qchisq(0.975, df = ncol(pca_coordinates))
outliers <- which(mahalanobis_dist > cutoff)
cleaned_data <- PS_bank_fin_ratio[-outliers, ]
pca_cleaned <- PCA(cleaned_data, scale = TRUE)
order(pca_cleaned$ind$coord[, 1], decreasing = TRUE)
pca_cleaned$ind$coord[order(pca_cleaned$ind$coord[, 1], decreasing = TRUE), 1]

######
#Identify feature clusters using PCA
######

pca_var <- results$var$coord
fviz_pca_var(results, col.var = "contrib")
var_clustering <- hclust(dist(pca_var))
plot(var_clustering, labels = colnames(PS_bank_fin_ratio))
cluster_cut <- cutree(var_clustering, k = 3)
cluster_cut

#########
#Check MVN of feature vector
mvn_result <- mvn(cleaned_data, mvnTest = "mardia")
mvn_result$multivariateNormality
mvnPlot <- mvn(cleaned_data, mvnTest = "mardia", multivariatePlot = "qq")


