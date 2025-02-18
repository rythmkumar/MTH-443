eco_dev_data <- read_csv("eco_dev_data.csv")
PS_bank_fin_ratio <- read_csv("PS_bank_fin_ratio.csv")
Wine_data <- read_csv("Wine_data.csv")
###############
# Problem 1
###############
library(aplpack)
library(stats)
library(ggplot2)
library(cluster)

eco_dev_data <- as.data.frame(eco_dev_data)
row.names(eco_dev_data) <- eco_dev_data$Country
eco_dev_data <- eco_dev_data[,-1] 

chernofff_eco <- faces(eco_dev_data)
dist_eco <- dist(eco_dev_data) 
hc_eco <- hclust(dist_eco, method = "ward.D")
plot(hc_eco, labels = row.names(eco_dev_data), main = "Dendrogram of Economic Development Data")
rect.hclust(hc_eco, k = 5, border = "red")
eco_clusters <- cutree(hc_eco, k = 5) 

###############
# Problem 2
###############

chernoff_wine <- faces(Wine_data[,-1])

wine_dist <- dist(Wine_data[,-1])
hc_wine <- hclust(wine_dist, method = "ward.D")
plot(hc_wine, labels = NULL, main = "Dendrogram of Wine Data")
rect.hclust(hc_wine, k = 3, border = "blue")
wine_clusters <- cutree(hc_wine, k = 3)
table(wine_clusters, Wine_data$Type)

###############
# Problem 3
###############
PS_bank_fin_ratio <- as.data.frame(PS_bank_fin_ratio)
row.names(PS_bank_fin_ratio) <- PS_bank_fin_ratio$Banks
PS_bank_fin_ratio <- PS_bank_fin_ratio[,-1]
chernoff_bank <- faces(PS_bank_fin_ratio)
for (year in unique(PS_bank_fin_ratio$Year)) {
  year_data <- PS_bank_fin_ratio[PS_bank_fin_ratio$Year == year, -1]  # Exclude Year column
  faces(year_data, main = paste("Chernoff Faces for Banks - Year", year))
}

