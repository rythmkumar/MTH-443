#######
#library
#######

library(readr)
library(MASS)
library(FNN)
library(ggplot2)

######
#Read data & splitting it
######
CHD <- read_csv("~/Desktop/MTH443/Labs/Lab 6/CHD.csv")
yesCHD <- na.omit(subset(CHD, TenYearCHD == 1))
noCHD <- na.omit(subset(CHD, TenYearCHD == 0))

##########################
par(mfrow = c(3, 4))

# Variables for Density Estimation
variables <- c("totChol", "sysBP", "diaBP")
kernels <- c("gaussian", "rectangular", "triangular", "epanechnikov")
colors <- c("red", "green")  # red for noCHD, green for yesCHD

# Loop over variables and kernels
for (var in variables) {
  for (kernel in kernels) {
    no_density <- density(noCHD[[var]], na.rm = TRUE, kernel = kernel)
    yes_density <- density(yesCHD[[var]], na.rm = TRUE, kernel = kernel)
    plot(no_density[["x"]], no_density[["y"]], type = 'l', col = colors[1], main = paste(var, "-", kernel))
    lines(yes_density[["x"]], yes_density[["y"]], col = colors[2])
  }
}

##########################
#using knn density estimation
k <- 5
compute_knn_density <- function(data, variable, k, query_points) {
  data_matrix <- as.matrix(data[[variable]])
  
  distances <- knnx.dist(data_matrix, as.matrix(query_points), k)

  density_estimates <- apply(distances, 1, function(d) {
    if (is.finite(d[k])) {
      return(k / (2 * nrow(data) * d[k]))
    } else {
      return(0) 
    }
  })
  
  return(density_estimates)
}
xrange_totChol <- seq(min(CHD$totChol, na.rm = TRUE), max(CHD$totChol, na.rm = TRUE), length.out = 100)
xrange_sysBP <- seq(min(CHD$sysBP, na.rm = TRUE), max(CHD$sysBP, na.rm = TRUE), length.out = 100)
xrange_diaBP <- seq(min(CHD$diaBP, na.rm = TRUE), max(CHD$diaBP, na.rm = TRUE), length.out = 100)

density_totChol_yes <- compute_knn_density(yesCHD, "totChol", k, xrange_totChol)
density_totChol_no <- compute_knn_density(noCHD, "totChol", k, xrange_totChol)

density_sysBP_yes <- compute_knn_density(yesCHD, "sysBP", k, xrange_sysBP)
density_sysBP_no <- compute_knn_density(noCHD, "sysBP", k, xrange_sysBP)

density_diaBP_yes <- compute_knn_density(yesCHD, "diaBP", k, xrange_diaBP)
density_diaBP_no <- compute_knn_density(noCHD, "diaBP", k, xrange_diaBP)

plot_knn_density <- function(xrange, density_yes, density_no, title, xlabel) {
  df <- data.frame(
    value = rep(xrange, 2),
    density = c(density_yes, density_no),
    group = rep(c("TenYearCHD = 1", "TenYearCHD = 0"), each = length(xrange))
  )
  
  ggplot(df, aes(x = value, y = density, color = group)) +
    geom_line() +
    labs(title = title, x = xlabel, y = "Density") +
    theme_minimal()
}

plot_knn_density(xrange_totChol, density_totChol_yes, density_totChol_no, 
                 "KNN Density Estimates of Total Cholesterol by TenYearCHD", "Total Cholesterol")

plot_knn_density(xrange_sysBP, density_sysBP_yes, density_sysBP_no, 
                 "KNN Density Estimates of Systolic BP by TenYearCHD", "Systolic BP")

plot_knn_density(xrange_diaBP, density_diaBP_yes, density_diaBP_no, 
                 "KNN Density Estimates of Diastolic BP by TenYearCHD", "Diastolic BP")



##########################
#Tail Probabilities at different treshold levels
quantiles_sysBP <- quantile(CHD$sysBP, probs = c(0.90, 0.95), na.rm = TRUE)
quantiles_diaBP <- quantile(CHD$diaBP, probs = c(0.90, 0.95), na.rm = TRUE)

print(paste("90th percentile sysBP:", quantiles_sysBP[4]))
print(paste("95th percentile sysBP:", quantiles_sysBP[5]))
print(paste("90th percentile diaBP:", quantiles_diaBP[4]))
print(paste("95th percentile diaBP:", quantiles_diaBP[5]))

# Function to estimate tail probabilities
estimate_tail_probabilities <- function(data, variable, thresholds) {
  total_count <- nrow(data)
  tail_probabilities <- sapply(thresholds, function(threshold) {
    sum(data[[variable]] > threshold) / total_count
  })
  return(tail_probabilities)
}

tail_prob_sysBP_yes <- estimate_tail_probabilities(yesCHD, "sysBP", quantiles_sysBP)
tail_prob_sysBP_no <- estimate_tail_probabilities(noCHD, "sysBP", quantiles_sysBP)

tail_prob_diaBP_yes <- estimate_tail_probabilities(yesCHD, "diaBP", quantiles_diaBP)
tail_prob_diaBP_no <- estimate_tail_probabilities(noCHD, "diaBP", quantiles_diaBP)

comparison_df <- data.frame(
  Variable = rep(c("sysBP", "diaBP"), each = length(quantiles_sysBP)),
  Thresholds = rep(c(quantiles_sysBP, quantiles_diaBP), times = 2),
  Group = rep(c("Yes CHD", "No CHD"), each = length(quantiles_sysBP) + length(quantiles_diaBP)),
  Probability = c(tail_prob_sysBP_yes, tail_prob_sysBP_no, tail_prob_diaBP_yes, tail_prob_diaBP_no)
)

print(comparison_df)

ggplot(comparison_df, aes(x = Thresholds, y = Probability, color = Group)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Tail Probabilities of Variables at Different Threshold Levels",
       x = "Threshold Level",
       y = "Estimated Probability") +
  theme_minimal()

##########################
#estimate of joint density of “sysBP” and “diaBP”

joint_density_yes <- kde2d(yesCHD$sysBP, yesCHD$diaBP, n = 100)
joint_density_no <- kde2d(noCHD$sysBP, noCHD$diaBP, n = 100)

density_yes_df <- data.frame(
  SysBP = rep(joint_density_yes$x, each = length(joint_density_yes$y)),
  DiaBP = rep(joint_density_yes$y, times = length(joint_density_yes$x)),
  Density = as.vector(joint_density_yes$z)
)

density_no_df <- data.frame(
  SysBP = rep(joint_density_no$x, each = length(joint_density_no$y)),
  DiaBP = rep(joint_density_no$y, times = length(joint_density_no$x)),
  Density = as.vector(joint_density_no$z)
)

ggplot() +
  geom_contour(data = density_yes_df, aes(x = SysBP, y = DiaBP, z = Density), color = "red") +
  geom_contour(data = density_no_df, aes(x = SysBP, y = DiaBP, z = Density), color = "blue") +
  labs(title = "Joint Density of SysBP and DiaBP",
       x = "Systolic Blood Pressure (sysBP)",
       y = "Diastolic Blood Pressure (diaBP)") +
  theme_minimal() +
  scale_color_gradient(name = "Density") +
  theme(legend.position = "none")

