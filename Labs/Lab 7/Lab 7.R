#############
# Load Libraries and Data
#############
library(readr)
library(arules)
library(arulesViz)

# Load the grocery basket data
grocery_data <- read_csv("~/Desktop/MTH443/Labs/Lab 7/grocery_baskets.csv")
txn <- read.transactions(file="~/Desktop/MTH443/Labs/Lab 7/grocery_baskets.csv",
                         rm.duplicates=TRUE, format="basket", sep=",", cols=1)

# Clean up item labels by removing any unwanted characters like quotes
txn@itemInfo$labels <- gsub("\"", "", txn@itemInfo$labels)

#############
# Generate Association Rules
#############
# Set initial support and confidence thresholds and generate rules
basket_rules <- apriori(txn, parameter = list(sup = 0.01, conf = 0.5, target="rules"))
inspect(basket_rules)

# Convert rules to a data frame for easier analysis
df_basket <- as(basket_rules, "data.frame")
View(df_basket)

#############
# Plot General Visualizations for the Rules
#############
# Basic plot of the rules
plot(basket_rules)
plot(basket_rules, method="grouped", control=list(k=5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord", control=list(alpha=0.5, reorder=TRUE))

# Plot item frequencies for the top 5 items by support
itemFrequencyPlot(txn, topN=5, main="Top 5 Items by Support")

#############
# Parameter Variation for Association Rules
#############
# Define parameter combinations for minimum support and confidence
params <- list(
  list(minsup=0.1, minconf=0.6),
  list(minsup=0.01, minconf=0.2),
  list(minsup=0.05, minconf=0.3),
  list(minsup=0.5, minconf=0.7)
)

# Generate and inspect rules for each parameter set
rules_list <- list()
for (i in seq_along(params)) {
  cat("\nRules for minsup =", params[[i]]$minsup, "and minconf =", params[[i]]$minconf, ":\n")
  cat("--------------------------------------------------\n")
  rules_list[[i]] <- apriori(txn, parameter=list(sup=params[[i]]$minsup, conf=params[[i]]$minconf, target="rules"))
  inspect(rules_list[[i]])
}

#############
# Display and Plot Rules for a Selected Parameter Set
#############
# Check and display rules for each parameter set
for (i in seq_along(rules_list)) {
  minsup <- params[[i]]$minsup
  minconf <- params[[i]]$minconf
  cat("\nParameter set:", i, "with minsup =", minsup, "and minconf =", minconf, "\n")
  cat("--------------------------------------------------\n")
  
  if (length(rules_list[[i]]) == 0) {
    cat("No rules found for this parameter set.\n")
  } else {
    inspect(rules_list[[i]])
  }
  cat("\n") 
}

# Convert rules to a data frame for easier analysis and view
df_basket <- as(rules_list[[2]], "data.frame")
View(df_basket)

#############
# Visualizations for a Selected Parameter Set
#############
# Scatter plot for visualizing rule strength
plot(rules_list[[2]], main="Scatter Plot of Rules")

# Grouped plot for the rules
plot(rules_list[[2]], method="grouped", control=list(k=5), main="Grouped Plot of Rules")

# Graph plot to visualize item relationships in the rules
plot(rules_list[[2]], method="graph", control=list(type="items"), main="Graph Plot of Rules")

# Parallel coordinate plot for a more detailed comparison
plot(rules_list[[2]], method="paracoord", control=list(alpha=0.5, reorder=TRUE), main="Parallel Coordinate Plot of Rules")

# Plot the top 5 items by support
itemFrequencyPlot(txn, topN=5, main="Top 5 Items by Support")

