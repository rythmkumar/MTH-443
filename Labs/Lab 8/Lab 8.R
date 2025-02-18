########
#libraries
#########

library(readr)
library(MASS)
library(broom)
library(tidyr)
library(caret)
library(data.table)

############
# Read data
############
heart <- read_csv("Desktop/MTH443/Labs/Lab 8/heart.csv")
heart[1:7] <- scale(heart[1:7])

##################
#Splitting the data
##################
smp_size <- floor(0.9 * nrow(heart))
train_ind <- sample(seq_len(nrow(heart)), size = smp_size)
train <- heart[train_ind, ]
test <- heart[-train_ind, ]

############
#fischer linear discriminant function
model <- lda(`coronary hd`~., data=train)
predicted <- predict(model, test)
###############
#Check for accuracy
lda_accuracy <-mean(predicted$class==test$`coronary hd`)
###############
#Check for misclassification
lda_misclassification <-mean(predicted$class!=test$`coronary hd`)

###############
#Check for confusion matrix
lda_conf_matrix <- table(Predicted = predicted$class, Actual = test$`coronary hd`)
print(lda_conf_matrix)

############
#Quadratic discriminant function
model2 <- qda(`coronary hd`~., data=train)
predicted2 <- predict(model2, test)
###############
#Check for accuracy
qda_accuracy <- mean(predicted2$class==test$`coronary hd`)
###############
#Check for misclassification
qda_misclassification <- mean(predicted2$class!=test$`coronary hd`)
###############
#Check for confusion matrix
qda_conf_matrix <- table(Predicted = predicted2$class, Actual = test$`coronary hd`)
print(qda_conf_matrix)

###############
#Summary table
##############
summary_table <- data.frame(
  Model = c("LDA", "QDA"),
  Accuracy = c(lda_accuracy, qda_accuracy),
  Misclassification = c(lda_misclassification, qda_misclassification)
)
print(summary_table)

###############
#Question2
###############

currency_crisis <- read_csv("Desktop/MTH443/Labs/Lab 8/currency_crisis.csv")
summary(currency_crisis)
currency_crisis <- data.frame(currency_crisis)

colnames(currency_crisis) <- currency_crisis[1,]
currency_crisis <- currency_crisis[-1,]

rownames(currency_crisis) <- currency_crisis[,1]
currency_crisis <- currency_crisis[ ,-1]
currency_crisis <- apply(currency_crisis, 2,as.numeric)
summary(currency_crisis)

colnames(currency_crisis)[17] <- "is_crisis"

#########
#Test/Train Split
#########
#1: Last 10% of observations in the test set
set.seed(123)
n <- nrow(currency_crisis)
test_indices1 <- seq((n - floor(0.1 * n) + 1), n)
train1 <- currency_crisis[-test_indices1, ]
test1 <- currency_crisis[test_indices1, ]

#########
#2: Every 10th observation in the test set
test_indices2 <- seq(1, n, by = 10)
train2 <- currency_crisis[-test_indices2, ]
test2 <- currency_crisis[test_indices2, ]

evaluate_model <- function(train_data, test_data) {
  # LDA model
  lda_model <- lda(is_crisis ~ ., data = as.data.frame(train_data))
  lda_train_pred <- predict(lda_model, as.data.frame(train_data))$class
  lda_test_pred <- predict(lda_model, as.data.frame(test_data))$class
  
  # Misclassification rates for LDA
  lda_train_misclassification <- mean(lda_train_pred != train_data[, "is_crisis"])
  lda_test_misclassification <- mean(lda_test_pred != test_data[, "is_crisis"])
  
  # QDA model
  qda_model <- qda(is_crisis ~ ., data = as.data.frame(train_data))
  qda_train_pred <- predict(qda_model, as.data.frame(train_data))$class
  qda_test_pred <- predict(qda_model, as.data.frame(test_data))$class
  
  # Misclassification rates for QDA
  qda_train_misclassification <- mean(qda_train_pred != train_data[, "is_crisis"])
  qda_test_misclassification <- mean(qda_test_pred != test_data[, "is_crisis"])
  
  # Return a summary of results
  return(data.frame(
    Model = c("LDA", "QDA"),
    Train_Misclassification = c(lda_train_misclassification, qda_train_misclassification),
    Test_Misclassification = c(lda_test_misclassification, qda_test_misclassification)
  ))
}

results_scenario1 <- evaluate_model(train1, test1)
results_scenario2 <- evaluate_model(train2, test2)

print(results_scenario1)
print(results_scenario2)

###############
#Question3
###############
wine <- read_csv("Desktop/MTH443/Labs/Lab 8/wine.csv")
wine <- wine[rowSums(is.na(wine)) < ncol(wine), ]
wine <- wine[,colSums(is.na(wine)) < nrow(wine)]
wine$Type <- as.factor(wine$Type)


#########
#Test/Train Split
#########

set.seed(123)
n <- nrow(wine)
test_indices <- seq((n - floor(0.1 * n) + 1), n)
train <- wine[-test_indices, ]
test <- wine[test_indices, ]

evaluate_wine_model <- function(train_data, test_data) {
  # LDA model
  lda_model <- lda(Type ~ ., data = train_data)
  lda_train_pred <- predict(lda_model, train_data)$class
  lda_test_pred <- predict(lda_model, test_data)$class
  
  # Misclassification rates for LDA
  lda_train_misclassification <- mean(lda_train_pred != train_data$Type)
  lda_test_misclassification <- mean(lda_test_pred != test_data$Type)
  
  # QDA model
  qda_model <- qda(Type ~ ., data = train_data)
  qda_train_pred <- predict(qda_model, train_data)$class
  qda_test_pred <- predict(qda_model, test_data)$class
  
  # Misclassification rates for QDA
  qda_train_misclassification <- mean(qda_train_pred != train_data$Type)
  qda_test_misclassification <- mean(qda_test_pred != test_data$Type)
  
  # Return a summary of results
  return(data.frame(
    Model = c("LDA", "QDA"),
    Train_Misclassification = c(lda_train_misclassification, qda_train_misclassification),
    Test_Misclassification = c(lda_test_misclassification, qda_test_misclassification)
  ))
}

results <- evaluate_wine_model(train, test)
print(results)
