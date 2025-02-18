###############
#loading libraries
###############
library(MASS)
library(naivebayes)


##############
#Question 1
##############
iris <- read_csv("Desktop/MTH443/Labs/Lab 9/iris.csv")

calculateMisclf <- function(model, x, y)
{
  model.pred <- predict(object = model, newdata = x)$class
  model.pred <- as.vector(model.pred)
  misclf <- mean(y != model.pred)
  return(misclf)
}

################
#Test/Train Split for the model
################

#randomly choose 90% for the train
n.samples <- nrow(iris)
train.indices <- sample(x = 1:n.samples,
                        size = floor(0.9 * n.samples))
train <- iris[train.indices, ]
test <- iris[-c(train.indices), ]

# Separate predictors (features) and target variable for both training and testing data
x.train <- subset(train, select = -c(Species_name))
y.train <- train$Species_name
x.test <- subset(test, select = -c(Species_name))
y.test <- test$Species_name

# LDA
lda.model <- lda(Species_name ~ ., data = train, 
                 prior = c(0.33, 0.33, 0.34))

misclf.train.lda <- calculateMisclf(lda.model, x.train, y.train) 
misclf.test.lda <- calculateMisclf(lda.model, x.test, y.test)  

# QDA
qda.model <- qda(Species_name ~ ., data = train, 
                 prior = c(0.33, 0.33, 0.34))

misclf.train.qda <- calculateMisclf(qda.model, x.train, y.train)
misclf.test.qda <- calculateMisclf(qda.model, x.test, y.test)

# Bayes
bayes.model <- naive_bayes(Species_name ~ ., data = train,
                           usekernel = TRUE)

# Predict class labels for training and test data
bayes.train.pred <- as.vector(predict(bayes.model, x.train))
bayes.test.pred <- as.vector(predict(bayes.model, x.test))

# Calculate misclassification rates for Naive Bayes
misclf.train.bayes <- mean(y.train != bayes.train.pred)
misclf.test.bayes <- mean(y.test != bayes.test.pred)

##########
#Results Summary
##########
misclf <- matrix(data = c(misclf.train.lda, misclf.test.lda, 
                          misclf.train.qda, misclf.test.qda,
                          misclf.train.bayes, misclf.test.bayes),
                 byrow = T, ncol = 2)
rownames(misclf) <- c("LDA", "QDA", "Bayes")
colnames(misclf) <- c("Train", "Test")
print(misclf)

##############
#Question 2
##############

wine <- read_csv("Desktop/MTH443/Labs/Lab 9/wine_italy.csv")

calculateMisclf <- function(model, x, y) {
  model.pred <- predict(object = model, newdata = x)$class
  model.pred <- as.vector(model.pred)
  misclf <- mean(y != model.pred)
  return(misclf)
}

################
#Test/Train Split for the model
################
n.samples <- nrow(wine)
train.indices <- sample(x = 1:n.samples, size = floor(0.9 * n.samples))
train <- wine[train.indices, ] 
test <- wine[-c(train.indices), ] 

# Separate predictors (features) and target variable for both training and testing data
x.train <- subset(train, select = -c(Type))
y.train <- train$Type                       
x.test <- subset(test, select = -c(Type))   
y.test <- test$Type      

#LDA
lda.model <- lda(Type ~ ., data = train)
misclf.train.lda <- calculateMisclf(lda.model, x.train, y.train)
misclf.test.lda <- calculateMisclf(lda.model, x.test, y.test) 

#QDA
qda.model <- qda(Type ~ ., data = train)
misclf.train.qda <- calculateMisclf(qda.model, x.train, y.train)
misclf.test.qda <- calculateMisclf(qda.model, x.test, y.test)  

#Naive Bayes
bayes.model <- naive_bayes(Type ~ ., data = train, prior = c(1/3, 1/3, 1/3), usekernel = TRUE)

# Predict class labels for training and test data
bayes.train.pred <- as.vector(predict(bayes.model, x.train))
bayes.test.pred <- as.vector(predict(bayes.model, x.test))

# Calculate misclassification rates for Naive Bayes
misclf.train.bayes <- mean(y.train != bayes.train.pred)  
misclf.test.bayes <- mean(y.test != bayes.test.pred)   

##########
#Results Summary
##########
misclf <- matrix(data = c(misclf.train.lda, misclf.test.lda, 
                          misclf.train.qda, misclf.test.qda,
                          misclf.train.bayes, misclf.test.bayes),
                 byrow = TRUE, ncol = 2)

rownames(misclf) <- c("LDA", "QDA", "Naive Bayes")
colnames(misclf) <- c("Train", "Test")
print(misclf)
