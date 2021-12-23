rm(list=ls())

#####################################################################################################################
## In this file we use a SVM model to predict default
## In this file we hypertune SVM with 4 different kernels.
## Unfortunately due to computational power, the linear model is the only model that could be completed.
## The code for the other models is in this file.
## With a more powerful computer this code should do the trick.
#####################################################################################################################

#####################################################################################################################
## installing and loading packages
#####################################################################################################################
#install.packages("e1071")
#install.packages("caret")
#install.packages("fastDummies")
#install.packages("pROC")

library(e1071)
library(caret)
library(fastDummies)
library(pROC)

#####################################################################################################################
## loading data
#####################################################################################################################
## Loading in data
train <- read.csv(file = "./data/gold/train.csv")
test <- read.csv(file = "./data/gold/test.csv")
validation <- read.csv(file = "./data/gold/validation.csv")
all <- read.csv(file = "./data/gold/all.csv")

#####################################################################################################################
## one-hot encoding variables
#####################################################################################################################
one_hot_columns = c("application_type", "home_status","purpose","earliest_cr_line", "city","date_funded")
  
train <- dummy_cols(train, select_columns = one_hot_columns, remove_selected_columns = TRUE)
test <- dummy_cols(test, select_columns = one_hot_columns, remove_selected_columns = TRUE)
validation <- dummy_cols(validation, select_columns = one_hot_columns, remove_selected_columns = TRUE)
all <- dummy_cols(all, select_columns = one_hot_columns, remove_selected_columns = TRUE)
  
  
#####################################################################################################################
## hypertune svm with linear kernel
#####################################################################################################################
tune.out <- tune(svm, default ~ ., data= trainadj , kernel = "linear", ranges = list(
  cost= c(0.1,1,10,100)))
  
tune.out$best.parameters

svmfit <- svm(default ~ . , data = train, kernel = "linear", cost = 0,01, probability = TRUE)
fitted <- predict(svmfit, newdata= validation, probability = TRUE )
str(fitted)

preds.prob <- attributes(fitted)$probabilities[,1]

pROC::auc(pROC::roc(validation$default, preds.prob))


#####################################################################################################################
## hypertune svm with sigmoid kernel
#####################################################################################################################
tune.out <- tune(svm, default ~ ., data= train , kernel = "sigmoid", ranges = list(
  cost= c(0.1,1,10,100),
  gamma = c(0.5,1,2,3)))

tune.out$best.parameters

svmfit <- svm(default ~ . , data = train, kernel = "sigmoid", cost = 0.01, probability = TRUE)
fitted <- predict(svmfit, newdata= validation, probability = TRUE )
str(fitted)

preds.prob <- attributes(fitted)$probabilities[,1]

pROC::auc(pROC::roc(validation$default, preds.prob))

#####################################################################################################################
## hypertune svm with polynomial kernel
#####################################################################################################################
tune.out <- tune(svm, default ~ ., data= train , kernel = "polynomial", ranges = list(
  cost= c(0.1,1,10,100),
  gamma = c(0.5,1,2,3)))

tune.out$best.parameters

svmfit <- svm(default ~ . , data = train, kernel = "polynomial", cost = 0.01, probability = TRUE)
fitted <- predict(svmfit, newdata= validation, probability = TRUE )
str(fitted)

preds.prob <- attributes(fitted)$probabilities[,1]

pROC::auc(pROC::roc(validation$default, preds.prob))

#####################################################################################################################
## hypertune svm with radial kernel
#####################################################################################################################
  
tune.out <- tune(svm, default ~ ., data= train , kernel = "radial", ranges = list(
  cost= c(0.1,1,10,100),
  gamma = c(0.5,1,2,3)))

tune.out$best.parameters

svmfit <- svm(default ~ . , data = train, kernel = "radial", cost = 0.01, probability = TRUE)
fitted <- predict(svmfit, newdata= validation, probability = TRUE )
str(fitted)

preds.prob <- attributes(fitted)$probabilities[,1]

pROC::auc(pROC::roc(validation$default, preds.prob))
  


#####################################################################################################################
##  train model on all data and predict the test set
#####################################################################################################################

svmfit <- svm(default ~ . , data = all, kernel = "linear", cost = 0.01,gamma = 1, probability = TRUE)
fitted <- predict(svmfit, newdata= test, probability = TRUE )
str(fitted)

preds.prob <- attributes(fitted)$probabilities[,1]

id <- test$id

default <- preds.prob

Kaggle <- data.frame(id,default)

write.csv(Kaggle,"./data/result/Kaggle_SVM.csv", row.names = FALSE)
  

  
  
  
  
  
  
  
  
  