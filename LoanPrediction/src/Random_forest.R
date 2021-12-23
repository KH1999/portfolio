###########################################################################
## This is the file where we hypertuned and fitted our
## random forest model
###########################################################################

library(randomForest)
library(caret)
library(pROC)
library(date)
library(lubridate)
###########################################################################
##reading in the data
###########################################################################
train <- read.csv("./data/gold/train.csv")
val <- read.csv("data/gold/validation.csv")
test <- read.csv("data/gold/test.csv")
all <- read.csv("data/gold/all.csv")

###########################################################################
##hypertuning
###########################################################################
train$default <- as.character(train$default)
train$default <- as.factor(train$default)
val$default <- as.character(val$default)
val$default <- as.factor(val$default)
#we will use the OOB estimate of the error rate to first
#estimate the optimal value of ntree keeping mtry (6) fixed
rf <- randomForest(default ~ ., data = train, importance = TRUE, verbose = TRUE, ntree = 800)
rf
#for computational reasons, we will not go any further
#now we will vary the mtry
rf <- randomForest(default ~ ., data = train, importance = TRUE, verbose = TRUE, ntree = 1000, mtry = 15)
rf
#we will fit a random forest with ntree=1000 and mtry=8
###################################################################
##final modeling
###################################################################
#we now estimate the AUC on the validation set
rf <- randomForest(default ~ ., data = train, importance = TRUE, verbose = TRUE, ntree = 1000, mtry = 8, keepforest = TRUE)
rf
rf_prob <- predict(rf, type = "prob", newdata = val)
rf_prob
rf_prob[,2]
roc_obj <- roc(val$default, rf_prob[,2])
auc(roc_obj)
#final model
rf_final <- randomForest(default ~ ., data = all, ntree = 1000, mtry = 8)
rf_prob_test <- predict(rf_final, type = "prob", newdata = test)
rf_prob_test[,2]
id <- test$id
default <- rf_prob_test[,2]
Kaggle <- data.frame(id,default)
View(Kaggle)
write.csv(Kaggle,"./data/result/Kaggle_randomForest_1.csv", row.names = FALSE)
