###########################################################################
## this is the file where we hypertuned and fitted 
## our boosting model
###########################################################################
library(pROC)
library(gbm)
library(date)
library(lubridate)
library(dummy)
###########################################################################
##reading in the data
###########################################################################
train <- read.csv("./data/gold/train.csv")
val <- read.csv("data/gold/validation.csv")
test <- read.csv("data/gold/test.csv")
all <- read.csv("data/gold/all.csv")
###########################################################################
##boosting
###########################################################################

##preprocessing data

#one hot encoding
one_hot_columns <- c("application_type","earliest_cr_line", "purpose", "home_status", "city", "date_funded")
# get categories and dummies
cats <- categories(train[, one_hot_columns])
# apply on train set (exclude reference categories)
train_dummies <- dummy(train[, one_hot_columns], object = cats)
# apply on test set (exclude reference categories)
test_dummies <- dummy(test[, one_hot_columns], object = cats)
#apply on validation set
val_dummies <- dummy(val[, one_hot_columns], object = cats)
cols.num <- names(train_dummies)
#apply on all set
all_dummies <- dummy(all[, one_hot_columns], object = cats)
#now we need to merge these with train, test and val
train <- cbind(train, train_dummies)
val <- cbind(val, val_dummies)
test <- cbind(test, test_dummies)
all <- cbind(all, all_dummies)
#now we drop the original columns
train <- subset(train, select = -c(application_type,earliest_cr_line, purpose, home_status, city, date_funded))
val <- subset(val, select = -c(application_type,earliest_cr_line, purpose, home_status, city, date_funded))
test <- subset(test, select = -c(application_type,earliest_cr_line, purpose, home_status, city, date_funded))
all <- subset(all, select = -c(application_type,earliest_cr_line, purpose, home_status, city, date_funded))
# we convert the columns to numeric
train[cols.num] <- sapply(train[cols.num],as.numeric)
all[cols.num] <- sapply(all[cols.num], as.numeric)

## hypertuning

#we will use the cross validation error to hypertune
cv_boosting <- gbm(default~., data = all, distribution = "bernoulli", n.trees = 1000, interaction.depth = 11, shrinkage = 0.09, cv.folds = 5)
mean(cv_boosting$cv.error)
#we will now estimate the AUC on the train data
cv_boosting <- gbm(default~., data = train, distribution = "bernoulli", n.trees = 1000, interaction.depth = 11, shrinkage = 0.09)
yhat_boost <- predict(cv_boosting, newdata = val, n.trees = 1000)
roc_val <- roc(val$default, yhat_boost)
auc(roc_val)
plot.roc(roc_val)

##now we fit the final model:
#fitting
cv_boosting <- gbm(default~., data = all, distribution = "bernoulli", n.trees = 1000, interaction.depth = 11, shrinkage = 0.09)
cv_boosting
#prediction
#test$home_status_ANY <- 0
#test$home_status_NONE <- 0
boosting_preds <- predict(cv_boosting, type = "response", newdata = test )
boosting_preds
id <- test$id
default <- boosting_preds
Kaggle <- data.frame(id,default)
write.csv(Kaggle,"./data/result/Kaggle_boosting_3.csv", row.names = FALSE)
