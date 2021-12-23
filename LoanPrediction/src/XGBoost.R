rm(list=ls())

#####################################################################################################################
## In this file we make an extreme gradient boosting model to predict default
#####################################################################################################################

#####################################################################################################################
## install and load packages
#####################################################################################################################

#install.packages("pROC")
#install.packages("data.table")
#install.packages("mlr")
#install.packages("caret")
#install.packages("xgboost")

library(pROC)
library(data.table)
library(mlr)
library(caret)
library(xgboost)

#####################################################################################################################
## load data
#####################################################################################################################
train <- read.csv(file = "./data/gold/train.csv")
validation <- read.csv(file = "./data/gold/validation.csv")
test <-read.csv(file = "./data/gold/test.csv")

#####################################################################################################################
## make this example reproducible
#####################################################################################################################

set.seed(12)

#####################################################################################################################
## prepare data for xgboost model
#####################################################################################################################
#we need to put the data in an xgb-matrix
#define predictor and response variables in training set
train_x = data.matrix(train[, -ncol(train)])
train_y = train[,ncol(train)]

#define predictor and response variables in testing set
val_x = data.matrix(validation[, -ncol(validation)])
val_y = validation[, ncol(validation)]

#define final training, validation and test
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_val = xgb.DMatrix(data = val_x, label = val_y)
xgb_test = xgb.DMatrix(data.matrix(test))

#####################################################################################################################
##  select parameters to hypertune
#####################################################################################################################
# we only looked at 10,000 random combinations because of
# computational and time constraints
# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(12)
for (iter in 1:10){
  param <- list(booster = "gbtree",
                objective = "binary:logistic",
                max_depth = sample(3:15, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1),
                alpha = 0,
                lambda = 0
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)


#####################################################################################################################
## hypertune parameters
#####################################################################################################################
for (row in 1:nrow(parameters_df)){
  set.seed(12)
  mdcv <- xgb.train(data=xgb_train,
                    booster = "gbtree",
                    objective = "binary:logistic",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    nrounds= 100000,
                    eval_metric = "auc",
                    early_stopping_rounds= 30,
                    print_every_n = 100,
                    watchlist = list(train= xgb_train, val= xgb_val),
                    alpha = 0,
                    lambda = 0
                    )
  
  lowest_error <- as.data.frame(max(mdcv$evaluation_log$val_auc))
  lowest_error <- cbind(lowest_error,mdcv$best_iteration)
  lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

#####################################################################################################################
## Use optimal parameters for final model
#####################################################################################################################
# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

mdcv$best_score

max_row <- randomsearch[randomsearch$`max(mdcv$evaluation_log$val_auc)` == max(randomsearch$`max(mdcv$evaluation_log$val_auc)`),]



final <-  xgboost(data = xgb_train,params = list(booster = "gbtree",
                                                 objective = "binary:logistic",
                                                 max_depth = max_row$max_depth,
                                                 eta = max_row$eta,
                                                 subsample = max_row$subsample,
                                                 colsample_bytree = max_row$colsample_bytree,
                                                 min_child_weight = max_row$min_child_weight),
                  nrounds = max_row$`mdcv$best_iteration`, print_every_n = 100,verbose = TRUE,eval_metric ="auc", alpha = 0, lambda = 0)



#####################################################################################################################
## predict output for the test set
#####################################################################################################################
pred_y <-predict(final,xgb_val)

roc_obj <- roc(validation$default, pred_y)
auc(roc_obj)
plot_roc_obj <- plot(roc_obj)

pred_test <- predict(final, xgb_test) 

#####################################################################################################################
## write the predictions of the test set
#####################################################################################################################
id <- test$id
default <- pred_test

Kaggle <- data.frame(id,default)

write.csv(Kaggle,"./data/result/Kaggle_XGBoost_2.csv", row.names = FALSE)


