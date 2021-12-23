rm(list=ls())

#####################################################################################################################
## load in data
#####################################################################################################################

pred_logreg <- read.csv(file = "./data/result/Kaggle_logreg_14.csv")
pred_tree <- read.csv(file = "./data/result/Kaggle_boosting_3.csv")
pred_gam <- read.csv(file = "./data/result/Kaggle_GAM_manual.csv")
pred_XGboost <- read.csv(file = "./data/result/Kaggle_XGBoost_2.csv")

#####################################################################################################################
## rename the default columns
#####################################################################################################################
names(pred_gam)[2] <- "pred_gam"
names(pred_logreg)[2] <- "pred_logreg"
names(pred_tree)[2] <- "pred_tree"
names(pred_XGboost)[2] <- "pred_XGBoost"

#####################################################################################################################
## merge the different dataframes
#####################################################################################################################
all <- merge(pred_logreg,pred_gam, by ="id")
all <-merge(all,pred_tree, by ="id")
all <-merge(all,pred_XGboost, by ="id")

#####################################################################################################################
## use soft voting to get a default prediction
#####################################################################################################################
all$sum <- all$pred_logreg+all$pred_gam+all$pred_tree+all$pred_XGBoost

all$default <- all$sum /4

#####################################################################################################################
## write predictions to results
#####################################################################################################################
Kaggle <- subset(all, select = c(id,default))

write.csv(Kaggle,"./data/result/ensemble_model_6.csv", row.names = FALSE)
