rm(list=ls())

#####################################################################################################################
## install and load packages
#####################################################################################################################
#install.packages("gam")
#install.packages("pROC")
#install.packages("ggplot2")

library(gam)
library(pROC)
library(ggplot2)

#####################################################################################################################
## install and load packages
#####################################################################################################################
set.seed(12)

#####################################################################################################################
## install and load packages
#####################################################################################################################
#load data 
train <- read.csv(file = "./data/gold/train.csv")
validation <- read.csv(file = "./data/gold/validation.csv")
test <-read.csv(file = "./data/gold/test.csv")
train_all <- read.csv(file = "./data/gold/all.csv")

#####################################################################################################################
## trying some different GAM models and evaluating with ANOVA
#####################################################################################################################
gam1 = gam(formula = default ~ s(amount,3) + s(annual_income,3) + s(debt_to_income,5) +
          s(grade_ordinal,3) + application_type, data = train)

gam2 = gam(formula = default ~ s(amount,3) + s(annual_income,3) + s(debt_to_income,5) + 
           s(grade_ordinal,5) + application_type, data = train)           

gam3 = gam(formula = default ~ s(amount,3) + s(annual_income,3) + s(debt_to_income,3) +
                s(grade_ordinal,5) + application_type, data = train)

#####################################################################################################################
##  linear model as baseline
#####################################################################################################################
gam_all = gam(formula = default ~ ., data = train)
summary(gam_all)

gam_manual = gam(formula = default ~ s(amount,3) + s(annual_income,3) + application_type + date_funded + s(debt_to_income,3) 
                 + earliest_cr_line + home_status + s(interest_rate,3) + s(monthly_payment,3) + s(num_bankrupts,3) + s(num_mortgages,3)
                 + s(num_open_credit,3) + s(num_records,3) + s(num_total_credit,3) + purpose + s(revol_balance,3) + s(revol_util,3) + had_missing_annual_income
                 + had_missing_monthly_payment + had_missing_num_bankrupts + had_missing_num_mortgages + had_missing_num_records + had_missing_num_total_credit
                 + had_missing_revol_util + s(days_between,3) + city + emp_length_ordinal + grade_ordinal + income_verif_status_ordinal 
                 + term_ordinal + s(sub_grade_ordinal,3) + manager + director + supervisor + senior + engineer + analyst + accountant
                 , data = train)



anova(gam1, gam2, gam3, gam_all, gam_manual, test="F")
summary(gam_manual)

#####################################################################################################################
## funtion to determine optimal levels
#####################################################################################################################

#function to check if change in df of a single variable improves anova
#try this for all non-categorical variables
gam_optimization <- function(variable){
  
  gam_1 <- gam(formula = default ~ ., data = train)
  gam_2 <- gam(formula = default ~ . - variable + s(variable,1), data = train)
  gam_3 <- gam(formula = default ~ . - variable + s(variable,2), data = train)
  gam_4 <- gam(formula = default ~ . - variable + s(variable,3), data = train)
  return(anova(gam_1,gam_2,gam_3,gam_4, test="F"))
}

#####################################################################################################################
## find the optimal model
#####################################################################################################################
attach(train)
gam_optimization(amount) #2 is best
gam_optimization(annual_income) #2 is best
gam_optimization(date_funded) #2 and 3 equally significant but 3 has highest F value
gam_optimization(debt_to_income) #2 highest F with 0.04 significance
gam_optimization(interest_rate) # 2 is best
gam_optimization(monthly_payment) # 2 is best
gam_optimization(num_bankrupts) # 2 is slightly better than 3
gam_optimization(num_mortgages) # 3 is best
gam_optimization(num_open_credit) # 1 is best
gam_optimization(num_records) # 3 is best
gam_optimization(num_total_credit) # linear is best
gam_optimization(revol_balance) # 2 is best
gam_optimization(revol_util) # 1 is best
gam_optimization(days_between) # 1 is best
gam_optimization(sub_grade_ordinal) # 3 is best

detach(train)

gam_evaluated = gam(formula = default ~ s(amount,2) + s(annual_income,2) + application_type + date_funded + s(debt_to_income,2) 
                    + earliest_cr_line + home_status + s(interest_rate,2) + s(monthly_payment,2) + s(num_mortgages,3)
                    + s(num_open_credit,1) + s(num_records,3) + num_total_credit + purpose + s(revol_balance,2) + s(revol_util,1)
                    + had_missing_monthly_payment + had_missing_num_bankrupts + had_missing_num_mortgages + had_missing_num_records
                    + had_missing_revol_util + s(days_between,1) + city + emp_length_ordinal + grade_ordinal + income_verif_status_ordinal 
                    + term_ordinal + s(sub_grade_ordinal,3) + senior + engineer + analyst + accountant
                    , data = train)


#####################################################################################################################
## evaluate the models
#####################################################################################################################
#also remove variables that are certainly not significant
summary(gam_evaluated)

#look at anova
anova(gam_all, gam_manual, gam_evaluated, test = "F")

#####################################################################################################################
## make predictions of the validation set
#####################################################################################################################
preds_all <- predict(gam_all,type = 'response',  newdata = validation)
preds_manual <- predict(gam_manual, type = 'response', newdata = validation)
preds_evaluated <- predict(gam_evaluated, type = 'response', newdata = validation) 

roc_all <- roc(validation$default, preds_all)
roc_manual <- roc(validation$default, preds_manual)
roc_evaluated <- roc(validation$default, preds_evaluated)

# auc all
auc(roc_all)

#auc manual gam model
auc(roc_manual)

# auc evaluated
auc(roc_evaluated)
plot_roc_obj <- plot(roc_evaluated)

#####################################################################################################################
## train the model on all the data and predicct the test set
#####################################################################################################################
gam_all_testing = gam(formula = default ~ ., data = train_all)

gam_manual_testing = gam(formula = default ~ s(amount,3) + s(annual_income,3) + application_type + date_funded + s(debt_to_income,3) 
                         + earliest_cr_line + home_status + s(interest_rate,3) + s(monthly_payment,3) + s(num_bankrupts,3) + s(num_mortgages,3)
                         + s(num_open_credit,3) + s(num_records,3) + s(num_total_credit,3) + purpose + s(revol_balance,3) + s(revol_util,3) + had_missing_annual_income
                         + had_missing_monthly_payment + had_missing_num_bankrupts + had_missing_num_mortgages + had_missing_num_records + had_missing_num_total_credit
                         + had_missing_revol_util + s(days_between,3) + city + emp_length_ordinal + grade_ordinal + income_verif_status_ordinal 
                         + term_ordinal + s(sub_grade_ordinal,3) + manager + director + supervisor + senior + engineer + analyst + accountant
                         , data = train_all)

gam_evaluated_testing = gam(formula = default ~ s(amount,2) + s(annual_income,2) + application_type + date_funded + s(debt_to_income,2) 
                            + earliest_cr_line + home_status + s(interest_rate,2) + s(monthly_payment,2) + s(num_mortgages,3)
                            + s(num_open_credit,1) + s(num_records,3) + num_total_credit + purpose + s(revol_balance,2) + s(revol_util,1)
                            + had_missing_monthly_payment + had_missing_num_bankrupts + had_missing_num_mortgages + had_missing_num_records
                            + had_missing_revol_util + s(days_between,1) + city + emp_length_ordinal + grade_ordinal + income_verif_status_ordinal 
                            + term_ordinal + s(sub_grade_ordinal,3) + senior + engineer + analyst + accountant
                            , data = train_all)

preds_all_test <- predict(gam_all_testing,type = 'response',  newdata = test)
preds_manual_test <- predict(gam_manual_testing, type = 'response', newdata = test)
preds_evaluated_test <- predict(gam_evaluated_testing, type = 'response', newdata = test)

#####################################################################################################################
## write predictions to results
#####################################################################################################################

#all
gam_all_preds <- data.frame(id = test$id,
                              default = preds_all_test)
write.csv(gam_all_preds, file = "./data/result/Kaggle_GAM_all.csv", row.names = F)

#manual
gam_manual_preds <- data.frame(id = test$id,
                                  default = preds_manual_test)
write.csv(gam_manual_preds, file = "./data/result/Kaggle_GAM_manual.csv", row.names = F)
  
#evaluated
gam_evaluated_preds <- data.frame(id = test$id,
                                  default = preds_evaluated_test)
write.csv(gam_evaluated_preds, file = "./data/result/Kaggle_GAM_evaluated.csv", row.names = F)



