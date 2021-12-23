
rm(list=ls())

#####################################################################################################################
## In this file a general exploration of the data is done.
## The missing variables are located and handled
## The complete data is written to a new csv file.
#####################################################################################################################

#####################################################################################################################
## installing and loading packages
#####################################################################################################################

#install.packages("mice")
#install.packages("VIM")
#install.packages("Hmisc")

## load in packages
library(Hmisc)
library(mice)
library(VIM)

#####################################################################################################################
## load data
#####################################################################################################################
#we decided not to split the train in train and validation,
#because we only used the validation set to get an estimate
#of how our model performed.
train <- read.csv(file = "./data/bronze/train.csv")
test <- read.csv(file = "./data/bronze/test.csv")

#####################################################################################################################
## data exploration
#####################################################################################################################

## initial exploration
head(train)
colnames(train)
head(test)
colnames(test)

## number of variables per set
length(names(train))
length(names(test))

## list of unique values per column
list_uniques_train <- lapply(train, unique)
list_uniques_test <- lapply(test,unique)

## histograms of the data
names <- names(train)
for(name in names){
  hist(train[name])
}

## data description and summary
describe(train)
summary(train)

describe(test)
summary(test)

## Missing values train

## annual income is incomplete
length(which(is.na(train$annual_income) ))

## emp_length is incomplete
length(which(is.na(train$emp_length) ))

## emp_title is incomplete
length(which(is.na(train$emp_title) ))

## home status is incomplete
length(which(is.na(train$home_status) ))

##  monthly payment is incomplete
length(which(is.na(train$monthly_payment) ))

## num_bankrupts is incomplete 
length(which(is.na(train$num_bankrupts) ))

## num mortgages is incomplete
length(which(is.na(train$num_mortgages) ))

## num records is incomplete
length(which(is.na(train$num_records) ))

## num total credit is incomplete
length(which(is.na(train$num_total_credit) ))

## revol util is incomplete
length(which(is.na(train$revol_util) ))

## if not mentioned above the column has no missing values

## missing data is in same columns as in training set

## interpretation of sub_grade
unique(train$sub_grade) # if we follow the logic that the rating agencies follow, we conclude that A1 is the best and G5 is the worst
# within each letter 1 is best and 5 is worst. The 1 of a lower letter is also worse than a 5 of a higher letter


#####################################################################################################################
## make indicator column of missing variable of training set
#####################################################################################################################
train$had_missing_annual_income <- is.na(train$annual_income)
train$had_missing_emp_length <-is.na(train$emp_length)
train$had_missing_emp_title <- is.na(train$emp_title)
train$had_missing_home_status <-is.na(train$home_status)
train$had_missing_monthly_payment <- is.na(train$monthly_payment)
train$had_missing_num_bankrupts <- is.na(train$num_bankrupts)
train$had_missing_num_mortgages <-is.na(train$num_mortgages)
train$had_missing_num_records <-is.na(train$num_records)
train$had_missing_num_total_credit <- is.na(train$num_total_credit)
train$had_missing_revol_util <-is.na(train$revol_util)

#####################################################################################################################
## Handling missing data of training set
#####################################################################################################################
md.pattern(train)
agg_plot <- aggr(train, col = c("navyblue","red"), numbers = TRUE, sortVars =TRUE, labels = names(train), cex.axis = .7, gap =3, ylab = c("Histogram of missing data","Pattern"))

missing_value_computation <- mice(train, m=5, maxit = 10,meth ="pmm", seed = 12)
complete_train <- complete(missing_value_computation,1)

complete_train$home_status[is.na(complete_train$home_status)] <- "Missing"
complete_train$emp_title[is.na(complete_train$emp_title)] <- "Missing"
complete_train$emp_length[is.na(complete_train$emp_length)] <- "Missing"

describe(complete_train)

#####################################################################################################################
## make indicator column of missing variable of test set
#####################################################################################################################
test$had_missing_annual_income <- is.na(test$annual_income)
test$had_missing_emp_length <-is.na(test$emp_length)
test$had_missing_emp_title <- is.na(test$emp_title)
test$had_missing_home_status <-is.na(test$home_status)
test$had_missing_monthly_payment <- is.na(test$monthly_payment)
test$had_missing_num_bankrupts <- is.na(test$num_bankrupts)
test$had_missing_num_mortgages <-is.na(test$num_mortgages)
test$had_missing_num_records <-is.na(test$num_records)
test$had_missing_num_total_credit <- is.na(test$num_total_credit)
test$had_missing_revol_util <-is.na(test$revol_util)

#####################################################################################################################
## Handling missing data of test set
#####################################################################################################################
# categorical variables
test$home_status[is.na(test$home_status)] <- "Missing"
test$emp_title[is.na(test$emp_title)] <- "Missing"
test$emp_length[is.na(test$emp_length)] <- "Missing"

# numeric variables
mean_annual_income_train <- round(mean(complete_train$annual_income),2)
test$annual_income[is.na(test$annual_income)] <- mean_annual_income_train

mean_monthly_payment_train <- round(mean(complete_train$monthly_payment),2)
test$monthly_payment[is.na(test$monthly_payment)] <- mean_monthly_payment_train

median_num_bankrupts_train <- median(complete_train$num_bankrupts)
test$num_bankrupts[is.na(test$num_bankrupts)] <- median_num_bankrupts_train

median_num_mortgages_train <- median(complete_train$num_mortgages)
test$num_mortgages[is.na(test$num_mortgages)] <- median_num_mortgages_train

median_num_records_train <- median(complete_train$num_records)
test$num_records[is.na(test$num_records)] <- median_num_records_train

mean_num_total_credit_train <- round(mean(complete_train$num_total_credit))
test$num_total_credit[is.na(test$num_total_credit)] <- mean_num_total_credit_train

mean_revol_util_train <- round(mean(complete_train$revol_util),2)
test$revol_util[is.na(test$revol_util)] <- mean_revol_util_train

describe(test)

#####################################################################################################################
## write csv of data with no missing values to silver layer
#####################################################################################################################

write.csv(complete_train,"./data/silver/train.csv")
write.csv(test,"./data/silver/test.csv")







