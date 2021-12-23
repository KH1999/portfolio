rm(list=ls())


#####################################################################################################################
## In this file, we do the feature engineering. First new variables will be created.
## Then there will be integer coding of some categorical variables. Hereafter we do one hot encoding.
## At the end we scale the data and we handle class imbalance
#####################################################################################################################


#####################################################################################################################
## installing and loading packages
#####################################################################################################################
#install.packages("Hmisc")
#install.packages("fastDummies")
#install.packages("xts")
#install.packages("date")
#install.packages("lubridate")
library(Hmisc)
library(fastDummies)
library(xts)
library(date)
library(lubridate)
library(caret)



#####################################################################################################################
## load data and remove unnecessary index
#####################################################################################################################
train <- read.csv(file = "./data/silver/train.csv")
test <- read.csv(file = "./data/silver/test.csv")

train <- subset(train, select= -c(X))
test <- subset(test, select= -c(X))

describe(train)
describe(test)

train_no_default <- subset(train, select = -default)

#####################################################################################################################
## merge train and test for easier computations
#####################################################################################################################


all <- rbind(train_no_default,test)


#####################################################################################################################
## compute days between earliest_cr_time and date_funded
#####################################################################################################################

get_days_between <- function(data_end, data_start){
  dates_funded <- parse_date_time(data_end, "%b-%Y")
  date_funded <- as.Date(dates_funded, format = "%Y-%m-%d")
  dates_cr <- parse_date_time(data_start, "%b-%Y")
  date_cr <- as.Date(dates_cr, format = "%Y-%m-%d")
  paytime <- date_funded - date_cr
  paytime <- as.numeric(paytime)
  return(paytime)
}

all$days_between <- get_days_between(all$date_funded,all$earliest_cr_line)

#####################################################################################################################
## compute city extraction
#####################################################################################################################

city_extraction <- function(addresses){
  #we first extract the state and the postal code
  match <- regexec ("[A-Z]{2}\\s[0-9]{5}$", addresses)
  state_postal <- regmatches(addresses,match)
}

all$city <- unlist(city_extraction(all$address))

#####################################################################################################################
##  only use year in date_funded and earliest_cr_time
#####################################################################################################################

all$date_funded <- parse_date_time(all$date_funded, "%b-%Y")

all$earliest_cr_line<- parse_date_time(all$earliest_cr_line, "%b-%Y")
all$earliest_cr_line <- as.Date(all$earliest_cr_line, format = "%Y")
all$earliest_cr_line <- format(all$earliest_cr_line, "%Y")

all$earliest_cr_line <- cut(as.numeric(all$earliest_cr_line), 7,include.lowest = TRUE)

#####################################################################################################################
## extracting information emp_title
#####################################################################################################################

all$manager <- grepl("Manager", ignore.case = TRUE, x = all$emp_title)
all$manager <- ifelse(all$manager == TRUE, 1, 0)

all$director <- grepl("director", ignore.case = TRUE, x = all$emp_title)
all$director <- ifelse(all$director == TRUE, 1, 0)

all$supervisor <- grepl("supervisor", ignore.case = TRUE, x = all$emp_title)
all$supervisor <- ifelse(all$supervisor == TRUE, 1, 0)

all$senior <- grepl("senior", ignore.case = TRUE, x = all$emp_title) | grepl("sr.", ignore.case = TRUE, x = all$emp_title)
all$senior <- ifelse(all$senior == TRUE, 1, 0)

all$engineer <- grepl("engineer", ignore.case = TRUE, x = all$emp_title) | grepl("engr.", ignore.case = TRUE, x = all$emp_title)
all$engineer <- ifelse(all$engineer == TRUE, 1, 0)

all$analyst <- grepl("Analyst", ignore.case = TRUE, x = all$emp_title)
all$analyst <- ifelse(all$analyst == TRUE, 1, 0)

all$accountant <- grepl("accountant", ignore.case = TRUE, x = all$emp_title)
all$accountant <- ifelse(all$accountant == TRUE, 1, 0)


#####################################################################################################################
## integer encoding
#####################################################################################################################
#emp_length
unique(all$emp_length)
emp_length_levels <- c("Missing","< 1 year", "1 year", "2 years", "3 years","4 years","5 years","6 years","7 years","8 years","9 years","10 years","10+ years")
all$emp_length_ordinal <- as.numeric(factor(all$emp_length, levels = emp_length_levels))

#grade
unique(all$grade)
grade_levels <- c("G","F","E","D","C","B","A")
all$grade_ordinal <- as.numeric(factor(all$grade), levels = grade_levels)

#income_verif_status
unique(all$income_verif_status)
income_verif_status_levels <- c("Not Verified", "Source Verified", "Verified")
all$income_verif_status_ordinal <- as.numeric(factor(all$income_verif_status), levels = income_verif_status_levels)

#term
unique(all$term)
term_levels <- c("36 months","60 months")
all$term_ordinal <- as.numeric(factor(all$term), levels = term_levels)

# sub_grade
unique(all$sub_grade)
sub_grade_levels <- c("G5","G4","G3","G2","G1","F5","F4","F3","F2","F1","E5","E4","E3","E2","E1","D5","D4","D3","D2","D1",
                      "C5","C4","C3","C2","C1","B5","B4","B3","B2","B1","A5","A4","A3","A2","A1")
all$sub_grade_ordinal <- as.numeric(factor(all$sub_grade), levels = sub_grade_levels)

#####################################################################################################################
## one-hot-encoding
#####################################################################################################################

all$had_missing_annual_income <- ifelse(all$had_missing_annual_income == TRUE ,1,0)
all$had_missing_monthly_payment <- ifelse(all$had_missing_monthly_payment== TRUE ,1,0)
all$had_missing_num_bankrupts <- ifelse(all$had_missing_num_bankrupts== TRUE ,1,0)
all$had_missing_num_mortgages <- ifelse(all$had_missing_num_mortgages== TRUE ,1,0)
all$had_missing_num_records <- ifelse(all$had_missing_num_records== TRUE ,1,0)
all$had_missing_num_total_credit <- ifelse(all$had_missing_num_total_credit== TRUE ,1,0)
all$had_missing_revol_util <- ifelse(all$had_missing_revol_util== TRUE ,1,0)

#####################################################################################################################
## drop unneccessary columns
#####################################################################################################################

all <- subset(all, select = -c(address,emp_title,grade,income_verif_status,emp_length,term,sub_grade))

#####################################################################################################################
## unmerging
#####################################################################################################################

final_train <- all[all$id %in% train$id,]
final_test <- all[all$id %in% test$id,]

final_train$default <- train$default

#####################################################################################################################
## scaling numeric columns
#####################################################################################################################
#we decided not to split the train in train and validation,
#because we only used the validation set to get an estimate
#of how our model performed.
to_scale <- c("amount","annual_income","debt_to_income","interest_rate","revol_balance","revol_util","monthly_payment","days_between","emp_length_ordinal","grade_ordinal","income_verif_status_ordinal","term_ordinal",
              "num_bankrupts","num_mortgages","num_total_credit","num_open_credit","num_records","sub_grade_ordinal" ) #determine numeric columns

columns <- names(final_train) %in% to_scale

scale_data <- scale(final_train[,columns]) #scale numeric columns off train data
final_train[,columns] <- scale_data #add columns to main dataframe
means_train <- attr(scale_data, "scaled:center") #get means of train dataframe
standard_deviations_train <- attr(scale_data, "scaled:scale") #get sd of train dataframe

final_test[,columns]<- scale(final_test[,columns], center = means_train, scale = standard_deviations_train) # scale test

#####################################################################################################################
## class imbalance
#####################################################################################################################
set.seed(12)
train_idx <- sample(1:nrow(final_train), nrow(final_train)*0.7) #split train set in validation and train set (30/70 division)
train_split <- final_train[train_idx,]
validation_split <- final_train[-(train_idx),]

set.seed(12)
train_split$default <- as.factor(train_split$default)
final_train$default <- as.factor(final_train$default)
#only upscaling train
up_train <- upSample(x = train_split[, -ncol(train_split)],
                     y = train_split$default)    
#upscaling train and validation
up_all <- upSample(x = final_train[, -ncol(final_train)],
                   y = final_train$default
                   )
names(up_train)[names(up_train) == "Class"] <- "default"
names(up_all)[names(up_all) == "Class"] <- "default"

#####################################################################################################################
## write the data to gold
#####################################################################################################################

# save data files as csv
write.csv(up_train,"./data/gold/train_month.csv", row.names = FALSE)
write.csv(validation_split,"./data/gold/validation_month.csv", row.names = FALSE)
write.csv(final_test,"./data/gold/test_month.csv", row.names = FALSE)
write.csv(up_all,"./data/gold/all.csv", row.names = FALSE)

