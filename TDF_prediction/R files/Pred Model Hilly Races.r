library(pacman)
p_load(lubridate, AUC, tidyverse)

p_load(magrittr) 
p_load(dplyr)
p_load(tidyverse)
p_load(tidyr)
p_load(pacman)
p_load(rlist)
p_load(dummy)
p_load(e1071)
p_load(ROSE)
p_load(rotationForest)
p_load(xgboost)

p_load(caret)

p_load(FNN)

p_load(randomForest)

options(repr.matrix.max.cols=1000, repr.matrix.max.rows=1000)

p_load(ranger)

table = (read.csv("Data//Hilly_races_final.csv", sep = ";"))
head(table)

table_train = table[table$Year<=2017,]

table_val = table[(table$Year>2017)&(table$Year<=2019) ,]

table_test = table[table$Year>2019,]

basetable_train = table_train %>% group_by(Race, Year,Team) %>% summarise(flat_mean = round(mean(FLAT))
                                             , mountain_mean = round(mean(MOUNTAIN))
                                            , downhill_mean = round(mean(DOWNHILL))
                                            ,cobbles_mean = round(mean(COBBLES))
                                            ,tt_mean = round(mean(TT)),
                                            ,prologue_mean = round(mean(PROLOGUE))
                                            ,sprint_mean = round(mean(SPRINT))
                                            ,acceleration_mean = round(mean(ACCELERATION))
                                            ,endurance_mean = round(mean(ENDURANCE))
                                            ,resistance_mean = round(mean(RESISTANCE))
                                            ,recup_mean = round(mean(RECUP))
                                            ,hill_mean = round(mean(HILL))
                                            ,attack_mean = round(mean(ATTACK))
                                            
                                            ,flat_max = round(max(FLAT))
                                            ,mountain_max = round(max(MOUNTAIN))
                                            ,downhill_max = round(max(DOWNHILL))
                                            ,cobbles_max = round(max(COBBLES))
                                            ,tt_max = round(max(TT)),
                                            ,prologue_max = round(max(PROLOGUE))
                                            ,sprint_max = round(max(SPRINT))
                                            ,acceleration_max = round(max(ACCELERATION))
                                            ,endurance_max = round(max(ENDURANCE))
                                            ,resistance_max = round(max(RESISTANCE))
                                            ,recup_max = round(max(RECUP))
                                            ,hill_max = round(max(HILL))
                                            ,attack_max = round(max(ATTACK))
                                            
                                            ,flat_min = round(min(FLAT))
                                            ,mountain_min = round(min(MOUNTAIN))
                                            ,downhill_min = round(min(DOWNHILL))
                                            ,cobbles_min = round(min(COBBLES))
                                            ,tt_min = round(min(TT)),
                                            ,prologue_min = round(min(PROLOGUE))
                                            ,sprint_min = round(min(SPRINT))
                                            ,acceleration_min = round(min(ACCELERATION))
                                            ,endurance_min = round(min(ENDURANCE))
                                            ,resistance_min = round(min(RESISTANCE))
                                            ,recup_min = round(min(RECUP))
                                            ,hill_min = round(min(HILL))
                                            ,attack_min = round(min(ATTACK))
                                    
                                            ,best_position = min(Pos))

basetable_val = table_val %>% group_by(Race, Year,Team) %>% summarise(flat_mean = round(mean(FLAT))
                                             , mountain_mean = round(mean(MOUNTAIN))
                                            , downhill_mean = round(mean(DOWNHILL))
                                            ,cobbles_mean = round(mean(COBBLES))
                                            ,tt_mean = round(mean(TT)),
                                            ,prologue_mean = round(mean(PROLOGUE))
                                            ,sprint_mean = round(mean(SPRINT))
                                            ,acceleration_mean = round(mean(ACCELERATION))
                                            ,endurance_mean = round(mean(ENDURANCE))
                                            ,resistance_mean = round(mean(RESISTANCE))
                                            ,recup_mean = round(mean(RECUP))
                                            ,hill_mean = round(mean(HILL))
                                            ,attack_mean = round(mean(ATTACK))
                                            
                                            ,flat_max = round(max(FLAT))
                                            ,mountain_max = round(max(MOUNTAIN))
                                            ,downhill_max = round(max(DOWNHILL))
                                            ,cobbles_max = round(max(COBBLES))
                                            ,tt_max = round(max(TT)),
                                            ,prologue_max = round(max(PROLOGUE))
                                            ,sprint_max = round(max(SPRINT))
                                            ,acceleration_max = round(max(ACCELERATION))
                                            ,endurance_max = round(max(ENDURANCE))
                                            ,resistance_max = round(max(RESISTANCE))
                                            ,recup_max = round(max(RECUP))
                                            ,hill_max = round(max(HILL))
                                            ,attack_max = round(max(ATTACK))
                                            
                                            ,flat_min = round(min(FLAT))
                                            ,mountain_min = round(min(MOUNTAIN))
                                            ,downhill_min = round(min(DOWNHILL))
                                            ,cobbles_min = round(min(COBBLES))
                                            ,tt_min = round(min(TT)),
                                            ,prologue_min = round(min(PROLOGUE))
                                            ,sprint_min = round(min(SPRINT))
                                            ,acceleration_min = round(min(ACCELERATION))
                                            ,endurance_min = round(min(ENDURANCE))
                                            ,resistance_min = round(min(RESISTANCE))
                                            ,recup_min = round(min(RECUP))
                                            ,hill_min = round(min(HILL))
                                            ,attack_min = round(min(ATTACK))
                                    
                                            ,best_position = min(Pos))

basetable_test = table_test %>% group_by(Race, Year,Team) %>% summarise(flat_mean = round(mean(FLAT))
                                             , mountain_mean = round(mean(MOUNTAIN))
                                            , downhill_mean = round(mean(DOWNHILL))
                                            ,cobbles_mean = round(mean(COBBLES))
                                            ,tt_mean = round(mean(TT)),
                                            ,prologue_mean = round(mean(PROLOGUE))
                                            ,sprint_mean = round(mean(SPRINT))
                                            ,acceleration_mean = round(mean(ACCELERATION))
                                            ,endurance_mean = round(mean(ENDURANCE))
                                            ,resistance_mean = round(mean(RESISTANCE))
                                            ,recup_mean = round(mean(RECUP))
                                            ,hill_mean = round(mean(HILL))
                                            ,attack_mean = round(mean(ATTACK))
                                            
                                            ,flat_max = round(max(FLAT))
                                            ,mountain_max = round(max(MOUNTAIN))
                                            ,downhill_max = round(max(DOWNHILL))
                                            ,cobbles_max = round(max(COBBLES))
                                            ,tt_max = round(max(TT)),
                                            ,prologue_max = round(max(PROLOGUE))
                                            ,sprint_max = round(max(SPRINT))
                                            ,acceleration_max = round(max(ACCELERATION))
                                            ,endurance_max = round(max(ENDURANCE))
                                            ,resistance_max = round(max(RESISTANCE))
                                            ,recup_max = round(max(RECUP))
                                            ,hill_max = round(max(HILL))
                                            ,attack_max = round(max(ATTACK))
                                            
                                            ,flat_min = round(min(FLAT))
                                            ,mountain_min = round(min(MOUNTAIN))
                                            ,downhill_min = round(min(DOWNHILL))
                                            ,cobbles_min = round(min(COBBLES))
                                            ,tt_min = round(min(TT)),
                                            ,prologue_min = round(min(PROLOGUE))
                                            ,sprint_min = round(min(SPRINT))
                                            ,acceleration_min = round(min(ACCELERATION))
                                            ,endurance_min = round(min(ENDURANCE))
                                            ,resistance_min = round(min(RESISTANCE))
                                            ,recup_min = round(min(RECUP))
                                            ,hill_min = round(min(HILL))
                                            ,attack_min = round(min(ATTACK))
                                    
                                            ,best_position = min(Pos))

basetable_train = (basetable_train)%>% drop_na()
basetable_val = (basetable_val)%>% drop_na()
basetable_test = (basetable_test)%>% drop_na()

basetable_train$Top10 = as.factor(ifelse(basetable_train$best_position <=10, 1,0))
basetable_train$Top5 = as.factor(ifelse(basetable_train$best_position <=5, 1,0))
basetable_train$Top4 = as.factor(ifelse(basetable_train$best_position <=4, 1,0))
basetable_train$Top3 = as.factor(ifelse(basetable_train$best_position <=3, 1,0))

table(basetable_train$Top10)

basetable_train$minutes_from_top = NULL
basetable_train$best_position = NULL
basetable_train$Team = NULL
basetable_train$Year = NULL
basetable_train$Stage = NULL
basetable_train$Team_ID = NULL
basetable_train$Race= NULL

basetable_val$Top10 = as.factor(ifelse(basetable_val$best_position <=10, 1,0))
basetable_val$Top5 = as.factor(ifelse(basetable_val$best_position <=5, 1,0))
basetable_val$Top4 = as.factor(ifelse(basetable_val$best_position <=4, 1,0))
basetable_val$Top3 = as.factor(ifelse(basetable_val$best_position <=3, 1,0))

table(basetable_val$Top10)

basetable_val$minutes_from_top = NULL
basetable_val$best_position = NULL
basetable_val$Team = NULL
basetable_val$Year = NULL
basetable_val$Stage = NULL
basetable_val$Team_ID = NULL
basetable_val$Race= NULL

# hyperparameter grid search
hyper_grid <- expand.grid(
   num_trees = c(1000),
  mtry       = seq(15, 35, by = 1),
  node_size  = seq(1, 13, by = 2),
  sampe_size = c(.55, .632, .70,0.75,.80),
  OOB_RMSE   = 0
 
)
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    x               = basetable_train[,-(40:43)], 
    y               = basetable_train$Top5, 
    num.trees       = hyper_grid$mtry[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    probability     = TRUE,
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


# By setting a new unique seed, you ensure a different
# 'random' split
seeds <- c(123, 246, 91, 949, 9000, 1860, 1853, 1416, 515, 369,145,36920,877,124,617,1743,4852,9001,9005,9004,14,15,19,18,246,47,1,2,3,4,5,6,7,20)  #give 10 random values
all_aucs <- vector(length = length(seeds))

for (i in 1:length(seeds)) {
    set.seed(seeds[i])
    allind <- sample(x = 1:nrow(basetable_train), size = nrow(basetable_train),
        replace = TRUE)  # WITH replacement
    

    # block to get indices
    trainind <- allind[1:round(length(allind) * 0.70)]
    testind <- allind[!allind %in% trainind]  #get all indices which are not in training

    #actual subsetting
    train <- basetable_train[trainind, ]
    yTRAIN <- train$Top10
    train$Top10 = NULL
    train$Top5 = NULL
    train$Top4 = NULL
    train$Top3 = NULL
    
    test <- basetable_train[testind, ]
    yTEST <- test$Top10
    test$Top10 <- NULL
    test$Top5 = NULL
    test$Top4 = NULL
    test$Top3 = NULL
    
    #fit
    model <- ranger(x=train, y = yTRAIN, num.trees = 500, mtry = 10, sample.fraction = 0.75, probability = TRUE)
    # predict on second set (test)0
    predictions =  predict(model,test)$predictions[,2]   
    
    # evaluate and store
    all_aucs[i] <- AUC::auc(roc(predictions, yTEST))
}

# Plot
plot(all_aucs, type = "b")

mean(all_aucs)

set.seed(123)
train <- basetable_train
yTRAIN <- train$Top10
train$Top10 = NULL
train$Top5 = NULL
train$Top4 = NULL
train$Top3 = NULL

test <- basetable_val
yTEST <- test$Top10
test$Top10 = NULL
test$Top5 = NULL
test$Top4 = NULL
test$Top3 = NULL

set.seed(123)
#fit
rFmodel <- randomForest(x=train, y = yTRAIN, ntree = 1000,mtry=15)
# predict on second set (test)
predictions <- predict(rFmodel, test, type = "prob")[, 2]

AUC::auc(roc(predictions,factor(yTEST)))

auc = AUC::auc(roc(predictions,factor(yTEST)))

res = ifelse(predictions>=0.5,1,0)
true_outcome = yTEST

confusionMatrix(as.factor(res), as.factor(true_outcome))[[2]]
TN =(confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,1]
TP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,2]
FN = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,2]
FP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,1]

accuracy = (TP+TN)/(TP+FP+TN+FN)
recall = TP/(TP+FN)
precision = TP/(TP+FP)

paste("AUC: ", auc)
paste("Accuracy: ", accuracy)
paste("Recall: ", recall)
paste("Precision: ", precision)

#fit
model <- ranger(x=train, y = yTRAIN, num.trees = 1000, mtry = 8,min.node.size = 13, sample.fraction = 0.75, probability = TRUE, seed = 123)
# predict on second set (test)
predictions =  predict(model,test)$predictions[,2]

AUC::auc(roc(predictions,factor(yTEST)))

auc = AUC::auc(roc(predictions,factor(yTEST)))

res = ifelse(predictions>=0.5,1,0)
true_outcome = yTEST

confusionMatrix(as.factor(res), as.factor(true_outcome))[[2]]
TN =(confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,1]
TP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,2]
FN = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,2]
FP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,1]

accuracy = (TP+TN)/(TP+FP+TN+FN)
recall = TP/(TP+FN)
precision = TP/(TP+FP)

paste("AUC: ", auc)
paste("Accuracy: ", accuracy)
paste("Recall: ", recall)
paste("Precision: ", precision)

# hyperparameter grid search
hyper_grid <- expand.grid(
   num_trees = c(1000),
  mtry       = seq(15, 35, by = 1),
  node_size  = seq(1, 13, by = 2),
  sampe_size = c(.55, .632, .70,0.75,.80),
  OOB_RMSE   = 0
 
)
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    x               = basetable_train[,-(40:43)], 
    y               = basetable_train$Top5, 
    num.trees       = hyper_grid$mtry[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    probability     = TRUE,
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


# By setting a new unique seed, you ensure a different
# 'random' split
seeds <- c(123, 246, 91, 949, 9000, 1860, 1853, 1416, 515, 369,145,36920,877,124,617,1743,4852,9001,9005,9004,14,15,19,18,246,47,1,2,3,4,5,6,7,20)  #give 10 random values
all_aucs <- vector(length = length(seeds))

for (i in 1:length(seeds)) {
    set.seed(seeds[i])
    allind <- sample(x = 1:nrow(basetable_train), size = nrow(basetable_train),
        replace = TRUE)  # WITH replacement
    

    # block to get indices
    trainind <- allind[1:round(length(allind) * 0.70)]
    testind <- allind[!allind %in% trainind]  #get all indices which are not in training

    #actual subsetting
    train <- basetable_train[trainind, ]
    yTRAIN <- train$Top5
    train$Top10 = NULL
    train$Top5 = NULL
    train$Top4 = NULL
    train$Top3 = NULL
    
    test <- basetable_train[testind, ]
    yTEST <- test$Top5
    test$Top5 <- NULL
    test$Top5 = NULL
    test$Top4 = NULL
    test$Top3 = NULL
    
    #fit
    model <- ranger(x=train, y = yTRAIN, num.trees = 500, mtry = 10, sample.fraction = 0.75, probability = TRUE)
    # predict on second set (test)0
    predictions =  predict(model,test)$predictions[,2]   
    
    # evaluate and store
    all_aucs[i] <- AUC::auc(roc(predictions, yTEST))
}

# Plot
plot(all_aucs, type = "b")

mean(all_aucs)

set.seed(123)
train <- basetable_train
yTRAIN <- train$Top5
train$Top10 = NULL
train$Top5 = NULL
train$Top4 = NULL
train$Top3 = NULL

test <- basetable_val
yTEST <- test$Top5
test$Top10 = NULL
test$Top5 = NULL
test$Top4 = NULL
test$Top3 = NULL

set.seed(123)
#fit
rFmodel <- randomForest(x=train, y = yTRAIN, ntree = 1000,mtry=15)
# predict on second set (test)
predictions <- predict(rFmodel, test, type = "prob")[, 2]

AUC::auc(roc(predictions,factor(yTEST)))

auc = AUC::auc(roc(predictions,factor(yTEST)))

res = ifelse(predictions>=0.5,1,0)
true_outcome = yTEST

confusionMatrix(as.factor(res), as.factor(true_outcome))[[2]]
TN =(confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,1]
TP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,2]
FN = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,2]
FP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,1]

accuracy = (TP+TN)/(TP+FP+TN+FN)
recall = TP/(TP+FN)
precision = TP/(TP+FP)

paste("AUC: ", auc)
paste("Accuracy: ", accuracy)
paste("Recall: ", recall)
paste("Precision: ", precision)

#fit
model <- ranger(x=train, y = yTRAIN, num.trees = 1000, mtry = 8,min.node.size = 13, sample.fraction = 0.75, probability = TRUE, seed = 123)
# predict on second set (test)
predictions =  predict(model,test)$predictions[,2]

AUC::auc(roc(predictions,factor(yTEST)))

auc = AUC::auc(roc(predictions,factor(yTEST)))

res = ifelse(predictions>=0.5,1,0)
true_outcome = yTEST

confusionMatrix(as.factor(res), as.factor(true_outcome))[[2]]
TN =(confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,1]
TP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,2]
FN = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][1,2]
FP = (confusionMatrix(as.factor(res), as.factor(true_outcome)))[[2]][2,1]

accuracy = (TP+TN)/(TP+FP+TN+FN)
recall = TP/(TP+FN)
precision = TP/(TP+FP)

paste("AUC: ", auc)
paste("Accuracy: ", accuracy)
paste("Recall: ", recall)
paste("Precision: ", precision)
