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
p_load(GA)

table = (read.csv("Data//Flat_races_final.csv", sep = ";"))
head(table)

table_train = table[table$Year<=2017,]

table_val = table[(table$Year>2017)&(table$Year<=2019) ,]

table_test = table[table$Year>2019,]

table_train2 = table[table$Year<=2019,]

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
                                            ,mean_size = round(mean(Size))
                                            ,mean_weight = round(mean(Weight))
                                                                          
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
                                            ,size_max = round(max(Size))
                                            ,weight_max = round(max(Weight))
                                                                          
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
                                            ,size_min = round(min(Size))
                                            ,weight_min = round(min(Weight))
                                    
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
                                            ,mean_size = round(mean(Size))
                                            ,mean_weight = round(mean(Weight))
                                                                          
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
                                            ,size_max = round(max(Size))
                                            ,weight_max = round(max(Weight))
                                                                          
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
                                            ,size_min = round(min(Size))
                                            ,weight_min = round(min(Weight))
                                    
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
                                            ,mean_size = round(mean(Size))
                                            ,mean_weight = round(mean(Weight))
                                                                          
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
                                            ,size_max = round(max(Size))
                                            ,weight_max = round(max(Weight))
                                                                          
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
                                            ,size_min = round(min(Size))
                                            ,weight_min = round(min(Weight))
                                    
                                            ,best_position = min(Pos))

basetable = table_train2 %>% group_by(Race, Year,Team) %>% summarise(flat_mean = round(mean(FLAT))
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
                                            ,mean_size = round(mean(Size))
                                            ,mean_weight = round(mean(Weight))
                                                                          
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
                                            ,size_max = round(max(Size))
                                            ,weight_max = round(max(Weight))
                                                                          
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
                                            ,size_min = round(min(Size))
                                            ,weight_min = round(min(Weight))
                                    
                                            ,best_position = min(Pos))

basetable_train = (basetable_train)%>% drop_na()
basetable_val = (basetable_val)%>% drop_na()
basetable_test = (basetable_test)%>% drop_na()
basetable = (basetable)%>% drop_na()

basetable_train$Top10 = as.factor(ifelse(basetable_train$best_position <=10, 1,0))
basetable_train$Top5 = as.factor(ifelse(basetable_train$best_position <=5, 1,0))
basetable_train$Top4 = as.factor(ifelse(basetable_train$best_position <=4, 1,0))
basetable_train$Top3 = as.factor(ifelse(basetable_train$best_position <=3, 1,0))
basetable_train$Top2 = as.factor(ifelse(basetable_train$best_position <=2, 1,0))

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
basetable_val$Top2 = as.factor(ifelse(basetable_val$best_position <=2, 1,0))

table(basetable_val$Top10)

basetable_test$minutes_from_top = NULL
basetable_val$best_position = NULL
basetable_val$Team = NULL
basetable_val$Year = NULL
basetable_val$Stage = NULL
basetable_val$Team_ID = NULL
basetable_val$Race= NULL

basetable_test$Top10 = as.factor(ifelse(basetable_test$best_position <=10, 1,0))
basetable_test$Top5 = as.factor(ifelse(basetable_test$best_position <=5, 1,0))
basetable_test$Top4 = as.factor(ifelse(basetable_test$best_position <=4, 1,0))
basetable_test$Top3 = as.factor(ifelse(basetable_test$best_position <=3, 1,0))
basetable_test$Top2 = as.factor(ifelse(basetable_test$best_position <=2, 1,0))

table(basetable_test$Top5)

basetable_test$minutes_from_top = NULL
basetable_test$best_position = NULL
basetable_test$Team = NULL
basetable_test$Year = NULL
basetable_test$Stage = NULL
basetable_test$Team_ID = NULL
basetable_test$Race= NULL

basetable$Top10 = as.factor(ifelse(basetable$best_position <=10, 1,0))
basetable$Top5 = as.factor(ifelse(basetable$best_position <=5, 1,0))
basetable$Top4 = as.factor(ifelse(basetable$best_position <=4, 1,0))
basetable$Top3 = as.factor(ifelse(basetable$best_position <=3, 1,0))
basetable$Top2 = as.factor(ifelse(basetable$best_position <=2, 1,0))

table(basetable$Top5)

basetable$minutes_from_top = NULL
basetable$best_position = NULL
basetable$Team = NULL
basetable$Year = NULL
basetable$Stage = NULL
basetable$Team_ID = NULL
basetable$Race= NULL

set.seed(123)
train <- basetable
yTRAIN <- train$Top5
train$Top10 = NULL
train$Top5 = NULL
train$Top4 = NULL
train$Top3 = NULL
train$Top2 = NULL

test <- basetable_test
yTEST <- test$Top5
test$Top10 = NULL
test$Top5 = NULL
test$Top4 = NULL
test$Top3 = NULL
test$Top2 = NULL

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

head(test)

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

unique(table_test[(table_test$Race=="TDF")&(table_test$Year==2021)&(table_test$Pos <=5),]$Team)

team_name = "Intermarché-Wanty-Gobert"

ranking = read.csv("Data//WorldRanking2021.csv", sep=";")
team_table = read.csv("Data//RiderData.csv", sep=";")

ranking = (ranking[,c("Year", "Team", "Pos", "ID")])
names(ranking)[names(ranking) == "Pos"] <- "WorldRanking"

team_table = merge(x = team_table, y = ranking, by = c("Year", "Team", "ID"), all.x = TRUE)

team = team_table[((team_table$Team) == team_name)&((team_table$Year) == 2021),]
team = team[order(team$OVERALL, decreasing = TRUE),]

#Alpecin Team that finished Top 5 six different times in the flat stages of the Tour De France 2021
team_results = (table_test[(table_test$Race=="TDF")&(table_test$Year==2021)&(table_test$Team ==team_name), ])
team_top5 = unique(team_results[,c("Rider", "Team", "ID")])

team_top5$selected = c(1,1,1,1,1,1,1,1)

team_top5

team = merge(x = team, y = team_top5, by = c("Team", "Rider", "ID"), all.x = TRUE)
team = team[order(team$OVERALL, decreasing = TRUE),]
team$selected[is.na(team$selected)] <- 0

#Suggestion
suggestion = (c(rep(1, 8), rep(0, nrow(team)-8)))
suggestion

#Full information objective value
mean(team[team$selected == 1,]$OVERALL)

objective <- function(x) {
  nr_riders = sum(x)
  
    
  if(nr_riders != 8) 
    return(-100000000000000)
  
  selection <- team[as.logical(x),]
  obj = mean(selection$OVERALL)
    
  res = selection %>% group_by(Team) %>% summarise(flat_mean = round(mean(FLAT))
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
                                            ,mean_size = round(mean(Size))
                                            ,mean_weight = round(mean(Weight))
                                                                          
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
                                            ,size_max = round(max(Size))
                                            ,weight_max = round(max(Weight))
                                                                          
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
                                            ,size_min = round(min(Size))
                                            ,weight_min = round(min(Weight)))
  res$Team = NULL
  prediction = predict(rFmodel, res, type = "prob")[, 2]
  print(cat(x,"->",obj))
  return(obj*prediction)
}

result <- ga(type = "binary",
               fitness = objective, 
               crossover=gabin_uCrossover,
               mutation = gabin_raMutation,
               elitism = 8,
               nBits = nrow(team),
               popSize = 50, 
               maxiter = 100, 
               pmutation = 0.05, 
               monitor = TRUE,
               suggestions = suggestion,
               keepBest = TRUE,
               seed = (103))

head(result@bestSol)

#Our selection
selection = team[as.logical(result@population[nrow(result@population), ] ),]
(result@population[nrow(result@population), ])

#True Selected Team
team$selected

res = selection %>% group_by(Team) %>% summarise(flat_mean = round(mean(FLAT))
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
                                            ,mean_size = round(mean(Size))
                                            ,mean_weight = round(mean(Weight))
                                                                          
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
                                            ,size_max = round(max(Size))
                                            ,weight_max = round(max(Weight))
                                                                          
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
                                            ,size_min = round(min(Size))
                                            ,weight_min = round(min(Weight)))

#Probability of winning: team selected by us
res$Team = NULL
predict(rFmodel, res, type = "prob")[, 2]

 #Obj value our team
mean(selection$OVERALL)

#Obj value true team
mean(team[team$selected == 1,]$OVERALL)






