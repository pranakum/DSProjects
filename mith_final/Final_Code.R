#Clear workspace
rm(list=ls(all=TRUE))
#set working directory
setwd("C:/Users/Hai/Desktop/insofe/mith")

#Load all the Libraries Required
library(caret)
library(DMwR)
library(dplyr)


#Importing data
#Read train.csv---------------------------------------------------------------------------------------------
data=read.csv("train.csv", header=TRUE, sep=",",na.strings=c("","NA"))
#dimensions 249644     13
dim(data)
#check structure
str(data)
#check summary
summary(data)
#has_repair_started has NA's   :16264
#check na values
sum(is.na(data))
#16264

#Read test.csv-------------------------------------------------------------------------------------------------------
test=read.csv("test.csv",header=TRUE,sep=",",na.strings=c("","NA"))
#check dimensions 106988     13
dim(test)
#check structure
str(test)
#check summary
summary(test)
#has_repair_started  na's =7025
#damage_grade nas=106988

sum(is.na(test))
#114013

#Read buildingownership--------------------------------------------------------------------------------------------------------
buildingownership=read.csv("buildingownership.csv", header=TRUE, sep=",",na.strings=c("","NA"))
#check dimensions 356632     17
dim(buildingownership)
#check structure
str(buildingownership)
summary(buildingownership)
# 1 na value in count_families
sum(is.na(buildingownership))

#Read buildingstructure--------------------------------------------------------------------------------------------------------------
buildingstructure=read.csv("buildingstructure.csv", header=TRUE, sep=",",na.strings=c("","NA"))
#check dimensions
dim(buildingstructure)
#356632     29

str(buildingstructure)
names(buildingstructure)
View(buildingstructure)

summary(buildingstructure)
# no na values
sum(is.na(buildingstructure))

#---------------------------------------------------------------------------------------------------------------------
#Checking which columns are common between data and buildingstructure & buildingownership
names(data)
#building_id","district_id"  "vdcmun_id"  
names(buildingstructure)
# "building_id" "district_id"  "vdcmun_id" "ward_id" 

names(buildingownership)
#building_id"                   "district_id"                   "vdcmun_id"                    "ward_id" 


#merge data wrt buidling id and district id
mergetrainwithownership=left_join(data, buildingownership, by = NULL, copy = FALSE, suffix = c("building_id", "district_id"))
Mergeddatawithstructure=left_join(mergetrainwithownership, buildingstructure, by = NULL, copy = FALSE, suffix = c("building_id", "district_id"))
dim(Mergeddatawithstructure)
#249644     52
#Same rows as the original given dataset
#get unique rows
traindata_v1=unique(mergetrainwithbuildings[,setdiff(names(mergetrainwithbuildings)),"building_id"])

#merge test wrt buidling id and district id
mergetestwithownership=left_join(test, buildingownership, by = NULL, copy = FALSE, suffix = c("building_id", "district_id"))
Mergedtestwithstructure=left_join(mergetestwithownership, buildingstructure, by = NULL, copy = FALSE, suffix = c("building_id", "district_id"))
dim(Mergedtestwithstructure)

#same rows as 106988     52


#checking for duplicates
Mergeddatawithstructure[!duplicated(Mergeddatawithstructure$building_id), ]
dim(Mergeddatawithstructure)
Mergedtestwithstructure[!duplicated(Mergedtestwithstructure$building_id), ]
dim(Mergedtestwithstructure)

#Naming the merge files to train_data and test_data----------------------------------------------------------------------------------------------------------------
train_data=Mergeddatawithstructure
test_data=Mergedtestwithstructure

#Preprocessing---------------------------------------------------------------------------------
#check dimensions

dim(train_data)
#249644     52
dim(test_data)
#106988     52
#check structure

str(train_data)

str(test_data)


#check summary

summary(train_data)

summary(test_data)

#remove variables which donot add any information using nerozerovar
x=nearZeroVar(train_data,names=TRUE)
# reduced to 29 variables
train_data=train_data[,setdiff(names(train_data),x)]
str(train_data)
summary(train_data)

test_data=test_data[,setdiff(names(test_data),x)]
summary(test_data)
#106988     29
dim(test_data)

#imputation
# Find Number of missing values and drop a row if it contains more than 20% of columns contains missing values. Impute the remining.
sum(is.na(train_data))
# 33689
length(manyNAs(train_data, 0.2) )
ExcludedMissing= train_data[!rowSums(is.na(train_data)) > ncol(train_data)*.2,]
sum(is.na(ExcludedMissing))
#16232
ImputedData=centralImputation(ExcludedMissing)
sum(is.na(ImputedData))
str(ImputedData)
dim(ImputedData)
#248852     29

str(test_data)
#impute on test_data excluding target
Imputedtest=centralImputation(test_data[,-c(2)])

dim(Imputedtest)
#106988     28

#Write the files to csv
# Write CSV in R
write.csv(ImputedData, file = "Imputedtrain.csv",row.names=FALSE)
write.csv(Imputedtest,file = "Imputedtest.csv",row.names=FALSE)


#--------------------------------------------------------------------------------------------------------------
#type conversions

str(ImputedData)
ImputedData_v1=ImputedData
#remove $ building_id, $ vdcmun_id ,$ district_id ,$ ward_id 


ImputedData_v1$building_id=NULL
ImputedData_v1$vdcmun_id=NULL
ImputedData_v1$district_id=NULL
ImputedData_v1$ward_id=NULL



str(Imputedtest)
Imputedtest_v1=Imputedtest

Imputedtest_v1$building_id=NULL
Imputedtest_v1$vdcmun_id=NULL
Imputedtest_v1$district_id=NULL
Imputedtest_v1$ward_id=NULL



#check for duplicates
ImputedData_v1[!duplicated(ImputedData_v1), ]
dim(ImputedData_v1)
#248852     25
dim(Imputedtest_v1)
#106988     24



#split into train and test

table(ImputedData_v1$damage_grade)
# High    Low Medium 
# 102915  71572  74365 

#conditionofbuildingpostearthquake is a combination of buildingid and condition taking only second part of column value
library(stringr)

ImputedData_v1$conditionofbuildingpostearthquake=sub(".*;", "", ImputedData_v1$conditionofbuildingpostearthquake)

Imputedtest_v1$conditionofbuildingpostearthquake=sub(".*;", "", Imputedtest_v1$conditionofbuildingpostearthquake)


table(tdata$conditionofbuildingpostearthquake)




#Write the files to csv
# Write CSV in R
write.csv(ImputedData_v1, file = "ImputedtrainConv.csv",row.names=FALSE)
write.csv(Imputedtest_v1,file = "ImputedtestConv.csv",row.names=FALSE)


str(ImputedData_v1)


library(caret)
set.seed(123)
rows=createDataPartition(ImputedData_v1$damage_grade,p = 0.7,list = FALSE)
tdata=ImputedData_v1[rows,]
vdata=ImputedData_v1[-rows,]

table(tdata$damage_grade)
# High    Low Medium 
# 72041  50101  52056 
table(vdata$damage_grade)
# High    Low Medium 
# 30874  21471  22309 


#Standardize all the real valued variables in the dataset using only the train data
library("dplyr")
names(select_if(tdata, is.numeric))
names(select_if(tdata, is.factor))
names(select_if(tdata, is.character))


std_method <- preProcess(tdata[, names(select_if(tdata, is.numeric))], method = c("center", "scale"))

tdata_std <- predict(std_method, tdata)

vdata_std <-predict(std_method,vdata)



testdata_std<- predict(std_method, Imputedtest_v1)

#check structure and summary of the standardized train,val and test
str(tdata_std)
str(vdata_std)
str(testdata_std)

dim(testdata_std)


#convert conditionofbuildingpostearthquake (character to factor)

tdata_std$conditionofbuildingpostearthquake=as.factor(tdata_std$ conditionofbuildingpostearthquake)
vdata_std$conditionofbuildingpostearthquake=as.factor(vdata_std$ conditionofbuildingpostearthquake)
testdata_std$conditionofbuildingpostearthquake=as.factor(testdata_std$ conditionofbuildingpostearthquake)


str(tdata_std)
str(vdata_std)
str(testdata_std)


# Write CSV in R
write.csv(tdata_std, file = "tdatastd.csv",row.names=FALSE)
write.csv(vdata_std,file = "vdatastd.csv",row.names=FALSE)
write.csv(testdata_std,file = "testdatastd.csv",row.names=FALSE)



#Model building ---------------------------------------------------------------------------------------------------




--------------------------#dt rpart -----------------------------------------------------------------------------------
table(tdata_std$damage_grade)
vdata_std$damage_grade

library(rpart)
DT_rpart_class<-rpart(damage_grade~.,data=tdata_std,method="class")
pred_val_rpart = predict(DT_rpart_class, newdata=vdata_std, type="class")

pred_test_rpart = predict(DT_rpart_class, newdata=testdata_std, type="class")
confusionMatrix(pred_val_rpart,vdata_std$damage_grade,positive = "1")
#accuracy on validation Accuracy : 0.86

typeof(pred_test_rpart)
summary(test$building_id)


dim(test$building_id)
Output_DT_rpart = as.data.frame(cbind("building_id" = test_data$building_id,"damage_grade" = as.character(pred_test_rpart)))
write.csv(Output_DT_rpart, "Prediction_rpart",row.names = FALSE)

#result 

#-------------------------------------------------------------------------
#Nave Bayes
# 
library(e1071)
library(tidyverse)
x.val <- vdata_std %>% select(-damage_grade)
View(x.val)

model.NB <- naiveBayes(damage_grade~., data = tdata_std)

val.pred.nb <- predict(model.NB, x.val)
typeof(val.pred.nb)
pred=as.factor(val.pred.nb)
levels(pred)y.val
levels(y.val)
# 
#cnf.NB.train <- confusionMatrix(tdata_std$damage_grade, train.pred.nb, positive = '1')
cnf.NB.val <- confusionMatrix(vdata_std$damage_grade, val.pred.nb, positive = '1')
#  Accuracy : 0.7495
# 

test.pred.nb <- predict(model.NB, testdata_std)

  final.NB <- cbind.data.frame('building_id'=test_data$building_id, 'damage_grade'=test.pred.nb)
  write.csv(final.NB, file = 'final_NB.csv', row.names = F)

 # accuracy :82.10
  
-------------------------------#xgboost-------------------------------------------------------------------------------------------------
  
  str(tdata_std)
  str(vdata_std)
  str(testdata_std)

  
  #using one hot encoding 
 
  labels <- tdata_std$damage_grade
  class(labels)
 
  ts_label <- vdata_std$damage_grade
  names(tdata_std[1])
 # target is the first column remove before doing onehot encoding
  new_tr <- model.matrix(~.+0,data = tdata_std[,-1], with = F) 
  new_ts <- model.matrix(~.+0,data = vdata_std[,-1], with = F)
  
  #convert factor to numeric 
  labels <- as.numeric(labels)-1
  ts_label <- as.numeric(ts_label)-1
  
  
  library(xgboost)
  #preparing matrix 
  dtrain <- xgb.DMatrix(data = new_tr,label = labels )
  dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
  numberOfClasses <- length(unique(tdata_std$damage_grade))
  #default parameters
  params <- list(booster = "gbtree", objective = "multi:softmax", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1,num_class = numberOfClasses)
  
  xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
  
  #first default - model training
  xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 20, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "mlogloss")
  #model prediction
  xgbpred <- predict (xgb1,dtest)
  #xgbpred <- ifelse (xgbpred = ,1,0)
  
  #confusion matrix
  library(caret)
  confusionMatrix (as.factor(xgbpred), as.factor(ts_label))
  # Accuracy : 0.8849 
  #view variable importance plot
  mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
  par("mar")
  par(mar=c(1,1,1,1))
  xgb.plot.importance (importance_matrix = mat[1:20])
  
  #model prediction on the Test Data
  new_test <- model.matrix(~.+0,data = testdata_std, with = F)
  dtestData <- xgb.DMatrix(data = new_test)
  final1 <- predict (xgb1,dtestData)
  #final1 <- ifelse (final1 > 0.5,1,0)
  
  final = data.frame(building_id=test_data$building_id,damage_grade = final1)
  head(final,3)
  
  write.csv(final, "submission_xgboost.csv", row.names = FALSE)
  
#88.37 result score
  
#--------------------------------adaboost-------------------------------------------------

  library(ada)
  str(tdata_std)
  str(vdata_std)
  str(testdata_std)
  
  
  trctrl <- trainControl(method = "cv", number = 5)
  
  grid <- expand.grid(iter = 20,maxdepth=7,nu=0.2)
  set.seed(3233)
  Ada_Model <- train(damage_grade~., data=tdata_std, method = "ada",
                     trControl=trctrl,
                     tuneGrid = grid)
  
  train.pred.ada <- predict(Ada_Model, tdata_std)
  val.pred.ada <- predict(Ada_Model, vdata_std)
  test.pred.ada <- predict(Ada_Model, testdata_std)
  
  prop.table(table(test.pred.ada))
  
  final.ada <- cbind.data.frame(building_id=test_data$building_id,damage_grade = test.pred.ada)
  write.csv(final.ada, file = 'final_ada.csv', row.names = F)
  
  
#RF------------------------------------------------------------------------------------------------------------
  library(randomForest)
  library(caret)
  
  
  
  RandomForest_Model <- randomForest(damage_grade ~ . , tdata_std,ntree = 50,mtry = 5)
  
  # We can also look at variable importance from the built model using the importance() function and visualise it using the varImpPlot() funcion
  varImpPlot(RandomForest_Model)
  
  # Predict on the train data
  preds_train_rf <- predict(RandomForest_Model,tdata_std)
  confusionMatrix(preds_train_rf, tdata_std$damage_grade)
  
  
  # Store predictions from the model
  preds_val_rf <- predict(RandomForest_Model, vdata_std)
  confusionMatrix(preds_val_rf, vdata_std$damage_grade)
  #Accuracy : 0.891 
  preds_test_rf <- predict(RandomForest_Model,testdata_std)
  
  Output_RF = data.frame(building_id=test_data$building_id,damage_grade = preds_test_rf)
  write.csv(Output_RF, "Prediction_RF.csv",row.names = FALSE)
  
#---------------------------------------------------------------------------------------------- 































