# Reading & Understanding the Data




# Clear the environment variables
rm(list=ls(all=TRUE))
#Load all the Libraries Required
library(glmnet)
library(caret)
library(DMwR)
# Setting the current working directory
setwd("C:/Users/Hai/Desktop/insofe/Cute3")

#Importing 
data=read.csv("train (1).csv", header=TRUE, sep=",",na.strings=c("","NA"))

test=read.csv("test (1).csv",header=TRUE,sep=",",na.strings=c("","NA"))


# Understanding the data
dim(data)
#244285    51
head(data)
names(data)
str(data)
summary(data)
dim(test)
# 104693   50
summary(test)
str(test)


## Remove columns which does not add any information

x=nearZeroVar(data,names=TRUE)

data=data[,setdiff(names(data),x)]
summary(data)

test=test[,setdiff(names(test),x)]
summary(test)
dim(data)
#244285    29
dim(test)
#104693   28

##Check for Missing Values

sum(is.na(data))
sum(is.na(test))
# II. Find Number of missing values and drop a row if it contains more than 20% of columns contains missing values. Impute the remining.
sum(is.na(data))
# 118577
length(manyNAs(data, 0.2) )
ExcludedMissing= data[!rowSums(is.na(data)) > ncol(data)*.2,]
sum(is.na(ExcludedMissing))
ImputedData=centralImputation(ExcludedMissing)
sum(is.na(ImputedData))
str(ImputedData)

# need to do according to train data mean(numeric) & mode(cat)
sum(is.na(test))
# 50796
length(manyNAs(test, 0.2) )
ExcludedMissingTest= test[!rowSums(is.na(test)) > ncol(test)*.2,]
sum(is.na(ExcludedMissingTest))
ImputedTest=centralImputation(ExcludedMissingTest)
sum(is.na(ImputedTest))
str(ImputedTest)



#28  variables
#drop id
ImputedDataRemovedId=ImputedData
ImputedDataRemovedId$transaction_id=NULL
#ImputedDataRemovedIdandName$Itemname=NULL
sum(is.na(ImputedDataRemovedId))
str(ImputedDataRemovedId)
summary(ImputedDataRemovedId)

ImputedTestRemovedId=ImputedTest
ImputedTestRemovedId$transaction_id=NULL
sum(is.na(ImputedTestRemovedId))
str(ImputedTestRemovedId)
summary(ImputedTestRemovedId)


# Type Conversion 
str(ImputedDataRemovedId)
ImputedDataRemovedId$cat_var_19 = as.factor(ImputedDataRemovedId$cat_var_19)
ImputedDataRemovedId$cat_var_20 = as.factor(ImputedDataRemovedId$cat_var_20)
ImputedDataRemovedId$cat_var_21 = as.factor(ImputedDataRemovedId$cat_var_21)
ImputedDataRemovedId$cat_var_22 = as.factor(ImputedDataRemovedId$cat_var_22)
ImputedDataRemovedId$target = as.factor(ImputedDataRemovedId$target)
summary(ImputedDataRemovedId)
table(ImputedDataRemovedId$cat_var_1)

ImputedDataRemovedId = ImputedDataRemovedId[sapply(ImputedDataRemovedId,nlevels) <= 50]
str(ImputedTestRemovedId)
ImputedTestRemovedId$cat_var_19 = as.factor(ImputedTestRemovedId$cat_var_19)
ImputedTestRemovedId$cat_var_20 = as.factor(ImputedTestRemovedId$cat_var_20)
ImputedTestRemovedId$cat_var_21 = as.factor(ImputedTestRemovedId$cat_var_21)
ImputedTestRemovedId$cat_var_22 = as.factor(ImputedTestRemovedId$cat_var_22)

summary(ImputedTestRemovedId)

# Dropped columns with more than 50 levels cat_var_1 cat_var_3 cat_var_6 cat_var_8 cat_var_13

trainData = ImputedDataRemovedId[sapply(ImputedDataRemovedId,nlevels) <= 50]
testData = ImputedTestRemovedId[sapply(ImputedTestRemovedId,nlevels) <= 50]
# Dropped columns with more than 50 levels cat_var_1 cat_var_3 cat_var_6 cat_var_8 cat_var_13


# Do Train-Test Split
library(caret)
set.seed(1224)
rows=createDataPartition(trainData$target,p = 0.7,list = FALSE)
train=trainData[rows,]
val=trainData[-rows,]


table(trainData$target)
prop.table(table(trainData$target))


# 0         1 
# 0.8929202 0.1070798 

#Let us handle through Synthetic Minority OverSampling Technique
# Divide data in to train and validation then ****apply SMOTE on train****

table(train$target)
prop.table(table(train$target))

# 0         1 
# 0.8929181 0.1070819

trainsmote<-SMOTE(target~.,data=train,perc.over = 700,perc.under = 100 )
table(trainsmote$target)
prop.table(table(trainsmote$target))
# 0         1 
# 0.4666667 0.5333333 


#Standardize all the real valued variables in the dataset using only the train data
library("dplyr")
names(select_if(trainsmote, is.numeric))
names(select_if(trainsmote, is.factor))
#num_var_1    num_var_2    num_var_4    num_var_5    num_var_6    num_var_7

std_method <- preProcess(trainsmote[, names(select_if(trainsmote, is.numeric))], method = c("center", "scale"))

train_data <- predict(std_method, trainsmote)
names(trainsmote)
# [1] "num_var_1"  "num_var_2"  "num_var_4"  "num_var_5"  "num_var_6"  "num_var_7"  "cat_var_2"  "cat_var_4"  "cat_var_5"  "cat_var_9" 
# [11] "cat_var_10" "cat_var_11" "cat_var_12" "cat_var_14" "cat_var_15" "cat_var_16" "cat_var_17" "cat_var_18" "cat_var_19" "cat_var_20"
# [21] "cat_var_21" "cat_var_22" "target" 
val_data <- predict(std_method, val)

test_data<- predict(std_method, testData)
# There is a mismatch in the levels in train and test data
str(train_data)#cat var 2 44 levels
str(test_data)#cat var 2 35 levels
levels(test_data$cat_var_2) = levels(train_data$cat_var_2)


#standardized data
str(train_data)
str(test_data)
str(val_data)



#using one hot encoding 

#using one hot encoding 
#train_data$target=as.numeric(train_data$target)
labels <- train_data$target
class(labels)
#train_data$target=as.numeric(train_data$target)

#new_ts <- model.matrix(~.+0,data = test_data)
ts_label <- val_data$target
names(train_data[23])
new_tr <- model.matrix(~.+0,data = train_data[,-23], with = F) 
new_ts <- model.matrix(~.+0,data = val_data[,-23], with = F)

#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

library(xgboost)
#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels )
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

#first default - model training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 79, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

#confusion matrix
library(caret)
confusionMatrix (as.factor(xgbpred), as.factor(ts_label))

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])

#model prediction on the Test Data
new_test <- model.matrix(~.+0,data = test_data, with = F)
dtestData <- xgb.DMatrix(data = new_test)
final1 <- predict (xgb1,dtestData)
final1 <- ifelse (final1 > 0.5,1,0)

final = data.frame(transaction_id = ImputedTest$transaction_id,target = final1)
head(final,3)

write.csv(final, "submission_xgboost.csv", row.names = FALSE)

