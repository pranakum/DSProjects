# Clear the environment variables
rm(list=ls(all=TRUE))
#Load all the Libraries Required
library(glmnet)
library(caret)
library(DMwR)
# Setting the current working directory
setwd("C:/Users/Hai/Desktop/insofe/Cute3")

#Importing & exporting data
data=read.csv("train.csv", header=TRUE, sep=",",na.strings=c("","NA"))
str(data)
test=read.csv("test.csv",header=TRUE,sep=",",na.strings=c("","NA"))
str(test)

# Understanding the data
dim(data)
#53631*51
head(data)
names(data)
str(data)
summary(data)
#cat_var_1  NA's   : 2426,cat_var_3
dim(test)
#53545*50
summary(test)
str(test)

## Remove columns which does not add any information
x=nearZeroVar(data,names=TRUE)
# reduced to 29 variables
data=data[,setdiff(names(data),x)]
dim(data)
str(data)
summary(data)

test=test[,setdiff(names(test),x)]
dim(data)
#53631    29
summary(test)

##Check for Missing Values
sum(is.na(data))
#26126
sum(is.na(test))
#25803

# II. Find Number of missing values and drop a row if it contains more than 20% of columns contains missing values. Impute the remining.
sum(is.na(data))
length(manyNAs(data, 0.2) )
ExcludedMissing= data[!rowSums(is.na(data)) > ncol(data)*.2,]
sum(is.na(ExcludedMissing))
#library(aCRM)
ImputedData=centralImputation(ExcludedMissing)
sum(is.na(ImputedData))

# need to do according to train data mean(numeric) & mode(cat)
sum(is.na(test))
length(manyNAs(test, 0.2) )
ExcludedMissingTest= test[!rowSums(is.na(test)) > ncol(test)*.2,]
sum(is.na(ExcludedMissingTest))
ImputedTest=centralImputation(ExcludedMissingTest)
sum(is.na(ImputedTest))

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

ImputedDataRemovedId = ImputedDataRemovedId[sapply(ImputedDataRemovedId,nlevels) <= 50]
# Dropped columns with more than 50 levels cat_var_1 cat_var_3 cat_var_6 cat_var_8 cat_var_13

# Do Train-Test Split
library(caret)
set.seed(125)
rows=createDataPartition(ImputedDataRemovedId$target,p = 0.7,list = FALSE)
train=ImputedDataRemovedId[rows,]
val=ImputedDataRemovedId[-rows,]

##############Building Random Forest###############################################
## Random Forest
# We can build a random forest model using the randomForest() function from the randomForest() package
# Below, we use the default parameters to build the random forest model

library(randomForest)
str(train)
model_rf <- randomForest(target ~ . , train,ntree = 50,mtry = 5)

# We can also look at variable importance from the built model using the importance() function and visualise it using the varImpPlot() funcion
importance(model_rf)
varImpPlot(model_rf)

# Predict on the train data
preds_train_rf <- predict(model_rf)
confusionMatrix(preds_train_rf, train$target)

# Store predictions from the model
preds_rf <- predict(model_rf, val)
confusionMatrix(preds_rf, val$target)

# Precision: P=TP/(TP+FP)
# Recall: R=TP/(TP+FN)
# F1-score: 2/(1/P+1/R)
# ROC/AUC: TPR=TP/(TP+FN), FPR=FP/(FP+TN)

#### Repeat the same preprocessing steps on test data
# Type Conversion 
str(ImputedTestRemovedId)
ImputedTestRemovedId$cat_var_19 = as.factor(ImputedTestRemovedId$cat_var_19)
ImputedTestRemovedId$cat_var_20 = as.factor(ImputedTestRemovedId$cat_var_20)
ImputedTestRemovedId$cat_var_21 = as.factor(ImputedTestRemovedId$cat_var_21)
ImputedTestRemovedId$cat_var_22 = as.factor(ImputedTestRemovedId$cat_var_22)
summary(ImputedTestRemovedId)

testData = ImputedTestRemovedId[sapply(ImputedTestRemovedId,nlevels) <= 50]
# Dropped columns with more than 50 levels cat_var_1 cat_var_3 cat_var_6 cat_var_8 cat_var_13

# There is a mismatch in the levels in train and test data
str(train)
str(testData)
levels(testData$cat_var_2) = levels(train$cat_var_2)

# Getting the Predictions for the test data
preds_rf <- predict(model_rf, testData)

final = data.frame(transaction_id = ImputedTest$transaction_id,target = preds_rf)
head(final,3)

write.csv(final, "submission.csv", row.names = FALSE)

####Building  randomforest using caret
control <- trainControl(method="cv", number=3)
set.seed(1235869)
tunegrid <- expand.grid(mtry=c(1:10))
rf_gridsearch <- train(target ~ ., data=train, method = "rf",
                       trControl=control,
                       tuneGrid = tunegrid)


# Predict on the train data
preds_train_rf1 <- predict(rf_gridsearch)
confusionMatrix(preds_train_rf1, train$target)


# Store predictions from the model
preds_rf1 <- predict(rf_gridsearch, val)

confusionMatrix(preds_rf1, val$Personal.Loan)
