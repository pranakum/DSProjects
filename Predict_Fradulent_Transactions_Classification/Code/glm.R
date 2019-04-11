# Clear the environment variables
rm(list=ls(all=TRUE))
#Load all the Libraries Required
library(glmnet)
library(caret)
library(DMwR)
# Setting the current working directory
setwd("C:/Users/Hai/Desktop/insofe/Cute3")

#Importing 
data=read.csv("train.csv", header=TRUE, sep=",",na.strings=c("","NA"))

test=read.csv("test.csv",header=TRUE,sep=",",na.strings=c("","NA"))
# Understanding the data
dim(data)
head(data)
names(data)
str(data)
summary(data)
dim(test)
summary(test)
str(test)
## Remove columns which does not add any information

x=nearZeroVar(data,names=TRUE)

data=data[,setdiff(names(data),x)]
summary(data)

test=test[,setdiff(names(test),x)]
summary(test)
dim(data)
#53631    29
dim(test)
#53545    28

##Check for Missing Values

sum(is.na(data))
sum(is.na(test))
# II. Find Number of missing values and drop a row if it contains more than 20% of columns contains missing values. Impute the remining.
sum(is.na(data))
length(manyNAs(data, 0.2) )
ExcludedMissing= data[!rowSums(is.na(data)) > ncol(data)*.2,]
sum(is.na(ExcludedMissing))
ImputedData=centralImputation(ExcludedMissing)
sum(is.na(ImputedData))
str(ImputedData)

# need to do according to train data mean(numeric) & mode(cat)
sum(is.na(test))
length(manyNAs(test, 0.2) )
ExcludedMissingTest= test[!rowSums(is.na(test)) > ncol(test)*.2,]
sum(is.na(ExcludedMissingTest))
ImputedTest=centralImputation(ExcludedMissingTest[,-c(3)])
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
table(ImputedDataRemovedId$cat_var_1)

ImputedDataRemovedId = ImputedDataRemovedId[sapply(ImputedDataRemovedId,nlevels) <= 50]
str(ImputedTestRemovedId)
ImputedTestRemovedId$cat_var_19 = as.factor(ImputedTestRemovedId$cat_var_19)
ImputedTestRemovedId$cat_var_20 = as.factor(ImputedTestRemovedId$cat_var_20)
ImputedTestRemovedId$cat_var_21 = as.factor(ImputedTestRemovedId$cat_var_21)
ImputedTestRemovedId$cat_var_22 = as.factor(ImputedTestRemovedId$cat_var_22)
ImputedTestRemovedId$target = as.factor(ImputedTestRemovedId$target)
summary(ImputedTestRemovedId)

# Dropped columns with more than 50 levels cat_var_1 cat_var_3 cat_var_6 cat_var_8 cat_var_13

ImputedDataRemovedId = ImputedDataRemovedId[sapply(ImputedDataRemovedId,nlevels) <= 50]



testData = ImputedTestRemovedId[sapply(ImputedTestRemovedId,nlevels) <= 50]
# Dropped columns with more than 50 levels cat_var_1 cat_var_3 cat_var_6 cat_var_8 cat_var_13






# Do Train-Test Split
library(caret)
set.seed(125)
rows=createDataPartition(ImputedDataRemovedId$target,p = 0.7,list = FALSE)
train=ImputedDataRemovedId[rows,]
val=ImputedDataRemovedId[-rows,]


table(ImputedDataRemovedId$target)
prop.table(table(ImputedDataRemovedId$target))

# 0         1 
# 0.8923757 0.1076243 

#Let us handle through Synthetic Minority OverSampling Technique
# Dive data in to train and validation then ****apply SMOTE on train****

table(train$target)
prop.table(table(train$target))
# 0         1 
# 0.8923634 0.1076366 

trainsmote<-SMOTE(target~.,data=train,perc.over = 700,perc.under = 100 )
table(trainsmote$target)
prop.table(table(trainsmote$target))
# 0         1 
# 0.4666667 0.5333333 

glm_model <- glm(target~.,data = trainsmote, family = binomial)
summary(glm_model)



# There is a mismatch in the levels in train and test data
str(train)
str(testData)
levels(testData$cat_var_2) = levels(train$cat_var_2)
levels(val$cat_var_2)=levels(trainsmote$cat_var_2)
# Getting the Predictions for the test data
preds_rf <- predict(glm_model, val)
preds_rf <- predict(glm_model, testData)

final3 = data.frame(transaction_id = ImputedTest$transaction_id,target = preds_rf)
head(final,3)

write.csv(final, "submissionglm.csv", row.names = FALSE)




