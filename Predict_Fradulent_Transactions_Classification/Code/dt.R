rm(list=ls(all=TRUE))
setwd("C:/Users/Hai/Desktop/insofe/Cute3")

#Load all the Libraries Required
library(caret)
library(DMwR)



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


table(ImputedDataRemovedId$target)
prop.table(table(ImputedDataRemovedId$target))
# Observe that there is class imbalance in the data -even if you put Premium for all samples we 
#will get 91 % as accuracy.
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
valsmote<-SMOTE(target~.,data=val,perc.over = 700,perc.under = 100 )
table(valsmote$target)
prop.table(table(valsmote$target))
# 0         1 
# 0.4666667 0.5333333 
##Build classification model using C50
library(C50)
#Build model on nomal data- train
DT_C50 <- C5.0(target~.,data=trainsmote,rules=TRUE)
summary(DT_C50)# All datapoint classified as Premium.

#Build model on smoteddata trainsmote
DT_C50 <- C5.0(Revenue~.,data=trainsmote,rules=TRUE)
summary(DT_C50)
#Trails builds 10 trees
# DT_C50 <- C5.0(Revenue~.,data=trainsmote,rules=TRUE,trails=10)
# summary(DT_C50)

##predict on train and validation
pred_Train = predict(DT_C50,newdata=trainsmote, type="class")
pred_val = predict(DT_C50, newdata=val, type="class")

#Error Metrics on train and test
confusionMatrix(trainsmote$target,pred_Train)
confusionMatrix(val$target,pred_val)

#Check variable importance
C5imp(DT_C50, pct=TRUE)



################################################################
#Build classification model using RPART
library(rpart)

#Build model on train (non smoted data)
DT_rpart_class<-rpart(target~.,data=train,method="class")
DT_rpart_class

# All samples classified in to one class Premium
#summary(DT_rpart_class)

# Build on smoted data
DT_rpart_class<-rpart(target~.,data=trainsmote,method="class")
DT_rpart_class


#Predict "Revenue" for train and test datasets
pred_Train = predict(DT_rpart_class,newdata=trainsmote, type="class")
pred_Test = predict(DT_rpart_class, newdata=val, type="class")

#Error Metrics on train and test
confusionMatrix(trainsmote$target,pred_Train)
confusionMatrix(val$target,pred_Test)

# Parameter tuning - Choosing Best CP
#try with 0.1,0.01,0.001
#check xerror dec and then inc take the previous cp value and build the tree
DT_rpart_class1<-rpart(target~.,data=trainsmote,method="class",control = rpart.control(cp=0.000001))
printcp(DT_rpart_class1)
# 
plot(DT_rpart_class1)
text(DT_rpart_class1)

# observe that xerror/rel error are decresing as cp reduces.
# See at one cp xerror/rel error increses . choose the cp where you got minumum  xerror/relerror 
DT_rpart_class1<-rpart(target~.,data=trainsmote,method="class",control = rpart.control(cp= 1.6498e-04))
printcp(DT_rpart_class1)
plot(DT_rpart_class1)
text(DT_rpart_class1)

#Predict "Revenue" for train and test datasets
pred_Train1 = predict(DT_rpart_class1,newdata=trainsmote, type="class")
pred_Test1 = predict(DT_rpart_class1, newdata=val, type="class")

#Error Metrics on train and test
confusionMatrix(trainsmote$target,pred_Train1)
confusionMatrix(val$target,pred_Test1)





# Grid Search using Rpart 
tc <- trainControl("cv",10)
rpart.grid <- expand.grid(cp=seq(0,0.3,0.01))
train.rpart <- train(target ~., data=trainsmote, method="rpart",trControl=tc,tuneGrid=rpart.grid)

plot(train.rpart$finalModel)
text(train.rpart$finalModel)

pred_Train1 = predict(train.rpart,newdata=trainsmote, type="raw")
pred_Test1 = predict(train.rpart, newdata=val, type="raw")


confusionMatrix(trainsmote$target,pred_Train1)
confusionMatrix(val$target,pred_Test1)
library(MLmetrics)
F1_Score(y_pred = pred_Train1, y_true = trainsmote$target, positive = "0")
#0.9528466

#0.9466626
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
pred_Testdata1 = predict(train.rpart, newdata=testData, type="raw")

final2 = data.frame(transaction_id = ImputedTest$transaction_id,target = pred_Testdata1)
head(final2,3)


write.csv(final2, "submissionrpartgrid.csv", row.names = FALSE)


# #Grid Search using c5.0
# tc <- trainControl("cv",10)
# c50Grid <- expand.grid(.trials = c(1:9, (1:10)*10),
#                        .model = c("tree", "rules"),
#                        .winnow = c(TRUE, FALSE))
# train.c50 <- train(Revenue ~., data=trainsmote, method="C5.0",trControl=tc,tuneGrid=c50Grid)
# 
# summary(train.c50)
# pred_Train1 = predict(train.c50,newdata=trainsmote, type="raw")
# pred_Test1 = predict(train.c50, newdata=validation1, type="raw")
# 
# confusionMatrix(trainsmote$Revenue,pred_Train1)
# confusionMatrix(validation$Revenue,pred_Test1)
# 
model_initial = glm(y ~ 1, family = binomial, data = train_data)
model_final = glm(formula = y ~ ., family = binomial, data = train_data)
model = stepAIC(model_initial, scope =  as.formula(model_final), direction = "forward", steps = target_size, trace = FALSE)
coefs = model$coefficients;


###Apply standardizations

pre1<-preProcess(trainsmote[,setdiff(colnames(trainsmote),"target")])
train_scale<-predict(pre1,trainsmote[,setdiff(colnames(trainsmote),"target")])
val_scale<-predict(pre1,val[,setdiff(colnames(val),"target")])
test_scale=predict(pre1,ImputedTestRemovedIdandName[,setdiff(colnames(ImputedTestRemovedIdandName),"Rating")])
#reduced to 58 varibles

# Running a PCA using princomp function only on independent varaibles
prcomp_train <- princomp(train_scale)
plot(prcomp_train)
names(prcomp_train)
prcomp_train$loadings
summary(prcomp_train)