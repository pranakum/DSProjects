

#Best model was Ridge Regression with RMSE of2.445258

# PCA 2.559
# 
# PCA with Linear 2.475772
# 
# PCA with Step AIC 2.478196
# 
# PCA with Lasso 2.475772
# 
# 
# Linear  2.454668
# 
# Ridge 2.445258


# Clear the environment variables
rm(list=ls(all=TRUE))
#Load all the Libraries Required
library(glmnet)
library(caret)
library(DMwR)
# Setting the current working directory
setwd("C:/Users/Hai/Desktop/insofe/Cute2")

#Importing & exporting data
data<-read.csv("train.csv",header=T,sep=",")
test<-read.csv("test.csv",header=T,sep=",")

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
# reduced to 61 variables
data=data[,setdiff(names(data),x)]
summary(data)

test=test[,setdiff(names(test),x)]
summary(test)
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

# need to do according to train data mean(numeric) & mode(cat)
sum(is.na(test))
length(manyNAs(test, 0.2) )
ExcludedMissingTest= test[!rowSums(is.na(test)) > ncol(test)*.2,]
sum(is.na(ExcludedMissingTest))
ImputedTest=centralImputation(ExcludedMissingTest[,-c(3)])
sum(is.na(ImputedTest))

#59 variables
#drop id and name attributes
ImputedDataRemovedIdandName=ImputedData
ImputedDataRemovedIdandName$rowId=NULL
ImputedDataRemovedIdandName$Itemname=NULL
sum(is.na(ImputedDataRemovedIdandName))
summary(ImputedDataRemovedIdandName)


ImputedTestRemovedIdandName=ImputedTest
ImputedTestRemovedIdandName$rowId=NULL
ImputedTestRemovedIdandName$Itemname=NULL
sum(is.na(ImputedTestRemovedIdandName))
summary(ImputedTestRemovedIdandName)


##Split data into train and val

set.seed(125)
rows=createDataPartition(ImputedDataRemovedIdandName$Rating,p = 0.7,list = FALSE)
train=ImputedDataRemovedIdandName[rows,]
val=ImputedDataRemovedIdandName[-rows,]

###Apply standardizations

pre1<-preProcess(train[,setdiff(colnames(train),"Rating")])
train_scale<-predict(pre1,train[,setdiff(colnames(train),"Rating")])
val_scale<-predict(pre1,val[,setdiff(colnames(val),"Rating")])
test_scale=predict(pre1,ImputedTestRemovedIdandName[,setdiff(colnames(ImputedTestRemovedIdandName),"Rating")])
#reduced to 58 varibles

# Running a PCA using princomp function only on independent varaibles
prcomp_train <- princomp(train_scale)
plot(prcomp_train)
names(prcomp_train)
prcomp_train$loadings
summary(prcomp_train)


# Setting the threshold and storing the component names for future use
#0.90 culmulate proportion
comp_Names=c(1:41)
#0.80
comp_Names=c(1:32)

train_data<-prcomp_train$scores

# get only highest variation components and bind target
train_data<-data.frame(train_data[,comp_Names],"Rating"=train$Rating)

# apply same transformation on test
val_data<-predict(prcomp_train,val_scale)
test_data<-predict(prcomp_train,test_scale)

#subset the components only which are in train_data and bind target 
val_data<-data.frame(val_data[,comp_Names],"Rating"=val$Rating)
test_data<-data.frame(test_data[,comp_Names])


# Model1- Build model with all attributes into model 
LinReg1<- lm(Rating~ ., data=train)
summary(LinReg1)

# Adjusted R-squared: 0.1183 

#Residual analysis
par(mfrow=c(2,2))
plot(LinReg1)
##Model Performance Evaluation
#Error verification on train data
library(DMwR)
regr.eval(train$Rating, LinReg1$fitted.values) 
#mape--
# mae      mse     rmse     mape 
# 1.747836 6.352620 2.520440      Inf 
#Error verification on test data
Pred<-predict(LinReg1,val)
regr.eval(val$Rating, Pred)
# mae      mse     rmse     mape 
# 1.765643 6.538477 2.557045      Inf 
PredTest<-predict(LinReg1,ImputedTestRemovedIdandName)
#regr.eval(ImputedTestRemovedIdandName$Rating, PredTest)
final4 = data.frame(rowId = test$rowId,Itemname = test$Itemname,Rating = round(PredTest,2))
head(final4,3)

write.csv(final, "submission4.csv")



#Apply Linear Regression after PCA
LinPCAReg1<- lm(Rating~ ., data=train_data)
summary(LinPCAReg1)
regr.eval(train_data$Rating, LinPCAReg1$fitted.values) 
# mae      mse     rmse     mape 
# 1.774351 6.580570 2.565262      Inf '
PredPCAReg1<-predict(LinPCAReg1,val_data)
regr.eval(val_data$Rating, PredPCAReg1)
# mae      mse     rmse     mape 
# 1.787597 6.749406 2.597962      Inf 
PredTestPCAReg1<-predict(LinPCAReg1,test_data)

# Upload to Grader 1
final = data.frame(rowId = test$rowId,Itemname = test$Itemname,Rating = round(PredTestPCAReg1,2))
head(final,3)

write.csv(final, "submission1.csv")

final2 = data.frame(rowId = test$rowId,Itemname = test$Itemname,Rating = round(PredTestPCAReg1,2))
head(final2,3)
final2[,1:3]
write.csv(final2,"submission2.csv",row.names=FALSE)


#### Model Building 

### LASSO Regression
# 1. Let us build a simple Lasso  regression
# 2. Lets do a cross validation with Lasso  regression
# 3. Also Will tune the Model for perfect "lamda value"
x.train=as.matrix(train_data[,setdiff(names(train_data),"Rating")])
y.train=train_data$Rating
x.val = as.matrix(val_data[,setdiff(names(val_data),"Rating")])
y.val = val$Rating
x.test=as.matrix(test_data[,setdiff(names(test_data),"Rating")])
#y.test=ImputedTestRemovedIdandName$Rating
#test=as.matrix(test)
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)

fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="class", alpha=1, 
                          family="gaussian",nfolds=5,parallel=TRUE)

plot(fit.lasso, xvar="lambda")
plot(fit.lasso.cv)

coef(fit.lasso.cv,s = fit.lasso.cv$lambda.min)
pred.lasso.cv.train <- predict(fit.lasso.cv,x.train,s = fit.lasso.cv$lambda.min,type="response")
pred.lasso.cv.val <- predict(fit.lasso.cv,x.val,s = fit.lasso.cv$lambda.min,type="response")
pred.lasso.cv.test <- predict(fit.lasso.cv,x.test,s = fit.lasso.cv$lambda.min,type="response")

regr.eval(pred.lasso.cv.train, y.train)
# mae       mse      rmse      mape 
# 1.7731073 6.5822809 2.5655956 0.2562147
regr.eval(pred.lasso.cv.val, y.val)
# mae       mse      rmse      mape 
# 1.7869950 6.7503600 2.5981455 0.2566935 

final3 = data.frame(rowId = test$rowId,Itemname = test$Itemname,Rating = round(pred.lasso.cv.test,2))
head(final3,3)
colnames(final3)[3] <- "Rating"
write.csv(final3,"submission3.csv",row.names=FALSE)



#Step AIC
# 2. Stepwise Regression
#Model5 - Build Model based on Stepwise Regression
library(MASS)
Step1 <- stepAIC(LinPCAReg1, direction="backward")
#Step2 <- stepAIC(LinReg1, direction="forward")
Step3 <- stepAIC(LinPCAReg1, direction="both")
summary(Step3)
Mass_LinReg1 <- lm(formula = Rating ~ Comp.1 + Comp.3 + Comp.4 + Comp.5 + Comp.6 + 
                     Comp.7 + Comp.8 + Comp.10 + Comp.12 + Comp.14 + Comp.15 + 
                     Comp.16 + Comp.17 + Comp.19 + Comp.20 + Comp.21 + Comp.23 + 
                     Comp.24 + Comp.25 + Comp.27 + Comp.28, data = train_data)

summary(Mass_LinReg1)
par(mfrow=c(2,2))
plot(Mass_LinReg1)
plot(Mass_LinReg1,which=4)
par(mfrow=c(1,1))
head(train)

#Error verification on train data
regr.eval(train_data$Rating, Mass_LinReg1$fitted.values) 
# mae      mse     rmse     mape 
# 1.776419 6.583993 2.565929      Inf 
#Error verification on test data
MASS_Pred1<-predict(Mass_LinReg1,val_data)
regr.eval(val_data$Rating, MASS_Pred1)
# mae      mse     rmse     mape 
# 1.790823 6.752108 2.598482      Inf 
MASS_PredTest<-predict(Mass_LinReg1,test_data)
final5 = data.frame(rowId = test$rowId,Itemname = test$Itemname,Rating = round(MASS_PredTest,2))
head(final5,3)

write.csv(final5,"submission5.csv",row.names=FALSE)


#---------------------------------------------------------------------------------------------------------------------------

# ## Standardizing the Data
# library(caret)
# # The "preProcess()" function creates a model object required for standardizing unseen data
# # Do not standardize the target variable
# 
# train_nonstd = train
# test_nonstd = test
# 
# independentattr<-setdiff(names(train),c("Rating"))
# std_model <- preProcess(train[, independentattr], method = c("range"))
# 
# # The predict() function is used to standardize any other unseen data
# 
# train[, independentattr] <- predict(object = std_model, newdata = train[, independentattr])
# test[, independentattr] <- predict(object = std_model, newdata = test[, independentattr])
# 
# 
# # Model3- Build linear regression with all standardized attributes 
# LinReg_std1<-lm(Rating~., data=train)
# summary(LinReg_std1)
# #Multiple R-squared:  0.1235,	Adjusted R-squared:  0.1183
# #Error verification on train data
# regr.eval(train$Rating, LinReg_std1$fitted.values) 
# # mae      mse     rmse     mape 
# # 1.747836 6.352620 2.520440      Inf 
# #Error verification on test data
# Pred<-predict(LinReg_std1,val)
# regr.eval(val$Rating, Pred)
# # mae          mse         rmse         mape 
# # 2.961688e+05 3.663721e+11 6.052868e+05          Inf
# plot(LinReg_std1)
# 
# 
# 
# # Check for multicollinearity and perform dimensionality reduction analysis
# library(corrplot)
# cor(train[,-c(11370,13772,11429)])
# #Model4 - Build Model based on VIF Analysis
# # 1. VIF: (check attributes with high VIF value)
# library(car)
# vif(LinReg_std1)
# # attr3        attr4        attr5        attr6       attr23       attr39       attr64      attr119      attr123      attr128 
# # 73318.735945    96.968996 60089.094732   741.119632     1.251862     1.497957     1.942067     1.152436     1.130315     1.074258 
# # attr131      attr141      attr177      attr178      attr183      attr186      attr193      attr199      attr211      attr235 
# # 1.166323     1.460687     1.138658     2.117237     2.370262     1.549639     1.633474     1.163884     1.252291     1.172404 
# # attr239      attr243      attr247      attr258      attr273      attr274      attr312      attr313      attr317      attr331 
# # 1.080999     1.041226     1.991926     1.185816     1.241431     1.056709     1.091709     1.575882     5.466254     1.036607 
# # attr347      attr355      attr384      attr412      attr413      attr429      attr460      attr469      attr495      attr509 
# # 1.146130     1.238278     1.164746     1.868060     1.177222     1.088088    11.720052     5.551004     1.110411     1.181174 
# # attr527      attr539      attr549      attr551      attr567      attr576      attr581      attr588      attr601      attr604 
# # 1.151415     1.229016     1.127124     1.144957     1.776128     1.185447    10.119904     1.045617     1.563020     1.238840 
# # attr623      attr628      attr631      attr641      attr642      attr643      attr661      attr668 
# # 1.316163     1.105594     3.603530     1.722886     1.118947     3.010862     2.091487     1.182318 
# # 
# 
#-----------------------------------------------------------code merge

#2------- Ridge

remove(list=ls())
dev.off()
# setwd("D:/insofe/Cute")

# Imputing the missing value in Train -------------------------------------
train=centralImputation(mydata)
sum(is.na(train))
# 0


# Determining the zero variance columns and removing them from train --------
x=nearZeroVar(train,names=T)
length(x)
#620 columns add near to 0 variance to the data set which would be removed.
train=train[,setdiff(names(train),x)]
dim(train)
#14038 61

# Determining the range of each column to understand the spread of the train data ------------------------------------
range_values_train=data.frame(names(train[,-c(1,2,3)]),t(sapply(train[,-c(1,2,3)], range)))
range_values_train$Range=range_values_train$X2-range_values_train$X1
range_values_train[range_values_train[,"Range"]>1,]

# names.train...
# .c.1..2..3... X1 X2 Range
# attr3 attr3 0 13062948 13062948
# attr4 attr4 0 236489 236489
# attr5 attr5 0 747374 747374
# attr6 attr6 0 12005810 12005810

# Seperating the attributes that would not be used for the model from train i.e. rowID and itemname -----------

train_unimportant_attributes=train[,c(1,2)]
train = train[,-c(1,2)]
dim(train)
#14038 59


# Checking for outliers ---------------------------------------------------
boxplot(train[,c(2,3,4,5)],horizontal=T)

#From the range and boxplot we notice visible outliers are present in the dataset
#Also from the dataset it can be noticed that the high valued rows for attr3,attr4,attr5 and attr6
#are in the same 5 rows
Max_Outliers=head(sort(train$attr3,decreasing = T),6)
#13062948 4157357 4157357 3358273 3358029 54512
i=0
vec=c()
while (i<length(Max_Outliers)){
  vec[i]=which(train$attr3==Max_Outliers[i])
  i=i+1
}
train=train[-vec,]
dim(train)
#14034 59

boxplot(train[,c(2,3,4,5)],horizontal=T)
#There seem to be another big outlier for attr3 running the same code again
Max_Outliers=head(sort(train$attr3,decreasing = T),2)
#4157357 54512
i=0
vec=c()
while (i<length(Max_Outliers)){
  vec[i]=which(train$attr3==Max_Outliers[i])
  i=i+1
}
train=train[-vec,]
dim(train)
#14033 59
boxplot(train[,c(2,3,4,5)],horizontal=T)

#Checking for the big outlier in attr6
Max_Outliers=head(sort(train$attr6,decreasing = T),5)

#Doesn't seem like a big outlier. Checking the new ranges and
range_values_train.before_outliers=range_values_train
range_values_train=data.frame(names(train[,-c(1)]),t(sapply(train[,-c(1)], range)))
range_values_train$Range=range_values_train$X2-range_values_train$X1
range_values_train[range_values_train[,"Range"]>1,]

# names.train....c.1... X1 X2 Range
# attr3 attr3 0 54512 54512
# attr4 attr4 0 2074 2074
# attr5 attr5 0 2228 2228
# attr6 attr6 0 132220 132220

#There is a noticable change in the range after the outliers are removed.

# Splitting train into validation and train -------------------------------

set.seed(125)
rows=createDataPartition(train$Rating,p = 0.7,list = FALSE)
train1=train[rows,]
val1=train[-rows,]

# PreProcess the data to standadize the numeric attributes
preProc<-preProcess(train1[,setdiff(names(train1),"Rating")],method = c("center", "scale"))
train1<-predict(preProc,train1)
val1<-predict(preProc,val1)


# Lasso regression --------------------------------------------------------

#install.packages("glmnet")
library(glmnet)
str(train1)
dim(train1)
#9825 59


### LASSO Regression
# 1. Let us build a simple Lasso regression
# 2. Lets do a cross validation with Lasso regression
# 3. Also Will tune the Model for perfect "lamda value"
x.train=as.matrix(train1[,setdiff(names(train1),"Rating")])
y.train=train1$Rating
x.val = as.matrix(val1[,setdiff(names(val1),"Rating")])
y.val = val1$Rating
#test=as.matrix(test)
fit.Ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)

fit.Ridge.cv <- cv.glmnet(x.train, y.train, type.measure="mae", alpha=0, 
                          family="gaussian",nfolds=5,parallel=TRUE)

plot(fit.Ridge, xvar="lambda")
plot(fit.Ridge.cv)
penalty=fit.Ridge.cv$lambda.min
#coef(fit.lasso.cv,s = fit.lasso.cv$lambda.min)
pred.Ridge.cv.train <- predict(fit.Ridge.cv,x.train,s = fit.Ridge.cv$lambda.min,type="response")
pred.Ridge.cv.val <- predict(fit.Ridge.cv,x.val,s = fit.Ridge.cv$lambda.min,type="response")
# Error metrics for train and validation ----------------------------------

regr.eval(pred.Ridge.cv.train, y.train)
# mae       mse      rmse      mape 
# 1.7712422 6.5218205 2.5537855 0.2607418
regr.eval(pred.Ridge.cv.val, y.val)
# mae       mse      rmse      mape 
# 1.7303387 6.1849043 2.4869468 0.2548263

# Predicting test values with this model ----------------------------------

test=centralImputation(mydata1[,-c(3)])
#Removing the rowID and itemName column
test$rowId=NULL
test$Itemname=NULL

# Determining the zero variance columns and removing them from test --------
#x=nearZeroVar(test,names=T)
length(x)
#620 columns add near to 0 variance to the data set which would be removed.
test=test[,setdiff(names(test),x)]
dim(test)
#6014 58 (the column is one less than train as rating column is removed)

#which(names(train)=='attr131')

#setdiff(names(train),names(test))

#Applying the same preprocessing as that of train onto test----
test=predict(preProc,test)


# Predicting the model on the PCA transformed test data -------------------
pred_test=predict(fit.Ridge.cv,as.matrix(test),s = penalty,type="response")

test$Rating=round(pred_test,digits = 2)

mean_test=mean(test$Rating[test$Rating<10])#7.431038
mean_val=mean(val1$Rating) #7.380288
mean_train=mean(train1$Rating) #7.431552

test_Pred=cbind(mydata1[,c(1,2)],"Ratings"=test$Rating)
colnames(test_Pred)=c("rowId","Itemname","Rating")
test_pred1=test_Pred
test_Pred[test_Pred[,c(3)]>10,c(3)]=mean_test

write.csv("Test_Prediction_Ridge.csv",x = test_Pred,sep = ',',row.names = F,quote = F)

if (!exists("Test_Result")){
  Test_Result=mydata1[,c(1,2)]
}
Test_Result$Ridge=test_Pred$Rating
head(Test_Result)










