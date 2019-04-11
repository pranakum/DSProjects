rm(list=ls(all=TRUE))
setwd("C:/Users/Hai/Desktop/insofe/Cute3")
library(caret)
library(DMwR)
#bankdata=read.csv("UniversalBank.csv", header=TRUE, sep=",")
data=read.csv("train.csv", header=TRUE, sep=",",na.strings=c("","NA"))
str(data)
dat2 <- read.csv("train.csv", header=T, na.strings=c("","NA"))
str(data)
test=read.csv("test.csv",header=TRUE,sep=",",na.strings=c("","NA"))
str(test)
#OBJECTIVE: Will a person take a personal loan or not? 
#Response variable is "Personal.Loan" 

# Understanding the data 

# summary(bankdata)
# str(bankdata)
# head(bankdata)
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



# # Removing unnecessary columns ID and zipcode
# bankdata=subset(bankdata, select=-c(ID,ZIP.Code))
# 
# # Do necessary type conversions
# bankdata$Education=as.factor(as.character(bankdata$Education))
# bankdata$Personal.Loan=as.factor(as.character(bankdata$Personal.Loan))

# Do Train-Test Split
library(caret)
set.seed(125)
rows=createDataPartition(ImputedDataRemovedId$target,p = 0.7,list = FALSE)
train=ImputedDataRemovedId[rows,]
val=ImputedDataRemovedId[-rows,]

# PreProcess the data to standadize the numeric attributes
preProc<-preProcess(train[,setdiff(names(train),"target")],method = c("center", "scale"))
train<-predict(preProc,train)
val<-predict(preProc,val)

###create dummies for factor varibales 
dummies <- dummyVars(target~., data = ImputedDataRemovedId)


x.train=predict(dummies, newdata = train)
y.train=train$target
x.val = predict(dummies, newdata = val)
y.val = val$target

####Use Caret package to build adaboost 

trctrl <- trainControl(method = "cv", number = 5)

grid <- expand.grid(iter = c(50,100,150,200),maxdepth=7:10,nu=c(0.1,0.5,0.9))
set.seed(3233)
Ada_Model <- train(target~., data=train, method = "ada",
                   trControl=trctrl,
                   tuneGrid = grid)


Ada_Model$bestTune

ada_preds <- predict(Ada_Model, val)

confusionMatrix(val$Personal.Loan, ada_preds)



