rm(list = ls())
setwd('E:\\Batch46 - R\\Cute03')

library(tidyverse)
library(caret)
library(DMwR)
#library(ModelMetrics)

#saveRDS(bank, file = 'bankRDS.rds')
bank <- readRDS('bankRDS.rds')
str(bank)

#bank <- read.csv('train.csv', na.strings = '')
test <- read.csv('test.csv', na.strings = '')
str(bank)
summary(bank)

bank$transaction_id <- NULL
test.id <- test$transaction_id
test$transaction_id <- NULL
str(test)

#Number of unique values in each columns
apply(bank, 2, function(x){length(table(x))})

#-----------------------------------------------------------------------#

#Error Metric is F1-Score, and since we can't get the value directly,
#I am writing a function to calculate the score

f1.score  <- function(x, y) {     #x, and y are precision and recall
  return(2*x*y/(x+y))
}

#-----------------------------------------------------------------------#

#-----------------------------------------------------------------------#
#-------------------------PRE-PROCESSING--------------------------------#

#Converting Category 19 to 42, and target to category
factor_cols <- names(bank[,26:ncol(bank)])
df <- bank %>% 
  dplyr::select(factor_cols) %>%
  lapply(factor) %>%
  as.data.frame()
bankdata <- bank %>% dplyr::select(-factor_cols) %>% cbind(df)
str(bankdata)

factor_cols_test <- names(test[,26:ncol(test)])
df_test <- test %>%
  dplyr::select(factor_cols_test) %>%
  lapply(factor) %>%
  as.data.frame()
test <- test %>% dplyr::select(-factor_cols_test) %>% cbind(df_test)
str(test)

#Check NA values
sum(is.na(bankdata))
#Column name with NA 
na_columns <- names(bankdata[apply(bankdata, 2, function(x)
  {sum(is.na(x))}) > 0])
na_columns

apply(bankdata[na_columns], 2, function(x){sum(is.na(x))})
#We can't remove NA rows, because if we do we'll be left with only 70% data

#Checking class imbalance
prop.table(table(bankdata$target))

#Class-imbalance is present

#Dividing Data to Train and Validation
set.seed(123)
trainrows <- createDataPartition(bankdata$target, p = 0.7, list = F)
train <- bankdata[trainrows,]
val <- bankdata[-trainrows,]

#----------------------------------------------------------------#

#Removing columns with only 1 values
single_val_cols_train <- names(train[apply(train, 2, function(x)
{length(table(x))}) == 1])

single_val_cols_val <- names(val[apply(val, 2, function(x)
{length(table(x))}) == 1])

single_val_cols_test <- names(test[apply(test, 2, function(x)
{length(table(x))}) == 1])

#Since val has more number of columns with only 1 data, I am removing 
#combining all the columns in new vector so as to remove all cols from them

single_val_cols <- union(single_val_cols_train, single_val_cols_val) %>%
  union(single_val_cols_test)

train <- train %>% dplyr::select(-single_val_cols)
val <- val %>% dplyr::select(-single_val_cols)
test <- test %>% dplyr::select(-single_val_cols)

#----------------------------------------------------------------#
#Imputing NA values
traindata <- centralImputation(train)
valdata <- centralImputation(val)

testdata <- centralImputation(test)

#saveRDS(valdata, file = 'valdata.rds')
#saveRDS(testdata, file = 'testdata.rds')
#----------------------------------------------------------------#
x.traindata <- traindata %>% dplyr::select(-target)
y.traindata <- traindata %>% dplyr::select(target) %>% as.matrix()

x.valdata <- valdata %>% dplyr::select(-target)
y.valdata <- valdata %>% dplyr::select(target) %>% as.matrix()

#-----------------------------------------------------------------#
#Split traindata into 10 parts
#Doesn't Work
#---------------------------LOGISTIC MODEL -----------------------#
#Problems with Logistic model:
#1. Large Size (around 3GB)
#2. Tried Splitting the data to 10 parts, but problem of some categories 
#   with only 1 value make problems
#3. Better not do Logistic Model
#-----------------------------------------------------------------#
#----------------------------NAIVE BAYES--------------------------#
library(e1071)

model.NB <- naiveBayes(target~., data = traindata)

train.pred <- predict(model.NB, x.traindata)
val.pred <- predict(model.NB, x.valdata)

cnf.NB.train <- confusionMatrix(as.factor(y.traindata), train.pred, positive = '1')
cnf.NB.val <- confusionMatrix(as.factor(y.valdata), val.pred, positive = '1')

#c1 <- table(pred=train.pred, true=y.traindata)
#c2 <- table(true=y.valdata, pred=val.pred)

test.pred.NB <- predict(model.NB, testdata)
final.NB <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.NB, file = 'final_Naive_Bayes.csv', row.names = F)


#-----------------------------------------------------------------#
#----------------------------------------------------------------#

#----------------------------------------------------------------#

#---------------------------SMOTING------------------------------#

#trainsmote_7 <- SMOTE(target~., traindata, perc.over = 700, 
#                    perc.under = 100)

#Save the SMOTE Result
#saveRDS(trainsmote_7, file = 'trainSMOTE7.rds')
trainsmote_7 <- readRDS('trainSMOTE7.rds')

prop.table(table(trainsmote_7$target))
#----------------------------------------------------------------#
x.trainsmote <- trainsmote_7 %>% dplyr::select(-target)
y.trainsmote <- trainsmote_7 %>% dplyr::select(target) %>% as.matrix()

x.valdata <- valdata %>% dplyr::select(-target)
y.valdata <- valdata %>% dplyr::select(target) %>% as.matrix()

library(e1071)

model.NB.smote <- naiveBayes(target~., data = trainsmote_7)

trainsmote.pred <- predict(model.NB.smote, x.trainsmote)
val.pred <- predict(model.NB.smote, x.valdata)

cnf.NB_smote.train <- confusionMatrix(as.factor(y.trainsmote), trainsmote.pred, positive = '1')
cnf.NB_smote.val <- confusionMatrix(as.factor(y.valdata), val.pred, positive = '1')

test.pred <- predict(model.NB.smote, testdata)
final.NB <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.NB, file = 'final_Naive_Bayes_smote.csv', row.names = F)
#------------------------------------------------------------------#



#----------------------------------------------------------------#

#Duplicated rows
train.n <- trainsmote_7
val.n <- bankdata[-trainrows,]

duplicated.rows <- duplicated(trainsmote)

#----------------------------------------------------------------#

#Removing columns with only 1 values
train.n <- train.n %>% dplyr::select(-single_val_cols)
val.n <- val.n %>% dplyr::select(-single_val_cols)
test <- test %>% dplyr::select(-single_val_cols)

#----------------------------------------------------------------#
#Imputing NA values
traindata.n <- centralImputation(train.n)
valdata.n <- centralImputation(val.n)

testdata <- centralImputation(test)

x.traindata.n <- traindata.n %>% dplyr::select(-target)
y.traindata.n <- traindata.n %>% dplyr::select(target) %>% as.matrix()

x.valdata.n <- valdata.n %>% dplyr::select(-target)
y.valdata.n <- valdata.n %>% dplyr::select(target) %>% as.matrix()

library(e1071)

model.NB.n <- naiveBayes(target~., data = traindata.n)

train.pred.n <- predict(model.NB.n, x.traindata.n) %>% as.matrix()
val.pred.n <- predict(model.NB.n, x.valdata.n) %>% as.matrix()

c1 <- table(pred=train.pred.n, true=y.traindata.n)
c2 <- table(pred=val.pred.n, true=y.valdata.n)

test.pred <- predict(model.NB.n, testdata)
final.NB <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.NB, file = 'final_Naive_Bayes_new.csv', row.names = F)

#----------------------------------------------------------------------#
#DON'T GO BELOW
#----------------------------------------------------------------------#
#numerical variable cor-relation
str(bankdata)

num_bank <- bankdata[,1:7]
num_bank <- cbind.data.frame(num_bank, 'target'=bankdata$target)
str(num_bank)
#cor(num_bank)

#corrplot::corrplot(as.matrix(num_bank[,-8]), method = 'circle')

trainrows <- createDataPartition(num_bank$target, p = 0.7, list = F)
train.n <- num_bank[trainrows,]
val.n <- num_bank[-trainrows,]

x.traindata.n <- train.n %>% dplyr::select(-target)
y.traindata.n <- train.n %>% dplyr::select(target) %>% as.matrix()

x.valdata.n <- val.n %>% dplyr::select(-target)
y.valdata.n <- val.n %>% dplyr::select(target) %>% as.matrix()


#model using numerical variables
num.log <- glm(target~., data = train.n, family = 'binomial')
summary(num.log)

stepaic <- MASS::stepAIC(num.log)
car::vif(num.log)

train.pred <- predict(stepaic, x.traindata.n, type = 'response')
val.pred <- predict(stepaic, x.valdata.n, type = 'response')

test.pred <- predict(stepaic, test, type = 'response')
test.op <- ifelse(test.pred > 0.2, 1, 0)

train.op <- ifelse(train.pred > 0.20, 1,0)
val.op <- ifelse(val.pred > 0.20, 1,0)

cm.train <- table('trues' = (y.traindata.n), 'pred' = (train.op))
cm.val <- table('trues' = (y.valdata.n), 'pred' = (val.op))

p <- precision(cm.train)
r <- recall(cm.train)

f1.score(p, r)

#INCOMPLETE
