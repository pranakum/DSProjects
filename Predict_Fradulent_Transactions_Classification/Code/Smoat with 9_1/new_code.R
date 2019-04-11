rm(list = ls())

setwd('E:\\Batch46 - R\\Cute03\\Again')
library(tidyverse)
library(caret)
library(DMwR)

#bank <- read.csv('train_new.csv', na.strings = '')
#str(bank)

#saveRDS(bank, file = 'bankRDS.rds')
#bank <- readRDS('bankRDS.rds')
#str(bank)

#test <- read.csv('test_new.csv', na.strings = '')

#bank$transaction_id <- NULL
#test.id <- test$transaction_id
#saveRDS(test.id, file = 'test_id.rds')
#test$transaction_id <- NULL
#str(test)
#-------------------------PRE-PROCESSING--------------------------------#

#Converting Category 19 to 42, and target to category
#factor_cols <- names(bank[,26:ncol(bank)])
#df <- bank %>% 
#  dplyr::select(factor_cols) %>%
#  lapply(factor) %>%
#  as.data.frame()
#bankdata <- bank %>% dplyr::select(-factor_cols) %>% cbind(df)
#str(bankdata)

#factor_cols_test <- names(test[,26:ncol(test)])
#df_test <- test %>%
#  dplyr::select(factor_cols_test) %>%
#  lapply(factor) %>%
#  as.data.frame()
#test <- test %>% dplyr::select(-factor_cols_test) %>% cbind(df_test)
#str(test)

#Check NA values
#sum(is.na(bankdata))

#Checking class imbalance
#prop.table(table(bankdata$target))  #Class-imbalance is present

#Dividing Data to Train and Validation
#set.seed(123)
#trainrows <- createDataPartition(bankdata$target, p = 0.7, list = F)
#train <- bankdata[trainrows,]
#val <- bankdata[-trainrows,]

#Imputation in data with centralImputation
#traindata <- centralImputation(train)
#valdata <- centralImputation(val)

#testdata <- centralImputation(test)

#nearZeroVariance
#nearZeroCols <- nearZeroVar(traindata)
#traindata <- traindata[,-nearZeroCols]
#str(traindata)
#valdata <- valdata[,-nearZeroCols]
#testdata <- testdata[,-nearZeroCols]

#Smoting
#trainsmote9 <- SMOTE(target~., data = traindata, perc.over = 900,
#                     perc.under = 100)

#saveRDS(trainsmote9, file = 'trainsmote9.rds')
trainsmote9 <- readRDS('trainsmote9.rds')
str(trainsmote9)

test.id <- readRDS('test_id.rds')
#Saving test and val data in RDS format
#saveRDS(valdata, file = 'valdata.rds')
#saveRDS(testdata, file = 'testdata.rds')

testdata <- readRDS('testdata.rds')
valdata <- readRDS('valdata.rds')

#Dividing train and validation data
x.val <- valdata %>% select(-target)
y.val <- valdata %>% select(target) %>% as.matrix

#----------------------------------------------------------------#

#----------------------------------------------------------------#

#LOGIStiC REGRESSION

model.log <- glm(target~., data = trainsmote9, family = 'binomial')
summary(model.log)

train.probs <- predict(model.log, trainsmote9, type = 'response')
val.probs <- predict(model.log, x.val, type = 'response')

#--------------------------------------------------------------------#
#---------------THRESHOLD EVALUATION -------------------#

#Threshold as 80%
train.preds.log <- ifelse(train.probs > 0.80, 1, 0)
val.preds.log <- ifelse(val.probs > 0.80, 1, 0)

#confusionMatrix(trainsmote9$target, as.factor(train.preds), positive = '1')
#confusionMatrix(as.factor(y.val), as.factor(val.preds), positive = '1')

#Threshold as 90%
train.preds <- ifelse(train.probs > 0.90, 1, 0)
val.preds <- ifelse(val.probs > 0.90, 1, 0)

confusionMatrix(trainsmote9$target, as.factor(train.preds), positive = '1')
confusionMatrix(as.factor(y.val), as.factor(val.preds), positive = '1')

#Threshold as 95%
train.preds <- ifelse(train.probs > 0.95, 1, 0)
val.preds <- ifelse(val.probs > 0.95, 1, 0)

confusionMatrix(trainsmote9$target, as.factor(train.preds), positive = '1')
confusionMatrix(as.factor(y.val), as.factor(val.preds), positive = '1')

#Threshold as 99%
train.preds <- ifelse(train.probs > 0.99, 1, 0)
val.preds <- ifelse(val.probs > 0.99, 1, 0)

confusionMatrix(trainsmote9$target, as.factor(train.preds), positive = '1')
confusionMatrix(as.factor(y.val), as.factor(val.preds), positive = '1')

#Threshold as 96%
train.preds <- ifelse(train.probs > 0.96, 1, 0)
val.preds <- ifelse(val.probs > 0.96, 1, 0)

confusionMatrix(trainsmote9$target, as.factor(train.preds), positive = '1')
confusionMatrix(as.factor(y.val), as.factor(val.preds), positive = '1')

#Threshold as 97%
train.preds <- ifelse(train.probs > 0.97, 1, 0)
val.preds <- ifelse(val.probs > 0.97, 1, 0)

confusionMatrix(trainsmote9$target, as.factor(train.preds), positive = '1')
confusionMatrix(as.factor(y.val), as.factor(val.preds), positive = '1')

#Threshold as 98%
train.preds <- ifelse(train.probs > 0.98, 1, 0)
val.preds <- ifelse(val.probs > 0.98, 1, 0)

confusionMatrix(trainsmote9$target, as.factor(train.preds), positive = '1')
confusionMatrix(as.factor(y.val), as.factor(val.preds), positive = '1')

#--------------------------------------------------------------------#
#Taking threshold as 80%
test.probs <- predict(model.log, testdata, type = 'response')
test.preds.log <- ifelse(test.probs > 0.80, 1, 0)

final.log <- cbind.data.frame('transaction_id'=test.id, 'target'=test.preds.log)
write.csv(final.log, file = 'final_log.csv', row.names = F)

#---------------------------------------------------------------#
#for stacking
saveRDS(train.preds.log, 'train_pred_log.rds')
saveRDS(val.preds.log, 'val_pred_log.rds')
saveRDS(test.preds.log, 'test_pred_log.rds')
#---------------------------------------------------------------#

#--------------------------------------------------------------------#

