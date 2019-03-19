#0.50

rm(list = ls())

setwd('E:\\Batch46 - R\\Cute03\\Again')
library(caret)
library(tidyverse)
library(DMwR)

#names(getModelInfo())
test.id <- readRDS('test_id.rds')
bank <- read.csv('train_new.csv', na.strings = '')
test <- read.csv('test_new.csv', na.strings = '')

str(bank)
bank$transaction_id <- NULL
test$transaction_id <- NULL

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

#decreasing bank data rows
#set.seed(125)
#random.rows <- createDataPartition(bankdata$target, p = 0.1, list = F)
#bank.new <- bankdata[random.rows,]
#--------------------------------------------------------------#
#Train-Val split
set.seed(125)
trainrows <- createDataPartition(bank$target, p = 0.7, list = F)
train <- bankdata[trainrows, ]
val <- bankdata[-trainrows,]

#NearZeroVar cols
zeroCols <- nearZeroVar(train)
train <- train[,-zeroCols]
val <- val[,-zeroCols]
test <- test[,-zeroCols]

x.val <- val %>% select(-target)
y.val <- val %>% select(target)

#---------------------------------------------------------------#
###create dummies for factor varibales 
dummies <- dummyVars(target~., data = train)


x.train=predict(dummies, newdata = train)
y.train=train$target %>% as.matrix
x.val = predict(dummies, newdata = val)
y.val = val$target %>% as.matrix

test$target = 0
test.dummy <- predict(dummies, newdata = test)
test$target <- NULL

#which(colnames(test.dummy)!=colnames(x.train))

#test.dummy <- test.dummy[,-210:-217]

#---------------------

library(xgboost)
library(readr)
library(stringr)
library(car)


xgb <- xgboost(data = x.train, 
               label = y.train, 
               eta = 0.1, 
               max_depth = 15, 
               nround=10, 
               subsample = 0.5, 
               colsample_bytree = 0.5, 
               seed = 1, 
               eval_metric = "error", 
               objective = "reg:logistic")

model <- xgb.dump(xgb, with.stats = T)

importance_matrix <- xgb.importance(model = xgb)
xgb.plot.importance(importance_matrix[1:10,])

train.probs <- predict(xgb, data.matrix(x.train)) %>% as.matrix
train.pred.xg <- ifelse(train.probs > 0.5, 1, 0)
confusionMatrix(as.factor(y.train), as.factor(train.pred.xg), positive = '1')

val.probs <- predict(xgb, data.matrix(x.val)) %>% as.matrix
val.pred.xg <- ifelse(val.probs > 0.5, 1, 0)
confusionMatrix(as.factor(y.val), as.factor(val.pred.xg), positive = '1')

test.probs <- predict(xgb, data.matrix(test.dummy)) %>% as.matrix
test.pred.xg <- ifelse(test.probs > 0.5, 1, 0)

final.xg <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred.xg)
write.csv(final.xg, file = 'final_xg.csv', row.names = F)

#---------------------------------------------------------------#
#for stacking
saveRDS(train.pred.xg, 'train_pred_xg.rds')
saveRDS(val.pred.xg, 'val_pred_xg.rds')
saveRDS(test.pred.xg, 'test_pred_xg.rds')
#---------------------------------------------------------------#