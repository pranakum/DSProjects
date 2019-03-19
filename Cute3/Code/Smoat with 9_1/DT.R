#Decision Tree

rm(list = ls())

library(tidyverse)
library(caret)
library(DMwR)
library(C50)
library(rpart)
library(rpart.plot)

test.id <- readRDS('test_id.rds')
train <- readRDS('trainsmote9.rds')
test <- readRDS('testdata.rds')
val <- readRDS('valdata.rds')

x.val <- val %>% select(-target)
y.val <- val %>% select(target) %>% as.matrix

#--------------------------------------------------------------------#
#MODEL

DT.C50 <- C5.0(target~., data = train, rules = T)
summary(DT.C50)

train.pred <- predict(DT.C50, newdata = train, type = 'class')
val.pred <- predict(DT.C50, newdata = x.val, type = 'class')

cnf.DTC50.train <- confusionMatrix(train$target, train.pred, positive = '1')
cnf.DTC50.val <- confusionMatrix(as.factor(y.val), val.pred, positive = '1')

test.pred <- predict(DT.C50, newdata = test, type = 'class')

final.C50 <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.C50, file = 'final_C50.csv', row.names = F)
  
#-----------

DT.C50_5trial <- C5.0(target~., data = train, rules = T, trials = 5)

train.pred <- predict(DT.C50_5trial, newdata = train, type = 'class')
val.pred <- predict(DT.C50_5trial, newdata = x.val, type = 'class')

cnf.DTC50_5trial.train <- confusionMatrix(train$target, train.pred, positive = '1')
cnf.DTC50_5trial.val <- confusionMatrix(as.factor(y.val), val.pred, positive = '1')

test.pred <- predict(DT.C50_5trial, newdata = test, type = 'class')

prop.table(table(pred.test))

final.C50_5trial <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.C50_5trial, file = 'final_C50_5trials.csv', row.names = F)

#-----------

DT_rpart <- rpart(target~., data = train, method = 'class')
summary(DT_rpart)

plot(DT_rpart)
text(DT_rpart)

pred.train <- predict(DT_rpart, newdata = train, type = 'class')
pred.val <- predict(DT_rpart, newdata = x.val, type = 'class')

cnf.DTrpart.train <- confusionMatrix(train$target, pred.train, positive = '1')
cnf.DTrpart.val <- confusionMatrix(as.factor(y.val), pred.val, positive = '1')

pred.test <- predict(DT_rpart, test, type = 'class')

prop.table(table(pred.test))

final.rpart <- cbind.data.frame('transaction_id'=test.id, 'target'=pred.test)
write.csv(final.rpart, file = 'final_rpart.csv', row.names = F)

#---------------------PARAMETER TUNING---------------------------#
#choose value of cp
DT_rpart_cp <- rpart(target~., data = train, 
                     method = 'class',
                     control = rpart.control(cp = 0.00001))
printcp(DT_rpart_cp)

#cp = 3.2363e-05
DT_rpart_cp_opt <- rpart(target~., data = train, 
                     method = 'class',
                     control = rpart.control(cp = 3.2363e-05))
#summary(DT_rpart_cp_opt)

plot(DT_rpart_cp_opt)
text(DT_rpart_cp_opt)

pred.train <- predict(DT_rpart_cp_opt, newdata = train, type = 'class')
pred.val <- predict(DT_rpart_cp_opt, newdata = x.val, type = 'class')

cnf.DTrpart.cp.train <- confusionMatrix(train$target, pred.train, positive = '1')
cnf.DTrpart.cp.val <- confusionMatrix(as.factor(y.val), pred.val, positive = '1')

pred.test <- predict(DT_rpart_cp_opt, test, type = 'class')

prop.table(table(pred.test))

final.rpart.cp.opt <- cbind.data.frame('transaction_id'=test.id, 'target'=pred.test)
write.csv(final.rpart.cp.opt, file = 'final_rpart_opt.csv', row.names = F)

#cp = 0.05
DT_rpart_try <- rpart(target~., data = train, 
                         method = 'class',
                         control = rpart.control(cp = 0.05))

pred.test <- predict(DT_rpart_try, test, type = 'class')

prop.table(table(pred.test))

#cp = 0.055
DT_rpart_try <- rpart(target~., data = train, 
                         method = 'class',
                         control = rpart.control(cp = 0.055))

pred.test <- predict(DT_rpart_try, test, type = 'class')

prop.table(table(pred.test))

#cp = 0.00005
DT_rpart_try <- rpart(target~., data = train, 
                      method = 'class',
                      control = rpart.control(cp = 0.00005))

pred.test <- predict(DT_rpart_try, test, type = 'class')

prop.table(table(pred.test))

#-----------------------------------------------------------------#