#setwd('E:\\Batch46 - R\\Cute03')

#------------------------------------------------------------------#
#continuation of Cute
#run this after main file
#------------------------------------------------------------------#

#C_50, C_50_5trials : 0.24
#rpart : 0.23

#------------------------------------------------------------------#
#In this i'll be doing Decision Trees
#c5.0 and CART
#------------------------------------------------------------------#
#use trainsmote
library(caret)
library(tidyverse)

valsmote <- readRDS('valdata.rds')
testsmote <- readRDS('testdata.rds')
trainsmote_7 <- readRDS('trainSMOTE7.rds')

x.val <- valsmote %>% select(-target)
y.val <- valsmote %>% select(target) %>% as.matrix()

testsmote$cat_var_2[testsmote$cat_var_2 == "ot"]=NA
testsmote$cat_var_8[testsmote$cat_var_8 == "wl"]=NA
testsmote$cat_var_7[testsmote$cat_var_7 == "da"]=NA
testsmote$cat_var_7[testsmote$cat_var_7 == "ip"]=NA
testsmote$cat_var_7[testsmote$cat_var_7 == "no"]=NA
testsmote$cat_var_7[testsmote$cat_var_7 == "ye"]=NA

testsmote <- centralImputation(testsmote)

#-----------------------------------------------------------------#

library(C50)

DT.C50 <- C5.0(target~., data = trainsmote_7, rules = T)
summary(DT.C50)

train.pred <- predict(DT.C50, newdata = trainsmote_7, type = 'class')
val.pred <- predict(DT.C50, newdata = x.val, type = 'class')

test.pred <- predict(DT.C50, newdata = testsmote, type = 'class')

cnf.DTC50.train <- confusionMatrix(trainsmote_7$target, train.pred, positive = '1')
cnf.DTC50.val <- confusionMatrix(as.factor(y.val), val.pred, positive = '1')

final.C50 <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.C50, file = 'final_C50.csv', row.names = F)

#---------------------------------------------------------------#
#Removing above cols

train.temp <- trainsmote_7
val.temp <- valsmote
test.temp <- testsmote

train.temp$cat_var_2 <- NULL
train.temp$cat_var_7 <- NULL
train.temp$cat_var_8 <- NULL

val.temp$cat_var_2 <- NULL
val.temp$cat_var_7 <- NULL
val.temp$cat_var_8 <- NULL

test.temp$cat_var_2 <- NULL
test.temp$cat_var_7 <- NULL
test.temp$cat_var_8 <- NULL

DT.C50_remove_cols <- C5.0(target~., data = train.temp, rules = T)
summary(DT.C50_remove_cols)

train.pred <- predict(DT.C50_remove_cols, newdata = train.temp, type = 'class')
val.pred <- predict(DT.C50_remove_cols, newdata = val.temp, type = 'class')

test.pred <- predict(DT.C50_remove_cols, newdata = test.temp, type = 'class')

cnf.DTC50.train_remove_cols <- confusionMatrix(trainsmote_7$target, train.pred, positive = '1')
cnf.DTC50.val_remove_cols <- confusionMatrix(as.factor(y.val), val.pred, positive = '1')

final.C50_remove_cols <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.C50_remove_cols, file = 'final_C50.csv', row.names = F)

#-------------------------------------------------------------------#
#5 trials

DT.C50_5trial <- C5.0(target~., data = trainsmote_7, rules = T, trials = 5)

train.pred <- predict(DT.C50_5trial, newdata = trainsmote_7, type = 'class')
val.pred <- predict(DT.C50_5trial, newdata = x.val, type = 'class')

cnf.DTC50_5trial.train <- confusionMatrix(trainsmote_7$target, train.pred, positive = '1')
cnf.DTC50_5trial.val <- confusionMatrix(as.factor(y.val), val.pred, positive = '1')

test.pred <- predict(DT.C50_5trial, newdata = testsmote, type = 'class')

final.C50_5trial <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred)
write.csv(final.C50_5trial, file = 'final_C50_5trials.csv', row.names = F)

#-------------------------------------------------------------------#

#-------------------------------------------------------------------#

#rpart
library(rpart)
library(rpart.plot)

DT_rpart <- rpart(target~., data = trainsmote_7, method = 'class')
summary(DT_rpart)

plot(DT_rpart)
text(DT_rpart)

pred.train <- predict(DT_rpart, newdata = trainsmote_7, type = 'class')
pred.val <- predict(DT_rpart, newdata = x.val, type = 'class')

cnf.DTrpart.train <- confusionMatrix(trainsmote_7$target, pred.train)
cnf.DTrpart.val <- confusionMatrix(as.factor(y.val), pred.val)

pred.test <- predict(DT_rpart, testsmote, type = 'class')

prop.table(table(pred.test))

final.rpart <- cbind.data.frame('transaction_id'=test.id, 'target'=pred.test)
write.csv(final.rpart, file = 'final_rpart.csv', row.names = F)

#---------------------PARAMETER TUNING---------------------------#
#choose value of cp
DT_rpart_cp <- rpart(target~., data = trainsmote_7, 
                          method = 'class',
                          control = rpart.control(cp = 0.00001))
cp_val <- printcp(DT_rpart_cp)

#Take cp = 0.01 as we can see the pattern in xerror

DT_rpart_cp_opt <- rpart(target~., data = trainsmote_7,
                         method = 'class',
                         control = rpart.control(cp = 0.01))

pred.train.opt <- predict(DT_rpart_cp_opt, newdata = trainsmote_7,
                          type = 'class')
pred.val.opt <- predict(DT_rpart_cp_opt, newdata = x.val, type = 'class')

cnf.DTrpart_cp_01.train <- confusionMatrix(trainsmote_7$target, pred.train.opt, positive = '1')
cnf.DTrpart_cp_01.val <- confusionMatrix(as.factor(y.val), pred.val.opt, positive = '1')

# cp = 0.0001
DT_rpart_cp_opt <- rpart(target~., data = trainsmote_7,
                         method = 'class',
                         control = rpart.control(cp = 0.0001))

pred.train.opt <- predict(DT_rpart_cp_opt, newdata = trainsmote_7,
                          type = 'class')
pred.val.opt <- predict(DT_rpart_cp_opt, newdata = x.val, type = 'class')

cnf.DTrpart_cp_0001.train <- confusionMatrix(trainsmote_7$target, pred.train.opt, positive = '1')
cnf.DTrpart_cp_0001.val <- confusionMatrix(as.factor(y.val), pred.val.opt, positive = '1')
#----
# cp = 0.0002
DT_rpart_cp_opt <- rpart(target~., data = trainsmote_7,
                         method = 'class',
                         control = rpart.control(cp = 0.0002))

pred.train.opt <- predict(DT_rpart_cp_opt, newdata = trainsmote_7,
                          type = 'class')
pred.val.opt <- predict(DT_rpart_cp_opt, newdata = x.val, type = 'class')

cnf.DTrpart_cp_0002.train <- confusionMatrix(trainsmote_7$target, pred.train.opt, positive = '1')
cnf.DTrpart_cp_0002.val <- confusionMatrix(as.factor(y.val), pred.val.opt, positive = '1')

#----
# cp = 0.001
DT_rpart_cp_opt <- rpart(target~., data = trainsmote_7,
                         method = 'class',
                         control = rpart.control(cp = 0.001))

pred.train.opt <- predict(DT_rpart_cp_opt, newdata = trainsmote_7,
                          type = 'class')
pred.val.opt <- predict(DT_rpart_cp_opt, newdata = x.val, type = 'class')

cnf.DTrpart_cp_001.train <- confusionMatrix(trainsmote_7$target, pred.train.opt, positive = '1')
cnf.DTrpart_cp_001.val <- confusionMatrix(as.factor(y.val), pred.val.opt, positive = '1')
#----
# cp = 0.00012
DT_rpart_cp_opt <- rpart(target~., data = trainsmote_7,
                         method = 'class',
                         control = rpart.control(cp = 0.00012))

pred.train.opt <- predict(DT_rpart_cp_opt, newdata = trainsmote_7,
                          type = 'class')
pred.val.opt <- predict(DT_rpart_cp_opt, newdata = x.val, type = 'class')

cnf.DTrpart_cp_00012.train <- confusionMatrix(trainsmote_7$target, pred.train.opt, positive = '1')
cnf.DTrpart_cp_00012.val <- confusionMatrix(as.factor(y.val), pred.val.opt, positive = '1')
#----
# final cp = 0.00015
DT_rpart_cp_opt <- rpart(target~., data = trainsmote_7,
                         method = 'class',
                         control = rpart.control(cp = 0.00015))

pred.train.opt <- predict(DT_rpart_cp_opt, newdata = trainsmote_7,
                          type = 'class')
pred.val.opt <- predict(DT_rpart_cp_opt, newdata = x.val, type = 'class')

cnf.DTrpart_cp_00015.train <- confusionMatrix(trainsmote_7$target, pred.train.opt, positive = '1')
cnf.DTrpart_cp_00015.val <- confusionMatrix(as.factor(y.val), pred.val.opt, positive = '1')

#------------------------------------------------------------------#

plot(DT_rpart_cp_opt)
text(DT_rpart_cp_opt)

pred.test.opt <- predict(DT_rpart_cp_opt, newdata = testsmote, 
                         type = 'class')
prop.table(table(pred.test.opt))

final.rpart_opt <- cbind.data.frame('transaction_id'=test.id, 'target'=pred.test.opt)
write.csv(final.rpart_opt, file = 'final_rpart_opt.csv', row.names = F)
#-------------------------------------------------------------#
# Grid Search using Rpart 
#tc <- trainControl("cv",10)
#rpart.grid <- expand.grid(cp=seq(0.00004,0.00009, 0.000005))
#train.rpart <- train(target~., data=trainsmote_7, method="rpart",
#                     trControl=tc,tuneGrid=rpart.grid, type = 'class')

#rpart.plot(train.rpart$finalModel)
#text(train.rpart$finalModel)

#-------------------------------------------------------------#

#--------------------------------------------------------------#
#--------------------END OF DECISION TREES---------------------#