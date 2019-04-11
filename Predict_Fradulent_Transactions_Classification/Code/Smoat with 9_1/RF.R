#Random Forest

rm(list = ls())
setwd('E:\\Batch46 - R\\Cute03\\Again')

library(tidyverse)
library(caret)
library(DMwR)

test.id <- readRDS('test_id.rds')
train <- readRDS('trainsmote9.rds')
test <- readRDS('testdata.rds')
val <- readRDS('valdata.rds')

set.seed(123)
rand.rows <- createDataPartition(train$target, p = 0.05, list = F)
train <- train[rand.rows,]

x.val <- val %>% select(-target)
y.val <- val %>% select(target) %>% as.matrix

x.train <- train %>% select(-target)

##############Building Random Forest###############################################

library(randomForest)

model_rf <- randomForest(target ~ . , train,ntree = 70,mtry = 5)

#importance from the built model
importance(model_rf)
varImpPlot(model_rf)
getTree(model_rf, labelVar = T, k = 1)

# Predict on the train data
train.pred.rf <- predict(model_rf)
confusionMatrix(train$target, train.pred.rf, positive = '1')


# Store predictions from the model
val.pred.rf <- predict(model_rf, x.val)
confusionMatrix(as.factor(y.val), val.pred.rf, positive = '1')


x.test <- rbind(x.train[1, ] , test)
x.test <- x.test[-1,]

test.pred.rf <- predict(model_rf, x.test)

prop.table(table(test.pred.rf))

final.RF <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred.rf)
write.csv(final.RF, file = 'final_RF.csv', row.names = F)

#0.21 Score

####Building  randomforest using caret

#control <- trainControl(method="cv", number=5)
#set.seed(123)
#tunegrid <- expand.grid(mtry=c(1:5))
#rf_gridsearch <- train(target ~ ., data=train, method = "rf",
#                       trControl=control,
#                       tuneGrid = tunegrid)


# Predict on the train data
#preds_train_rf1 <- predict(rf_gridsearch)
#confusionMatrix(train$target, preds_train_rf1, positive = '1')


# Store predictions from the model
#preds_rf1 <- predict(rf_gridsearch, x.val)

#confusionMatrix(val$target, preds_rf1, positive = '1')

#---------------------------------------------------------------#
#for stacking
saveRDS(train.pred.rf, 'train_pred_rf.rds')
saveRDS(val.pred.rf, 'val_pred_rf.rds')
saveRDS(test.pred.rf, 'test_pred_rf.rds')
#---------------------------------------------------------------#