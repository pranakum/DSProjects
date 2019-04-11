#Naive Bayes theorem

rm(list = ls())

library(e1071)
library(tidyverse)
library(caret)
library(DMwR)

test.id <- readRDS('test_id.rds')
train <- readRDS('trainsmote9.rds')
test <- readRDS('testdata.rds')
val <- readRDS('valdata.rds')
library(e1071)
library(tidyverse)
x.val <- val %>% select(-target)
y.val <- val %>% select(target) %>% as.matrix

model.NB <- naiveBayes(target~., data = train)
train.pred.nb <- predict(model.NB, train$target)
val.pred.nb <- predict(model.NB, x.val)

#cnf.NB.train <- confusionMatrix(train$target, train.pred.nb, positive = '1')
#cnf.NB.val <- confusionMatrix(as.factor(y.val), val.pred.nb, positive = '1')

test.pred.nb <- predict(model.NB, test)

final.NB <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred.nb)
write.csv(final.NB, file = 'final_NB.csv', row.names = F)

#---------------------------------------------------------------#
#for stacking
saveRDS(train.pred.nb, 'train_pred_nb.rds')
saveRDS(val.pred.nb, 'val_pred_nb.rds')
saveRDS(test.pred.nb, 'test_pred_nb.rds')
#---------------------------------------------------------------#
