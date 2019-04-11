#SVM
#0.31

rm(list = ls())

setwd('E:\\Batch46 - R\\Cute03\\Again')

library(tidyverse)
library(caret)
library(DMwR)

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
#str(bankdata)

factor_cols_test <- names(test[,26:ncol(test)])
df_test <- test %>%
  dplyr::select(factor_cols_test) %>%
  lapply(factor) %>%
  as.data.frame()
test <- test %>% dplyr::select(-factor_cols_test) %>% cbind(df_test)
#str(test)
#--------------------------------------------------------------#
#decreasing bank data rows
set.seed(125)
random.rows <- createDataPartition(bankdata$target, p = 0.05, list = F)
bank.new <- bankdata[random.rows,]
#--------------------------------------------------------------#
#Train-Val split
set.seed(125)
trainrows <- createDataPartition(bank.new$target, p = 0.7, list = F)
train <- bank.new[trainrows, ]
val <- bank.new[-trainrows,]

#Imputation
train <- centralImputation(train)
val <- centralImputation(val)
test <- centralImputation(test)

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
y.train=train$target
x.val = predict(dummies, newdata = val)
y.val = val$target

test$target = 0

test.dummy <- predict(dummies, newdata = test)

test$target <- NULL

#---------------------------------------------------------------#
library(e1071)

model.svm <- svm(x = x.train, y = y.train, type = "C-classification",
                 kernel = "radial", cost = 10, gamma = 0.1)
summary(model.svm)

pred_train = predict(model.svm, x.train) # x is all the input variables
pred_val = predict(model.svm,x.val)

# Build Confusion matrix
confusionMatrix(y.train,pred_train,,positive="1")
confusionMatrix(y.val,pred_val,positive="1")

pred.test <- predict(model.svm, test.dummy)

final.svm <- cbind.data.frame('transaction_id'=test.id, 'target'=pred.test)
write.csv(final.svm, file = 'final_svm.csv', row.names = F)
