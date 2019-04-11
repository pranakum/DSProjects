#0.45
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
set.seed(125)
random.rows <- createDataPartition(bankdata$target, p = 0.1, list = F)
bank.new <- bankdata[random.rows,]
#--------------------------------------------------------------#
#Train-Val split
set.seed(125)
trainrows <- createDataPartition(bank.new$target, p = 0.7, list = F)
train <- bank.new[trainrows, ]
val <- bank.new[-trainrows,]

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
#---------------------
library(ada)

trctrl <- trainControl(method = "cv", number = 5)

grid <- expand.grid(iter = 50,maxdepth=7,nu=0.2)
set.seed(3233)
Ada_Model <- train(target~., data=train, method = "ada",
                   trControl=trctrl,
                   tuneGrid = grid)

train.pred.ada <- predict(Ada_Model, train)
val.pred.ada <- predict(Ada_Model, val)
test.pred.ada <- predict(Ada_Model, test)

prop.table(table(test.pred.ada))

final.ada <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred.ada)
write.csv(final.ada, file = 'final_ada.csv', row.names = F)


#---------------------------------------------------------------#
#for stacking
saveRDS(train.pred.ada, 'train_pred_ada.rds')
saveRDS(val.pred.ada, 'val_pred_ada.rds')
saveRDS(test.pred.ada, 'test_pred_ada.rds')
#---------------------------------------------------------------#