rm(list = ls())

setwd('E:\\Batch46 - R\\Cute03\\Again')
library(caret)
library(tidyverse)
library(DMwR)

#names(getModelInfo())
test.id <- readRDS('test_id.rds')
bank <- read.csv('train_new.csv', na.strings = '')
test <- read.csv('test_new.csv', na.strings = '')

#str(bank)
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

#Imputation
train <- centralImputation(train)
val <- centralImputation(val)
test <- centralImputation(test)

#---------------------
library(gbm)

fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
set.seed(33)
gbmFit1 <- train(target~ .,
                 data = train,
                 method = "gbm", 
                 trControl = fitControl)
