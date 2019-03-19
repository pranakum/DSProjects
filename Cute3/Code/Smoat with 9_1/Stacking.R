#0.3

rm(list = ls())
setwd('E:\\Batch46 - R\\Cute03\\Again\\Stacking')

test.id <- readRDS('test_id.rds')

train.dtc50 <- readRDS('train_pred_dtc50.rds')
train.dtc50.5trial <- readRDS('train_pred_dtc50_5trial.rds')
train.rf <- readRDS('train_pred_rf.rds')
train.rpart <- readRDS('train_pred_rpart.rds')
train.rpart_opt <- readRDS('train_pred_rpart_opt.rds')

val.dtc50 <- readRDS('val_pred_dtc50.rds')
val.dtc50.5trial <- readRDS('val_pred_dtc50_5trial.rds')
val.rf <- readRDS('val_pred_rf.rds')
val.rpart <- readRDS('val_pred_rpart.rds')
val.rpart_opt <- readRDS('val_pred_rpart_opt.rds')

test.dtc50 <- readRDS('test_pred_dtc50.rds')
test.dtc50.5trial <- readRDS('test_pred_dtc50_5trial.rds')
test.rf <- readRDS('test_pred_rf.rds')
test.rpart <- readRDS('test_pred_rpart.rds')
test.rpart_opt <- readRDS('test_pred_rpart_opt.rds')

#Target Variables
train <- readRDS('train_stack.rds')
train.target <- train$target %>% as.factor
val <- readRDS('valdata.rds')
val.target <- val$target %>% as.factor

#---------------------------------------------------------------------#
#cbind data

train.input <- cbind.data.frame('C50'=train.dtc50,
                    'C50_trial'=train.dtc50.5trial, 
                    'rf'=train.rf,
                    'rpart'=train.rpart, 
                    'rpart_opt'=train.rpart_opt)

val.input <- cbind.data.frame('C50'=val.dtc50,
                              'C50_trial'=val.dtc50.5trial, 
                              'rf'=val.rf,
                              'rpart'=val.rpart, 
                              'rpart_opt'=val.rpart_opt)

test.input <- cbind.data.frame('C50'=test.dtc50,
                               'C50_trial'=test.dtc50.5trial, 
                               'rf'=test.rf,
                               'rpart'=test.rpart, 
                               'rpart_opt'=test.rpart_opt)


train.input$target <- train.target
val.input$target <- val.target

str(train.input)

train.numerical <- sapply(train.input[, !(names(train.input) %in% "target")], 
                        function(x) as.numeric(as.character(x))) %>% as.data.frame
cor(train.numerical)

val.numerical <- sapply(val.input[, !(names(val.input) %in% "target")], 
                        function(x) as.numeric(as.character(x))) %>% as.data.frame

test.numerical <- sapply(test.input, 
                         function(x) as.numeric(as.character(x))) %>% as.data.frame
#Highly cor-related

#Now, since the outputs of the various models are extremely correlated 
#let's use PCA to reduce the dimensionality of the dataset

pca <- prcomp(train.numerical, scale = F)
summary(pca)

#Transform the data into the principal components using the 
#predict() fucntion and keep only 3 of the original components
#98.4
train.pca <- predict(pca, train.numerical)[,1:3]
val.pca <- predict(pca, val.numerical)[,1:3]
test.pca <- predict(pca, test.numerical)[,1:3] %>% as.data.frame

#Now, add those columns to the target variable and 
#convert it to a data frame
train <- data.frame(train.pca, target = train.input$target)
val <- data.frame(val.pca, target = val.input$target)

#We will be building a logistic regression on the dataset to 
#predict the final target variable

stacked_model <- glm(target~. , data = train, family = "binomial")
summary(stacked_model)

# Convert the target variable into a factor
train$target <- as.factor(train$target)
val$target <- as.factor(val$target)

#Predict values
train.pred.stack.probs <- predict(stacked_model, train)
val.pred.stack.probs <- predict(stacked_model, val[,setdiff(names(val), 'target')]) %>% as.matrix

test.pred.stack.probs <- predict(stacked_model, test.pca) 

train.pred.stack <- ifelse(train.pred.stack.probs > 0.5, 1, 0)
val.pred.stack <- ifelse(val.pred.stack.probs > 0.5, 1, 0)
test.pred.stack <- ifelse(test.pred.stack.probs > 0.5, 1, 0)

confusionMatrix(train$target, as.factor(train.pred.stack), positive = '1')
confusionMatrix(val$target, as.factor(val.pred.stack), positive = '1')

prop.table(table(test.pred.stack))

final.stack <- cbind.data.frame('transaction_id'=test.id, 'target'=test.pred.stack)
write.csv(final.stack, file = 'final_stack.csv', row.names = F)
