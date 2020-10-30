library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)

# Using the full data --------------------------------------------------------

data <- read.csv("BP_separated_with_H2H.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

data$head_to_head_record <- ifelse(is.na(data$head_to_head_record), 50, data$head_to_head_record)
data$head_to_head_record_court_surface <- ifelse(is.na(data$head_to_head_record_court_surface), 
                                                 50, data$head_to_head_record_court_surface)

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

library(randomForest)

set.seed(2020)
rf_tennis_all_data <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                           paste(colnames(data)[c(5:6, 58:225, 288:455)], collapse = "+"),
                                           sep = "")), data = train_data, 
                          ntree = 500, #no mtry argument, keep it default
                          importance = TRUE, 
                          do.trace = 10)

rf_tennis_all_data

plot(rf_tennis_all_data$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_all_data <- predict(rf_tennis_all_data, newdata = test_data) 
table(rf_pred_all_data, test_data$wl)
(rf_err_all_data <- mean(rf_pred_all_data != test_data$wl))

varImpPlot(rf_tennis_all_data, type = 2)

# Random Forest in Ranger ----------------------------------------------------

# library(caret) 
# 
# set.seed(1234)
# cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)
# 
# ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
# rf_grid <- expand.grid(mtry = c(2:7),
#                        splitrule = 'gini',
#                        min.node.size = c(1:5))
# 
# set.seed(2020)
# rf_tennis <- train(as.formula(paste(colnames(features)[4], "~",
#                                     paste(colnames(features)[c(5:6, 16:26)], collapse = "+"),
#                                     sep = "")), data = train_data, 
#                    method = 'ranger',
#                    num.trees = 250, 
#                    trControl = ctrl, 
#                    verbose = F, 
#                    tuneGrid = rf_grid)
# 
# rf_pred <- predict(rf_tennis, test_data)
# rf_cf <- confusionMatrix(rf_pred, test_data$wl)
# sum(diag(rf_cf$table))/sum(rf_cf$table)



# Using the feature set ------------------------------------------------------
features <- read.csv("features_with_H2H.csv")
features <- features[, -1]

features$wl <- as.factor(features$wl)
features$wl <- relevel(features$wl,"Player B")

features$head_to_head_record <- ifelse(is.na(features$head_to_head_record), 50, features$head_to_head_record)
features$head_to_head_record_court_surface <- ifelse(is.na(features$head_to_head_record_court_surface), 
                                                     50, features$head_to_head_record_court_surface)

train_data_features <- features[ind, ]
test_data_features <- features[-ind, ]

library(randomForest)

set.seed(2020)
rf_tennis_featues <- randomForest(as.formula(paste(colnames(features)[4], "~",
                                           paste(colnames(features)[c(16:98)], collapse = "+"),
                                           sep = "")), data = train_data_features, 
                          ntree = 500, #no mtry argument, keep it defualt
                          importance = TRUE, 
                          do.trace = 10)

rf_tennis_featues

plot(rf_tennis_featues$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_features <- predict(rf_tennis_featues, newdata = test_data_features) 
table(rf_pred_features, test_data_features$wl)
(rf_err <- mean(rf_pred_features != test_data_features$wl))

varImpPlot(rf_tennis_featues, type = 2)

# Random Forest in Ranger ----------------------------------------------------

library(caret) 

set.seed(2020)
cv_folds <- createFolds(train_data_features$wl, k = 5, returnTrain = TRUE)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
rf_grid <- expand.grid(mtry = c(2:7),
                       splitrule = 'gini',
                       min.node.size = c(1:5))

set.seed(2020)
rf_tennis_optim_features <- train(as.formula(paste(colnames(features)[4], "~",
                                    paste(colnames(features)[c(5:6, 16:98)], collapse = "+"),
                                    sep = "")), data = train_data_features, 
                   method = 'ranger',
                   num.trees = 250, 
                   trControl = ctrl, 
                   verbose = F, 
                   tuneGrid = rf_grid)

rf_pred_optim_features <- predict(rf_tennis_optim_features, test_data_features)
rf_cf_of <- confusionMatrix(rf_pred_optim_features, test_data_features$wl)
sum(diag(rf_cf_of$table))/sum(rf_cf_of$table)


