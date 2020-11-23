library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)

# Using the raw data --------------------------------------------------------
# raw rolled -----------------------------------
data <- read.csv("raw_rolled.csv")
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
rf_tennis_raw_rolled <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                      paste(colnames(data)[c(5:6, 16:75)], collapse = "+"),
                                                      sep = "")), data = train_data, 
                                     ntree = 500, #no mtry argument, keep it default
                                     importance = TRUE, 
                                     do.trace = 10)

rf_tennis_raw_rolled

plot(rf_tennis_raw_rolled$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_raw_rolled <- predict(rf_tennis_raw_rolled, newdata = test_data) 
table(rf_pred_raw_rolled, test_data$wl)
(rf_err_raw_rolled <- mean(rf_pred_raw_rolled != test_data$wl))

varImpPlot(rf_tennis_raw_rolled, type = 2)

# Raw Rolled BC --------------------------------
data <- read.csv("raw_rolled_bc.csv")
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
rf_tennis_raw_rolled_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                         paste(colnames(data)[c(5:6, 58:225, 288:455)], collapse = "+"),
                                                         sep = "")), data = train_data, 
                                        ntree = 500, #no mtry argument, keep it default
                                        importance = TRUE, 
                                        do.trace = 10)

rf_tennis_raw_rolled_bc

plot(rf_tennis_raw_rolled_bc$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_raw_rolled_bc <- predict(rf_tennis_raw_rolled_bc, newdata = test_data) 
table(rf_pred_raw_rolled_bc, test_data$wl)
(rf_err_raw_rolled_bc <- mean(rf_pred_raw_rolled_bc != test_data$wl))

varImpPlot(rf_tennis_raw_rolled_bc, type = 2)

# raw weighted -----------------------------------
data <- read.csv("raw_weighted.csv")
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
rf_tennis_raw_weighted <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                        paste(colnames(data)[c(5:6, 16:75)], collapse = "+"),
                                                        sep = "")), data = train_data, 
                                       ntree = 500, #no mtry argument, keep it default
                                       importance = TRUE, 
                                       do.trace = 10)

rf_tennis_raw_weighted

plot(rf_tennis_raw_weighted$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_raw_weighted <- predict(rf_tennis_raw_weighted, newdata = test_data) 
table(rf_pred_raw_weighted, test_data$wl)
(rf_err_raw_weighted <- mean(rf_pred_raw_weighted != test_data$wl))

varImpPlot(rf_tennis_raw_weighted, type = 2)

# Raw weighted BC --------------------------------
data <- read.csv("raw_weighted_bc.csv")
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
rf_tennis_raw_weighted_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                           paste(colnames(data)[c(5:6, 16:75)], collapse = "+"),
                                                           sep = "")), data = train_data, 
                                          ntree = 500, #no mtry argument, keep it default
                                          importance = TRUE, 
                                          do.trace = 10)

rf_tennis_raw_weighted_bc

plot(rf_tennis_raw_weighted_bc$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_raw_weighted_bc <- predict(rf_tennis_raw_weighted_bc, newdata = test_data) 
table(rf_pred_raw_weighted_bc, test_data$wl)
(rf_err_raw_weighted_bc <- mean(rf_pred_raw_weighted_bc != test_data$wl))

varImpPlot(rf_tennis_raw_weighted_bc, type = 2)

# Using the engineered data --------------------------------------------------------
# engineered rolled -----------------------------------
data <- read.csv("engineered_rolled.csv")
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
rf_tennis_engineered_rolled <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                             paste(colnames(data)[c(5:6, 16:42)], collapse = "+"),
                                                             sep = "")), data = train_data, 
                                            ntree = 500, #no mtry argument, keep it default
                                            importance = TRUE, 
                                            do.trace = 10)

rf_tennis_engineered_rolled

plot(rf_tennis_engineered_rolled$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_engineered_rolled <- predict(rf_tennis_engineered_rolled, newdata = test_data) 
table(rf_pred_engineered_rolled, test_data$wl)
(rf_err_engineered_rolled <- mean(rf_pred_engineered_rolled != test_data$wl))

varImpPlot(rf_tennis_engineered_rolled, type = 2)

# engineered Rolled BC --------------------------------
data <- read.csv("engineered_rolled_bc.csv")
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
rf_tennis_engineered_rolled_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                                paste(colnames(data)[c(5:6, 16:42)], collapse = "+"),
                                                                sep = "")), data = train_data, 
                                               ntree = 500, #no mtry argument, keep it default
                                               importance = TRUE, 
                                               do.trace = 10)

rf_tennis_engineered_rolled_bc

plot(rf_tennis_engineered_rolled_bc$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_engineered_rolled_bc <- predict(rf_tennis_engineered_rolled_bc, newdata = test_data) 
table(rf_pred_engineered_rolled_bc, test_data$wl)
(rf_err_engineered_rolled_bc <- mean(rf_pred_engineered_rolled_bc != test_data$wl))

varImpPlot(rf_tennis_engineered_rolled_bc, type = 2)

# engineered weighted -----------------------------------
data <- read.csv("engineered_weighted.csv")
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
rf_tennis_engineered_weighted <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                               paste(colnames(data)[c(5:6, 16:42)], collapse = "+"),
                                                               sep = "")), data = train_data, 
                                              ntree = 500, #no mtry argument, keep it default
                                              importance = TRUE, 
                                              do.trace = 10)

rf_tennis_engineered_weighted

plot(rf_tennis_engineered_weighted$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_engineered_weighted <- predict(rf_tennis_engineered_weighted, newdata = test_data) 
table(rf_pred_engineered_weighted, test_data$wl)
(rf_err_engineered_weighted <- mean(rf_pred_engineered_weighted != test_data$wl))

varImpPlot(rf_tennis_engineered_weighted, type = 2)

# engineered weighted BC --------------------------------
data <- read.csv("engineered_weighted_bc.csv")
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
rf_tennis_engineered_weighted_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                                  paste(colnames(data)[c(5:6, 16:42)], collapse = "+"),
                                                                  sep = "")), data = train_data, 
                                                 ntree = 500, #no mtry argument, keep it default
                                                 importance = TRUE, 
                                                 do.trace = 10)

rf_tennis_engineered_weighted_bc

plot(rf_tennis_engineered_weighted_bc$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred_engineered_weighted_bc <- predict(rf_tennis_engineered_weighted_bc, newdata = test_data) 
table(rf_pred_engineered_weighted_bc, test_data$wl)
(rf_err_engineered_weighted_bc <- mean(rf_pred_engineered_weighted_bc != test_data$wl))

varImpPlot(rf_tennis_engineered_weighted_bc, type = 2)



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





# Random Forest in Ranger ----------------------------------------------------
# library(caret) 
# 
# set.seed(2020)
# cv_folds <- createFolds(train_data_features$wl, k = 5, returnTrain = TRUE)
# 
# ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
# rf_grid <- expand.grid(mtry = c(2:7),
#                        splitrule = 'gini',
#                        min.node.size = c(1:5))
# 
# set.seed(2020)
# rf_tennis_optim_features <- train(as.formula(paste(colnames(features)[4], "~",
#                                     paste(colnames(features)[c(5:6, 16:98)], collapse = "+"),
#                                     sep = "")), data = train_data_features, 
#                    method = 'ranger',
#                    num.trees = 250, 
#                    trControl = ctrl, 
#                    verbose = F, 
#                    tuneGrid = rf_grid)
# 
# rf_pred_optim_features <- predict(rf_tennis_optim_features, test_data_features)
# rf_cf_of <- confusionMatrix(rf_pred_optim_features, test_data_features$wl)
# sum(diag(rf_cf_of$table))/sum(rf_cf_of$table)
# 
# 
# xtable(colnames(data))
# 
# players <- read.csv("players.csv")


