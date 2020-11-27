library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)
library(randomForest)

# Defining ranger grid -------------------------------------------------------

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
rf_grid <- expand.grid(mtry = c(2:7),
                       splitrule = 'gini',
                       min.node.size = c(1,3,5,10))

# Using the raw data ---------------------------------------------------------
# raw rolled -----------------------------------
data <- read.csv("raw_rolled.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
rf_tennis_raw_rolled <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                      paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_raw_rolled <- train(as.formula(paste(colnames(data)[4], "~",
                                    paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
                                    sep = "")), data = train_data,
                                    method = 'ranger',
                                    num.trees = 350,
                                    trControl = ctrl,
                                    verbose = F,
                                    tuneGrid = rf_grid)

rf_caret_pred_raw_rolled <- predict(rf_caret_tennis_raw_rolled, test_data)
rf_caref_cf_raw_rolled <- confusionMatrix(rf_caret_pred_raw_rolled, test_data$wl)
sum(diag(rf_caref_cf_raw_rolled$table))/sum(rf_caref_cf_raw_rolled$table)


# Raw Rolled BC --------------------------------
data <- read.csv("raw_rolled_bc.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
rf_tennis_raw_rolled_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                         paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_raw_rolled_bc <- train(as.formula(paste(colnames(data)[4], "~",
                                                     paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
                                                     sep = "")), data = train_data,
                                    method = 'ranger',
                                    num.trees = 350,
                                    trControl = ctrl,
                                    verbose = F,
                                    tuneGrid = rf_grid)

rf_caret_pred_raw_rolled_bc <- predict(rf_caret_tennis_raw_rolled_bc, test_data)
rf_caref_cf_raw_rolled_bc <- confusionMatrix(rf_caret_pred_raw_rolled_bc, test_data$wl)
sum(diag(rf_caref_cf_raw_rolled_bc$table))/sum(rf_caref_cf_raw_rolled_bc$table)


# raw weighted -----------------------------------
data <- read.csv("raw_weighted.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 


ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
rf_tennis_raw_weighted <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                        paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_raw_weighted <- train(as.formula(paste(colnames(data)[4], "~",
                                                          paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
                                                          sep = "")), data = train_data,
                                         method = 'ranger',
                                         num.trees = 350,
                                         trControl = ctrl,
                                         verbose = F,
                                         tuneGrid = rf_grid)

rf_caret_pred_raw_weighted <- predict(rf_caret_tennis_raw_weighted, test_data)
rf_caref_cf_raw_weighted <- confusionMatrix(rf_caret_pred_raw_weighted, test_data$wl)
sum(diag(rf_caref_cf_raw_weighted$table))/sum(rf_caref_cf_raw_weighted$table)

# Raw weighted BC --------------------------------
data <- read.csv("raw_weighted_bc.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]


set.seed(2020)
rf_tennis_raw_weighted_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                           paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_raw_weighted_bc <- train(as.formula(paste(colnames(data)[4], "~",
                                                        paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
                                                        sep = "")), data = train_data,
                                       method = 'ranger',
                                       num.trees = 350,
                                       trControl = ctrl,
                                       verbose = F,
                                       tuneGrid = rf_grid)

rf_caret_pred_raw_weighted_bc <- predict(rf_caret_tennis_raw_weighted_bc, test_data)
rf_caref_cf_raw_weighted_bc <- confusionMatrix(rf_caret_pred_raw_weighted_bc, test_data$wl)
sum(diag(rf_caref_cf_raw_weighted_bc$table))/sum(rf_caref_cf_raw_weighted_bc$table)


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

set.seed(2020)
rf_tennis_engineered_rolled <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                             paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_engineered_rolled <- train(as.formula(paste(colnames(data)[4], "~",
                                                     paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
                                                     sep = "")), data = train_data,
                                    method = 'ranger',
                                    num.trees = 350,
                                    trControl = ctrl,
                                    verbose = F,
                                    tuneGrid = rf_grid)

rf_caret_pred_engineered_rolled <- predict(rf_caret_tennis_engineered_rolled, test_data)
rf_caref_cf_engineered_rolled <- confusionMatrix(rf_caret_pred_engineered_rolled, test_data$wl)
sum(diag(rf_caref_cf_engineered_rolled$table))/sum(rf_caref_cf_engineered_rolled$table)

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

set.seed(2020)
rf_tennis_engineered_rolled_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                                paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_engineered_rolled_bc <- train(as.formula(paste(colnames(data)[4], "~",
                                                        paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
                                                        sep = "")), data = train_data,
                                       method = 'ranger',
                                       num.trees = 350,
                                       trControl = ctrl,
                                       verbose = F,
                                       tuneGrid = rf_grid)

rf_caret_pred_engineered_rolled_bc <- predict(rf_caret_tennis_engineered_rolled_bc, test_data)
rf_caref_cf_engineered_rolled_bc <- confusionMatrix(rf_caret_pred_engineered_rolled_bc, test_data$wl)
sum(diag(rf_caref_cf_engineered_rolled_bc$table))/sum(rf_caref_cf_engineered_rolled_bc$table)

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

set.seed(2020)
rf_tennis_engineered_weighted <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                               paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_engineered_weighted <- train(as.formula(paste(colnames(data)[4], "~",
                                                       paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
                                                       sep = "")), data = train_data,
                                      method = 'ranger',
                                      num.trees = 350,
                                      trControl = ctrl,
                                      verbose = F,
                                      tuneGrid = rf_grid)

rf_caret_pred_engineered_weighted <- predict(rf_caret_tennis_engineered_weighted, test_data)
rf_caref_cf_engineered_weighted <- confusionMatrix(rf_caret_pred_engineered_weighted, test_data$wl)
sum(diag(rf_caref_cf_engineered_weighted$table))/sum(rf_caref_cf_engineered_weighted$table)

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

set.seed(2020)
rf_tennis_engineered_weighted_bc <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                                                  paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
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

# Ranger
set.seed(2020)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

rf_caret_tennis_engineered_weighted_bc <- train(as.formula(paste(colnames(data)[4], "~",
                                                          paste(colnames(data)[c(5:6, 16:41)], collapse = "+"),
                                                          sep = "")), data = train_data,
                                         method = 'ranger',
                                         num.trees = 350,
                                         trControl = ctrl,
                                         verbose = F,
                                         tuneGrid = rf_grid)

rf_caret_pred_engineered_weighted_bc <- predict(rf_caret_tennis_engineered_weighted_bc, test_data)
rf_caref_cf_engineered_weighted_bc <- confusionMatrix(rf_caret_pred_engineered_weighted_bc, test_data$wl)
sum(diag(rf_caref_cf_engineered_weighted_bc$table))/sum(rf_caref_cf_engineered_weighted_bc$table)

dev.new()
plot(rf_tennis_raw_rolled$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error', col = "blue4")
lines(rf_tennis_raw_rolled_bc$err.rate[, 'OOB'], type = 's', col = "blueviolet")
lines(rf_tennis_raw_weighted$err.rate[, 'OOB'], type = 's', col = "cyan2")
lines(rf_tennis_raw_weighted_bc$err.rate[, 'OOB'], type = 's', col = "deepskyblue1")
legend("topright", legend=c("Historical Average", "Historical Average By Court", 
                            "Time Discounted Historical Average", "Time Discounted Historical Average By Court"),
       col=c("blue4", "blueviolet", "cyan2", "deepskyblue1"), lty = c(1,1,1,1),cex=0.8, lwd = c(3,3,3,3))
dev.off()

dev.new()
plot(rf_tennis_engineered_rolled$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', 
     ylab = 'OOB error', col = "green4", ylim = c(0.33,0.43))
lines(rf_tennis_engineered_rolled_bc$err.rate[, 'OOB'], type = 's', col = "violetred1")
lines(rf_tennis_engineered_weighted$err.rate[, 'OOB'], type = 's', col = "green")
lines(rf_tennis_engineered_weighted_bc$err.rate[, 'OOB'], type = 's', col = "goldenrod1")
legend("topright", legend=c("Historical Average", "Historical Average By Court", 
                            "Time Discounted Historical Average", "Time Discounted Historical Average By Court"),
       fill=c("green4", "violetred1", "green", "goldenrod1"))
dev.off()

