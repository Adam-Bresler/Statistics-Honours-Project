# load relevant packages
library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)
library(ROCR)
library(gbm)


# GBM's-----------------------------------------------------------------------


# Raw Feature sets -----------------------------------------------------------

# Raw rolled -----------------------------------------------------------------

# read in data and make response variable a factor
raw_rolled<- read.csv("raw_rolled.csv")
raw_rolled<- raw_rolled[,-1]
raw_rolled$wl <- as.factor(raw_rolled$wl)
raw_rolled$wl <- relevel(raw_rolled$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_rr<- raw_rolled[ind, ]
test_data_rr<- raw_rolled[-ind, ]

# set hyperparameter options for grid search
ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(500,750,1000),
                        interaction.depth = c(1,5),
                        shrinkage = c(0.1,0.001),
                        n.minobsinnode = 1)

# run GBM model with grid search
set.seed(2020)
gbm_rr <- train(as.formula(paste(colnames(raw_rolled)[4], "~",
                                     paste(colnames(raw_rolled)[c(16:71)], collapse = "+"),
                                     sep = "")), data = train_data_rr, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_rr <- predict(gbm_rr, test_data_rr)
gbm_cf_rr <- confusionMatrix(gbm_pred_rr, test_data_rr$wl)

# classification rate
sum(diag(gbm_cf_rr$table))/sum(gbm_cf_rr$table)

# best hyperparameter combination 
gbm_rr$bestTune

# Raw rolled by court --------------------------------------------------------

# read in data and make response variable a factor
raw_rolled_bc<- read.csv("raw_rolled_bc.csv")
raw_rolled_bc<- raw_rolled_bc[,-1]
raw_rolled_bc$wl <- as.factor(raw_rolled_bc$wl)
raw_rolled_bc$wl <- relevel(raw_rolled_bc$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_rr_bc<- raw_rolled_bc[ind, ]
test_data_rr_bc<- raw_rolled_bc[-ind, ]

# run GBM model with grid search
set.seed(2020)
gbm_rr_bc <- train(as.formula(paste(colnames(raw_rolled_bc)[4], "~",
                                 paste(colnames(raw_rolled_bc)[c(16:71)], collapse = "+"),
                                 sep = "")), data = train_data_rr_bc, 
                method = 'gbm', 
                distribution = 'bernoulli', 
                trControl = ctrl, 
                verbose = F, 
                tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_rr_bc <- predict(gbm_rr_bc, test_data_rr_bc)
gbm_cf_rr_bc <- confusionMatrix(gbm_pred_rr_bc, test_data_rr_bc$wl)

# classification rate
sum(diag(gbm_cf_rr_bc$table))/sum(gbm_cf_rr_bc$table)

# best hyperparameter combination 
gbm_rr_bc$bestTune

# Raw weighted ---------------------------------------------------------------

# read in data and make response variable a factor
raw_weighted<- read.csv("raw_weighted.csv")
raw_weighted<- raw_weighted[,-1]
raw_weighted$wl <- as.factor(raw_weighted$wl)
raw_weighted$wl <- relevel(raw_weighted$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_rw<- raw_weighted[ind, ]
test_data_rw<- raw_weighted[-ind, ]

# run GBM model with grid search
set.seed(2020)
gbm_rw <- train(as.formula(paste(colnames(raw_weighted)[4], "~",
                                    paste(colnames(raw_weighted)[c(16:71)], collapse = "+"),
                                    sep = "")), data = train_data_rw, 
                   method = 'gbm', 
                   distribution = 'bernoulli', 
                   trControl = ctrl, 
                   verbose = F, 
                   tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_rw <- predict(gbm_rw, test_data_rw)
gbm_cf_rw <- confusionMatrix(gbm_pred_rw, test_data_rw$wl)

# classification rate
sum(diag(gbm_cf_rw$table))/sum(gbm_cf_rw$table)

# best hyperparameter combination 
gbm_rw$bestTune

# Raw weighted by court ------------------------------------------------------

# read in data and make response variable a factor
raw_weighted_bc<- read.csv("raw_weighted_bc.csv")
raw_weighted_bc<- raw_weighted_bc[,-1]
raw_weighted_bc$wl <- as.factor(raw_weighted_bc$wl)
raw_weighted_bc$wl <- relevel(raw_weighted_bc$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_rw_bc<- raw_weighted_bc[ind, ]
test_data_rw_bc<- raw_weighted_bc[-ind, ]

# run GBM model with grid search
set.seed(2020)
gbm_rw_bc <- train(as.formula(paste(colnames(raw_weighted_bc)[4], "~",
                                 paste(colnames(raw_weighted_bc)[c(16:71)], collapse = "+"),
                                 sep = "")), data = train_data_rw_bc, 
                method = 'gbm', 
                distribution = 'bernoulli', 
                trControl = ctrl, 
                verbose = F, 
                tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_rw_bc <- predict(gbm_rw_bc, test_data_rw_bc)
gbm_cf_rw_bc <- confusionMatrix(gbm_pred_rw_bc, test_data_rw_bc$wl)

# classification rate
sum(diag(gbm_cf_rw_bc$table))/sum(gbm_cf_rw_bc$table)

# best hyperparameter combination 
gbm_rw_bc$bestTune 


# Engineered Feature sets-----------------------------------------------------

# Engineered rolled ----------------------------------------------------------

# read in data and make response variable a factor
engineered_rolled<- read.csv("engineered_rolled.csv")
engineered_rolled<- engineered_rolled[,-1]
engineered_rolled$wl <- as.factor(engineered_rolled$wl)
engineered_rolled$wl <- relevel(engineered_rolled$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_er<- engineered_rolled[ind, ]
test_data_er<- engineered_rolled[-ind, ]

# run GBM model with grid search
set.seed(2020)
gbm_er <- train(as.formula(paste(colnames(engineered_rolled)[4], "~",
                                 paste(colnames(engineered_rolled)[c(16:39)], collapse = "+"),
                                 sep = "")), data = train_data_er, 
                method = 'gbm', 
                distribution = 'bernoulli', 
                trControl = ctrl, 
                verbose = F, 
                tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_er <- predict(gbm_er, test_data_er)
gbm_cf_er <- confusionMatrix(gbm_pred_er, test_data_er$wl)

# classification rate
sum(diag(gbm_cf_er$table))/sum(gbm_cf_er$table)

# best hyperparameter combination 
gbm_er$bestTune

# Engineered rolled by court -------------------------------------------------

# read in data and make response variable a factor
engineered_rolled_bc<- read.csv("engineered_rolled_bc.csv")
engineered_rolled_bc<- engineered_rolled_bc[,-1]
engineered_rolled_bc$wl <- as.factor(engineered_rolled_bc$wl)
engineered_rolled_bc$wl <- relevel(engineered_rolled_bc$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_er_bc<- engineered_rolled_bc[ind, ]
test_data_er_bc<- engineered_rolled_bc[-ind, ]

# run GBM model with grid search
set.seed(2020)
gbm_er_bc <- train(as.formula(paste(colnames(engineered_rolled_bc)[4], "~",
                                 paste(colnames(engineered_rolled_bc)[c(16:39)], collapse = "+"),
                                 sep = "")), data = train_data_er_bc, 
                method = 'gbm', 
                distribution = 'bernoulli', 
                trControl = ctrl, 
                verbose = F, 
                tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_er_bc <- predict(gbm_er_bc, test_data_er_bc)
gbm_cf_er_bc <- confusionMatrix(gbm_pred_er_bc, test_data_er_bc$wl)

# classification rate
sum(diag(gbm_cf_er_bc$table))/sum(gbm_cf_er_bc$table)

# best hyperparameter combination 
gbm_er_bc$bestTune

# Engineered weighted --------------------------------------------------------

# read in data and make response variable a factor
engineered_weighted<- read.csv("engineered_weighted.csv")
engineered_weighted<- engineered_weighted[,-1]
engineered_weighted$wl <- as.factor(engineered_weighted$wl)
engineered_weighted$wl <- relevel(engineered_weighted$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_ew<- engineered_weighted[ind, ]
test_data_ew<- engineered_weighted[-ind, ]

# run GBM model with grid search
set.seed(2020)
gbm_ew <- train(as.formula(paste(colnames(engineered_weighted)[4], "~",
                                    paste(colnames(engineered_weighted)[c(16:39)], collapse = "+"),
                                    sep = "")), data = train_data_ew, 
                   method = 'gbm', 
                   distribution = 'bernoulli', 
                   trControl = ctrl, 
                   verbose = F, 
                   tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_ew <- predict(gbm_ew, test_data_ew)
gbm_cf_ew <- confusionMatrix(gbm_pred_ew, test_data_ew$wl)

# classification rate
sum(diag(gbm_cf_ew$table))/sum(gbm_cf_ew$table)

# best hyperparameter combination 
gbm_ew$bestTune

# Engineered weighted by court------------------------------------------------

# read in data and make response variable a factor
engineered_weighted_bc<- read.csv("engineered_weighted_bc.csv")
engineered_weighted_bc<- engineered_weighted_bc[,-1]
engineered_weighted_bc$wl <- as.factor(engineered_weighted_bc$wl)
engineered_weighted_bc$wl <- relevel(engineered_weighted_bc$wl,"Player B")

# split data into training and test set
ind <- 1:23656
train_data_ew_bc<- engineered_weighted_bc[ind, ]
test_data_ew_bc<- engineered_weighted_bc[-ind, ]

# run GBM model with grid search
set.seed(2020)
gbm_ew_bc <- train(as.formula(paste(colnames(engineered_weighted_bc)[4], "~",
                                 paste(colnames(engineered_weighted_bc)[c(16:39)], collapse = "+"),
                                 sep = "")), data = train_data_ew_bc, 
                method = 'gbm', 
                distribution = 'bernoulli', 
                trControl = ctrl, 
                verbose = F, 
                tuneGrid = gbm_grid)

# get predictions for test set
gbm_pred_ew_bc <- predict(gbm_ew_bc, test_data_ew_bc)
gbm_cf_ew_bc <- confusionMatrix(gbm_pred_ew_bc, test_data_ew_bc$wl)

# classification rate
sum(diag(gbm_cf_ew_bc$table))/sum(gbm_cf_ew_bc$table)

# best hyperparameter combination 
gbm_ew_bc$bestTune


# Variable importance plots --------------------------------------------------

# best model : engineered feature set that is weighted 
dev.new()
plot(varImp(gbm_ew))
dev.off()


# raw feature sets
plot(varImp(gbm_rr))
plot(varImp(gbm_rr_bc))
plot(varImp(gbm_rw))
plot(varImp(gbm_rw_bc))

# engineered feature sets
plot(varImp(gbm_er))
plot(varImp(gbm_er_bc))
plot(varImp(gbm_ew))
plot(varImp(gbm_ew_bc))
