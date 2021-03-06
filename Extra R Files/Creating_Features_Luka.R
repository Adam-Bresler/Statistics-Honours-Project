library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)

data <- read.csv("BP_separated_with_H2H.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B")

features <- data[,1:15]

# duration feature -----------------------------------------------------------
RA_duration_diff <- data[,which(colnames(data) == "Player_A_rolling_average_duration")]-data[,which(colnames(data) == "Player_B_rolling_average_duration")]
Weighted_RA_duration_diff <-data[,which(colnames(data) == "Player_A_weighted_rolling_average_duration")]-data[,which(colnames(data) == "Player_B_weighted_rolling_average_duration")]
RA_BC_duration_diff <-data[,which(colnames(data) == "Player_A_rolling_average_by_court_duration")]-data[,which(colnames(data) == "Player_B_rolling_average_by_court_duration")]
Weighted_RA_BC_duration_diff <-data[,which(colnames(data) == "Player_A_weighted_by_court_duration")]-data[,which(colnames(data) == "Player_B_weighted_by_court_duration")]

features <- cbind.data.frame(features,RA_duration_diff,Weighted_RA_duration_diff,RA_BC_duration_diff,Weighted_RA_BC_duration_diff)


# seeding feature ------------------------------------------------------------
seeding_diff <- data[,which(colnames(data) == "Player_A_seed")]-data[,which(colnames(data) == "Player_B_seed")]
  
features <- cbind.data.frame(features,seeding_diff)

# Using the rolling average --------------------------------------------------

servadv_Player_A_RA <- data[,which(colnames(data) == "Player_A_rolling_average_percent_service_points_won")] - 
  data[,which(colnames(data) == "Player_B_rolling_average_percent_return_points_won")]
servadv_Player_B_RA <- data[,which(colnames(data) == "Player_B_rolling_average_percent_service_points_won")] - 
  data[,which(colnames(data) == "Player_A_rolling_average_percent_return_points_won")]
servadv_overall_RA <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_RA <- data[,97]*data[,98]
completeness_Player_B_RA <- data[,327]*data[,328]

features <- cbind.data.frame(features, servadv_Player_A_RA, servadv_Player_B_RA, 
                             servadv_overall_RA, completeness_Player_A_RA, completeness_Player_B_RA)
#colnames(features)[21] <- "H2H"
#features <- features[complete.cases(features[, 16:21]),]
#, data[,437]

# Rolling Average by Court ---------------------------------------------------
servadv_Player_A_RA_BC <- data[,139] - data[,370]
servadv_Player_B_RA_BC <- data[,369] - data[,140]
servadv_overall_RA_BC <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_RA_BC <- data[,139]*data[,140]
completeness_Player_B_RA_BC <- data[,369]*data[,370]

features <- cbind.data.frame(features, servadv_Player_A_RA_BC, servadv_Player_B_RA_BC, 
                             servadv_overall_RA_BC, completeness_Player_A_RA_BC, completeness_Player_B_RA_BC)

# Weighted Rolling Average ---------------------------------------------------

servadv_Player_A_Weighted_RA <- data[,181] - data[,412]
servadv_Player_B_Weighted_RA <- data[,411] - data[,182]
servadv_overall_Weighted_RA <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_Weighted_RA <- data[,181]*data[,182]
completeness_Player_B_Weighted_RA <- data[,411]*data[,412]

features <- cbind.data.frame(features, servadv_Player_A_Weighted_RA, servadv_Player_B_Weighted_RA, 
                             servadv_overall_Weighted_RA, completeness_Player_A_Weighted_RA, completeness_Player_B_Weighted_RA)

# Weighted Rolling Average by Court ------------------------------------------

servadv_Player_A_Weighted_RA_BC <- data[,223] - data[,454]
servadv_Player_B_Weighted_RA_BC <- data[,453] - data[,224]
servadv_overall_Weighted_RA_BC <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_Weighted_RA_BC <- data[,223]*data[,224]
completeness_Player_B_Weighted_RA_BC <- data[,453]*data[,454]

features <- cbind.data.frame(features, servadv_Player_A_Weighted_RA_BC, servadv_Player_B_Weighted_RA_BC, 
                             servadv_overall_Weighted_RA_BC, completeness_Player_A_Weighted_RA_BC, 
                             completeness_Player_B_Weighted_RA_BC)

# Breakpoints ----------------------------------------------------------------

# Rolling average ------------------------------
BP_conversion_adv_Player_A_RA <- data[,231] - data[,463]
BP_conversion_adv_Player_B_RA <- data[,461] - data[,233]
BP_conversion_adv_overall_RA <- BP_conversion_adv_Player_A_RA - BP_conversion_adv_Player_B_RA

BP_frequency_adv_Player_A_RA <- data[,230] - data[,462]
BP_frequency_adv_Player_B_RA <- data[,460] - data[,232]
BP_frequency_adv_overall_RA <- BP_frequency_adv_Player_A_RA - BP_frequency_adv_Player_B_RA

features <- cbind.data.frame(features, BP_conversion_adv_Player_A_RA, BP_conversion_adv_Player_B_RA, 
                             BP_conversion_adv_overall_RA, BP_frequency_adv_Player_A_RA, BP_frequency_adv_Player_B_RA, 
                             BP_frequency_adv_overall_RA)


# Rolling average by court ---------------------
BP_conversion_adv_Player_A_RA_BC <- data[,235] - data[,467]
BP_conversion_adv_Player_B_RA_BC <- data[,465] - data[,237]
BP_conversion_adv_overall_RA_BC <- BP_conversion_adv_Player_A_RA_BC - BP_conversion_adv_Player_B_RA_BC

BP_frequency_adv_Player_A_RA_BC <- data[,234] - data[,466]
BP_frequency_adv_Player_B_RA_BC <- data[,464] - data[,236]
BP_frequency_adv_overall_RA_BC <- BP_frequency_adv_Player_A_RA_BC - BP_frequency_adv_Player_B_RA_BC

features <- cbind.data.frame(features, BP_conversion_adv_Player_A_RA_BC, BP_conversion_adv_Player_B_RA_BC, 
                             BP_conversion_adv_overall_RA_BC, BP_frequency_adv_Player_A_RA_BC, BP_frequency_adv_Player_B_RA_BC, 
                             BP_frequency_adv_overall_RA_BC)

# Weighted Average -----------------------------
BP_conversion_adv_Player_A_weighted_RA <- data[,239] - data[,471]
BP_conversion_adv_Player_B_weighted_RA <- data[,469] - data[,241]
BP_conversion_adv_overall_weighted_RA <- BP_conversion_adv_Player_A_weighted_RA - BP_conversion_adv_Player_B_weighted_RA

BP_frequency_adv_Player_A_weighted_RA <- data[,238] - data[,470]
BP_frequency_adv_Player_B_weighted_RA <- data[,468] - data[,240]
BP_frequency_adv_overall_weighted_RA <- BP_frequency_adv_Player_A_weighted_RA - BP_frequency_adv_Player_B_weighted_RA

features <- cbind.data.frame(features, BP_conversion_adv_Player_A_weighted_RA, BP_conversion_adv_Player_B_weighted_RA, 
                             BP_conversion_adv_overall_weighted_RA, BP_frequency_adv_Player_A_weighted_RA, BP_frequency_adv_Player_B_weighted_RA, 
                             BP_frequency_adv_overall_weighted_RA)

# Weighted Average by court --------------------
BP_conversion_adv_Player_A_weighted_RA_BC <- data[,243] - data[,475]
BP_conversion_adv_Player_B_weighted_RA_BC <- data[,473] - data[,245]
BP_conversion_adv_overall_weighted_RA_BC <- BP_conversion_adv_Player_A_weighted_RA_BC - BP_conversion_adv_Player_B_weighted_RA_BC

BP_frequency_adv_Player_A_weighted_RA_BC <- data[,242] - data[,474]
BP_frequency_adv_Player_B_weighted_RA_BC <- data[,472] - data[,244]
BP_frequency_adv_overall_weighted_RA_BC <- BP_frequency_adv_Player_A_weighted_RA_BC - BP_frequency_adv_Player_B_weighted_RA_BC

features <- cbind.data.frame(features, BP_conversion_adv_Player_A_weighted_RA_BC, BP_conversion_adv_Player_B_weighted_RA_BC, 
                             BP_conversion_adv_overall_weighted_RA_BC, BP_frequency_adv_Player_A_weighted_RA_BC, BP_frequency_adv_Player_B_weighted_RA_BC, 
                             BP_frequency_adv_overall_weighted_RA_BC)

# Aces and double faults -----------------------------------------------------
features <- cbind.data.frame(features, data[,c(88, 89, 130, 131, 172, 173, 214, 215, 318, 319, 360, 361, 402, 403, 444, 445)])
colnames(features)[65:80] <- colnames(data)[c(88, 89, 130, 131, 172, 173, 214, 215, 318, 319, 360, 361, 402, 403, 444, 445)]

# Games won and sets won -----------------------------------------------------
features <- cbind.data.frame(features, data[,c(85, 86, 127, 128, 169, 170, 211, 212, 315, 316, 357, 358, 399, 400, 441, 442)])
colnames(features)[81:96] <- colnames(data)[c(85, 86, 127, 128, 169, 170, 211, 212, 315, 316, 357, 358, 399, 400, 441, 442)]

#write.csv(features, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/features_no_H2H.csv")

# Head to head ---------------------------------------------------------------
features_h2h <- read.csv("features_no_H2H.csv")
features_h2h <- features_h2h[,-1]

features_h2h <- cbind.data.frame(features_h2h, data[c(476,477)])
colnames(features_h2h)[97:98] <- colnames(data)[c(476,477)]

features_h2h$head_to_head_record <- ifelse(is.na(features_h2h$head_to_head_record), 50, features_h2h$head_to_head_record)
features_h2h$head_to_head_record_court_surface <- ifelse(is.na(features_h2h$head_to_head_record_court_surface), 
                                                         50, features_h2h$head_to_head_record_court_surface)

write.csv(features_h2h, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/features_with_H2H.csv")


# 0.6142684 when removing NA's for H2H by court....

# Playing around with features -----------------------------------------------

features <- read.csv("features_with_H2H.csv")
# features <- read.csv("BP_separated_with_H2H.csv")
features <- features[,-1]


ind <- 1:23656

features$wl <- as.factor(features$wl)
features$wl <- relevel(features$wl,"Player B")

#features <- na.omit(features)
train_data <- features[ind, ]
#train_data <- na.omit(train_data)
test_data <- features[-ind, ]
#test_data <- na.omit(test_data)


# stratification by court surface --------------------------------------------

train_clay <-train_data[which(train_data$tournament_surface=="Clay"),] 
test_clay <- test_data[which(test_data$tournament_surface=="Clay"),]

train_grass <-train_data[which(train_data$tournament_surface=="Grass"),] 
test_grass <- test_data[which(test_data$tournament_surface=="Grass"),]

train_hard_court <-train_data[which(train_data$tournament_surface=="Hard"),] 
test_hard_court <- test_data[which(test_data$tournament_surface=="Hard"),]

# column 38

# Testing Using a GLM --------------------------------------------------------

mod <- glm(as.formula(paste(colnames(features)[4], "~",
                            paste(colnames(features)[c(16:98)], collapse = "+"),
                            sep = "")), data = train_data, family = "binomial")

summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.6186408
y.hat <- ifelse(predict(mod, newdata = test_data, type = 'response') > threshold, "Player A", "Player B") 

y.hat[which(is.na(y.hat))]
y.hat <- as.factor(y.hat)
y.hat <- relevel(y.hat,"Player B")
conf_matrix <- table(y.hat, test_data$wl)
conf_matrix

sum(diag(conf_matrix))/sum(conf_matrix)

sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])

library(ROCR)
prediction.object <- prediction(fitted(mod), labels = train_data$wl,label.ordering = c("Player B","Player A"))

roc <-  performance(prediction.object,"tpr","fpr") 
par(mfrow = c(1,1))
plot(roc)
abline(a = 0, b = 1) 

cutoffs <- data.frame(cut=roc@alpha.values[[1]], tpr=roc@y.values[[1]], spec = 1 - roc@x.values[[1]],
                      fpr=roc@x.values[[1]])
d <- cutoffs[,2] + cutoffs[,3]
cutoffs[which.max(d),]


# Boosting -------------------------------------------------------------------
library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(500, 1000),
                        interaction.depth = c(1,2,5),
                        shrinkage = c(0.1, 0.01,0.001),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(as.formula(paste(colnames(features)[4], "~",
                                     paste(colnames(features)[c(16:98)], collapse = "+"),
                                     sep = "")), data = train_data, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test_data)
gbm_cf <- confusionMatrix(gbm_pred, test_data$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)




































# grass
mod <- glm(as.formula(paste(colnames(features)[4], "~",
                            paste(colnames(features)[c(38,61,64,71,79)], collapse = "+"),
                            sep = "")), data = train_grass, family = "binomial")

summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.55
y.hat <- ifelse(predict(mod, newdata = test_grass, type = 'response') > threshold, "Player A", "Player B") 

y.hat[which(is.na(y.hat))]
y.hat <- as.factor(y.hat)
y.hat <- relevel(y.hat,"Player B")
conf_matrix <- table(y.hat, test_grass$wl)
conf_matrix

sum(diag(conf_matrix))/sum(conf_matrix)

sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])

library(ROCR)
prediction.object <- prediction(fitted(mod), labels = train_grass$wl,label.ordering = c("Player B","Player A"))

roc <-  performance(prediction.object,"tpr","fpr") 
par(mfrow = c(1,1))
plot(roc)
abline(a = 0, b = 1) 

cutoffs <- data.frame(cut=roc@alpha.values[[1]], tpr=roc@y.values[[1]], spec = 1 - roc@x.values[[1]],
                      fpr=roc@x.values[[1]])
d <- cutoffs[,2] + cutoffs[,3]
cutoffs[which.max(d),]


# hard court
mod <- glm(as.formula(paste(colnames(features)[4], "~",
                            paste(colnames(features)[c(38,61,64,71,79)], collapse = "+"),
                            sep = "")), data = train_hard_court, family = "binomial")

summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.52
y.hat <- ifelse(predict(mod, newdata = test_hard_court, type = 'response') > threshold, "Player A", "Player B") 

y.hat[which(is.na(y.hat))]
y.hat <- as.factor(y.hat)
y.hat <- relevel(y.hat,"Player B")
conf_matrix <- table(y.hat, test_hard_court$wl)
conf_matrix

sum(diag(conf_matrix))/sum(conf_matrix)

sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])

library(ROCR)
prediction.object <- prediction(fitted(mod), labels = train_hard_court$wl,label.ordering = c("Player B","Player A"))

roc <-  performance(prediction.object,"tpr","fpr") 
par(mfrow = c(1,1))
plot(roc)
abline(a = 0, b = 1) 

cutoffs <- data.frame(cut=roc@alpha.values[[1]], tpr=roc@y.values[[1]], spec = 1 - roc@x.values[[1]],
                      fpr=roc@x.values[[1]])
d <- cutoffs[,2] + cutoffs[,3]
cutoffs[which.max(d),]


# clay
mod <- glm(as.formula(paste(colnames(features)[4], "~",
                            paste(colnames(features)[c(38,61,64,71,79)], collapse = "+"),
                            sep = "")), data = train_clay, family = "binomial")

summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.55 
y.hat <- ifelse(predict(mod, newdata = test_clay, type = 'response') > threshold, "Player A", "Player B") 

y.hat[which(is.na(y.hat))]
y.hat <- as.factor(y.hat)
y.hat <- relevel(y.hat,"Player B")
conf_matrix <- table(y.hat, test_clay$wl)
conf_matrix

sum(diag(conf_matrix))/sum(conf_matrix)

sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])

library(ROCR)
prediction.object <- prediction(fitted(mod), labels = train_clay$wl,label.ordering = c("Player B","Player A"))

roc <-  performance(prediction.object,"tpr","fpr") 
par(mfrow = c(1,1))
plot(roc)
abline(a = 0, b = 1) 

cutoffs <- data.frame(cut=roc@alpha.values[[1]], tpr=roc@y.values[[1]], spec = 1 - roc@x.values[[1]],
                      fpr=roc@x.values[[1]])
d <- cutoffs[,2] + cutoffs[,3]
cutoffs[which.max(d),]
















# gbm grass
library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(500, 1000),
                        interaction.depth = c(1,2,3,4,5),
                        shrinkage = c(0.1, 0.01,0.001),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(as.formula(paste(colnames(features)[4], "~",
                                     paste(colnames(features)[c(38,61,64,71,79)], collapse = "+"),
                                     sep = "")), data = train_grass, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test_grass)
gbm_cf <- confusionMatrix(gbm_pred, test_grass$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)


# gbm hard court
library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(500, 1000),
                        interaction.depth = c(1,2,3,4,5),
                        shrinkage = c(0.1, 0.01,0.001),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(as.formula(paste(colnames(features)[4], "~",
                                     paste(colnames(features)[c(38,61,64,71,79)], collapse = "+"),
                                     sep = "")), data = train_hard_court, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test_hard_court)
gbm_cf <- confusionMatrix(gbm_pred, test_hard_court$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)


# gbm clay
library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(500, 1000),
                        interaction.depth = c(1,2,3,4,5),
                        shrinkage = c(0.1, 0.01,0.001),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(as.formula(paste(colnames(features)[4], "~",
                                     paste(colnames(features)[c(38,61,64,71,79)], collapse = "+"),
                                     sep = "")), data = train_clay, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test_clay)
gbm_cf <- confusionMatrix(gbm_pred, test_clay$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)






















# Run with all the raw features

data <- read.csv("BP_separated_with_H2H.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B")

data$head_to_head_record <- ifelse(is.na(data$head_to_head_record), 50, data$head_to_head_record)
data$head_to_head_record_court_surface <- ifelse(is.na(data$head_to_head_record_court_surface), 
                                                         50, data$head_to_head_record_court_surface)

ind <- 1:23656

#features <- na.omit(features)
train_data2 <- data[ind, ]
test_data2 <- data[-ind, ]

# Testing Using a GLM --------------------------------------------------------

mod <- glm(as.formula(paste(colnames(data)[4], "~",
                            paste(colnames(data)[c(5,6,58:225,288:455)], collapse = "+"),
                            sep = "")), data = train_data2, family = "binomial")

summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.5804463
y.hat <- ifelse(predict(mod, newdata = test_data2, type = 'response') > threshold, "Player A", "Player B") 

y.hat[which(is.na(y.hat))]
y.hat <- as.factor(y.hat)
y.hat <- relevel(y.hat,"Player B")
conf_matrix <- table(y.hat, test_data2$wl)
conf_matrix

sum(diag(conf_matrix))/sum(conf_matrix)

sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])

library(ROCR)
prediction.object <- prediction(fitted(mod), labels = train_data2$wl,label.ordering = c("Player B","Player A"))

roc <-  performance(prediction.object,"tpr","fpr") 
par(mfrow = c(1,1))
plot(roc)
abline(a = 0, b = 1) 

cutoffs <- data.frame(cut=roc@alpha.values[[1]], tpr=roc@y.values[[1]], spec = 1 - roc@x.values[[1]],
                      fpr=roc@x.values[[1]])
d <- cutoffs[,2] + cutoffs[,3]
cutoffs[which.max(d),]


# Boosting -------------------------------------------------------------------
library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(500),
                        interaction.depth = c(1,2),
                        shrinkage = c(0.01),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(as.formula(paste(colnames(data)[4], "~",
                                     paste(colnames(data)[c(5,6,58:225,288:455)], collapse = "+"),
                                     sep = "")), data = train_data2, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)



gbm_pred <- predict(gbm_tennis, test_data2)
gbm_cf <- confusionMatrix(gbm_pred, test_data2$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)




