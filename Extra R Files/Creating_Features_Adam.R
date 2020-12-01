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

#write.csv(features_h2h, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/features_with_H2H.csv")

# Reading in the final features data set -------------------------------------
features <- read.csv("features_with_H2H.csv")
features <- features[, -1]

# features <- features[,c(1:15, )]
# 
# #features <- features[,c(1:15, 19, 20, 38, 61, 64, 79, 80, 95:98)]
# #features <- data[,c(1:15, 230:245, 460:475)]
# 
# hard <- features %>% filter(features$tournament_surface=="Hard")
# clay <- features %>% filter(features$tournament_surface=="Clay")
# grass <- features %>% filter(features$tournament_surface=="Grass")
# 
# # Testing using a tree -------------------------------------------------------
# # Hard -----------------------------------------
# features <- hard
# ind <- 1:13494
# 
# # Clay -----------------------------------------
# features <- clay
# ind <- 1:7357
# 
# # Grass ----------------------------------------
# features <- grass
# ind <- 1:2886

# Actual tree ----------------------------------------------------------------
features$wl <- as.factor(features$wl)
features$wl <- relevel(features$wl,"Player B")
#features$wl <- make.names(features$wl)

ind <- 1:23656

train_data <- features[ind, ]
test_data <- features[-ind, ]

library(tree)

#Super basic, default everything
set.seed(2020)
tree_tennis<- tree(as.formula(paste(colnames(features)[4], "~",
                                    paste(colnames(features)[c(5:6, 16:26)], collapse = "+"),
                                    sep = "")), data = train_data, split = 'deviance', 
                   control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis) 
tree_tennis
plot(tree_tennis)
text(tree_tennis, cex = 0.9)

yhat<- predict(tree_tennis,  test_data, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis <- cv.tree(tree_tennis, FUN=prune.misclass)
# size = number of terminal nodes
# k = alpha, the tuning parameter (large = fewer terminal nodes)
# dev = cross validation error rate (in this case, not deviance, despite name)

size <- cv.tennis$size
plot(size, cv.tennis$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis$k[1] <- 0
alpha <- round(cv.tennis$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis$dev, substitute(leaves, list(leaves = size)), cex = 0.9)


# prune the tree back:
prune.tennis <- prune.misclass(tree_tennis, best = 3)
plot(prune.tennis)
text(prune.tennis, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred <- predict(prune.tennis, test_data, type='class')
table(tree.pred, test_data$wl)
sum(diag(table(tree.pred, test_data$wl)))/length(test_data$wl)*100



# Testing using a Random Forest ----------------------------------------------
library(randomForest)

set.seed(2020)
rf_tennis <- randomForest(as.formula(paste(colnames(features)[4], "~",
                                           paste(colnames(features)[c(16:26)], collapse = "+"),
                                           sep = "")), data = train_data, 
                          ntree = 500, #no mtry argument, keep it defualt
                          importance = TRUE, 
                          do.trace = 10)

rf_tennis

plot(rf_tennis$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred <- predict(rf_tennis, newdata = test_data) 
table(rf_pred, test_data$wl)
(rf_err <- mean(rf_pred != test_data$wl))

varImpPlot(rf_tennis, type = 2)

# Random Forest in Ranger ----------------------------------------------------

library(ranger)

library(caret) 

set.seed(1234)
cv_folds <- createFolds(train_data$wl, k = 5, returnTrain = TRUE)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
rf_grid <- expand.grid(mtry = c(2:7),
                      splitrule = 'gini',
                      min.node.size = c(1:5))

set.seed(2020)
rf_tennis <- train(as.formula(paste(colnames(features)[4], "~",
                                     paste(colnames(features)[c(5:6, 16:26)], collapse = "+"),
                                     sep = "")), data = train_data, 
                    method = 'ranger',
                    num.trees = 250, 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = rf_grid)

rf_pred <- predict(rf_tennis, test_data)
rf_cf <- confusionMatrix(rf_pred, test_data$wl)
sum(diag(rf_cf$table))/sum(rf_cf$table)



