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

freq_created_RA <- data[,232] - data[,462]
freq_faced_RA <- data[,230] - data[,460]

saving_rate_RA <- data[,231] - data[,461]
conversion_rate_RA <- data[,233] - data[,463]

features <- cbind.data.frame(features, freq_created_RA, freq_faced_RA, 
                             saving_rate_RA, conversion_rate_RA)


# Rolling average by court ---------------------
freq_created_RA_BC <- data[,236] - data[,466]
freq_faced_RA_BC <- data[,234] - data[,464]

saving_rate_RA_BC <- data[,235] - data[,465]
conversion_rate_RA_BC <- data[,237] - data[,467]

features <- cbind.data.frame(features, freq_created_RA_BC, freq_faced_RA_BC, 
                             saving_rate_RA_BC, conversion_rate_RA_BC)


# Weighted Average -----------------------------
freq_created_WA <- data[,240] - data[,470]
freq_faced_WA <- data[,238] - data[,468]

saving_rate_WA <- data[,239] - data[,469]
conversion_rate_WA <- data[,241] - data[,471]

features <- cbind.data.frame(features, freq_created_WA, freq_faced_WA, 
                             saving_rate_WA, conversion_rate_WA)

# Weighted Average by court --------------------
freq_created_WA_BC <- data[,244] - data[,474]
freq_faced_WA_BC <- data[,242] - data[,472]

saving_rate_WA_BC <- data[,243] - data[,473]
conversion_rate_WA_BC <- data[,245] - data[,475]

features <- cbind.data.frame(features, freq_created_WA_BC, freq_faced_WA_BC, 
                             saving_rate_WA_BC, conversion_rate_WA_BC)

# Aces and double faults -----------------------------------------------------
features <- cbind.data.frame(features, data[,c(88, 89, 130, 131, 172, 173, 214, 215, 318, 319, 360, 361, 402, 403, 444, 445)])
colnames(features)[57:72] <- colnames(data)[c(88, 89, 130, 131, 172, 173, 214, 215, 318, 319, 360, 361, 402, 403, 444, 445)]

# Games won and sets won -----------------------------------------------------
features <- cbind.data.frame(features, data[,c(85, 86, 127, 128, 169, 170, 211, 212, 315, 316, 357, 358, 399, 400, 441, 442)])
colnames(features)[73:88] <- colnames(data)[c(85, 86, 127, 128, 169, 170, 211, 212, 315, 316, 357, 358, 399, 400, 441, 442)]

write.csv(features, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/features_no_H2H.csv")

# Head to head ---------------------------------------------------------------
features_h2h <- read.csv("features_no_H2H.csv")
features_h2h <- features_h2h[,-1]

features_h2h <- cbind.data.frame(features_h2h, data[c(476,477)])
colnames(features_h2h)[89:90] <- colnames(data)[c(476,477)]

features_h2h$head_to_head_record <- ifelse(is.na(features_h2h$head_to_head_record), 50, features_h2h$head_to_head_record)
features_h2h$head_to_head_record_court_surface <- ifelse(is.na(features_h2h$head_to_head_record_court_surface), 
                                                         50, features_h2h$head_to_head_record_court_surface)

write.csv(features_h2h, file = "C:/Users/bresl/Documents/GitHub/Statistics-Honours-Project/Data/features_with_H2H.csv")

# Testing using a tree -------------------------------------------------------
features <- bp_features
ind <- 1:23656

features$wl <- as.factor(features$wl)
features$wl <- relevel(features$wl,"Player B")

train_data <- features[ind, ]
test_data <- features[-ind, ]

library(tree)

#Super basic, default everything
set.seed(2020)
tree_tennis<- tree(as.formula(paste(colnames(features)[4], "~",
                                    paste(colnames(features)[c(19:26, 30:37)], collapse = "+"),
                                    sep = "")), data = train_data, split = 'deviance')

summary(tree_tennis) 
tree_tennis
plot(tree_tennis)
text(tree_tennis, cex = 0.9)

yhat<- predict(tree_tennis,  test_data, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)



# Testing Using a GLM --------------------------------------------------------
set.seed(2020)
mod <- glm(as.formula(paste(colnames(features)[4], "~",
                            paste(colnames(features)[c(19:26, 30:37)], collapse = "+"),
                            sep = "")), data = train_data, family = "binomial")

#mod <- glm(wl ~ servadv_overall_RA + BP_adv_overall_RA, data = train_data, family = 'binomial')

summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.588398  
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
