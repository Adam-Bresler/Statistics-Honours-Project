library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)

data <- read.csv("final_predictive_data.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B")

features <- data[,1:15]

# Using the rolling average --------------------------------------------------

servadv_Player_A_RA <- data[,97] - data[,308]
servadv_Player_B_RA <- data[,307] - data[,98]
servadv_overall_RA <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_RA <- data[,97]*data[,98]
completeness_Player_B_RA <- data[,307]*data[,308]

features <- cbind.data.frame(features, servadv_Player_A_RA, servadv_Player_B_RA, 
                             servadv_overall_RA, completeness_Player_A_RA, completeness_Player_B_RA)
#colnames(features)[21] <- "H2H"
#features <- features[complete.cases(features[, 16:21]),]
#, data[,437]

# Rolling Average by Court ---------------------------------------------------
servadv_Player_A_RA_BC <- data[,139] - data[,350]
servadv_Player_B_RA_BC <- data[,349] - data[,140]
servadv_overall_RA_BC <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_RA_BC <- data[,139]*data[,140]
completeness_Player_B_RA_BC <- data[,349]*data[,350]

features <- cbind.data.frame(features, servadv_Player_A_RA_BC, servadv_Player_B_RA_BC, 
                             servadv_overall_RA_BC, completeness_Player_A_RA_BC, completeness_Player_B_RA_BC)

# Weighted Rolling Average ---------------------------------------------------

servadv_Player_A_Weighted_RA <- data[,181] - data[,392]
servadv_Player_B_Weighted_RA <- data[,391] - data[,182]
servadv_overall_Weighted_RA <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_Weighted_RA <- data[,181]*data[,182]
completeness_Player_B_Weighted_RA <- data[,391]*data[,392]

features <- cbind.data.frame(features, servadv_Player_A_Weighted_RA, servadv_Player_B_Weighted_RA, 
                             servadv_overall_Weighted_RA, completeness_Player_A_Weighted_RA, completeness_Player_B_Weighted_RA)

# Weighted Rolling Average by Court ------------------------------------------

servadv_Player_A_Weighted_RA_BC <- data[,223] - data[,434]
servadv_Player_B_Weighted_RA_BC <- data[,433] - data[,224]
servadv_overall_Weighted_RA_BC <- servadv_Player_A_RA - servadv_Player_B_RA

completeness_Player_A_Weighted_RA_BC <- data[,223]*data[,224]
completeness_Player_B_Weighted_RA_BC <- data[,433]*data[,434]

features <- cbind.data.frame(features, servadv_Player_A_Weighted_RA_BC, servadv_Player_B_Weighted_RA_BC, 
                             servadv_overall_Weighted_RA_BC, completeness_Player_A_Weighted_RA_BC, 
                             completeness_Player_B_Weighted_RA_BC)

# Breakpoints ----------------------------------------------------------------



# Testing using a tree -------------------------------------------------------
ind <- 1:23658

train_data <- features[ind, ]
test_data <- features[-ind, ]

library(tree)

#Super basic, default everything
set.seed(2020)
tree_tennis<- tree(as.formula(paste(colnames(features)[4], "~",
                                    paste(colnames(features)[16:35], collapse = "+"),
                                    sep = "")), data = train_data, split = 'deviance')

summary(tree_tennis) 
tree_tennis
plot(tree_tennis)
text(tree_tennis, cex = 0.9)

yhat<- predict(tree_tennis,  test_data, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

#, control = tree.control(nobs=23658, mincut = 2, minsize = 4, mindev = 0.0001)

# Testing Using a GLM --------------------------------------------------------
set.seed(2020)
mod <- glm(as.formula(paste(colnames(features)[4], "~",
                            paste(colnames(features)[33], collapse = "+"),
                            sep = "")), data = train_data, family = binomial)
summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.5875821
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
