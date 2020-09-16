library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)

data <- read.csv("all_differences_no_custom_features.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
# Model fitting --------------------------------------------------------------

ind <- 1:23658

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
mod <- glm(as.formula(paste(colnames(data)[4], "~",
                            paste(colnames(data)[88:99], collapse = "+"),
                            sep = "")), data = train_data, family = binomial)
summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.3
y.hat <- ifelse(predict(mod, newdata = test_data, type = 'response') > threshold, "Player A", "Player B") 

y.hat[which(is.na(y.hat))]

conf_matrix <- table(y.hat, test_data$wl)
conf_matrix

sum(diag(conf_matrix))/sum(conf_matrix)

sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])

# Decision Tree --------------------------------------------------------------
library(tree)

#Super basic, default everything
set.seed(2020)
tree_tennis<- tree(as.formula(paste(colnames(data)[4], "~",
                                    paste(colnames(data)[88:99], collapse = "+"),
                                    sep = "")), data = train_data, split = 'deviance')

summary(tree_tennis) 
tree_tennis
plot(tree_tennis)
text(tree_tennis, cex = 0.9)

yhat<- predict(tree_tennis,  test_data, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Random Forest --------------------------------------------------------------
#Super basic, default everything

library(randomForest)

set.seed(2020)
rf_tennis <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                          paste(colnames(data)[88:99], collapse = "+"),
                                          sep = "")), data = train_data, 
                          ntree = 1000, #no mtry argument, keep it defualt
                          importance = TRUE, 
                          do.trace = 100)

rf_tennis

plot(rf_tennis$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred <- predict(rf_tennis, newdata = test_data) 
table(rf_pred, test_data$wl)
(rf_err <- mean(rf_pred != test_data$wl))

# Boosting -------------------------------------------------------------------
library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(250, 500, 1000),
                        interaction.depth = c(1, 2),
                        shrinkage = c(0.1, 0.05, 0.01),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(as.formula(paste(colnames(data)[4], "~",
                                          paste(colnames(data)[88:99], collapse = "+"),
                                          sep = "")), data = train_data, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test_data)
gbm_cf <- confusionMatrix(gbm_pred, test_data$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)
