library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)

data <- read.csv("final_predictive_data.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
# Model fitting --------------------------------------------------------------

ind <- 1:23658

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
mod <- glm(as.formula(paste(colnames(data)[4], "~",
                            paste(colnames(data)[c(5:6, 58:225, 268:435)], collapse = "+"),
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
                                    paste(colnames(data)[c(5:6, 58:225, 268:435)], collapse = "+"),
                                    sep = "")), data = train_data, split = 'deviance', 
                   control = tree.control(nobs=23658, mincut = 2, minsize = 4, mindev = 0.0001))

summary(tree_tennis) 
tree_tennis
plot(tree_tennis)
text(tree_tennis, cex = 0.9)

yhat<- predict(tree_tennis,  test_data, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

yhat[which(is.na(yhat))]

cv.tennis <- cv.tree(tree_tennis, FUN=prune.misclass)
size <- cv.tennis$size
plot(size, cv.tennis$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis$k[1] <- 0
alpha <- round(cv.tennis$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis$dev, substitute(leaves, list(leaves = size)), cex = 0.9)

prune.tree <- prune.misclass(tree_tennis, best = 7)
plot(prune.tree)
text(prune.tree, pretty=0, cex=0.8)
# Random Forest --------------------------------------------------------------
#Super basic, default everything

library(randomForest)

set.seed(2020)
rf_tennis <- randomForest(as.formula(paste(colnames(data)[4], "~",
                                          paste(colnames(data)[c(5:6, 58:225, 268:435)], collapse = "+"),
                                          sep = "")), data = train_data, 
                          ntree = 200, #no mtry argument, keep it defualt
                          importance = TRUE, 
                          do.trace = 10)

rf_tennis

plot(rf_tennis$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred <- predict(rf_tennis, newdata = test_data) 
table(rf_pred, test_data$wl)
(rf_err <- mean(rf_pred != test_data$wl))

# Boosting -------------------------------------------------------------------
library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(50, 100, 200),
                        interaction.depth = c(1, 2),
                        shrinkage = c(0.1, 0.05),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(as.formula(paste(colnames(data)[4], "~",
                                          paste(colnames(data)[c(5:6, 58:225, 268:435)], collapse = "+"),
                                          sep = "")), data = train_data, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test_data)
gbm_cf <- confusionMatrix(gbm_pred, test_data$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)
