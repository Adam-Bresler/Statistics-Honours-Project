library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)

data <- dat

# Model fitting --------------------------------------------------------------

ind <- 1:23731

train <- predictive_dataset[ind, ]
test <- predictive_dataset[-ind, ]

set.seed(2020)
mod <- glm(wl ~ average_serve_rating + average_return_rating, data = train, family = binomial)
summary(mod)
plot(sort(predict(mod, type = 'response')), type = "l")

threshold <- 0.3
y.hat <- ifelse(predict(mod, newdata = test, type = 'response') > threshold, "Player A", "Player B") 

y.hat[which(is.na(y.hat))]

conf_matrix <- table(y.hat, test$wl)
conf_matrix

sum(diag(conf_matrix))/sum(conf_matrix)

sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])

# Decision Tree --------------------------------------------------------------
library(tree)

#Super basic, default everything
set.seed(2020)
tree_tennis<- tree(wl ~ average_serve_rating + average_return_rating, data = train, split = 'deviance')

summary(tree_tennis) 
tree_tennis
plot(tree_tennis)
text(tree_tennis, cex = 0.9)

yhat<- predict(tree_tennis,  test, type = 'class')

(c_mat <- table(yhat, test$wl))          
sum(diag(c_mat))/nrow(test)*100                
1 - sum(diag(c_mat))/nrow(test)

# Random Forest --------------------------------------------------------------
#Super basic, default everything

library(randomForest)

set.seed(2020)
rf_tennis <- randomForest(wl ~ average_serve_rating + average_return_rating, data = train, 
                          ntree = 1000, #no mtry argument, keep it defualt
                          importance = TRUE, 
                          do.trace = 100)

rf_tennis

plot(rf_tennis$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

rf_pred <- predict(rf_tennis, newdata = test) 
table(rf_pred, test$wl)
(rf_err <- mean(rf_pred != test$wl))

# Boosting -------------------------------------------------------------------
library(gbm)



ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(250, 500, 1000),
                        interaction.depth = c(1, 2),
                        shrinkage = c(0.1, 0.05, 0.01),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(wl ~ average_serve_rating + average_return_rating, data = train, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test)
gbm_cf <- confusionMatrix(gbm_pred, test$wl)
sum(diag(gbm_cf$table))/sum(gbm_cf$table)
