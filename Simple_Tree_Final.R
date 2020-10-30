library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)

# Using all the data ---------------------------------------------------------
data <- read.csv("BP_separated_with_H2H.csv")
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
tree_tennis_all_data <- tree(as.formula(paste(colnames(data)[4], "~",
                                    paste(colnames(data)[c(58:225, 288:455)], collapse = "+"),
                                    sep = "")), data = train_data, split = 'deviance', 
                   control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_all_data) 
tree_tennis_all_data
plot(tree_tennis_all_data)
text(tree_tennis_all_data, cex = 0.9)

yhat_data <- predict(tree_tennis_all_data,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_all_data <- cv.tree(tree_tennis_all_data, FUN=prune.misclass)

size <- cv.tennis_all_data$size
plot(size, cv.tennis_all_data$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_all_data$k[1] <- 0
alpha <- round(cv.tennis_all_data$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_all_data$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_all_data$dev)

# prune the tree back:
prune.tennis_all_data <- prune.misclass(tree_tennis_all_data, best = 6)
plot(prune.tennis_all_data)
text(prune.tennis_all_data, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_all_data <- predict(prune.tennis_all_data, test_data, type='class')
table(tree.pred_all_data, test_data$wl)
sum(diag(table(tree.pred_all_data, test_data$wl)))/length(test_data$wl)*100


# Using the feature set ------------------------------------------------------

features <- read.csv("features_with_H2H.csv")
features <- features[, -1]

features$wl <- as.factor(features$wl)
features$wl <- relevel(features$wl,"Player B")

features$head_to_head_record <- ifelse(is.na(features$head_to_head_record), 50, features$head_to_head_record)
features$head_to_head_record_court_surface <- ifelse(is.na(features$head_to_head_record_court_surface), 
                                                         50, features$head_to_head_record_court_surface)

train_data_features <- features[ind, ]
test_data_features <- features[-ind, ]

set.seed(2020)
tree_tennis_features <- tree(as.formula(paste(colnames(features)[4], "~",
                                              paste(colnames(features)[c(16:98)], collapse = "+"),
                                              sep = "")), data = train_data_features, split = 'deviance', 
                             control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_features) 
tree_tennis_features
plot(tree_tennis_features)
text(tree_tennis_features, cex = 0.9)

yhat_features <- predict(tree_tennis_features,  test_data_features, type = 'class')

(c_mat <- table(yhat_features, test_data_features$wl))          
sum(diag(c_mat))/nrow(test_data_features)*100                
1 - sum(diag(c_mat))/nrow(test_data_features)

# Prune that tree---------------------------------------
cv.tennis_features <- cv.tree(tree_tennis_features, FUN=prune.misclass)

size <- cv.tennis_features$size
plot(size, cv.tennis_features$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_features$k[1] <- 0
alpha <- round(cv.tennis_features$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_features$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_features$dev)

# prune the tree back:
prune.tennis_features <- prune.misclass(tree_tennis_features, best = 6)
plot(prune.tennis_features)
text(prune.tennis_features, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_features <- predict(prune.tennis_features, test_data_features, type='class')
table(tree.pred_features, test_data_features$wl)
sum(diag(table(tree.pred_features, test_data_features$wl)))/length(test_data_features$wl)*100



#loadhistory("Chapter3.Rhistory")