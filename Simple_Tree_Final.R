library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)
library(tree)

# Using raw data -------------------------------------------------------------
#Raw Rolled ------------------------------------
data <- read.csv("raw_rolled.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_raw_rolled <- tree(as.formula(paste(colnames(data)[4], "~",
                                    paste(colnames(data)[c(5:6, 16:71)], collapse = "+"),
                                    sep = "")), data = train_data, split = 'deviance', 
                   control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_raw_rolled) 
tree_tennis_raw_rolled
plot(tree_tennis_raw_rolled)
text(tree_tennis_raw_rolled, cex = 0.9)

yhat_data <- predict(tree_tennis_raw_rolled,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_raw_rolled <- cv.tree(tree_tennis_raw_rolled, FUN=prune.misclass)

size <- cv.tennis_raw_rolled$size
plot(size, cv.tennis_raw_rolled$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_raw_rolled$k[1] <- 0
alpha <- round(cv.tennis_raw_rolled$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_raw_rolled$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_raw_rolled$dev)

# prune the tree back:
prune.tennis_raw_rolled <- prune.misclass(tree_tennis_raw_rolled, best = 3)
plot(prune.tennis_raw_rolled)
text(prune.tennis_raw_rolled, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_raw_rolled <- predict(prune.tennis_raw_rolled, test_data, type='class')
table(tree.pred_raw_rolled, test_data$wl)
sum(diag(table(tree.pred_raw_rolled, test_data$wl)))/length(test_data$wl)*100

#Raw Rolled BC ---------------------------------
data <- read.csv("raw_rolled_bc.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_raw_rolled_bc <- tree(as.formula(paste(colnames(data)[4], "~",
                                                paste(colnames(data)[c(5:6, 16:75)], collapse = "+"),
                                                sep = "")), data = train_data, split = 'deviance', 
                               control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_raw_rolled_bc) 
tree_tennis_raw_rolled_bc
plot(tree_tennis_raw_rolled_bc)
text(tree_tennis_raw_rolled_bc, cex = 0.9)

yhat_data <- predict(tree_tennis_raw_rolled_bc,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_raw_rolled_bc <- cv.tree(tree_tennis_raw_rolled_bc, FUN=prune.misclass)

size <- cv.tennis_raw_rolled_bc$size
plot(size, cv.tennis_raw_rolled_bc$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_raw_rolled_bc$k[1] <- 0
alpha <- round(cv.tennis_raw_rolled_bc$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_raw_rolled_bc$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_raw_rolled_bc$dev)

# prune the tree back:
prune.tennis_raw_rolled_bc <- prune.misclass(tree_tennis_raw_rolled_bc, best = 4)
plot(prune.tennis_raw_rolled_bc)
text(prune.tennis_raw_rolled_bc, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_raw_rolled_bc <- predict(prune.tennis_raw_rolled_bc, test_data, type='class')
table(tree.pred_raw_rolled_bc, test_data$wl)
sum(diag(table(tree.pred_raw_rolled_bc, test_data$wl)))/length(test_data$wl)*100



#Raw weighted ------------------------------------
data <- read.csv("raw_weighted.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_raw_weighted <- tree(as.formula(paste(colnames(data)[4], "~",
                                                paste(colnames(data)[c(5:6, 16:75)], collapse = "+"),
                                                sep = "")), data = train_data, split = 'deviance', 
                               control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_raw_weighted) 
tree_tennis_raw_weighted
plot(tree_tennis_raw_weighted)
text(tree_tennis_raw_weighted, cex = 0.9)

yhat_data <- predict(tree_tennis_raw_weighted,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_raw_weighted <- cv.tree(tree_tennis_raw_weighted, FUN=prune.misclass)

size <- cv.tennis_raw_weighted$size
plot(size, cv.tennis_raw_weighted$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_raw_weighted$k[1] <- 0
alpha <- round(cv.tennis_raw_weighted$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_raw_weighted$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_raw_weighted$dev)

# prune the tree back:
prune.tennis_raw_weighted <- prune.misclass(tree_tennis_raw_weighted, best = 3)
plot(prune.tennis_raw_weighted)
text(prune.tennis_raw_weighted, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_raw_weighted <- predict(prune.tennis_raw_weighted, test_data, type='class')
table(tree.pred_raw_weighted, test_data$wl)
sum(diag(table(tree.pred_raw_weighted, test_data$wl)))/length(test_data$wl)*100

#Raw weighted BC ---------------------------------
data <- read.csv("raw_weighted_bc.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_raw_weighted_bc <- tree(as.formula(paste(colnames(data)[4], "~",
                                                   paste(colnames(data)[c(5:6, 16:75)], collapse = "+"),
                                                   sep = "")), data = train_data, split = 'deviance', 
                                  control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_raw_weighted_bc) 
tree_tennis_raw_weighted_bc
plot(tree_tennis_raw_weighted_bc)
text(tree_tennis_raw_weighted_bc, cex = 0.9)

yhat_data <- predict(tree_tennis_raw_weighted_bc,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_raw_weighted_bc <- cv.tree(tree_tennis_raw_weighted_bc, FUN=prune.misclass)

size <- cv.tennis_raw_weighted_bc$size
plot(size, cv.tennis_raw_weighted_bc$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_raw_weighted_bc$k[1] <- 0
alpha <- round(cv.tennis_raw_weighted_bc$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_raw_weighted_bc$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_raw_weighted_bc$dev)

# prune the tree back:
prune.tennis_raw_weighted_bc <- prune.misclass(tree_tennis_raw_weighted_bc, best = 4)
plot(prune.tennis_raw_weighted_bc)
text(prune.tennis_raw_weighted_bc, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_raw_weighted_bc <- predict(prune.tennis_raw_weighted_bc, test_data, type='class')
table(tree.pred_raw_weighted_bc, test_data$wl)
sum(diag(table(tree.pred_raw_weighted_bc, test_data$wl)))/length(test_data$wl)*100


# Using engineered data -------------------------------------------------------------
#engineered Rolled ------------------------------------
data <- read.csv("engineered_rolled.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_engineered_rolled <- tree(as.formula(paste(colnames(data)[4], "~",
                                                paste(colnames(data)[c(5:6, 16:39)], collapse = "+"),
                                                sep = "")), data = train_data, split = 'deviance', 
                               control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_engineered_rolled) 
tree_tennis_engineered_rolled
plot(tree_tennis_engineered_rolled)
text(tree_tennis_engineered_rolled, cex = 0.9)

yhat_data <- predict(tree_tennis_engineered_rolled,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_engineered_rolled <- cv.tree(tree_tennis_engineered_rolled, FUN=prune.misclass)

size <- cv.tennis_engineered_rolled$size
plot(size, cv.tennis_engineered_rolled$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_engineered_rolled$k[1] <- 0
alpha <- round(cv.tennis_engineered_rolled$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_engineered_rolled$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_engineered_rolled$dev)

# prune the tree back:
prune.tennis_engineered_rolled <- prune.misclass(tree_tennis_engineered_rolled, best = 3)
plot(prune.tennis_engineered_rolled)
text(prune.tennis_engineered_rolled, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_engineered_rolled <- predict(prune.tennis_engineered_rolled, test_data, type='class')
table(tree.pred_engineered_rolled, test_data$wl)
sum(diag(table(tree.pred_engineered_rolled, test_data$wl)))/length(test_data$wl)*100

#engineered Rolled BC ---------------------------------
data <- read.csv("engineered_rolled_bc.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_engineered_rolled_bc <- tree(as.formula(paste(colnames(data)[4], "~",
                                                   paste(colnames(data)[c(5:6, 16:39)], collapse = "+"),
                                                   sep = "")), data = train_data, split = 'deviance', 
                                  control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_engineered_rolled_bc) 
tree_tennis_engineered_rolled_bc
plot(tree_tennis_engineered_rolled_bc)
text(tree_tennis_engineered_rolled_bc, cex = 0.9)

yhat_data <- predict(tree_tennis_engineered_rolled_bc,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_engineered_rolled_bc <- cv.tree(tree_tennis_engineered_rolled_bc, FUN=prune.misclass)

size <- cv.tennis_engineered_rolled_bc$size
plot(size, cv.tennis_engineered_rolled_bc$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_engineered_rolled_bc$k[1] <- 0
alpha <- round(cv.tennis_engineered_rolled_bc$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_engineered_rolled_bc$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_engineered_rolled_bc$dev)

# prune the tree back:
prune.tennis_engineered_rolled_bc <- prune.misclass(tree_tennis_engineered_rolled_bc, best = 3)
plot(prune.tennis_engineered_rolled_bc)
text(prune.tennis_engineered_rolled_bc, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_engineered_rolled_bc <- predict(prune.tennis_engineered_rolled_bc, test_data, type='class')
table(tree.pred_engineered_rolled_bc, test_data$wl)
sum(diag(table(tree.pred_engineered_rolled_bc, test_data$wl)))/length(test_data$wl)*100



#engineered weighted ------------------------------------
data <- read.csv("engineered_weighted.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_engineered_weighted <- tree(as.formula(paste(colnames(data)[4], "~",
                                                  paste(colnames(data)[c(5:6, 16:39)], collapse = "+"),
                                                  sep = "")), data = train_data, split = 'deviance', 
                                 control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_engineered_weighted) 
tree_tennis_engineered_weighted
plot(tree_tennis_engineered_weighted)
text(tree_tennis_engineered_weighted, cex = 0.9)

yhat_data <- predict(tree_tennis_engineered_weighted,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_engineered_weighted <- cv.tree(tree_tennis_engineered_weighted, FUN=prune.misclass)

size <- cv.tennis_engineered_weighted$size
plot(size, cv.tennis_engineered_weighted$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_engineered_weighted$k[1] <- 0
alpha <- round(cv.tennis_engineered_weighted$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_engineered_weighted$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_engineered_weighted$dev)

# prune the tree back:
prune.tennis_engineered_weighted <- prune.misclass(tree_tennis_engineered_weighted, best = 3)
plot(prune.tennis_engineered_weighted)
text(prune.tennis_engineered_weighted, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_engineered_weighted <- predict(prune.tennis_engineered_weighted, test_data, type='class')
table(tree.pred_engineered_weighted, test_data$wl)
sum(diag(table(tree.pred_engineered_weighted, test_data$wl)))/length(test_data$wl)*100

#engineered weighted BC ---------------------------------
data <- read.csv("engineered_weighted_bc.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)
data$wl <- relevel(data$wl,"Player B") 

ind <- 1:23656

train_data <- data[ind, ]
test_data <- data[-ind, ]

set.seed(2020)
tree_tennis_engineered_weighted_bc <- tree(as.formula(paste(colnames(data)[4], "~",
                                                     paste(colnames(data)[c(5:6, 16:39)], collapse = "+"),
                                                     sep = "")), data = train_data, split = 'deviance', 
                                    control = tree.control(nobs=23656, mincut = 2, minsize = 4, mindev = 0.001))

summary(tree_tennis_engineered_weighted_bc) 
tree_tennis_engineered_weighted_bc
plot(tree_tennis_engineered_weighted_bc)
text(tree_tennis_engineered_weighted_bc, cex = 0.9)

yhat_data <- predict(tree_tennis_engineered_weighted_bc,  test_data, type = 'class')

(c_mat <- table(yhat_data, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# Prune that tree---------------------------------------
cv.tennis_engineered_weighted_bc <- cv.tree(tree_tennis_engineered_weighted_bc, FUN=prune.misclass)

size <- cv.tennis_engineered_weighted_bc$size
plot(size, cv.tennis_engineered_weighted_bc$dev, type = 'c', xlab = 'Number of terminal nodes', ylab = 'CV error')
# add alpha values to the plot:
cv.tennis_engineered_weighted_bc$k[1] <- 0
alpha <- round(cv.tennis_engineered_weighted_bc$k,1)
axis(3, at = size, lab = alpha, cex.axis = 0.8)
mtext(expression(alpha), 3, line=2.5, cex=1.2)
text(size, cv.tennis_engineered_weighted_bc$dev, substitute(leaves, list(leaves = size)), cex = 0.9)
best = min(cv.tennis_engineered_weighted_bc$dev)

# prune the tree back:
prune.tennis_engineered_weighted_bc <- prune.misclass(tree_tennis_engineered_weighted_bc, best = 6)
plot(prune.tennis_engineered_weighted_bc)
text(prune.tennis_engineered_weighted_bc, pretty=0, cex=0.8)

# how well does the pruned tree perform?
tree.pred_engineered_weighted_bc <- predict(prune.tennis_engineered_weighted_bc, test_data, type='class')
table(tree.pred_engineered_weighted_bc, test_data$wl)
sum(diag(table(tree.pred_engineered_weighted_bc, test_data$wl)))/length(test_data$wl)*100












#loadhistory("Chapter3.Rhistory")