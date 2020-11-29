library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stats)
library(ROCR)
library(gbm)
library(arulesViz)
library(MASS)
library(xtable)

# GLM's ----------------------------------------------------------------------

# Raw Features ---------------------------------------------------------------

# raw rolled -----------------------------------------------------------------

raw_rolled<- read.csv("raw_rolled.csv")
raw_rolled<- raw_rolled[,-1]


ind <- 1:23656

raw_rolled$wl <- as.factor(raw_rolled$wl)
raw_rolled$wl <- relevel(raw_rolled$wl,"Player B")

train_data_rr<- raw_rolled[ind, ]
test_data_rr<- raw_rolled[-ind, ]


mod_rr <- glm(as.formula(paste(colnames(raw_rolled)[4], "~",
                            paste(colnames(raw_rolled)[c(16:71)], collapse = "+"),
                            sep = "")), data = train_data_rr, family = "binomial")

summary(mod_rr)
# plot(sort(predict(mod_rr, type = 'response')), type = "l")

threshold_rr <- 0.6003466
y.hat_rr <- ifelse(predict(mod_rr, newdata = test_data_rr, type = 'response') > threshold_rr, "Player A", "Player B") 

y.hat_rr <- as.factor(y.hat_rr)
y.hat_rr <- relevel(y.hat_rr,"Player B")
conf_matrix_rr <- table(y.hat_rr, test_data_rr$wl)
conf_matrix_rr

sum(diag(conf_matrix_rr))/sum(conf_matrix_rr)

sens_rr <-conf_matrix_rr[2,2]/(conf_matrix_rr[1,2]+conf_matrix_rr[2,2])
spec_rr <-conf_matrix_rr[1,1]/(conf_matrix_rr[1,1]+conf_matrix_rr[2,1])

prediction.object_rr <- prediction(fitted(mod_rr), labels = train_data_rr$wl,label.ordering = c("Player B","Player A"))

roc_rr <-  performance(prediction.object_rr,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_rr)
abline(a = 0, b = 1)
dev.off()


cutoffs_rr <- data.frame(cut=roc_rr@alpha.values[[1]], tpr=roc_rr@y.values[[1]], spec = 1 - roc_rr@x.values[[1]],
                      fpr=roc_rr@x.values[[1]])
d_rr <- cutoffs_rr[,2] + cutoffs_rr[,3]
cutoffs_rr[which.max(d_rr),] 

# raw rolled by court --------------------------------------------------------

raw_rolled_bc<- read.csv("raw_rolled_bc.csv")
raw_rolled_bc<- raw_rolled_bc[,-1]


ind <- 1:23656

raw_rolled_bc$wl <- as.factor(raw_rolled_bc$wl)
raw_rolled_bc$wl <- relevel(raw_rolled_bc$wl,"Player B")

train_data_rr_bc<- raw_rolled_bc[ind, ]
test_data_rr_bc<- raw_rolled_bc[-ind, ]


mod_rr_bc <- glm(as.formula(paste(colnames(raw_rolled_bc)[4], "~",
                               paste(colnames(raw_rolled_bc)[c(16:71)], collapse = "+"),
                               sep = "")), data = train_data_rr_bc, family = "binomial")

summary(mod_rr_bc)
# plot(sort(predict(mod_rr_bc, type = 'response')), type = "l")

threshold_rr_bc <- 0.5944206
y.hat_rr_bc <- ifelse(predict(mod_rr_bc, newdata = test_data_rr_bc, type = 'response') > threshold_rr_bc, "Player A", "Player B") 

y.hat_rr_bc <- as.factor(y.hat_rr_bc)
y.hat_rr_bc <- relevel(y.hat_rr_bc,"Player B")
conf_matrix_rr_bc <- table(y.hat_rr_bc, test_data_rr_bc$wl)
conf_matrix_rr_bc

sum(diag(conf_matrix_rr_bc))/sum(conf_matrix_rr_bc)

sens_rr_bc <-conf_matrix_rr_bc[2,2]/(conf_matrix_rr_bc[1,2]+conf_matrix_rr_bc[2,2])
spec_rr_bc <-conf_matrix_rr_bc[1,1]/(conf_matrix_rr_bc[1,1]+conf_matrix_rr_bc[2,1])

prediction.object_rr_bc <- prediction(fitted(mod_rr_bc), labels = train_data_rr_bc$wl,label.ordering = c("Player B","Player A"))

roc_rr_bc <-  performance(prediction.object_rr_bc,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_rr_bc)
abline(a = 0, b = 1) 
dev.off()

cutoffs_rr_bc <- data.frame(cut=roc_rr_bc@alpha.values[[1]], tpr=roc_rr_bc@y.values[[1]], spec = 1 - roc_rr_bc@x.values[[1]],
                         fpr=roc_rr_bc@x.values[[1]])
d_rr_bc <- cutoffs_rr_bc[,2] + cutoffs_rr_bc[,3]
cutoffs_rr_bc[which.max(d_rr_bc),] 


# raw weighted ---------------------------------------------------------------

raw_weighted<- read.csv("raw_weighted.csv")
raw_weighted<- raw_weighted[,-1]


ind <- 1:23656

raw_weighted$wl <- as.factor(raw_weighted$wl)
raw_weighted$wl <- relevel(raw_weighted$wl,"Player B")

train_data_rw<- raw_weighted[ind, ]
test_data_rw<- raw_weighted[-ind, ]


mod_rw <- glm(as.formula(paste(colnames(raw_weighted)[4], "~",
                               paste(colnames(raw_weighted)[c(16:71)], collapse = "+"),
                               sep = "")), data = train_data_rw, family = "binomial")

summary(mod_rw)
# plot(sort(predict(mod_rw, type = 'response')), type = "l")

threshold_rw <- 0.5909854
y.hat_rw <- ifelse(predict(mod_rw, newdata = test_data_rw, type = 'response') > threshold_rw, "Player A", "Player B") 

y.hat_rw <- as.factor(y.hat_rw)
y.hat_rw <- relevel(y.hat_rw,"Player B")
conf_matrix_rw <- table(y.hat_rw, test_data_rw$wl)
conf_matrix_rw

sum(diag(conf_matrix_rw))/sum(conf_matrix_rw)

sens_rw <-conf_matrix_rw[2,2]/(conf_matrix_rw[1,2]+conf_matrix_rw[2,2])
spec_rw <-conf_matrix_rw[1,1]/(conf_matrix_rw[1,1]+conf_matrix_rw[2,1])

prediction.object_rw <- prediction(fitted(mod_rw), labels = train_data_rw$wl,label.ordering = c("Player B","Player A"))

roc_rw <-  performance(prediction.object_rw,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_rw)
abline(a = 0, b = 1) 
dev.off()

cutoffs_rw <- data.frame(cut=roc_rw@alpha.values[[1]], tpr=roc_rw@y.values[[1]], spec = 1 - roc_rw@x.values[[1]],
                         fpr=roc_rw@x.values[[1]])
d_rw <- cutoffs_rw[,2] + cutoffs_rw[,3]
cutoffs_rw[which.max(d_rw),] 


# raw weighted by court ------------------------------------------------------

raw_weighted_bc<- read.csv("raw_weighted_bc.csv")
raw_weighted_bc<- raw_weighted_bc[,-1]


ind <- 1:23656

raw_weighted_bc$wl <- as.factor(raw_weighted_bc$wl)
raw_weighted_bc$wl <- relevel(raw_weighted_bc$wl,"Player B")

train_data_rw_bc<- raw_weighted_bc[ind, ]
test_data_rw_bc<- raw_weighted_bc[-ind, ]


mod_rw_bc <- glm(as.formula(paste(colnames(raw_weighted_bc)[4], "~",
                               paste(colnames(raw_weighted_bc)[c(16:71)], collapse = "+"),
                               sep = "")), data = train_data_rw_bc, family = "binomial")

summary(mod_rw_bc)
# plot(sort(predict(mod_rw_bc, type = 'response')), type = "l")

threshold_rw_bc <- 0.6237948
y.hat_rw_bc <- ifelse(predict(mod_rw_bc, newdata = test_data_rw_bc, type = 'response') > threshold_rw_bc, "Player A", "Player B") 

y.hat_rw_bc <- as.factor(y.hat_rw_bc)
y.hat_rw_bc <- relevel(y.hat_rw_bc,"Player B")
conf_matrix_rw_bc <- table(y.hat_rw_bc, test_data_rw_bc$wl)
conf_matrix_rw_bc

sum(diag(conf_matrix_rw_bc))/sum(conf_matrix_rw_bc)

sens_rw_bc <-conf_matrix_rw_bc[2,2]/(conf_matrix_rw_bc[1,2]+conf_matrix_rw_bc[2,2])
spec_rw_bc <-conf_matrix_rw_bc[1,1]/(conf_matrix_rw_bc[1,1]+conf_matrix_rw_bc[2,1])

prediction.object_rw_bc <- prediction(fitted(mod_rw_bc), labels = train_data_rw_bc$wl,label.ordering = c("Player B","Player A"))

roc_rw_bc <-  performance(prediction.object_rw_bc,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_rw_bc)
abline(a = 0, b = 1) 
dev.off()

cutoffs_rw_bc <- data.frame(cut=roc_rw_bc@alpha.values[[1]], tpr=roc_rw_bc@y.values[[1]], spec = 1 - roc_rw_bc@x.values[[1]],
                         fpr=roc_rw_bc@x.values[[1]])
d_rw_bc <- cutoffs_rw_bc[,2] + cutoffs_rw_bc[,3]
cutoffs_rw_bc[which.max(d_rw_bc),] 





# Engineered Features --------------------------------------------------------

# Engineered rolled ----------------------------------------------------------

engineered_rolled<- read.csv("engineered_rolled.csv")
engineered_rolled<- engineered_rolled[,-1]


ind <- 1:23656

engineered_rolled$wl <- as.factor(engineered_rolled$wl)
engineered_rolled$wl <- relevel(engineered_rolled$wl,"Player B")

train_data_er<- engineered_rolled[ind, ]
test_data_er<- engineered_rolled[-ind, ]


mod_er <- glm(as.formula(paste(colnames(engineered_rolled)[4], "~",
                                  paste(colnames(engineered_rolled)[c(16:39)], collapse = "+"),
                                  sep = "")), data = train_data_er, family = "binomial")

summary(mod_er)
# plot(sort(predict(mod_er, type = 'response')), type = "l")

threshold_er <- 0.6093746
y.hat_er <- ifelse(predict(mod_er, newdata = test_data_er, type = 'response') > threshold_er, "Player A", "Player B") 

y.hat_er <- as.factor(y.hat_er)
y.hat_er <- relevel(y.hat_er,"Player B")
conf_matrix_er <- table(y.hat_er, test_data_er$wl)
conf_matrix_er

sum(diag(conf_matrix_er))/sum(conf_matrix_er)

sens_er <-conf_matrix_er[2,2]/(conf_matrix_er[1,2]+conf_matrix_er[2,2])
spec_er <-conf_matrix_er[1,1]/(conf_matrix_er[1,1]+conf_matrix_er[2,1])

prediction.object_er <- prediction(fitted(mod_er), labels = train_data_er$wl,label.ordering = c("Player B","Player A"))

roc_er <-  performance(prediction.object_er,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_er)
abline(a = 0, b = 1) 
dev.off()

cutoffs_er <- data.frame(cut=roc_er@alpha.values[[1]], tpr=roc_er@y.values[[1]], spec = 1 - roc_er@x.values[[1]],
                            fpr=roc_er@x.values[[1]])
d_er <- cutoffs_er[,2] + cutoffs_er[,3]
cutoffs_er[which.max(d_er),] 


# Engineered rolled by court -------------------------------------------------

engineered_rolled_bc<- read.csv("engineered_rolled_bc.csv")
engineered_rolled_bc<- engineered_rolled_bc[,-1]

ind <- 1:23656

engineered_rolled_bc$wl <- as.factor(engineered_rolled_bc$wl)
engineered_rolled_bc$wl <- relevel(engineered_rolled_bc$wl,"Player B")

train_data_er_bc<- engineered_rolled_bc[ind, ]
test_data_er_bc<- engineered_rolled_bc[-ind, ]


mod_er_bc <- glm(as.formula(paste(colnames(engineered_rolled_bc)[4], "~",
                               paste(colnames(engineered_rolled_bc)[c(16:39)], collapse = "+"),
                               sep = "")), data = train_data_er_bc, family = "binomial")

summary(mod_er_bc)
# plot(sort(predict(mod_er_bc, type = 'response')), type = "l")

threshold_er_bc <- 0.5532467
y.hat_er_bc <- ifelse(predict(mod_er_bc, newdata = test_data_er_bc, type = 'response') > threshold_er_bc, "Player A", "Player B") 

y.hat_er_bc <- as.factor(y.hat_er_bc)
y.hat_er_bc <- relevel(y.hat_er_bc,"Player B")
conf_matrix_er_bc <- table(y.hat_er_bc, test_data_er_bc$wl)
conf_matrix_er_bc

sum(diag(conf_matrix_er_bc))/sum(conf_matrix_er_bc)

sens_er_bc <-conf_matrix_er_bc[2,2]/(conf_matrix_er_bc[1,2]+conf_matrix_er_bc[2,2])
spec_er_bc <-conf_matrix_er_bc[1,1]/(conf_matrix_er_bc[1,1]+conf_matrix_er_bc[2,1])

prediction.object_er_bc <- prediction(fitted(mod_er_bc), labels = train_data_er_bc$wl,label.ordering = c("Player B","Player A"))

roc_er_bc <-  performance(prediction.object_er_bc,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_er_bc)
abline(a = 0, b = 1) 
dev.off()

cutoffs_er_bc <- data.frame(cut=roc_er_bc@alpha.values[[1]], tpr=roc_er_bc@y.values[[1]], spec = 1 - roc_er_bc@x.values[[1]],
                         fpr=roc_er_bc@x.values[[1]])
d_er_bc <- cutoffs_er_bc[,2] + cutoffs_er_bc[,3]
cutoffs_er_bc[which.max(d_er_bc),] 


# Engineered Weighted -------------------------------------------------------- 

engineered_weighted<- read.csv("engineered_weighted.csv")
engineered_weighted<- engineered_weighted[,-1]

ind <- 1:23656

engineered_weighted$wl <- as.factor(engineered_weighted$wl)
engineered_weighted$wl <- relevel(engineered_weighted$wl,"Player B")

train_data_ew<- engineered_weighted[ind, ]
test_data_ew<- engineered_weighted[-ind, ]


mod_ew <- glm(as.formula(paste(colnames(engineered_weighted)[4], "~",
                                  paste(colnames(engineered_weighted)[c(16:39)], collapse = "+"),
                                  sep = "")), data = train_data_ew, family = "binomial")

summary(mod_ew)
# plot(sort(predict(mod_ew, type = 'response')), type = "l")

threshold_ew <- 0.641727
y.hat_ew <- ifelse(predict(mod_ew, newdata = test_data_ew, type = 'response') > threshold_ew, "Player A", "Player B") 

y.hat_ew <- as.factor(y.hat_ew)
y.hat_ew <- relevel(y.hat_ew,"Player B")
conf_matrix_ew <- table(y.hat_ew, test_data_ew$wl)
conf_matrix_ew

sum(diag(conf_matrix_ew))/sum(conf_matrix_ew)

sens_ew <-conf_matrix_ew[2,2]/(conf_matrix_ew[1,2]+conf_matrix_ew[2,2])
spec_ew <-conf_matrix_ew[1,1]/(conf_matrix_ew[1,1]+conf_matrix_ew[2,1])

prediction.object_ew <- prediction(fitted(mod_ew), labels = train_data_ew$wl,label.ordering = c("Player B","Player A"))

roc_ew <-  performance(prediction.object_ew,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_ew)
abline(a = 0, b = 1) 
dev.off()

cutoffs_ew <- data.frame(cut=roc_ew@alpha.values[[1]], tpr=roc_ew@y.values[[1]], spec = 1 - roc_ew@x.values[[1]],
                            fpr=roc_ew@x.values[[1]])
d_ew <- cutoffs_ew[,2] + cutoffs_ew[,3]
cutoffs_ew[which.max(d_ew),]


# Engineered weighted by court -----------------------------------------------

engineered_weighted_bc<- read.csv("engineered_weighted_bc.csv")
engineered_weighted_bc<- engineered_weighted_bc[,-1]

ind <- 1:23656

engineered_weighted_bc$wl <- as.factor(engineered_weighted_bc$wl)
engineered_weighted_bc$wl <- relevel(engineered_weighted_bc$wl,"Player B")

train_data_ew_bc<- engineered_weighted_bc[ind, ]
test_data_ew_bc<- engineered_weighted_bc[-ind, ]


mod_ew_bc <- glm(as.formula(paste(colnames(engineered_weighted_bc)[4], "~",
                               paste(colnames(engineered_weighted_bc)[c(16:39)], collapse = "+"),
                               sep = "")), data = train_data_ew_bc, family = "binomial")

summary(mod_ew_bc)
# plot(sort(predict(mod_ew_bc, type = 'response')), type = "l")

threshold_ew_bc <- 0.6006164
y.hat_ew_bc <- ifelse(predict(mod_ew_bc, newdata = test_data_ew_bc, type = 'response') > threshold_ew_bc, "Player A", "Player B") 

y.hat_ew_bc <- as.factor(y.hat_ew_bc)
y.hat_ew_bc <- relevel(y.hat_ew_bc,"Player B")
conf_matrix_ew_bc <- table(y.hat_ew_bc, test_data_ew_bc$wl)
conf_matrix_ew_bc

sum(diag(conf_matrix_ew_bc))/sum(conf_matrix_ew_bc)

sens_ew_bc <-conf_matrix_ew_bc[2,2]/(conf_matrix_ew_bc[1,2]+conf_matrix_ew_bc[2,2])
spec_ew_bc <-conf_matrix_ew_bc[1,1]/(conf_matrix_ew_bc[1,1]+conf_matrix_ew_bc[2,1])

prediction.object_ew_bc <- prediction(fitted(mod_ew_bc), labels = train_data_ew_bc$wl,label.ordering = c("Player B","Player A"))

roc_ew_bc <-  performance(prediction.object_ew_bc,"tpr","fpr") 
# par(mfrow = c(1,1))
dev.new()
plot(roc_ew_bc)
abline(a = 0, b = 1)
dev.off()

cutoffs_ew_bc <- data.frame(cut=roc_ew_bc@alpha.values[[1]], tpr=roc_ew_bc@y.values[[1]], spec = 1 - roc_ew_bc@x.values[[1]],
                         fpr=roc_ew_bc@x.values[[1]])
d_ew_bc <- cutoffs_ew_bc[,2] + cutoffs_ew_bc[,3]
cutoffs_ew_bc[which.max(d_ew_bc),]


# ROC ------------------------------------------------------------------------
# raw
plot(roc_rr, col = "blue4")
plot(roc_rr_bc,add=T,col = "blueviolet")
plot(roc_rw,add=T, col = "cyan2")
plot(roc_rw_bc,add=T, col = "deepskyblue1")
abline(a = 0, b = 1)
legend("bottomright", legend=c("Historical Average", "Historical Average By Court", 
                            "Time Discounted Historical Average", "Time Discounted Historical Average By Court"),
       fill=c("blue4", "blueviolet", "cyan2", "deepskyblue1"))


# engineered
plot(roc_er, col = "green4")
plot(roc_er_bc,add=T, col = "violetred1")
plot(roc_ew,add=T, col = "green")
plot(roc_ew_bc,add=T,col = "goldenrod1")
abline(a = 0, b = 1)
legend("bottomright", legend=c("Historical Average", "Historical Average By Court", 
                            "Time Discounted Historical Average", "Time Discounted Historical Average By Court"),
       fill=c("green4", "violetred1", "green", "goldenrod1"))




# raw weighted is best, do model building
final_mod_rw <- stepAIC(mod_rw, direction = "forward", 
                          trace = FALSE)
xtable(summary(final_mod_rw))

# confusion matrix
xtable(conf_matrix_rw)
