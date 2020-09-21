library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stringr)
library(tree)

# Creating the predictive ----------------------------------------------------
data <- read.csv("rolling_average_all.csv")
data <- data[,-1]

#Eventually, add this into loop
#colnames(data)[c(10,11)] <- c("average_serve_rating", "average_return_rating")


predictive <- data %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)

predictive2 <- data %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)%>%
  select(c(1,57:98))

predictive3 <- predictive[seq(1,52522,2),-c(57:98)]


first_player<-predictive2[seq(1,52522,2),-1]
second_player<-predictive2[seq(2,52522,2),-1]

difference <- first_player-second_player

predictive_dataset <- cbind(predictive3,difference)

rm(predictive, predictive2, predictive3, second_player, first_player, difference)

colnames(predictive_dataset)[c(2,3)] <- c("Player_A", "Player_B")

predictive_dataset$wl <- ifelse(predictive_dataset$wl == 'winner', "Player A", "Player B")

predictive_dataset$wl <- as.factor(predictive_dataset$wl)

# Adding the seeding ---------------------------------------------------------

predictive <- data %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)

first_player<-predictive[seq(1,52522,2),-1]
player_1_seed <-first_player$seed

second_player<-predictive[seq(2,52522,2),-1]
player_2_seed <-second_player$seed

rm(predictive)

predictive_dataset$player_A_seed <- player_1_seed
predictive_dataset$player_B_seed <- player_2_seed

predictive_dataset <- predictive_dataset[,c(1:4,99:100,6:98)]

rm(first_player,second_player)

#write.csv(predictive_dataset, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/all_differences_no_custom_features.csv")

# Begin with feature design --------------------------------------------------

predictive_dataset2 <- predictive_dataset

# rm()

predictive_dataset2$wl <- as.factor(predictive_dataset2$wl)
predictive_dataset2$seed <- predictive_dataset2$player_A_seed-predictive_dataset2$player_B_seed

ind <- 1:23658
train_data <- predictive_dataset2[ind, ]
test_data <- predictive_dataset2[-ind, ]
 

# Decision Tree --------------------------------------------------------------
library(tree)

tree_tennis<- tree(wl~seed, data = train_data, split = 'deviance')


yhat<- predict(tree_tennis,  test_data, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)

# rm()

# Stratify according to court surface ----------------------------------------

# split into training and test set

train_clay <- train_data[which(train_data$tournament_surface=="Clay"),]
test_clay <- test_data[which(test_data$tournament_surface=="Clay"),]

train_grass <-train_data[which(train_data$tournament_surface=="Grass"),]
test_grass <-test_data[which(test_data$tournament_surface=="Grass"),]

train_hard_court <-train_data[which(train_data$tournament_surface=="Hard"),]
test_hard_court <-test_data[which(test_data$tournament_surface=="Hard"),]

# rm ()

# predictions ----------------------------------------------------------------

# grass 

# 
tree_tennis_grass2<- tree(wl~seed+aces_percentage.1+dbl_fault_percentage.1+percent_first_serve_in.1+percent_first_serve_won.1+percent_second_serve_won.1, data = train_grass, split = 'deviance')

yhat_grass2<- predict(tree_tennis_grass2,  test_grass, type = 'class')

(c_mat_grass2 <- table(yhat_grass2, test_grass$wl))          
sum(diag(c_mat_grass2))/nrow(test_grass)*100                
1 - sum(diag(c_mat_grass2))/nrow(test_grass)



# decision tree
tree_tennis_grass<- tree(wl~seed+aces_percentage+dbl_fault_percentage+percent_first_serve_in+percent_first_serve_won+percent_second_serve_won, data = train_grass, split = 'deviance')

yhat_grass<- predict(tree_tennis_grass,  test_grass, type = 'class')

(c_mat_grass <- table(yhat_grass, test_grass$wl))          
sum(diag(c_mat_grass))/nrow(test_grass)*100                
1 - sum(diag(c_mat_grass))/nrow(test_grass)

# gbm

library(gbm)

ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(250, 500, 1000),
                        interaction.depth = c(1, 2),
                        shrinkage = c(0.1, 0.05, 0.01),
                        n.minobsinnode = 1)
set.seed(2020)
gbm_tennis <- train(wl~seed+aces_percentage+dbl_fault_percentage+percent_first_serve_in+percent_first_serve_won+percent_second_serve_won, data = train_grass, 
                    method = 'gbm', 
                    distribution = 'bernoulli', 
                    trControl = ctrl, 
                    verbose = F, 
                    tuneGrid = gbm_grid)

gbm_pred <- predict(gbm_tennis, test_grass)
gbm_cf <- confusionMatrix(gbm_pred, test_grass$wl)
100*sum(diag(gbm_cf$table))/sum(gbm_cf$table)



# clay 

# decision tree

tree_tennis_clay<- tree(wl~seed, data = train_clay, split = 'deviance')

yhat_clay<- predict(tree_tennis_clay,  test_clay, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)


# hard 

# decision tree

tree_tennis<- tree(wl~seed, data = train_data, split = 'deviance')

yhat<- predict(tree_tennis,  test_data, type = 'class')

(c_mat <- table(yhat, test_data$wl))          
sum(diag(c_mat))/nrow(test_data)*100                
1 - sum(diag(c_mat))/nrow(test_data)




length(which(predictive_dataset$wl=="Player A"))
length(which(predictive_dataset$wl=="Player B"))
