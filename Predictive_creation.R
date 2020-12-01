# load required packages
library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stringr)
library(tree)

# Creating the predictive  data set-------------------------------------------
data <- read.csv("BP_serve_and_return_seperated.csv")
data <- data[,-1]

#Eventually, add this into loop
#colnames(data)[c(57:98)] <- paste("rolling_average", colnames(data[15:56]), sep="_")

predictive <- data %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)

predictive2 <- data %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)%>%
  select(c(1,57:224))

predictive3 <- predictive[seq(1,52522,2),-c(57:236)]

# Trying to merge them -------------------------------------------------------

first_player<-predictive[seq(1,52518,2),]
second_player<-predictive[seq(2,52518,2),]

colnames(first_player)[c(5,15:244)] <- paste("Player_A", colnames(first_player[c(5,15:244)]), sep="_")
colnames(second_player)[c(5,15:244)] <- paste("Player_B", colnames(second_player[c(5,15:244)]), sep="_")

second_player <- second_player[, -c(1:4, 6:14)]

predictive_dataset <- cbind.data.frame(first_player, second_player)

colnames(predictive_dataset)[c(2,3)] <- c("Player_A", "Player_B")

predictive_dataset$wl <- ifelse(predictive_dataset$wl == 'winner', "Player A", "Player B")

predictive_dataset$wl <- as.factor(predictive_dataset$wl)

predictive_dataset <- predictive_dataset[, c(1:5, 245, 6:244, 246:475)] #Watch out for this! Change as data changes

# ----------------------------------------------------------------------------
# first_player<-predictive2[seq(1,52522,2),-1]
# second_player<-predictive2[seq(2,52522,2),-1]
# 
# difference <- first_player-second_player
# 
# predictive_dataset <- cbind(predictive3,difference)
# 
# rm(predictive, predictive2, predictive3, second_player, first_player, difference)
# 
# colnames(predictive_dataset)[c(2,3)] <- c("Player_A", "Player_B")
# 
# predictive_dataset$wl <- ifelse(predictive_dataset$wl == 'winner', "Player A", "Player B")
# 
# predictive_dataset$wl <- as.factor(predictive_dataset$wl)
# 
# # Adding the seeding ---------------------------------------------------------
# 
# predictive <- data %>%
#   group_by(Match_ID)%>%arrange(.by_group = TRUE)
# 
# first_player<-predictive[seq(1,52522,2),-1]
# player_1_seed <-first_player$seed
# 
# second_player<-predictive[seq(2,52522,2),-1]
# player_2_seed <-second_player$seed
# 
# rm(predictive)
# 
# predictive_dataset$player_A_seed <- player_1_seed
# predictive_dataset$player_B_seed <- player_2_seed
# 
# predictive_dataset <- predictive_dataset[,c(1:4,99:100,6:98)]
# 
# rm(first_player,second_player)

write.csv(predictive_dataset, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/BP_seperated_data_no_h2h.csv")
