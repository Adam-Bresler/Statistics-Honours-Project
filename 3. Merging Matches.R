# load required packages
library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stringr)
library(tree)

# Currently we have data that is "long", in terms of a single player.
# Now we would like to make it such that each match is a row again

# Creating the predictive  data set-------------------------------------------
data <- read.csv("BP_serve_and_return_seperated.csv")
data <- data[,-1]

predictive <- data %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)

predictive2 <- data %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)%>%
  select(c(1,57:224))

predictive3 <- predictive[seq(1,52522,2),-c(57:236)]

# Merging matches to contain both players ------------------------------------

first_player<-predictive[seq(1,52518,2),]
second_player<-predictive[seq(2,52518,2),]

colnames(first_player)[c(5,15:244)] <- paste("Player_A", colnames(first_player[c(5,15:244)]), sep="_")
colnames(second_player)[c(5,15:244)] <- paste("Player_B", colnames(second_player[c(5,15:244)]), sep="_")

second_player <- second_player[, -c(1:4, 6:14)]

predictive_dataset <- cbind.data.frame(first_player, second_player)

colnames(predictive_dataset)[c(2,3)] <- c("Player_A", "Player_B")

# Wl is still in terms of Player A, having come from the first_player
# Thus if it says winner, player a won

predictive_dataset$wl <- ifelse(predictive_dataset$wl == 'winner', "Player A", "Player B")

predictive_dataset$wl <- as.factor(predictive_dataset$wl)

# Rearranging columns for clarity
predictive_dataset <- predictive_dataset[, c(1:5, 245, 6:244, 246:475)] #Watch out for this! Change as data changes

# write.csv(predictive_dataset, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/BP_seperated_data_no_h2h.csv")
