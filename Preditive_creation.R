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