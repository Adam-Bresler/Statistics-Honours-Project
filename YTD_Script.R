library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)

w_dat <- read.csv("finaldata2.csv")
w_dat <- w_dat[, -1]
w_dat$tournament_date <- lubridate::as_date(w_dat$tournament_date)

#w_dat$first_serve_percentage <- round((w_dat$first_serves_in/w_dat$first_serves_total)*100,3)
# We need to convert our data to percentages

# Percentages ----------------------------------------------------------------




# Matching Player Hand -------------------------------------------------------






# Selecting Feastures --------------------------------------------------------

#Select serve and return ratings as our inital features
w_dat <- w_dat[, c(1:4, 6, 18, 36, 37, 39)]
data <- w_dat
months = 24

#Didnt exclude NA!
w_dat <- w_dat[-which(is.na(w_dat$serve_rating)),]

#w_dat <- w_dat[, c(1:4, )]

average_past_games <- function(data, columns = c(5, 6), months = 24){
  n <- nrow(data)
  player <- unique(data$name)
  return_matrix <- matrix(0, nrow = 1, ncol = ncol(data) + length(columns))
  colnames(return_matrix) <- c(colnames(data), colnames(data)[columns])



  for(i in player){
    dat <- data %>% filter(name == i)
    player_dat <- matrix(0, nrow = 1, ncol = ncol(data) + length(columns))
    colnames(player_dat) <- colnames(return_matrix)

    for(j in 1:nrow(dat)){ # This include all matches in a tournament, even if we are in the quarters. Thus, we need to remove the semis etc
      matches <- dat %>% filter(dat$tournament_date <= dat$tournament_date[j] & dat$tournament_date >= (dat$tournament_date[j] - months(months)))

      if(j == 1){ #The first game has no ytd from before
        average <- as.data.frame(t(c(0,0)))
        colnames(average) <- colnames(data)[columns]
      }

      else{
      ind <- which(matches$Match_ID == dat$Match_ID[j])  #Find which j we are in matches, and throw older stuff away
      matches <- matches[1:(ind-1), ]
      average <- matches %>% select(columns) %>% summarise_if(is.numeric, mean)
      }

      player_dat <- rbind(player_dat, cbind(dat[j, ], average))
    }

     return_matrix <- rbind(return_matrix, player_dat[-1, ])
    }

  return(return_matrix[-1, ])
}

rolling_average_serve_return <- average_past_games(w_dat)
 
write.csv(rolling_average_serve_return, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/rolling_average_serve_return.csv")

# Creating the predictive ----------------------------------------------------
data <- read.csv("rolling_average_serve_return.csv") 
data <- data[,-1]

#Eventually, add this into loop
colnames(data)[c(10,11)] <- c("average_serve_rating", "average_return_rating")

  
predictive <- data %>% 
  group_by(Match_ID)%>%arrange(.by_group = TRUE)

predictive2 <- data %>% 
  group_by(Match_ID)%>%arrange(.by_group = TRUE)%>%
  select(c(1,10,11))

predictive3 <- predictive[seq(1,52668,2),-c(10,11)]


first_player<-predictive2[seq(1,52668,2),-1]
second_player<-predictive2[seq(2,52668,2),-1]

difference <- first_player-second_player

predictive_dataset <- cbind(predictive3,difference)

rm(predictive, predictive2, predictive3, second_player, first_player, difference)

colnames(predictive_dataset)[c(2,3)] <- c("Player_A", "Player_B")

predictive_dataset$wl <- ifelse(predictive_dataset$wl == 'winner', "Player A", "Player B")

predictive_dataset$wl <- as.factor(predictive_dataset$wl)



