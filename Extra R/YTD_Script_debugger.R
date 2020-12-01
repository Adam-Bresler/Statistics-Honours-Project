library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(stringr)

w_dat <- read.csv("finaldata2.csv")
w_dat <- w_dat[, -1]
w_dat$tournament_date <- lubridate::as_date(w_dat$tournament_date)

#w_dat$first_serve_percentage <- round((w_dat$first_serves_in/w_dat$first_serves_total)*100,3)
# We need to convert our data to percentages

# Percentages ----------------------------------------------------------------

# serve percentages
w_dat$aces_percentage <- (w_dat[,7] / w_dat[,27])*100
w_dat$dbl_fault_percentage <- (w_dat[,8] / w_dat[,14])*100
w_dat$percent_first_serve_in <- (w_dat[,9] / w_dat[,10])*100
w_dat$percent_first_serve_won<- (w_dat[,11] / w_dat[,12])*100
w_dat$percent_second_serve_won<- (w_dat[,13] / w_dat[,14])*100
w_dat$percent_break_points_saved <- (w_dat[,15] / w_dat[,16])*100

# return percentages
w_dat$percent_first_serve_return_won <- (w_dat[,19] / w_dat[,20])*100
w_dat$percent_second_serve_return_won <- (w_dat[,21] / w_dat[,22])*100
w_dat$percent_break_points_converted <- (w_dat[,23] / w_dat[,24])*100

# percentage of point won
w_dat$percent_service_points_won <- (w_dat[,26] / w_dat[,27])*100
w_dat$percent_return_points_won  <- (w_dat[,28] / w_dat[,29])*100
w_dat$percent_total_points_won   <- (w_dat[,30] / w_dat[,31])*100

# replacing NaN's 
w_dat[which(is.na(w_dat$percent_break_points_saved)),48] <- 100
w_dat[which(is.na(w_dat$percent_break_points_converted)),51] <- 0
w_dat[which(is.na(w_dat$seed)),32] <- 34

w_dat$seed <- as.factor(w_dat$seed)
str(w_dat$seed)

# Matching Player Hand -------------------------------------------------------

players <- read.csv("players.csv")
players <- players[ , -1]

plays <- players$plays
plays <- strsplit(plays, ",")

for (i in 1:773) {
  players$hand[i] <- plays[[i]][1]
  players$back[i] <- plays[[i]][2]
}

w_dat$player_A_hand <- players$hand[match(w_dat$name, players$players)]
w_dat$player_B_hand <- players$hand[match(w_dat$Opponent, players$players)]

# Selecting Features --------------------------------------------------------

cols <- c(5:31, 33:35, 43:54)
w_dat <- w_dat[, c(1:4, 32, 36:42, 55:56, cols)]

#Exclude NA!
w_dat <- w_dat[complete.cases(w_dat[, 15:56]),]
data <- w_dat

# Weighted by court surface --------------------------------------------------
weighted_rolling_average_by_court <- function(data, columns){
  n <- nrow(data)
  player <- unique(data$name)
  return_matrix <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(data) + length(columns)))
  return_matrix[,9] <- as.Date("2010-01-01")
  colnames(return_matrix) <- c(colnames(data), colnames(data)[columns])
  
  for(i in player){
    dat <- data %>% filter(name == i)
    player_dat <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(data) + length(columns)))
    player_dat[,9] <- as.Date("2010-01-01")
    colnames(player_dat) <- colnames(return_matrix)
    
    for(j in 1:nrow(dat)){ # This include all matches in a tournament, even if we are in the quarters. Thus, we need to remove the semis etc
      j = 21
      matches_6_months <- dat %>% filter(dat$tournament_date <= dat$tournament_date[j] & dat$tournament_date >= (dat$tournament_date[j] %m-% months(6)))
      matches_6_months <- matches_6_months%>%filter(matches_6_months$tournament_surface==dat$tournament_surface[j])
      
      matches_12_months <- dat %>% filter(dat$tournament_date < 
                                            dat$tournament_date[j] %m-% months(6) & dat$tournament_date >= (dat$tournament_date[j] %m-% months(12)))
      matches_12_months <- matches_12_months%>%filter(matches_12_months$tournament_surface==dat$tournament_surface[j])
      
      matches_24_months <- dat %>% filter(dat$tournament_date <
                                            dat$tournament_date[j] %m-% months(12) & dat$tournament_date >= (dat$tournament_date[j] %m-% months(24)))
      matches_24_months <- matches_24_months%>%filter(matches_24_months$tournament_surface==dat$tournament_surface[j])
      
      matches_all_after_24 <- dat %>% filter(dat$tournament_date < dat$tournament_date[j] %m-% months(24))
      matches_all_after_24 <- matches_all_after_24%>%filter(matches_all_after_24$tournament_surface==dat$tournament_surface[j])
      
      if(j == 1){ #The first game has no ytd from before
        average_initial <- as.data.frame(t(rep(0, length(columns))))
        colnames(average_initial) <- colnames(data)[columns]
        average <- average_initial
      }
      
      else{
        if(nrow(matches_6_months) <= 1){
          average_6 <- average_initial
        }
        else{
          ind6 <- which(matches_6_months$Match_ID == dat$Match_ID[j])  #Find which j we are in matches, and throw older stuff away
          
          matches_6_months <- matches_6_months[1:(ind6-1), ]
          average_6 <- matches_6_months %>% select(columns) %>% summarise_if(is.numeric, mean)
        }
        
        if(nrow(matches_12_months)<=1){
          average_12 <- average_initial
        }
        else{
          average_12 <- matches_12_months %>% select(columns) %>% summarise_if(is.numeric, mean)
        }
        
        if(nrow(matches_24_months)<=1){
          average_24 <- average_initial
        }
        else{
          average_24 <- matches_24_months %>% select(columns) %>% summarise_if(is.numeric, mean)
        }
        
        if(nrow(matches_all_after_24)<=1){
          average_rest <- average_initial
        }
        else{
          average_rest <- matches_all_after_24 %>% select(columns) %>% summarise_if(is.numeric, mean)
        }
      }
      
      if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(8,4,2,1)
        sum <- 15
      }
      else if(nrow(matches_6_months) < 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(0, 4, 2, 1)
        sum <- 7
      }
      else if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) < 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(4,2,0,1)
        sum <- 7
      }
      else if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) < 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(4,0,2,1)
        sum <- 7
      }
      else if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) < 1){
        weights <- c(4,2,1,0)
        sum <- 7
      }  
      else if(nrow(matches_6_months) < 1 & nrow(matches_12_months) < 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(0,0,2,1)
        sum <- 3
      }
      else if(nrow(matches_6_months) < 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) < 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(0,2,0,1)
        sum <- 3
      }
      else if(nrow(matches_6_months) < 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) < 1){
        weights <- c(0,2,1,0)
        sum <- 3
      }
      else if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) < 1 & nrow(matches_24_months) < 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(2,0,0,1)
        sum <- 3
      }
      else if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) < 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) < 1){
        weights <- c(2,0,1,0)
        sum <- 3
      }
      else if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) < 1 & nrow(matches_all_after_24) < 1){
        weights <- c(2,1,0,0)
        sum <- 3
      } 
      else if(nrow(matches_6_months) < 1 & nrow(matches_12_months) < 1 & nrow(matches_24_months) < 1 & nrow(matches_all_after_24) >= 1){
        weights <- c(0,0,0,1)
        sum <- 1
      }  
      else if(nrow(matches_6_months) < 1 & nrow(matches_12_months) < 1 & nrow(matches_24_months) >= 1 & nrow(matches_all_after_24) < 1){
        weights <- c(0,0,1,0)
        sum <- 1
      }
      else if(nrow(matches_6_months) < 1 & nrow(matches_12_months) >= 1 & nrow(matches_24_months) < 1 & nrow(matches_all_after_24) < 1){
        weights <- c(0,1,0,0)
        sum <- 1
      }
      else if(nrow(matches_6_months) >= 1 & nrow(matches_12_months) < 1 & nrow(matches_24_months) < 1 & nrow(matches_all_after_24) < 1){
        weights <- c(1,0,0,0)
        sum <- 1
      } 
      else{
        weights <- c(0,0,0,0)
        sum <- 0
      }
      
      if(sum == 0 | j == 1){      
        average <- average_initial
      }
      else{
        average <- (weights[1]*average_6 + weights[2]*average_12 + weights[3]*average_24 + weights[4]*average_rest)/sum
      }
      
      player_dat <- rbind.data.frame(player_dat, cbind.data.frame(dat[j, ], average))
    }
    
    return_matrix <- rbind.data.frame(return_matrix, player_dat[-1, ])
  }
  
  return(return_matrix[-1, ])
}


data <- weighted_rolling_average_by_court(data, 15:56)
colnames(data)[c(183:224)] <- paste("weighted_rolling_average_by_court", colnames(data[15:56]), sep="_")


