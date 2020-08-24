library(dplyr)
library(tidyverse)
library(magrittr)

w_dat <- read.csv("finaldata2.csv")
w_dat <- w_dat[, -1]

#w_dat$first_serve_percentage <- round((w_dat$first_serves_in/w_dat$first_serves_total)*100,3) 
# We need to convert our data to percentages

#Select serve and return ratings as our inital features
w_dat <- w_dat[, c(1:4, 6, 18, 36, 37, 39)]




#w_dat <- w_dat[, c(1:4, )]

average_past_games <- function(data, columns = c(5, 6), months = 24){
  n <- nrow(data)
  player <- unique(data$name)
  
  for(i in player){
    dat <- data %>% filter(name == i) %>% 
  }
  
  
  return(player)
}


