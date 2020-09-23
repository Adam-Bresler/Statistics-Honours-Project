
# rolling average according to court surface ---------------------------------

average_past_games <- function(data, columns, months = 24){
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
      matches <- matches%>%filter(matches$tournament_surface==matches$tournament_surface[j])
        
      if(j == 1 | nrow(matches)<=1){ #The first game has no ytd from before
        average <- as.data.frame(t(rep(0, length(columns))))
        colnames(average) <- colnames(data)[columns]
      }
      
      else{
        ind <- which(matches$Match_ID == dat$Match_ID[j])  #Find which j we are in matches, and throw older stuff away
       if (ind==1){
         average <- as.data.frame(t(rep(0, length(columns))))
         colnames(average) <- colnames(data)[columns]   
       }
      else{
          
        matches <- matches[1:(ind-1), ]
        average <- matches %>% select(columns) %>% summarise_if(is.numeric, mean)
       }
      }
      
      player_dat <- rbind(player_dat, cbind(dat[j, ], average))
    }
    
    return_matrix <- rbind(return_matrix, player_dat[-1, ])
  }
  
  return(return_matrix[-1, ])
}


# rolling_average_all <- read.csv("rolling_average_all.csv")
# rolling_average_all <- rolling_average_all[,-1]

# rolling_average_court <- average_past_games(rolling_average_all, 15:56)

# write.csv(rolling_average_court, file = "C:/Users/lukae/OneDrive/Documents/GitHub/Statistics-Honours-Project/rolling_average_court.csv")

# rolling_average_court <- read.csv("rolling_average_court.csv")
# rolling_average_court <- rolling_average_court[,-1]

# Head to head ---------------------------------------------------------------


library(installr)


data_H_2_H <-  rolling_average_court
  
colnames(data_H_2_H)[c(57:98)] <- paste("rolling_average", colnames(data_H_2_H[15:56]), sep="_")
colnames(data_H_2_H)[c(99:140)] <- paste("rolling_average_court", colnames(data_H_2_H[15:56]), sep="_")

H_2_H <- data_H_2_H %>%
  group_by(Match_ID)%>%arrange(.by_group = TRUE)

first_player<-H_2_H[seq(1,52522,2),]

second_player<-H_2_H[seq(2,52522,2),]


# All previous games between any 2 players -----------------------------------


w_dat <- read.csv("finaldata2.csv")
w_dat <- w_dat[,-1]
w_dat <- w_dat[,c(36,39)]

# bring back the tournament date 
for (i in 1:nrow(first_player)){
  first_player$tournament_date[i] <- w_dat[which(w_dat$tourney_id==first_player$tourney_id[i])[1],2]
}


# order data

first_player_3 <-first_player %>%
  group_by(tournament_date)%>%arrange(.by_group = TRUE)

for (i in unique(first_player_3$tourney_id)){
  indices <- which(first_player_3$tourney_id==i)
  first_player_3[indices,] <- first_player_3[indices,]%>%arrange(desc(Match_order))
}


H_2_H <- first_player_3


head_to_head <- function(player1, player2){
  ind <- which(H_2_H$name == player1 & H_2_H$Opponent  == player2 )
  
  if(is.empty(ind)){
    return("Players have never met before.") 
  }
  
  else{
    
    return(ind)
  }
  
}

head_to_head("Rafael Nadal","Roger Federer")




for (i in unique(H_2_H$name)){
 ind <- which(H_2_H$name == i)
 head_to_head(H_2_H$name[ind],H_2_H$Opponent[ind])
}



