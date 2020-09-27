
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


data_H_2_H <-read.csv("final_data_no_H2H.csv")
data_H_2_H <- data_H_2_H[,-1]
  
# colnames(data_H_2_H)[c(57:98)] <- paste("rolling_average", colnames(data_H_2_H[15:56]), sep="_")
# colnames(data_H_2_H)[c(99:140)] <- paste("rolling_average_court", colnames(data_H_2_H[15:56]), sep="_")

# H_2_H <- data_H_2_H %>%
#   group_by(Match_ID)%>%arrange(.by_group = TRUE)
# 
# first_player<-H_2_H[seq(1,52522,2),]
# 
# second_player<-H_2_H[seq(2,52522,2),]


# All previous games between any 2 players -----------------------------------


# w_dat <- read.csv("finaldata2.csv")
# w_dat <- w_dat[,-1]
# w_dat <- w_dat[,c(36,39)]
# 
# # bring back the tournament date 
# for (i in 1:nrow(first_player)){
#   first_player$tournament_date[i] <- w_dat[which(w_dat$tourney_id==first_player$tourney_id[i])[1],2]
# }


# order data

H2H <-data_H_2_H %>%
  group_by(tournament_date)%>%arrange(.by_group = TRUE)

for (i in unique(data_H_2_H$tourney_id)){
  indices <- which(H2H$tourney_id==i)
  H2H[indices,] <- H2H[indices,]%>%arrange(desc(Match_order))
}


H_2_H <- H2H


head_to_head <- function(player1, player2){
  ind <- which(H_2_H$Player_A == player1 & H_2_H$Player_B  == player2 )
  
  if(is.empty(ind)|length(ind)==1){
    return(list("Players have never met before.",ind))
  }
  else{
    
    return(ind)
  }
  
}

head_to_head("Roger Federer","Lloyd Harris")




# creating head to head record -----------------------------------------------

H_2_H$head_to_head_record <- 0

for (i in unique(H_2_H$Player_A)){
ind <- which(H_2_H$Player_A == i)
opponents <- unique(H_2_H$Player_B[ind])

 for (j in 1:length(opponents)){
   matches <- head_to_head(i,opponents[j])
   if (matches[1]=="Players have never met before."){
     H_2_H$head_to_head_record[matches[[2]]] <- NA
   }
   else{
     wins=0
     total=0
     H_2_H$head_to_head_record[matches[1]]<-NA
     for (k in 2:length(matches)){
       total=total+1
       if(H_2_H$wl[matches[k-1]]=="Player A"){
       wins=wins+1 
       }
       h2h_percent=100*(wins/total) 
       H_2_H$head_to_head_record[matches[k]]<-h2h_percent
     }
   }
 }
}

# testing

indices <- which(H_2_H$Player_A == "Rafael Nadal" & H_2_H$Player_B  == "Roger Federer" )

H_2_H[indices,c(2,3,4,436)]


indices2 <- which(H_2_H$Player_A  == "David Ferrer" & H_2_H$Player_B  == "Andy Murray" )

H_2_H[indices2,c(2,3,4,436)]


indices_overall <- which(H_2_H$Player_A== "Rafael Nadal" & H_2_H$Player_B  == "Novak Djokovic" )

H_2_H[indices_overall,c(2,3,4,436)]

length(which(is.na(H_2_H$head_to_head_record)))

# head to head per court surface ---------------------------------------------


clay <- H_2_H[which(H_2_H$tournament_surface=="Clay"),]

grass <-H_2_H[which(H_2_H$tournament_surface=="Grass"),]

hard_court <-H_2_H[which(H_2_H$tournament_surface=="Hard"),]


H_2_H$head_to_head_record_court_surface <- 0
clay$head_to_head_record_court_surface <- 0
grass$head_to_head_record_court_surface <- 0
hard_court$head_to_head_record_court_surface <- 0


# clay

head_to_head_clay <- function(player1, player2){
  ind <- which(clay$Player_A == player1 & clay$Player_B  == player2 )
  
  if(is.empty(ind)|length(ind)==1){
    return(list("Players have never met before.",ind))
  }
  else{
    
    return(ind)
  }
  
}

for (i in unique(clay$Player_A)){
  ind_clay <- which(clay$Player_A == i)
  opponents <- unique(clay$Player_B[ind_clay])
  
  for (j in 1:length(opponents)){
    matches <- head_to_head_clay(i,opponents[j])
    if (matches[1]=="Players have never met before."){
      clay$head_to_head_record_court_surface[matches[[2]]] <- NA
    }
    else{
      wins=0
      total=0
      clay$head_to_head_record_court_surface[matches[1]]<-NA
      for (k in 2:length(matches)){
        total=total+1
        if(clay$wl[matches[k-1]]=="Player A"){
          wins=wins+1 
        }
        h2h_percent=100*(wins/total) 
        clay$head_to_head_record_court_surface[matches[k]]<-h2h_percent
      }
    }
  }
}


indices_clay <- which(clay$Player_A == "Rafael Nadal" & clay$Player_B  == "Novak Djokovic" )

clay[indices_clay,c(2,3,4,437)]

# grass

head_to_head_grass <- function(player1, player2){
  ind <- which(grass$Player_A == player1 & grass$Player_B  == player2 )
  
  if(is.empty(ind)|length(ind)==1){
    return(list("Players have never met before.",ind))
  }
  else{
    
    return(ind)
  }
  
}

for (i in unique(grass$Player_A)){
  ind_grass <- which(grass$Player_A == i)
  opponents <- unique(grass$Player_B[ind_grass])
  
  for (j in 1:length(opponents)){
    matches <- head_to_head_grass(i,opponents[j])
    if (matches[1]=="Players have never met before."){
      grass$head_to_head_record_court_surface[matches[[2]]] <- NA
    }
    else{
      wins=0
      total=0
      grass$head_to_head_record_court_surface[matches[1]]<-NA
      for (k in 2:length(matches)){
        total=total+1
        if(grass$wl[matches[k-1]]=="Player A"){
          wins=wins+1 
        }
        h2h_percent=100*(wins/total) 
        grass$head_to_head_record_court_surface[matches[k]]<-h2h_percent
      }
    }
  }
}


indices_grass <- which(grass$Player_A == "Rafael Nadal" & grass$Player_B  == "Novak Djokovic" )

grass[indices_grass,c(2,3,4,437)]

# hard court

head_to_head_hard_court <- function(player1, player2){
  ind <- which(hard_court$Player_A == player1 & hard_court$Player_B  == player2 )
  
  if(is.empty(ind)|length(ind)==1){
    return(list("Players have never met before.",ind))
  }
  else{
    
    return(ind)
  }
  
}

for (i in unique(hard_court$Player_A)){
  ind_hard_court <- which(hard_court$Player_A == i)
  opponents <- unique(hard_court$Player_B[ind_hard_court])
  
  for (j in 1:length(opponents)){
    matches <- head_to_head_hard_court(i,opponents[j])
    if (matches[1]=="Players have never met before."){
      hard_court$head_to_head_record_court_surface[matches[[2]]] <- NA
    }
    else{
      wins=0
      total=0
      hard_court$head_to_head_record_court_surface[matches[1]]<-NA
      for (k in 2:length(matches)){
        total=total+1
        if(hard_court$wl[matches[k-1]]=="Player A"){
          wins=wins+1 
        }
        h2h_percent=100*(wins/total) 
        hard_court$head_to_head_record_court_surface[matches[k]]<-h2h_percent
      }
    }
  }
}


indices_hard_court <- which(hard_court$Player_A == "Rafael Nadal" & hard_court$Player_B  == "Novak Djokovic" )
hard_court[indices_hard_court,c(2,3,4,437)]

# combining and adding to overall data set 


grass_H2H <- grass[,c(1,2,3,4,6,7,9,10,12,437)]
clay_H2H <- clay[,c(1,2,3,4,6,7,9,10,12,437)]
hard_court_H2H <- hard_court[,c(1,2,3,4,6,7,9,10,12,437)]

court_H2H <- rbind.data.frame(grass_H2H,clay_H2H,hard_court_H2H)


court_H2H <-court_H2H %>%
  group_by(tournament_date)%>%arrange(.by_group = TRUE)

# for (i in unique(court_H2H$tourney_id)){
#   indices <- which(court_H2H$tourney_id==i)
#   court_H2H[indices,] <- court_H2H[indices,]%>%arrange(desc(Match_order))
# }

H_2_H$head_to_head_record_court_surface <- court_H2H$head_to_head_record_court_surface


indices <- which(H_2_H$Player_A == "Rafael Nadal" & H_2_H$Player_B  == "Novak Djokovic" )

H_2_H[indices,c(2,4,13,436,437)]

str(H_2_H$head_to_head_record)
str(H_2_H$head_to_head_record_court_surface)


# write.csv(H_2_H, file = "C:/Users/lukae/OneDrive/Documents/GitHub/Statistics-Honours-Project/Data/final_predictive_data.csv")




