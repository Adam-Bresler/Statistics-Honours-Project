# Read in data ---------------------------------------------------------------
library(readr)
colnames_match_stats <- c("match_id",
                    "tourney_slug",
                    "match_stats_url_suffix",
                    "match_time",
                    "match_duration",
                    "winner_slug",
                    "winner_serve_rating",
                    "winner_aces",
                    "winner_double_faults",
                    "winner_first_serves_in",
                    "winner_first_serves_total",
                    "winner_first_serve_points_won",
                    "winner_first_serve_points_total",
                    "winner_second_serve_points_won",
                    "winner_second_serve_points_total",
                    "winner_break_points_saved",
                    "winner_break_points_serve_total",
                    "winner_service_games_played",
                    "winner_return_rating",
                    "winner_first_serve_return_won",
                    "winner_first_serve_return_total",
                    "winner_second_serve_return_won",
                    "winner_second_serve_return_total",
                    "winner_break_points_converted",
                    "winner_break_points_return_total",
                    "winner_return_games_played",
                    "winner_service_points_won",
                    "winner_service_points_total",
                    "winner_return_points_won",
                    "winner_return_points_total",
                    "winner_total_points_won",
                    "winner_total_points_total",
                    "loser_slug",
                    "loser_serve_rating",
                    "loser_aces",
                    "loser_double_faults",
                    "loser_first_serves_in",
                    "loser_first_serves_total",
                    "loser_first_serve_points_won",
                    "loser_first_serve_points_total",
                    "loser_second_serve_points_won",
                    "loser_second_serve_points_total",
                    "loser_break_points_saved",
                    "loser_break_points_serve_total",
                    "loser_service_games_played",
                    "loser_return_rating",
                    "loser_first_serve_return_won",
                    "loser_first_serve_return_total",
                    "loser_second_serve_return_won",
                    "loser_second_serve_return_total",
                    "loser_break_points_converted",
                    "loser_break_points_return_total",
                    "loser_return_games_played",
                    "loser_service_points_won",
                    "loser_service_points_total",
                    "loser_return_points_won",
                    "loser_return_points_total",
                    "loser_total_points_won",
                    "loser_total_points_total")

colnames_match_scores <- c("tourney_year_id",
                           "tourney_order",
                           "tourney_name",
                           "tourney_slug",
                           "tourney_url_suffix",
                           "start_date",
                           "start_year",
                           "start_month",
                           "start_day",
                           "end_date",
                           "end_year",
                           "end_month",
                           "end_day",
                           "currency",
                           "prize_money",
                           "match_index",
                           "tourney_round_name",
                           "round_order",
                           "match_order",
                           "winner_name",
                           "winner_player_id",
                           "winner_slug",
                           "loser_name",
                           "loser_player_id",
                           "loser_slug",
                           "winner_seed",
                           "loser_seed",
                           "match_score_tiebreaks",
                           "winner_sets_won",
                           "loser_sets_won",
                           "winner_games_won",
                           "loser_games_won",
                           "winner_tiebreaks_won",
                           "loser_tiebreaks_won",
                           "match_id",
                           "match_stats_url_suffix")

colnames_tourney_stats<-c("tourney_year_id",
"tourney_order",
"tourney_type",
"tourney_name",
"tourney_id",
"tourney_slug",
"tourney_location",
"tourney_date",
"year",
"tourney_month",
"tourney_day",
"tourney_singles_draw",
"tourney_doubles_draw",
"tourney_conditions",
"tourney_surface",
"tourney_fin_commit_raw",
"currency",
"tourney_fin_commit",
"tourney_url_suffix",
"singles_winner_name",
"singles_winner_url",
"singles_winner_player_slug",
"singles_winner_player_id",
"doubles_winner_1_name",
"doubles_winner_1_url",
"doubles_winner_1_player_slug",
"doubles_winner_1_player_id",
"doubles_winner_2_name",
"doubles_winner_2_url",
"doubles_winner_2_player_slug",
"doubles_winner_2_player_id")

tourney_stats_2010_2019 <- read_csv("tournaments_2010-2019.csv", col_names = colnames_tourney_stats)

match_stats_2019 <- read_csv("match_stats_2019.csv", col_names = colnames_match_stats)
match_stats_2018 <- read_csv("match_stats_2018.csv", col_names = colnames_match_stats)
match_stats_2017 <- read_csv("match_stats_2017.csv", col_names = colnames_match_stats)
match_stats_2016 <- read_csv("match_stats_2016.csv", col_names = colnames_match_stats)
match_stats_2015 <- read_csv("match_stats_2015.csv", col_names = colnames_match_stats)
match_stats_2014 <- read_csv("match_stats_2014.csv", col_names = colnames_match_stats)
match_stats_2013 <- read_csv("match_stats_2013.csv", col_names = colnames_match_stats)
match_stats_2012 <- read_csv("match_stats_2012.csv", col_names = colnames_match_stats)
match_stats_2011 <- read_csv("match_stats_2011.csv", col_names = colnames_match_stats)
match_stats_2010 <- read_csv("match_stats_2010.csv", col_names = colnames_match_stats)

match_stats <- rbind(match_stats_2010, match_stats_2011, match_stats_2012,
                     match_stats_2013, match_stats_2014, match_stats_2015,
                     match_stats_2016, match_stats_2017, match_stats_2018,
                     match_stats_2019)

rm(match_stats_2010, match_stats_2011, match_stats_2012,
      match_stats_2013, match_stats_2014, match_stats_2015,
      match_stats_2016, match_stats_2017, match_stats_2018,
      match_stats_2019)

match_scores <- read_csv("match_scores_2010-2019.csv", col_names = colnames_match_scores)

# Combine --------------------------------------------------------------------
match <- merge(match_stats, match_scores, by = "match_stats_url_suffix")

# Remove Qualifying ----------------------------------------------------------
match <- match[-which(grepl("Qualifying", match$tourney_round_name)), ]

# Get rid of columns ---------------------------------------------------------
colnames(match)
rid <- c("match_stats_url_suffix", "match_time", "winner_slug.x", "loser_slug.x", "tourney_order", "tourney_name", "tourney_slug.y",  "tourney_url_suffix", 
        "start_year", "start_month", "start_day", "end_year", "end_month", "end_day", "currency", "prize_money", "winner_player_id", "winner_slug.y", 
        "loser_player_id", "loser_slug.y", "match_id.y")

match <- match[, -which(names(match) %in% rid)]

# Randomly Assign ------------------------------------------------------------
for(i in 1:nrow(match)){
  
  u <- runif(1, 0, 1)
  
  if(u < 0.5){
    match$player1[i] <- "winner"
    match$player2[i] <- "loser"
  }
  
  else{
    match$player1[i] <- "loser"
    match$player2[i] <- "winner"
  }
  
}

head(match$player1)
head(match$player2)

colnames(match)

# Player names ---------------------------------------------------------------
# players<-as.vector(unique(c(unique(match$winner_name),
#                             unique(match$loser_name))))
# 
# id <- numeric()
# slug <- numeric()
# 
# for (i in 1:length(players)){
#   id[i] <- match_scores[which(match_scores$winner_name==players[i])[1],"winner_player_id"]
#   slug[i] <- match_scores[which(match_scores$winner_name==players[i])[1],"winner_slug"]
# }
# 
# for (i in which(is.na(id))){
#   id[i] <- match_scores[which(match_scores$loser_name==players[i])[1],"loser_player_id"]
#   slug[i] <- match_scores[which(match_scores$loser_name==players[i])[1],"loser_slug"]
# }
# 


# Build player data base -----------------------------------------------------

# While this of course works, it is incredibly slow. Use the csv instead, 
# is the output of this code anyway

# library (rvest)
# library(dplyr)
# library(stringr)
# library(httr)
# 
# url_base_1 <- 'https://www.atptour.com/en/players'
# url_end <- 'overview'
# 
# for(i in 1:nrow(players)){
#   id <- players$id[i]
#   slug <- players$slug[i]
#   
#   url <- paste(url_base_1, slug, sep = "/")
#   url <- paste(url, id, sep = "/")
#   url <- paste(url, url_end, sep = "/")
#   
#   players$url[i] <- url
#   
#   if(!http_error(url)) {
#   webpage <- read_html(url)
#   
#   player_data1 <- html_nodes(webpage,'.table-big-value')
#   player_data2 <- html_nodes(webpage,'.table-value')
#   
#   rank_data1 <- html_text(player_data1)
#   rank_data2 <- html_text(player_data2)
#   
#   rank_data1 <- gsub(" ", "", rank_data1)
#   rank_data1 <- gsub("\r", "", rank_data1)
#   rank_data1 <- gsub("\n", "", rank_data1)
#   
#   rank_data2 <- gsub(" ", "", rank_data2)
#   rank_data2 <- gsub("\r", "", rank_data2)
#   rank_data2 <- gsub("\n", "", rank_data2)
#   
#   players$DOB[i] <- rank_data1[1]
#   players$year_pro[i] <- rank_data1[2]
#   players$weight[i] <- rank_data1[3]
#   players$height[i] <- rank_data1[4]
#   
#   players$place_OB[i] <- rank_data2[1]
#   players$residence[i] <- rank_data2[2]
#   players$plays[i] <- rank_data2[3]
#   players$coach[i] <- rank_data2[4]
#   }
#   
#   else{
#     players$DOB[i] <- NA
#     players$year_pro[i] <- NA
#     players$weight[i] <- NA
#     players$height[i] <- NA
#     
#     players$place_OB[i] <- NA
#     players$residence[i] <- NA
#     players$plays[i] <- NA
#     players$coach[i] <- NA
#   }
#   
# 
# }
# 
# players$slug <- unlist(players$slug)
# players$id <- unlist(players$id)
# players$players <- unlist(players$players)
# str(players)
# 
# write.csv(players, file = "C:/Users/Adam Bresler/Documents/2020/Honours/Project/Data/csv/Working/players.csv")

# Read in and clean players data ---------------------------------------------
library(stringr)

players <- read.csv("players.csv")
players <- players[,-c(1, 10, 11, 13)]

players$weight <- str_extract(players$weight, "\\([^()]+\\)")
players$weight <- substr(players$weight, 2,nchar(players$weight)-3)

players$height <- str_extract(players$height, "\\([^()]+\\)")
players$height <- substr(players$height, 2,nchar(players$height)-3)

players$DOB <- str_extract(players$DOB, "\\([^()]+\\)")
players$DOB <- substr(players$DOB, 2,nchar(players$DOB)-1)
players$DOB <- lubridate::ymd(players$DOB)

for(i in 1:nrow(players)){
  
  temp <- players$plays[i]
  split <- unlist(strsplit(temp, ","))
  
  players$hand[i] <- split[1]
  players$back[i] <- split[2]
}

players <- players[,-9]

# Find match order -----------------------------------------------------------

match$start_date <- lubridate::ymd(match$start_date)
match$match_order <- substr(match$match_index, 3, 5)
match$match_order <- as.numeric(match$match_order)

match <- match[order(match$start_date, decreasing = TRUE),]

# Get a players last n games -------------------------------------------------

last_game <- function(player, n = NA){
  
  ind <- which(match$winner_name == player | match$loser_name == player)
  
  if(is.na(n)){
    n = length(ind)
  }
  
  else{
    n = n
  }
  
  ind <- ind[1:n]
  
  match_n <- match[ind, ]
  
  data <- matrix(0, nrow = n, ncol = 37)
  
  
  winner <- grep("winner", colnames(match))
  loser <- grep("loser", colnames(match))
  
  colnames(data) <- c("duration", substring(colnames(match[winner]), 8), "wl", "tourney_id", "Match_order", "Opponent", "Match_ID")
  
  
  
  for (i in 1:n) {

    if(match_n$winner_name[i] == player){
      data[i,1:32] <- t(as.vector(match_n[i, c(3, winner)]))
      data[i, 33] <- 'winner'
      data[i, 34] <- match_n[i, 56]
      data[i, 35] <- match_n[i, 62]
      data[i, 36] <- match_n[i, 64]
      data[i, 37] <- match_n[i, 1]
    }
    
    else{
      data[i,1:32] <- t(as.vector(match_n[i, c(3, loser)]))
      data[i, 33] <- 'loser'
      data[i, 34] <- match_n[i, 56]
      data[i, 35] <- match_n[i, 62]
      data[i, 36] <- match_n[i, 63]
      data[i, 37] <- match_n[i, 1]
    }
  }
  
  data <- as.data.frame(data)
  cols <- c(1:27, 29:32)
  data[, cols] <- sapply(data[, cols], as.numeric)  
  return(data)
}

t <- last_game("Roger Federer")

# All previous games between any 2 players -----------------------------------
library(installr)

past_matches <- function(player1, player2){
  ind <- which(match$winner_name == player1 & match$loser_name == player2 | match$winner_name == player2 & match$loser_name == player1)
  
  if(is.empty(ind)){
    return("Players have never met.") # Replace with similar matches?
  }
  
  else{
  match_past <- match[ind, ]
  
  return(match_past)
  }
  
}
player2 <- "Emilio Nava"

past_matches("Roger Federer", "Emilio Nava")

# Similar past matches -------------------------------------------------------

similar_matches <- function(player, hand){
  
ind <- which(match$winner_name == player | match$loser_name == player)
player_games <- match[ind, ]

ind_2 <- numeric()
j <- 1
 
for(i in 1:length(ind))  {
  
  if(match$winner_name[ind[i]] == player){
    opponent <- player_games$loser_name[i]
    opp_hand <- players[which(players$players == opponent),]$hand
    
    if(opp_hand == hand){
      ind_2[j] <- i
      j = j +1
    }
    
    
  }
  
  else{
    opponent <- player_games$winner_name[i]
    opp_hand <- players[which(players$players == opponent),]$hand
    
    if(opp_hand == hand){
      ind_2[j] <- i
      j = j +1
    }
    
    
  }
}  


data <- matrix(0, nrow = length(ind_2), ncol = 33)


winner <- grep("winner", colnames(match))
loser <- grep("loser", colnames(match))

colnames(data) <- c("duration", substring(colnames(match[winner]), 8), "wl")

for (i in 1:length(ind_2)) {
  
  if(player_games$winner_name[ind_2[i]] == player){
    data[i,1:32] <- t(as.vector(player_games[ind_2[i], c(3, winner)]))
    data[i, 33] <- 'winner'
  }
  
  else{
    data[i,1:32] <- t(as.vector(player_games[ind_2[i], c(3, loser)]))
    data[i, 33] <- 'loser'
  }
}

data <- as.data.frame(data)
cols <- c(1:27, 29:32)
data[, cols] <- sapply(data[, cols], as.numeric)  

  
  return(data)
}

#Begin final data frame ------------------------------------------------------

# final_data <- as.data.frame(matrix(0, nrow = 1, ncol = 37))
# colnames(final_data) <- colnames(last_game("Roger Federer"))
# 
# for(i in 1:length(players$players)){
#   work <- last_game(players$players[i])
#   final_data <- rbind(final_data, work)
# }
# final_data <- final_data[, c(37, 28, 36, 33, 1:27, 29:32, 34:35)]
# final_data <- final_data[-1, ]
# write.csv(final_data, file = "C:/Users/Adam Bresler/Documents/2020/Honours/Project/Data/Working/finaldata.csv")

# adding tournament data -----------------------------------------------------

# for (i in 1:nrow(final_data)) {
#   final_data$tournament_name[i] <-
#     tourney_stats_2010_2019[which(final_data$tourney_id[i] == tourney_stats_2010_2019$tourney_year_id), "tourney_name"]
#   final_data$tournament_date[i] <-
#     tourney_stats_2010_2019[which(final_data$tourney_id[i] == tourney_stats_2010_2019$tourney_year_id), "tourney_date"]
#   final_data$tournament_type[i] <-
#     tourney_stats_2010_2019[which(final_data$tourney_id[i] == tourney_stats_2010_2019$tourney_year_id), "tourney_type"]
#   final_data$tournament_conditions[i] <-
#     tourney_stats_2010_2019[which(final_data$tourney_id[i] == tourney_stats_2010_2019$tourney_year_id), "tourney_conditions"]
#   final_data$tournament_surface[i] <-
#     tourney_stats_2010_2019[which(final_data$tourney_id[i] == tourney_stats_2010_2019$tourney_year_id), "tourney_surface"]
# }
# 
# final_data$tournament_name <- unlist(final_data$tournament_name)
# final_data$tournament_date <- unlist(final_data$tournament_date)
# final_data$tournament_type <- unlist(final_data$tournament_type)
# final_data$tournament_conditions <-
#   unlist(final_data$tournament_conditions)
# final_data$tournament_surface <- unlist(final_data$tournament_surface)
# 
# write.csv(final_data, file = "C:/Users/Adam Bresler/Documents/2020/Honours/Project/Data/Working/data_no_ytd.csv")








# Ordering within each player ------------------------------------------------

final_data <- read.csv('data_no_ytd.csv')
final_data <- final_data[, -1]

players_names<-unique(final_data$name)


final_data_2 <- matrix(0,nrow=1,ncol=42)
colnames(final_data_2) <- colnames(final_data)

for(i in players_names){
  
  current_data <- final_data[which(final_data$name==i),]
  rows<- order(final_data[which(final_data$name==i),"tournament_date"],decreasing = FALSE)
  current_data <- current_data[rows,]
  tourney_dates<-unique(current_data$tournament_date)
  current_player_data<- matrix(0,nrow=1,ncol=42)
  colnames(current_player_data) <- colnames(final_data)
  
  for(i in tourney_dates){
    
    current_data2 <- current_data[which(current_data$tournament_date==i),]
    rows2<- order(current_data2[which(current_data2$tournament_date==i),"Match_order"],decreasing = TRUE)
    current_data2 <- current_data2[rows2,]
    current_player_data <- rbind(current_player_data,current_data2)
  }

  
  final_data_2 <- rbind(final_data_2,current_player_data[-1,])
  
}

final_data_2 <- final_data_2[-1,]

write.csv(final_data_2, file = "C:/Users/lukae/OneDrive/Documents/GitHub/Statistics-Honours-Project/Data/finaldata2.csv")






# Summing all previous data --------------------------------------------------
library(stringr)

final_data <- read.csv('finaldata2.csv')
final_data <- final_data[, -1]


for(i in 43:74){
  final_data[,i] <- NA
}
#38 before
name_last <- "NULL"

for(i in 1:nrow(final_data)){

  if(name_last == final_data$name[i]){
    final_data[i, 74] <- final_data[i - 1, 74] + 1
    final_data[i, 43:73] <- (final_data[i-1, 43:73]*final_data[i - 1, 74] + final_data[i, 5:35])/final_data[i, 74]
  }

  else{
    final_data[i, 43:73] <- final_data[i, 5:35]
    final_data[i, 74] <- 1
  }

  name_last <- final_data$name[i]
}

names <- c(paste("average", colnames(final_data[5:35]), sep="_"), "num_games")

for (i in 43:74) {
  colnames(final_data)[i] <- names[i-42]
}

write.csv(final_data, file = "C:/Users/Adam Bresler/Documents/GitHub/Statistics-Honours-Project/Data/finaldata.csv")


# X number average -----------------------------------------------------------

# last_game <- function(player, n = NA){
#   
#   ind <- which(match$winner_name == player | match$loser_name == player)
#   
#   if(is.na(n)){
#     n = length(ind)
#   }
#   
#   else{
#     n = n
#   }
#   
#   ind <- ind[1:n]
#   
#   match_n <- match[ind, ]
#   
#   data <- matrix(0, nrow = n, ncol = 35)
#   
#   
#   winner <- grep("winner", colnames(match))
#   loser <- grep("loser", colnames(match))
#   
#   colnames(data) <- c("duration", substring(colnames(match[winner]), 8), "wl", "tourney_id", "Match_order")
#   
#   
#   
#   for (i in 1:n) {
#     
#     if(match_n$winner_name[i] == player){
#       data[i,1:32] <- t(as.vector(match_n[i, c(3, winner)]))
#       data[i, 33] <- 'winner'
#       data[i, 34] <- match_n[i, 56]
#       data[i, 35] <- match_n[i, 62]
#     }
#     
#     else{
#       data[i,1:32] <- t(as.vector(match_n[i, c(3, loser)]))
#       data[i, 33] <- 'loser'
#       data[i, 34] <- match_n[i, 56]
#       data[i, 35] <- match_n[i, 62]
#     }
#   }
#   
#   data <- as.data.frame(data)
#   cols <- c(1:27, 29:32)
#   data[, cols] <- sapply(data[, cols], as.numeric)  
#   
#   # for(i in 1:nrow(final_data)){
#   #   
#   #   if(name_last == final_data$name[i]){
#   #     final_data[i, 72] <- final_data[i - 1, 72] + 1
#   #     final_data[i, 41:71] <- (final_data[i-1, 41:71]*final_data[i - 1, 72] + final_data[i, 3:33])/final_data[i, 72]
#   #   }
#   #   
#   #   else{
#   #     final_data[i, 41:71] <- final_data[i, 3:33]
#   #     final_data[i, 72] <- 1
#   #   }
#   #   
#   #   name_last <- final_data$name[i]
#   # }
#   # 
#   # names <- c(paste("average", colnames(final_data[3:33]), sep="_"), "num_games")
#   # 
#   # for (i in 41:72) {
#   #   colnames(final_data)[i] <- names[i-40]
#   # }
#   
#   
#   return(data)
# }

# Read in final data ---------------------------------------------------------
final_data <- read.csv("finaldata.csv", header = TRUE)
final_data <- final_data[,-1]
