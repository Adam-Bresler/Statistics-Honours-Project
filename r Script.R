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

last_game <- function(player, n){
  
  
  
  
  
  
}

