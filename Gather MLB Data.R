#Top Regular Season Home Run Hitters: Judge, Schwarber, Alonso, Trout, Bonds
library(baseballr)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(tidyverse)

#ID LookUps
judgeLookup <- playerid_lookup("Judge","Aaron") #Judge
schwarberLookup <- playerid_lookup("Schwarber","Kyle") #Schwarber
alonsoLookup <- playerid_lookup("Alonso","Pete") #Alonso
troutLookup <- playerid_lookup("Trout", "Mike") #Trout
bondsLookup <- playerid_lookup("Bonds","Barry")

#Fangraphs game log data
judge <- fg_batter_game_logs(judgeLookup["fangraphs_id"],2022) %>% mutate(gameNum = row_number()) #FanGraphs Game Logs 
schwarber <- fg_batter_game_logs(schwarberLookup["fangraphs_id"],2022) %>% mutate(gameNum = row_number())
alonso <- fg_batter_game_logs(alonsoLookup["fangraphs_id"],2022) %>% mutate(gameNum = row_number())
trout <- fg_batter_game_logs(troutLookup["fangraphs_id"],2022) %>% mutate(gameNum = row_number())
bonds <- fg_batter_game_logs(1109,2001) %>% mutate(gameNum = row_number())
fangraphsLogs <- bind_rows(judge,schwarber,alonso,trout,bonds)


#Baseball Savant statcast data
judge2 <- scrape_statcast_savant(start_date = "2022-04-07",
                                 end_date = "2022-10-02",
                                 playerid=judgeLookup["mlbam_id"],
                                 player_type = "batter")

schwarber2 <- scrape_statcast_savant(start_date = "2022-04-07",
                                     end_date = "2022-10-02",
                                     playerid=schwarberLookup["mlbam_id"],
                                     player_type = "batter")

alonso2 <- scrape_statcast_savant(start_date = "2022-04-07",
                                  end_date = "2022-10-02",
                                  playerid=alonsoLookup["mlbam_id"],
                                  player_type = "batter")

trout2 <- scrape_statcast_savant(start_date = "2022-04-07",
                                end_date = "2022-10-02",
                                playerid=troutLookup["mlbam_id"],
                                player_type = "batter")

savantStats <- bind_rows(judge2,schwarber2,alonso2,trout2)
savantStats$player_name <- sub("(\\w+),\\s(\\w+)","\\2 \\1",savantStats$player_name)


#Baseball Reference season total stats
judge3 <- bref_daily_batter("2022-04-07","2022-10-02") %>% filter(Name == "Aaron Judge")
schwarber3 <- bref_daily_batter("2022-04-07","2022-10-02") %>% filter(Name == "Kyle Schwarber")
alonso3 <- bref_daily_batter("2022-04-07","2022-10-02") %>% filter(Name == "Pete Alonso")
trout3 <- bref_daily_batter("2022-04-07","2022-10-02") %>% filter(Name == "Mike Trout")
baseref <- bind_rows(judge3,schwarber3,alonso3,trout3)







