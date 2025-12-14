library(dplyr)
library(tidyverse)
library(nflfastR)
library(nflreadr)
supdata <- read.csv("114239_nfl_competition_files_published_analytics_final/supplementary_data.csv")
week1 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w01.csv")
week1out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w01.csv")
week2out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w02.csv")
week3out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w03.csv")
week4out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w04.csv")
week5out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w05.csv")
week6out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w06.csv")
week7out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w07.csv")
week8out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w08.csv")
week9out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w09.csv")
week10out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w10.csv")
week11out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w11.csv")
week12out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w12.csv")
week13out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w13.csv")
week14out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w14.csv")
week15out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w15.csv")
week16out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w16.csv")
week17out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w17.csv")
week18out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w18.csv")


week2 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w02.csv")
week3 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w03.csv")
week4 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w04.csv")
week5 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w05.csv")
week6 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w06.csv")
week7 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w07.csv")
week8 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w08.csv")
week9 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w09.csv")
week10 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w10.csv")
week11 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w11.csv")
week12 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w12.csv")
week13 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w13.csv")
week14 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w14.csv")
week15 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w15.csv")
week16 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w16.csv")
week17 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w17.csv")
week18 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w18.csv")


data <- rbind(week1, week2,week3,week4, week5, week6, week7, week8, week9,week10, week11, week12, 
              week13, week14, week15, week16, week17, week18)
outputs <- rbind(week1out,week2out, week3out, week4out, week5out, week6out, week7out, week8out,
                 week9out, week10out, week11out, week12out, week13out, week14out, week15out, week16out,
                 week17out, week18out)

playdirs <- data %>%
  select(game_id, play_id, play_direction) %>%
  distinct(game_id, play_id, .keep_all = TRUE)
data <- data %>% 
  mutate(X = ifelse(play_direction == "left", 120-x, x), ## Standardizes X
         Y = ifelse(play_direction == "right", 160/3-y, y))    ## Standardized Y

outputs <- outputs %>%
  left_join(playdirs, by = c("game_id", "play_id"))
#need to left join play direction onto here so that x and y can be standardized
outputs <- outputs %>% 
  mutate(X = ifelse(play_direction == "left", 120-x, x), ## Standardizes X
         Y = ifelse(play_direction == "right", 160/3-y, y))    ## Standardized Y

receivers <- data %>%
  filter(player_role == "Targeted Receiver") %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, X, Y, player_position, ball_land_x, ball_land_y)

# Get defenders at the same frame
defenders <- data %>%
  filter(player_position %in% c("CB", "SS","FS", "DB", "ILB", "LB", "MLB")) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = Y, defenderX = X, player_position)


joined <- inner_join(receivers, defenders, by = c("game_id", "play_id", "frame_id")) 

joinedtargs <- joined %>%
  left_join(targetdata, by = c("game_id", "play_id", "defenderId" = "nfl_id")) %>%
  filter(targeted_defender == TRUE) 

joinedtargs <- joinedtargs %>%
  mutate(
    distance = sqrt((X - defenderX)^2 + (Y - defenderY)^2)
  ) 

joinedvalid <- joinedtargs %>%
  group_by(game_id, play_id) %>%
  summarise(
    lastDistance = last(distance) 
  ) %>%
  ungroup() %>%
  left_join(joinedtargs, by = c("game_id", "play_id")) %>%
  filter(lastDistance >= 5)

outputsvalid <- outputs %>%
  semi_join(joinedvalid, by = c("game_id", "play_id"))

outputsvalid <- outputsvalid %>%
  left_join(playerinfo, by = c("nfl_id"))

receiversoutputs <- outputsvalid %>%
  filter(player_position %in% c("RB", "WR", "TE")) %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, X, Y, player_position)

# Get defenders at the same frame
defendersoutputs <- outputsvalid %>%
  filter(player_position %in% c("CB", "SS","FS", "DB", "ILB", "LB", "MLB")) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = Y, defenderX = X, player_position)

joinedoutputs <- inner_join(receiversoutputs, defendersoutputs, by = c("game_id", "play_id", "frame_id")) 

joinedoutputs <- joinedoutputs %>%
  left_join(targetdata, by = c("game_id", "play_id", "defenderId" = "nfl_id")) %>%
  filter(targeted_defender == TRUE) 

joinedoutputs <- joinedoutputs %>%
  left_join(balllands, by = c("game_id", "play_id"))

pbp <- load_pbp(seasons = 2023)

colnames(pbp)

compprobs <- pbp %>%
  select(old_game_id, play_id, cp) %>%
  filter(!is.na(cp)) %>%
  mutate(
    game_id = as.integer(old_game_id)
  ) %>%
  select(-old_game_id)

finaloutput <- frechetdistances %>%
  left_join(compprobs, by = c("game_id", "play_id"))

write.csv(finaloutput, "FinalOutput.csv", row.names = FALSE)

