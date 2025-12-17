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

angle_diff <- function(a, b) {
  diff <- abs(a - b) %% 360
  ifelse(diff > 180, 360 - diff, diff)
}

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
         Y = ifelse(play_direction == "left", 53.3-y, y),
         ball_land_x = ifelse(play_direction == "left", 120-ball_land_x, ball_land_x),
         ball_land_y = ifelse(play_direction == "left", 160/3 -ball_land_y, ball_land_y))    ## Standardized Y

outputs <- outputs %>%
  left_join(playdirs, by = c("game_id", "play_id"))
#need to left join play direction onto here so that x and y can be standardized
outputs <- outputs %>% 
  mutate(X = ifelse(play_direction == "left", 120-x, x), ## Standardizes X
         Y = ifelse(play_direction == "left", 53.3-y, y))    ## Standardized Y



receivers <- data %>%
  filter(player_role == "Targeted Receiver") %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, X, Y, player_position, ball_land_x, ball_land_y, recDir = dir, recO = o)

# Get defenders at the same frame
defenders <- data %>%
  filter(player_position %in% c("CB", "SS","FS", "DB", "ILB", "LB", "MLB")) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = Y, defenderX = X, player_position, defDir = dir, defO = o) 


joined <- inner_join(receivers, defenders, by = c("game_id", "play_id", "frame_id")) 

joined <- joined %>%
  group_by(game_id, play_id) %>%
  arrange(frame_id, .by_group = TRUE) %>%
  # compute valid flag based on the last frame distance of the play
  mutate(
    lastrecDir = last(recDir),
    lastdefDir = last(defDir),
    lastrecO = last(recO),
    lastdefO = last(defO)
  ) %>%
  ungroup()

dirOs <- joined %>%
  select(game_id, play_id, lastrecDir, lastdefDir, lastrecO, lastdefO) %>%
  distinct(game_id, play_id, .keep_all = TRUE)

dirOsAdj <- dirOs %>%
  mutate(
    lastrecDir = ((lastrecDir - 90) * -1) %% 360,
    lastdefDir = ((lastdefDir - 90) * -1) %% 360,
    lastrecO = ((lastrecO - 90) * -1) %% 360,
    lastdefO = ((lastdefO - 90) * -1) %% 360,
  )


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
    lastDistance = last(distance) ) %>%
  ungroup() %>%
  left_join(joinedtargs, by = c("game_id", "play_id")) %>%
  filter(lastDistance >= 4)

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

balllands <- data %>%
  select(game_id, play_id, ball_land_x, ball_land_y) %>%
  distinct(game_id, play_id, .keep_all = TRUE)

joinedoutputs <- joinedoutputs %>%
  left_join(balllands, by = c("game_id", "play_id"))

checkdefenders <- joinedoutputs %>%
  group_by(game_id, play_id) %>%        # group by play + receiver
  summarise(num_defenders = n_distinct(defenderId), .groups = "drop") %>%
  filter(num_defenders > 1)

joinedoutputs <- joinedoutputs %>%
  anti_join(checkdefenders,
            by = c("game_id", "play_id"))

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

finaloutput <- finaloutput %>%
  group_by(game_id, play_id) %>%
  mutate(
    framesToGo = last(frame_id) - frame_id
  )

complete <- supdata %>%
  select(game_id, play_id, pass_result, team_coverage_man_zone) %>%
  mutate(
    target = ifelse(pass_result == "C", 1, 0),
    man = ifelse(team_coverage_man_zone == "MAN_COVERAGE", 1, 0)
  ) 
finaloutput <- finaloutput %>%
  left_join(complete, by = c("game_id", "play_id"))

finaloutput <- finaloutput %>%
  group_by(game_id, play_id) %>%
  mutate(
    frechet_cumulative = ifelse(is.na(frechet_cumulative), 0, frechet_cumulative),
    frame_deviation = ifelse(is.na(frame_deviation), lag(frame_deviation), frame_deviation),
    angle_deviation_deg = ifelse(is.na(angle_deviation_deg), lag(angle_deviation_deg), angle_deviation_deg)
    ) %>%
  ungroup()

checkdefenders <- finaloutput %>%
  group_by(game_id, play_id) %>%        # group by play + receiver
  summarise(num_defenders = n_distinct(defenderId), .groups = "drop") %>%
  filter(num_defenders > 1)

finaloutput <- finaloutput %>%
  anti_join(checkdefenders,
            by = c("game_id", "play_id"))

finaloutput <- finaloutput %>%
  left_join(dirOsAdj, by = c("game_id", "play_id"))


write.csv(finaloutput, "FinalOutput.csv", row.names = FALSE)


testpreds <- read.csv("og_preds_data (1).csv")
slpreds <- read.csv("sl_preds_data (1).csv")

slpreds <- slpreds %>%
  rename(slpreds = pass_result_prob) %>%
  select(game_id, play_id, frame_id, slpreds)

testpreds <- testpreds %>%
  left_join(slpreds, by = c("game_id", "play_id", "frame_id"))

testpreds <- testpreds %>%
  mutate(
    deltaPred = pass_result_prob - slpreds
  )


  
