library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

testpreds <- read.csv("og_preds_data (1).csv")
slpreds <- read.csv("sl_preds_data (1).csv")

slpreds <- slpreds %>%
  rename(slpreds = pass_result_prob) %>%
  select(game_id, play_id, frame_id, slpreds)

testpreds <- testpreds %>%
  left_join(slpreds, by = c("game_id", "play_id", "frame_id"))

testpreds <- testpreds %>%
  mutate(
    deltaPred = pass_result_prob - slpreds,
    firstlas = first(pass_result_prob) - last(pass_result_prob)
  )

multimodel <- testpreds %>%
  left_join(finaloutput, by = c("game_id", "play_id", "frame_id")) %>%
  group_by(game_id, play_id, player_name.y, player_position.y) %>%
  summarise(
    totaldeltaPred = sum(deltaPred),
    frames = n(),
    deltaPredFrame = totaldeltaPred/frames
  ) %>%
  ungroup()



multimodeldata <- multimodel %>%
  left_join(supdata, by = c("game_id", "play_id"))

player_counts <- multimodeldata %>%
  group_by(player_name.y) %>%
  summarise(
    plays = n()
  ) %>%
  filter(plays >= 15)

multimodeldataclean<- multimodeldata %>%
  filter(player_name.y %in% player_counts$player_name.y)


multimodeldataclean <- multimodeldataclean %>%
  mutate(
    player_name.y = factor(player_name.y),
    player_position.y = factor(player_position.y),
    route_of_targeted_receiver = factor(route_of_targeted_receiver),
    team_coverage_man_zone = factor(team_coverage_man_zone),
    play_action = factor(play_action)
  )

dbmodel <- multimodeldataclean %>%
  filter(player_position.y == "CB" | player_position.y == "FS" | player_position.y == "SS")

lbmodel <- multimodeldataclean %>%
  filter(player_position.y == "ILB" | player_position.y == "LB" | player_position.y == "MLB")




dbmodel <-
  lmer(deltaPredFrame ~ 1 + (1 | player_name.y) + pass_length +  play_action +
         team_coverage_man_zone + frames,
       data = dbmodel)

summary(dbmodel)

lbmodel <-
  lmer(deltaPredFrame ~ 1 + (1 | player_name.y) + pass_length +  play_action +
         team_coverage_man_zone + frames,
       data = lbmodel)

summary(lbmodel)
  

dbtargdata <- ranef(dbmodel)
dbpatheffects <-
  data.frame(
    name = rownames(dbtargdata[["player_name.y"]]), 
    ipaTarg = dbtargdata[["player_name.y"]][,1])

lbtargdata <- ranef(lbmodel)
lbpatheffects <-
  data.frame(
    name = rownames(lbtargdata[["player_name.y"]]), 
    ipaTarg = lbtargdata[["player_name.y"]][,1])


dbres <- multimodeldata %>%
  filter(player_position.y %in% c("CB", "FS", "SS")) %>%
  group_by(player_name.y) %>%
  summarise(
    epa = mean(expected_points_added),
    yards = sum(pre_penalty_yards_gained),
    plays = n()
  ) %>%
  filter(plays>=15)

dbres <- dbres %>%
  left_join(dbpatheffects, by = c("player_name.y" = "name")) 

dbres <- dbres %>%
  mutate(
    ipaaTarg = plays * ipaTarg
  )

lbres <- multimodeldata %>%
  filter(player_position.y %in% c("LB", "ILB", "MLB")) %>%
  group_by(player_name.y) %>%
  summarise(
    epa = mean(expected_points_added),
    yards = sum(pre_penalty_yards_gained),
    plays = n()
  ) %>%
  filter(plays>=15)

lbres <- lbres %>%
  left_join(lbpatheffects, by = c("player_name.y" = "name")) 

lbres <- lbres %>%
  mutate(
    ipaaTarg = plays * ipaTarg
  )
