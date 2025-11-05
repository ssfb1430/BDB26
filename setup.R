library(dplyr)
library(tidyverse)

library(nflfastR)

pbp <- load_pbp(2025)
colnames(pbp)


unique(pbp$pass_defense_1_player_name)
unzip("nfl-big-data-bowl-2026-analytics.zip")

supdata <- read.csv("114239_nfl_competition_files_published_analytics_final/supplementary_data.csv")
week1 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w01.csv")
week2 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w02.csv")
week3 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w03.csv")
week4 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w04.csv")

print(unique(week1$player_role))

print(unique(supdata$route_of_targeted_receiver))

missing <- supdata %>%
  filter(is.na(route_of_targeted_receiver))

insides <- supdata %>%
  filter(route_of_targeted_receiver %in% c("IN", "OUT", "SLANT", "POST", "FLAT"))

week1targets <- week1 %>%
  filter(player_side == "Defense" | player_role == "Targeted Receiver")

snap_frames <- week1 %>%
  filter(frame_id == 1)

# Get receivers running a route at snap
receivers <- snap_frames %>%
  filter(player_role == "Targeted Receiver") %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, x, y, player_position)

# Get defenders at the same frame
defenders <- snap_frames %>%
  filter(player_position %in% c("CB", "SS","FS", "DB", "ILB", "LB", "MLB")) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = y, defenderX = x, player_position)

joined <- inner_join(receivers, defenders, by = c("game_id", "play_id", "frame_id")) %>%
  mutate(y_diff = abs(y - defenderY),
         x_diff = abs(x - defenderX)) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  slice_min(order_by = y_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(x_diff >= 4, x_diff <= 7) # this can be changed. just an arbitrary definition for off man at the snap. 

joinedcontext <- joined %>%
  left_join(supdata, by = c("game_id", "play_id"))

#joinedcontext has every targeted matchup that meets our off man threshold


#there are two of these functions for a reason. i forgot which one works correctly. we can test this
calculate_turn_angles <- function(df) {
  df %>%
    arrange(gameId, playId, nflId, frameId) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(
      # Calculate bearing angles between consecutive points
      # bt = atan2(yt+1 - yt, xt+1 - xt)
      bearing_current = atan2(lead(y) - y, lead(x) - x),
      bearing_previous = lag(bearing_current),
      
      # Turn angle = bt - bt-1 (difference between successive bearings)
      turn_angle_rad = bearing_current - bearing_previous,
      
      # Handle angle wraparound (-π to π)
      turn_angle_rad = case_when(
        turn_angle_rad > pi ~ turn_angle_rad - 2*pi,
        turn_angle_rad < -pi ~ turn_angle_rad + 2*pi,
        TRUE ~ turn_angle_rad
      ),
      
      # Convert to degrees for easier interpretation
      turn_angle_deg = turn_angle_rad * 180 / pi,
      
      # Take absolute value to get magnitude of turn
      turn_angle_abs_deg = abs(turn_angle_deg)
    ) %>%
    ungroup()
}

calculate_turn_angles <- function(df) {
  df %>%
    arrange(gameId, playId, nflId, frame) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(
      # Calculate bearing angles between consecutive points
      bearing_current = atan2(lead(y) - y, lead(x) - x),
      bearing_previous = lag(bearing_current),
      
      # Turn angle = bt - bt-1 (difference between successive bearings)
      turn_angle_rad = bearing_current - bearing_previous,
      
      # Handle angle wraparound (-π to π)
      turn_angle_rad = case_when(
        turn_angle_rad > pi ~ turn_angle_rad - 2*pi,
        turn_angle_rad < -pi ~ turn_angle_rad + 2*pi,
        TRUE ~ turn_angle_rad
      ),
      
      # Convert to degrees and get absolute value
      turn_angle_deg = turn_angle_rad * 180 / pi,
      turn_angle_abs_deg = abs(turn_angle_deg)
    ) %>%
    ungroup()
}

# Identify cuts of 15+ degrees that occur at least 5 frameIdIds after first frameId
cuts_analysis <- receiver_routes %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Get first frameId for each receiver route
    first_frameId = min(frameId, na.rm = TRUE),
    frameIds_from_start = frameId - first_frameId,
    
    # Identify significant cuts (15+ degrees) after frameId 5
    is_significant_cut = turn_angle_abs_deg >= 25 & 
      frameIds_from_start >= 5 & 
      !is.na(turn_angle_abs_deg)
  ) %>%
  ungroup()

