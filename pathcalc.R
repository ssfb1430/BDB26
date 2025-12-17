library(dplyr)
library(gganimate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(longitudinalData) 
library(SimilarityMeasures)
library(ggthemes)


calculate_frame_metrics <- function(df) {
  
  df <- df %>%
    group_by(game_id, play_id) %>%
    mutate(
      # Optimal path calculations
      distance = sqrt((X - defenderX)^2 + (Y - defenderY)^2),
      optimal_target_x = ball_land_x,
      optimal_target_y = ball_land_y,
      optimal_vector_x = ball_land_x - defenderX,
      optimal_vector_y = ball_land_y - defenderY,
      optimal_distance = sqrt(optimal_vector_x^2 + optimal_vector_y^2),
      optimal_dir_x = optimal_vector_x / optimal_distance,
      optimal_dir_y = optimal_vector_y / optimal_distance,
      
      # Actual movement
      actual_next_x = lead(defenderX),
      actual_next_y = lead(defenderY),
      actual_move_x = actual_next_x - defenderX,
      actual_move_y = actual_next_y - defenderY,
      actual_move_distance = sqrt(actual_move_x^2 + actual_move_y^2),
      
      # Optimal movement (same distance, optimal direction)
      should_move_x = optimal_dir_x * actual_move_distance,
      should_move_y = optimal_dir_y * actual_move_distance,
      should_next_x = defenderX + should_move_x,
      should_next_y = defenderY + should_move_y,
      
      # FRAME-BY-FRAME DEVIATION: where they went vs should have gone
      frame_deviation = sqrt((actual_next_x - should_next_x)^2 + 
                               (actual_next_y - should_next_y)^2),
      
      # PERPENDICULAR DISTANCE TO OPTIMAL PATH (instantaneous deviation)
      optimal_line_length = sqrt((ball_land_x[1] - defenderX[1])^2 + 
                                   (ball_land_y[1] - defenderY[1])^2),
      
      cross_product = abs((ball_land_y[1] - defenderY[1]) * (defenderX - defenderX[1]) -
                            (ball_land_x[1] - defenderX[1]) * (defenderY - defenderY[1])),
      perp_distance_to_optimal = cross_product / optimal_line_length,
      
      # Angle deviation
      actual_dir_x = actual_move_x / actual_move_distance,
      actual_dir_y = actual_move_y / actual_move_distance,
      dot_product = optimal_dir_x * actual_dir_x + optimal_dir_y * actual_dir_y,
      angle_deviation_deg = acos(pmin(pmax(dot_product, -1), 1)) * 180 / pi,
      
      # Cumulative metrics
      cumulative_frame_deviation = cumsum(coalesce(frame_deviation, 0)),
      cumulative_perp_deviation = cumsum(coalesce(perp_distance_to_optimal, 0)),
      cumulative_angle_error = cumsum(coalesce(angle_deviation_deg, 0))
    ) %>%
    ungroup()
  
  # Calculate straight line coordinates using polar conversion
  df <- df %>%
    group_by(game_id, play_id) %>%
    mutate(
      frame_num = row_number(),
      
      vec_x = ball_land_x[1] - defenderX[1],
      vec_y = ball_land_y[1] - defenderY[1],
      optimal_angle = atan2(vec_y, vec_x),  # angle from horizontal
      
      dx = c(0, diff(defenderX)),
      dy = c(0, diff(defenderY)),
      frame_distance = sqrt(dx^2 + dy^2),
      
      cumulative_distance = cumsum(frame_distance),
      
      straight_line_x = defenderX[1] + cumulative_distance * cos(optimal_angle),
      straight_line_y = defenderY[1] + cumulative_distance * sin(optimal_angle),
      
      distance_from_optimal = sqrt((defenderX - straight_line_x)^2 + (defenderY - straight_line_y)^2),
      
      frechet_cumulative = purrr::map_dbl(frame_num, function(i) {
        if (i < 2) return(NA_real_)
        
        defender_x <- defenderX[1:i]
        defender_y <- defenderY[1:i]
        
        straight_x <- straight_line_x[1:i]
        straight_y <- straight_line_y[1:i]
        
        if (any(is.na(defender_x)) || any(is.na(defender_y)) || 
            any(is.na(straight_x)) || any(is.na(straight_y))) {
          return(NA_real_)
        }
        
        tryCatch({
          result <- distFrechet(Px = defender_x, Py = defender_y, 
                                Qx = straight_x, Qy = straight_y)
          return(as.numeric(result))
        }, error = function(e) {
          message(paste("Error at frame", i, ":", e$message))
          return(NA_real_)
        })
      })
    ) %>%
    ungroup()
  
  return(df)
}

frechetdistances <- calculate_frame_metrics(joinedoutputs)

#check straight line path
checker <- frechetdistances %>%
  select(1:12, 48:59)
testfrechet <- frechetdistances %>%
  filter(game_id == 2023091800, play_id == 1945) %>%
  rename(testx = straight_line_x,
         testy = straight_line_y) %>%
  select(game_id, play_id, frame_id, testx, testy) %>%
  mutate(frame_id = frame_id + 33)


write.csv(frechetdistances, "FrechetDistancesOutput.csv", row.names = FALSE)

