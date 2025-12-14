library(dplyr)
library(gganimate)
library(ggplot2)

calculate_frame_by_frame_optimal <- function(df) {
  
  df <- df %>%
    group_by(game_id, play_id) %>%  # Add grouping by play
    mutate(
      optimal_target_x = ball_land_x,  # Where optimal line points to
      optimal_target_y = ball_land_y,
      optimal_vector_x = ball_land_x - defenderX,  # Vector from DB to WR
      optimal_vector_y = ball_land_y - defenderY,
      optimal_distance = sqrt(optimal_vector_x^2 + optimal_vector_y^2),
      
      # Normalized optimal direction
      optimal_dir_x = optimal_vector_x / optimal_distance,
      optimal_dir_y = optimal_vector_y / optimal_distance,
      
      # Where they actually went (lead by 1 frame within each play)
      actual_next_x = lead(defenderX),
      actual_next_y = lead(defenderY),
      
      # Actual movement vector
      actual_move_x = actual_next_x - defenderX,
      actual_move_y = actual_next_y - defenderY,
      actual_move_distance = sqrt(actual_move_x^2 + actual_move_y^2),
      
      # Where they SHOULD have moved (same distance, optimal direction)
      should_move_x = optimal_dir_x * actual_move_distance,
      should_move_y = optimal_dir_y * actual_move_distance,
      
      should_next_x = defenderX + should_move_x,
      should_next_y = defenderY + should_move_y,
      
      # DEVIATION: distance between where they went vs should have gone
      deviation = sqrt((actual_next_x - should_next_x)^2 + 
                         (actual_next_y - should_next_y)^2),
      
      # Angle deviation (degrees)
      actual_dir_x = actual_move_x / actual_move_distance,
      actual_dir_y = actual_move_y / actual_move_distance,
      dot_product = optimal_dir_x * actual_dir_x + optimal_dir_y * actual_dir_y,
      angle_deviation_rad = acos(pmin(pmax(dot_product, -1), 1)),
      angle_deviation_deg = angle_deviation_rad * 180 / pi,
      
      # Cumulative metrics (within each play)
      cumulative_deviation = cumsum(coalesce(deviation, 0)),
      cumulative_angle_error = cumsum(coalesce(angle_deviation_deg, 0))
    ) %>%
    ungroup()
  
  return(df)
}

# Usage
df_analyzed <- calculate_frame_by_frame_optimal(joinedoutputs)

play_summary <- df_analyzed %>%
  group_by(game_id, play_id) %>%
  summarize(
    total_deviation = sum(deviation, na.rm = TRUE),
    mean_deviation_per_frame = mean(deviation, na.rm = TRUE),
    max_deviation = max(deviation, na.rm = TRUE),
    mean_angle_error = mean(angle_deviation_deg, na.rm = TRUE),
    final_separation = last(optimal_distance)
  )

df_observe <- df_analyzed %>%
  left_join(play_summary, by = c("game_id", "play_id"))

library(gganimate)

viz <- df_observe %>%
  filter(play_id == 877, game_id == 2023090700)

# Create animated plot
anim_plot <- ggplot(viz) +
  # Paths up to current frame
  geom_path(aes(x = defenderX, y = defenderY), 
            color = "blue", size = 1.5) +
  geom_path(aes(x = ball_land_x, y = ball_land_y), 
            color = "red", size = 1.5) +
  
  # Current optimal line
  geom_segment(aes(x = defenderX, y = defenderY,
                   xend = ball_land_x, yend = ball_land_y),
               color = "green", linetype = "dashed", size = 1.5) +
  
  # Current positions
  geom_point(aes(x = defenderX, y = defenderY), 
             color = "blue", size = 5) +
  geom_point(aes(x = ball_land_x, y = ball_land_y), 
             color = "red", size = 5) +
  
  # Next step comparison
  geom_segment(aes(x = defenderX, y = defenderY,
                   xend = should_next_x, yend = should_next_y),
               color = "green", size = 1.5,
               arrow = arrow(length = unit(0.3, "cm"))) +
  
  labs(title = "Frame: {frame} | Deviation: {round(df_analyzed$deviation[frame], 2)} yards",
       x = "X Position", y = "Y Position") +
  theme_minimal() +
  coord_equal() +
  transition_reveal(frame_id)

# Render animation
animate(anim_plot, nframes = nrow(viz), fps = 5)

df_observe <- frechetscores %>%
  left_join(supdata, by = c("game_id", "play_id"))

df_observe <- df_observe %>%
  mutate(
    complete = ifelse(pass_result == "C", 1, 0)
  )

distincts <- df_observe %>%
  distinct(game_id, play_id, .keep_all = TRUE)

distincts <- distincts %>%
  filter(team_coverage_man_zone == "MAN_COVERAGE")

distincts <- distincts %>%
  select(frechet_dist, complete)

firstmodel <- lm(complete ~ frechet_dist, family = "binomial",
                 data = distincts)
summary(firstmodel)

library(keras)
library(tensorflow)

# Prepare 3D array: (samples, timesteps, features)
# Each play is a sample, frames are timesteps

# Normalize features first
model_data_scaled <- df_analyzed %>%
  group_by(game_id, play_id) %>%
  mutate(across(c(deviation, angle_deviation_deg, optimal_distance), scale)) %>%
  ungroup()

# Convert to sequences
plays_list <- model_data_scaled %>%
  group_split(game_id, play_id)

# Pad sequences to same length
max_frames <- max(sapply(plays_list, nrow))

X_list <- lapply(plays_list, function(play) {
  mat <- as.matrix(play[, c("deviation", "angle_deviation_deg", 
                            "optimal_distance", "cumulative_deviation")])
  # Pad with zeros if needed
  if(nrow(mat) < max_frames) {
    mat <- rbind(mat, matrix(0, max_frames - nrow(mat), ncol(mat)))
  }
  mat
})

X <- array(unlist(X_list), dim = c(length(X_list), max_frames, 4))

# Play-level features (repeated for each frame)
play_features <- model_data_scaled %>%
  group_by(game_id, play_id) %>%
  slice(1) %>%
  select(air_yards, down, ydstogo, qb_pressure) %>%
  as.matrix()

# Outcomes
y <- sapply(plays_list, function(play) {
  ifelse(play$pass_outcome[1] == "complete", 1, 0)
})

# Build LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 64, input_shape = c(max_frames, 4), 
             return_sequences = TRUE) %>%
  layer_dropout(0.2) %>%
  layer_lstm(units = 32) %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# Train
history <- model %>% fit(
  X, y,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)


# Static plot comparing optimal vs actual path
ggplot(viz) +
  # OPTIMAL PATH (green dashed - from first frame calculations)
  geom_path(aes(x = should_next_x, y = should_next_y), 
            color = "green", 
            size = 2, 
            linetype = "dashed",
            alpha = 0.8) +
  
  # ACTUAL DB PATH (blue solid - what really happened)
  geom_path(aes(x = defenderX, y = defenderY), 
            color = "blue", 
            size = 2) +
  
  # WR PATH
  geom_path(aes(x = x, y = y), 
            color = "red", 
            size = 1.5,
            alpha = 0.7) +
  
  # Start position
  geom_point(data = viz %>% slice(1),
             aes(x = defenderX, y = defenderY),
             color = "blue", size = 6, shape = 15) +
  
  # End positions
  
  geom_point(data = viz %>% slice(n()),
             aes(x = defenderX, y = defenderY),
             color = "blue", size = 6, shape = 16) +
  geom_point(data = viz %>% slice(n()),
             aes(x = x, y = y),
             color = "red", size = 6, shape = 17) +
  
  # Labels
  annotate("text", 
           x = viz$defenderX[1], 
           y = viz$defenderY[1], 
           label = "START", 
           vjust = -1, 
           fontface = "bold",
           size = 4) +
  
  labs(title = "DB Pursuit Path Analysis",
       subtitle = paste0("Green dashed = Optimal path | Blue solid = Actual path | Total Deviation: ", 
                         round(sum(viz$deviation, na.rm = TRUE), 2), " yards"),
       x = "X Position", 
       y = "Y Position") +
  theme_minimal() +
  coord_equal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
# Predict at each frame (you'd need a modified architecture)

install.packages("longitudinalData")
install.packages("rgl")
install.packages("OpenGL")
library(longitudinalData) 
library(SimilarityMeasures)


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
  
  # Calculate CUMULATIVE FRÉCHET up to each frame using distFrechet
  df <- df %>%
    group_by(game_id, play_id) %>%
    mutate(
      frame_num = row_number(),
      frechet_cumulative = purrr::map_dbl(frame_num, function(i) {
        if (i < 2) return(NA_real_)
        
        # Remove any NA values
        receiver_x <- X[1:i]
        receiver_y <- Y[1:i]
        defender_x <- defenderX[1:i]
        defender_y <- defenderY[1:i]
        
        # Check for NAs
        if (any(is.na(receiver_x)) || any(is.na(receiver_y)) || 
            any(is.na(defender_x)) || any(is.na(defender_y))) {
          return(NA_real_)
        }
        
        # Create matrices - each row is a point (x, y)
        # distFrechet expects (Px, Py, Qx, Qy) where P and Q are the two curves
        tryCatch({
          result <- distFrechet(Px = receiver_x, Py = receiver_y, 
                                Qx = defender_x, Qy = defender_y)
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

write.csv(frechetdistances, "FrechetDistancesOutput.csv", row.names = FALSE)

#NEXT STEPS
#calculate angle deviation at frame before throw and frame after throw (maybe on after)
#calculate how far defender is from the ball land spot at the time of the throw
#start modeling