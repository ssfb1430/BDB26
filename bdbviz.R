library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(gt)
library(gtExtras)
library(nflreadr)

playinput <- data %>% 
  filter(game_id == 2023091800, play_id == 1945) %>% 
  select(colnames(outputs))
frames = max(playinput$f)
playoutput <- outputs %>% 
  filter(game_id == 2023091800, play_id == 1945) %>% 
  mutate(frame_id = frame_id+frames)
play <- rbind(playinput,playoutput) %>% 
  mutate(X = X, Y = Y)

play <- play %>%
  left_join(playerinfo, by = "nfl_id")

sup <- supdata %>% 
  filter(game_id == 2023091800, play_id == 1945)

ball <- data %>% 
  filter(game_id == 2023091800, play_id == 1945) %>% 
  select(ball_land_x, ball_land_y) %>% 
  distinct()


outputplayers <- playoutput %>% 
  select(nfl_id) %>% 
  distinct()

inputplayers = play %>% 
  filter(!nfl_id %in% outputplayers) %>% 
  select(nfl_id) %>% 
  distinct()

one_p_players = play %>% 
  semi_join(players_out, by = "nfl_id")

checkplay <- finaloutput %>%
  filter(play_id == 1945, defenderId == 43350)

filled_in <- play %>% 
  mutate(alpha = 1) %>% 
  group_by(nfl_id) %>%
  complete(frame_id = full_seq(1:50, 1)) %>%     # ensure every id has frame_id 1–41
  mutate(
    alpha = if_else(is.na(alpha), 0, alpha)  # missing (new) rows become 0.5
  ) %>%
  filter(nfl_id == 54476 | nfl_id == 43350 ) %>%
  fill(everything(), .direction = "down") %>%     # fill missing rows using last known data
  ungroup()

filled_in <- filled_in %>%
  mutate(jersey_number = case_when(
    player_name == "Chris Olave" ~ "12",
    player_name == "Vonn Bell" ~ "24",
    TRUE ~ ""
  ))

db_path_perm <- filled_in %>%
  filter(nfl_id == 54476) %>%
  select(X, Y)

wr_path_perm <- filled_in %>%
  filter(nfl_id == 43350) %>%
  select(X, Y)


filled_in <- filled_in %>%
  mutate(frame_id = as.numeric(frame_id))

saintsplay <- finaloutput %>%
  filter(play_id == 1945, game_id == 2023091800, defenderId == 43350) %>%
  mutate(frame_id = as.numeric(frame_id))  %>%
  mutate(frame_id = frame_id + 33)


saintsplay_static <- saintsplay %>%
  select(-frame_id) %>%
  mutate(path_group = 1)

#Panthers Play
p <- ggplot() +
  annotate("text", 
           x = seq(40, 70, 10),
           y = 10,
           label = 10 * c(3,4,5,4),
           color = "#bebebe",
           family = "Chivo",
           size = 4) +
  annotate("text", 
           x = seq(40, 70, 10),
           y = 40,
           label = 10 * c(3,4,5,4),
           color = "#bebebe",
           family = "Chivo",
           angle = 180,
           size = 4) +
  annotate("text",
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 0,
           label = "—",
           angle = 90,
           color = "#bebebe") +
  annotate("text",
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 160/3,
           label = "—",
           angle = 90,
           color = "#bebebe") +
  geom_vline(xintercept = seq(35, 75, 5), color = "#bebebe") +
  geom_path(
    data = db_path_perm,
    aes(X, Y, group = 1),
    color = "grey",
    size = 1.2,
    alpha = 0.5
  ) +
  geom_path(
    data = wr_path_perm,
    aes(X, Y, group = 1),
    color = "grey",
    size = 1.2,
    alpha = 0.5
  ) +
  # Static paths (not animated) - always visible
  geom_path(data = saintsplay_static,
            aes(defenderX, defenderY, group = path_group),
            color = "#0088CE",
            size = 1.2) +
  geom_path(data = saintsplay_static,
            aes(straight_line_x, straight_line_y, group = path_group),
            color = "#FF6B6B",
            linetype = "dashed",
            size = 1.2) +
  # Animated points
  geom_point(data = filled_in,
             aes(X, Y, color = player_side),
             size = 7.5,
             alpha = 0.95) +
  geom_text(data = filled_in,
            aes(X, Y, label = jersey_number),  # replace with your actual column name
            color = "white",
            size = 3.7,
            fontface = "bold",
            family = "Chivo") +
  geom_point(data = ball,
             aes(ball_land_x, ball_land_y),
             shape = "x",
             size = 5,
             color = "brown") +
  labs(
    title = "2023 Week 2 - <span style='color:#0088CE;'>**Carolina Panthers**</span> vs. 
             <span style='color:#D3BC8D;'>**New Orleans Saints**</span>",
    subtitle = sup$play_description
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),
    plot.title = ggtext::element_markdown(hjust = 0.5, size = 14),
    text = element_text(family = "", color = "#26282A"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_color_manual(values = c(
    Defense = "#0088CE",
    Offense = "#D3BC8D"
  )) +
  transition_time(frame_id) +
  ease_aes("linear") +
  coord_cartesian(
    xlim = c(35, 75),
    ylim = c(0, 160/3),
    expand = FALSE,
    clip = "off"
  )
anim <- animate(
  p,
  fps = 10,
  width = 900,
  height = 400,
  duration = max(filled_in$frame_id) / 10  # ensures 1 frame per actual frame_id
)

anim_save("Play.gif", animation = anim)
anim

saintsprobs <- testpreds %>%
  filter(play_id == 1945, game_id == 2023091800) 


saintsprobslong <- saintsprobs %>%
  pivot_longer(
    cols = c(pass_result_prob, slpreds),  # Replace 'counterfactual_prob' with your actual column name
    names_to = "path_type",
    values_to = "probability"
  )
prob_plot_anim <- ggplot(saintsprobslong, aes(x = frame_id, y = probability, color = path_type, group = path_type)) +
  geom_line(size = 1.5, alpha = 0.9) +
  geom_point(size = 4, alpha = 0.9) +
  geom_point(aes(group = interaction(path_type, seq_along(frame_id))), 
             size = 2, alpha = 1) +
  labs(
    title = "Completion Probability After Throw",
    x = "Frame",
    y = "Completion Probability"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  scale_color_manual(
    values = c("pass_result_prob" = "#0088CE", "slpreds" = "#FF6B6B"),  # Customize colors
    labels = c("Actual Path", "Alternative Path"),  # Customize legend labels
    name = "Path Type"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e0e0e0", size = 0.3),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  transition_reveal(frame_id)

prob_anim <- animate(
  prob_plot_anim,
  fps = 10,
  width = 800,
  height = 500,
  duration = max(saintsprobs$frame_id) / 10
)
prob_anim
anim_save("probability_animation.gif", animation = prob_anim)


library(magick)

play_mgif <- image_read("Play.gif")
prob_mgif <- image_read("probability_animation.gif")

play_mgif <- image_scale(play_mgif, "800x400!")
prob_mgif <- image_scale(prob_mgif, "800x400!")

start_frame <- 34
padding_count <- start_frame - 1

first_prob_frame <- prob_mgif[1]
padding <- rep(list(first_prob_frame), padding_count)
prob_mgif_padded <- c(do.call(c, padding), prob_mgif)

combined_gif <- image_append(
  c(play_mgif[1], prob_mgif_padded[1]),
  stack = TRUE
)

for (i in 2:length(play_mgif)) {
  combined <- image_append(
    c(play_mgif[i], prob_mgif_padded[i]),
    stack = TRUE
  )
  combined_gif <- c(combined_gif, combined)
}

image_write(combined_gif, "combined_play_probability_vertical.gif")

combined_gif



library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyverse)
library(ggthemes)

playinput = data %>% 
  filter(game_id == 2023100800, play_id == 4478) %>% 
  select(colnames(outputs))
frames = max(playinput$f)
playoutput = outputs %>% 
  filter(game_id == 2023100800, play_id == 4478) %>% 
  mutate(frame_id = frame_id+frames)
play = rbind(playinput,playoutput) %>% 
  mutate(X = X, Y = Y)

play <- play %>%
  left_join(playerinfo, by = "nfl_id")

sup = supdata %>% 
  filter(game_id == 2023100800, play_id == 4478)

ball = data %>% 
  filter(game_id == 2023100800, play_id == 4478) %>% 
  select(ball_land_x, ball_land_y) %>% 
  distinct()


outputplayers <- playoutput %>% 
  select(nfl_id) %>% 
  distinct()

inputplayers <- play %>% 
  filter(!nfl_id %in% outputplayers) %>% 
  select(nfl_id) %>% 
  distinct()

one_p_players <- play %>% 
  semi_join(players_out, by = "nfl_id")


filled_in = play %>% 
  mutate(alpha = 1) %>% 
  group_by(nfl_id) %>%
  complete(frame_id = full_seq(1:61, 1)) %>%     # ensure every id has frame_id 1–41
  mutate(
    alpha = if_else(is.na(alpha), 0, alpha)  # missing (new) rows become 0.5
  ) %>%
  filter(nfl_id == 48415 | nfl_id == 53494 ) %>%
  fill(everything(), .direction = "down") %>%     # fill missing rows using last known data
  ungroup()

filled_in <- filled_in %>%
  mutate(jersey_number = case_when(
    player_name == "Deonte Harty" ~ "11",
    player_name == "Andre Cisco" ~ "5",
    TRUE ~ ""
  ))

db_path_perm <- filled_in %>%
  filter(nfl_id == 53494) %>%
  select(X, Y)

wr_path_perm <- filled_in %>%
  filter(nfl_id == 48415) %>%
  select(X, Y)


filled_in <- filled_in %>%
  mutate(frame_id = as.numeric(frame_id))

jagplay <- finaloutput %>%
  filter(play_id == 4478, game_id == 2023100800, defenderId == 53494) %>%
  mutate(frame_id = as.numeric(frame_id))  %>%
  mutate(frame_id = frame_id + 34)

jagplay_static <- jagplay %>%
  select(-frame_id) %>%
  mutate(path_group = 1)



#Jags Play
p2 <- ggplot() +
  annotate("text", 
           x = seq(40, 70, 10),
           y = 10,
           label = 10 * c(3,4,5,4),
           color = "#bebebe",
           family = "Chivo",
           size = 4) +
  annotate("text", 
           x = seq(40, 70, 10),
           y = 40,
           label = 10 * c(3,4,5,4),
           color = "#bebebe",
           family = "Chivo",
           angle = 180,
           size = 4) +
  annotate("text",
           x = setdiff(seq(25, 80, 1), seq(25, 80, 5)),
           y = 0,
           label = "—",
           angle = 90,
           color = "#bebebe") +
  annotate("text",
           x = setdiff(seq(25, 80, 1), seq(25, 80, 5)),
           y = 160/3,
           label = "—",
           angle = 90,
           color = "#bebebe") +
  geom_vline(xintercept = seq(25, 80, 5), color = "#bebebe") +
  geom_path(
    data = db_path_perm,
    aes(X, Y, group = 1),
    color = "grey",
    size = 1.2,
    alpha = 0.5
  ) +
  geom_path(
    data = wr_path_perm,
    aes(X, Y, group = 1),
    color = "grey",
    size = 1.2,
    alpha = 0.5
  ) +
  geom_path(data = jagplay_static,
            aes(defenderX, defenderY, group = path_group),
            color = "#006778",
            size = 1.2) +
  geom_path(data = jagplay_static,
            aes(straight_line_x, straight_line_y, group = path_group),
            color = "#FF6B6B",
            linetype = "dashed",
            size = 1.2) +
  # Animated points
  geom_point(data = filled_in,
             aes(X, Y, color = player_side),
             size = 7.5,
             alpha = 0.95) +
  geom_text(data = filled_in,
            aes(X, Y, label = jersey_number),  # replace with your actual column name
            color = "white",
            size = 3.7,
            fontface = "bold",
            family = "Chivo") +
  geom_point(data = ball,
             aes(ball_land_x, ball_land_y),
             shape = "x",
             size = 5,
             color = "brown") +
  labs(
    title = "2023 NFL Week 3 - <span style='color:#C60C30;'>**Buffalo Bills**</span> vs. 
             <span style='color:#006778;'>**Jacksonville Jaguars**</span>",
    subtitle = sup$play_description
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),
    plot.title = ggtext::element_markdown(hjust = 0.5, size = 14),
    text = element_text(family = "", color = "#26282A"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_color_manual(values = c(
    Defense = "#006778",
    Offense = "#C60C30"
  )) +
  transition_time(frame_id) +
  ease_aes("linear") +
  coord_cartesian(
    xlim = c(25, 80),
    ylim = c(0, 160/3),
    expand = FALSE,
    clip = "off"
  )
anim2 <- animate(
  p2,
  fps = 10,
  width = 900,
  height = 400,
  duration = max(filled_in$frame_id) / 10  # ensures 1 frame per actual frame_id
)

anim_save("Play2.gif", animation = anim2)
anim2

jagprobs <- testpreds %>%
  filter(play_id == 4478, game_id == 2023100800) 

jagprobslong <- jagprobs %>%
  pivot_longer(
    cols = c(pass_result_prob, slpreds),  # Replace 'counterfactual_prob' with your actual column name
    names_to = "path_type",
    values_to = "probability"
  )
prob_plot_anim2 <- ggplot(jagprobslong, aes(x = frame_id, y = probability, color = path_type, group = path_type)) +
  geom_line(size = 1.5, alpha = 0.9) +
  geom_point(size = 4, alpha = 0.9) +
  geom_point(aes(group = interaction(path_type, seq_along(frame_id))), 
             size = 2, alpha = 1) +
  labs(
    title = "Completion Probability After Throw",
    x = "Frame",
    y = "Completion Probability"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  scale_color_manual(
    values = c("pass_result_prob" = "#006778", "slpreds" = "#FF6B6B"),  # Customize colors
    labels = c("Actual Path", "Alternative Path"),  # Customize legend labels
    name = "Path Type"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e0e0e0", size = 0.3),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  transition_reveal(frame_id)

# Animate it
prob_anim2 <- animate(
  prob_plot_anim2,
  fps = 10,
  width = 800,
  height = 500,
  duration = max(jagprobs$frame_id) / 10
)
prob_anim2
anim_save("probability_animation2.gif", animation = prob_anim2)

library(magick)

play_mgif2 <- image_read("Play2.gif")
prob_mgif2 <- image_read("probability_animation2.gif")

play_mgif2 <- image_scale(play_mgif2, "800x400!")
prob_mgif2 <- image_scale(prob_mgif2, "800x400!")

start_frame <- 36
padding_count <- start_frame - 1

first_prob_frame <- prob_mgif2[1]
padding <- rep(list(first_prob_frame), padding_count)
prob_mgif2_padded <- c(do.call(c, padding), prob_mgif2)

combined_gif <- image_append(
  c(play_mgif2[1], prob_mgif2_padded[1]),
  stack = TRUE
)

for (i in 2:length(play_mgif2)) {
  combined <- image_append(
    c(play_mgif2[i], prob_mgif2_padded[i]),
    stack = TRUE
  )
  combined_gif <- c(combined_gif, combined)
}


image_write(combined_gif, "combined_play_probability_vertical2.gif")

combined_gif




#Final Tables
rosters <- load_rosters(seasons = 2023)
teams <- load_teams()
dbresgraph <- dbres %>%
  left_join(rosters, by = c("player_name.y" = "full_name")) %>%
  left_join(teams, by = c("team" = "team_abbr")) %>%
  arrange(ipaTarg)

dbs <- dbresgraph %>%
  dplyr::select(
    player_name.y,
    headshot_url,
    team_logo_espn,
    plays,
    epa,
    yards,
    ipaTarg
  ) %>%
  mutate(
    epa = round(epa, 2),
    ipaTarg = round(ipaTarg, 3)
  ) %>%
  arrange((ipaTarg)) %>%
  slice_head(n=10) %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(
    player_name.y = "Name",
    headshot_url = "",
    team_logo_espn = "Team",
    plays = "Targets",
    epa = "EPA/Target",
    yards = "Yards Allowed",
    ipaTarg = "Completion Probability Added Above Average"
  ) %>%
  tab_header(
    title = "Top 10 DB's by CPAA",
    subtitle = "Completion Probability Added = Actual Path Completion Probability - Straight Line Path Completion Probability"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  data_color(
    columns = c(ipaTarg),
    colors = scales::col_numeric(
      palette = c("#66CCEE", "#FFFFFF"),
      domain = NULL
    )
  )%>%
  fmt_percent(
    columns = (ipaTarg),
    decimals = 1
  )
dbs <- gt_img_rows(dbs, column = "headshot_url")
dbs <- gt_img_rows(dbs, column = "team_logo_espn")

dbs


rosters <- load_rosters(seasons = 2023)
teams <- load_teams()
lbresgraph <- lbsres %>%
  left_join(rosters, by = c("player_name.y" = "full_name")) %>%
  left_join(teams, by = c("team" = "team_abbr")) %>%
  arrange(ipaTarg)

lbs <- lbresgraph %>%
  dplyr::select(
    player_name.y,
    headshot_url,
    team_logo_espn,
    plays,
    epa,
    yards,
    ipaaTarg
  ) %>%
  mutate(
    epa = round(epa, 2),
    ipaaTarg = round(ipaaTarg, 3)
  ) %>%
  arrange((ipaaTarg)) %>%
  slice_head(n=10) %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(
    player_name.y = "Name",
    headshot_url = "",
    team_logo_espn = "Team",
    plays = "Targets",
    epa = "EPA/Target",
    yards = "Yards Allowed",
    ipaaTarg = "Completion Probability Added Above Average"
  ) %>%
  tab_header(
    title = "Top 10 LB's by CPAA",
    subtitle = "Completion Probability Added = Actual Path Completion Probability - Straight Line Path Completion Probability"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  data_color(
    columns = c(ipaaTarg),
    colors = scales::col_numeric(
      palette = c("#66CCEE", "#FFFFFF"),
      domain = NULL
    )
  )%>%
  fmt_percent(
    columns = (ipaaTarg),
    decimals = 1
  )
lbs <- gt_img_rows(lbs, column = "headshot_url")
lbs <- gt_img_rows(lbs, column = "team_logo_espn")

lbs

dbs %>%
  gtsave("DBTable.png")

lbs %>%
  gtsave("LBTable.png")


#Straight Line Plot
library(ggplot2)

single_play <- frechetdistances %>%
  filter(game_id == 2023100800,
         play_id == 4478)

key_frames <- single_play %>%
  filter(frame_num %in% seq(1, max(frame_num), length.out = 8)) %>%
  mutate(frame_label = paste0("Frame ", frame_num))

start_point <- key_frames %>% filter(frame_num == min(frame_num))
other_frames <- key_frames %>% filter(frame_num != min(frame_num))

pathcalc <- ggplot() +
  theme_fivethirtyeight() +
  coord_fixed() +
  geom_segment(data = single_play[1,],
               aes(x = defenderX, y = defenderY,
                   xend = ball_land_x, yend = ball_land_y),
               linetype = "dashed", color = "black", linewidth = 1.2,
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("text", x = single_play$defenderX[1] + 0.3*(single_play$ball_land_x[1] - single_play$defenderX[1]),
           y = single_play$defenderY[1] + 0.3*(single_play$ball_land_y[1] - single_play$defenderY[1]),
           label = "Optimal Angle θ", color = "black", fontface = "bold", size = 4) +
  geom_path(data = single_play,
            aes(x = defenderX, y = defenderY),
            color = "#006778", linewidth = 1.5, alpha = 0.7) +
  geom_point(data = key_frames,
             aes(x = defenderX, y = defenderY),
             color = "#006778", size = 4) +
  geom_path(data = single_play,
            aes(x = straight_line_x, y = straight_line_y),
            color = "orange", linewidth = 1.5, alpha = 0.7) +
  
  geom_point(data = key_frames,
             aes(x = straight_line_x, y = straight_line_y),
             color = "#006778", size = 3) +
  geom_segment(data = other_frames,
               aes(x = defenderX, y = defenderY,
                   xend = straight_line_x, yend = straight_line_y),
               linetype = "dotted", color = "gray40", alpha = 0.6) +
  geom_text(data = other_frames,
            aes(x = straight_line_x, y = straight_line_y,
                label = sprintf("r = %.1f", cumulative_distance)),
            vjust = -3,  color = "#006778", fontface = "bold", size = 3) +
  
  geom_point(data = single_play,
             aes(ball_land_x, ball_land_y),
             shape = "x",
             size = 6,
             color = "brown") +
  
  labs(title = "Converting Actual Path to Straight Line Path",
       subtitle = "Teal: Actual Path | Orange: Original Path (r) projected onto optimal angle (θ)",
       x = "X Position (yards)", 
       y = "Y Position (yards)") +
  
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "none") +
  theme(axis.title = element_text()) + ylab('Y') + xlab("X") 

pathcalc

ggsave("PathCalc.png", pathcalc, 
       width = 16, height = 12, dpi = 300)



