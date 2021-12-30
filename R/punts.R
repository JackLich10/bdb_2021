### Feature engineering

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(janitor)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(readr)))
suppressMessages(suppressWarnings(library(tidyr)))

option_list <- list(
  make_option("--start_season", type="integer", default = 2018,
              help="Start season of tracking data to read"),
  make_option("--end_season", type="integer", default = 2020,
              help="End season of tracking data to read")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript R/punts.R --start_season 2018 --end_season 2020

# Source helpers
source("R/helpers.R")

sns <- opt$start_season:opt$end_season

cat(paste0("Loading in processed BDB punt data from ", min(sns), " to ", max(sns), "...\n"))

# Read in processed punt data and field control data
punts <- purrr::map_df(sns, function(sn) {
  readRDS(paste0("data/proc/punts", sn, ".rds")) %>% 
    dplyr::mutate(season = sn)
})

# Plays that were partially blocked (get rid)
partial_blocks <- punts %>% 
  dplyr::filter(event == "punt_blocked") %>% 
  dplyr::distinct(play)

control <- purrr::map_df(sns, function(sn) {
  readRDS(paste0("data/proc/control", sn, ".rds"))
})

# Play level return probability predictions
play_level_preds <- readRDS("data/proc/play_level_preds.rds")

# Ball only data
ball <- punts %>% 
  dplyr::anti_join(partial_blocks, by = "play") %>% 
  dplyr::filter(display_name == "football") %>% 
  dplyr::select(game_id, play_id, frame_id, 
                ball_x = x, ball_y = y,
                ball_speed = s, ball_accel = a, ball_dis = dis)

# Just the returner
returners <- punts %>% 
  dplyr::anti_join(partial_blocks, by = "play") %>% 
  dplyr::filter(returner == 1) %>% 
  dplyr::select(-c(kicker, contains("_role"), defense)) %>% 
  dplyr::left_join(ball, by = c("frame_id", "game_id", "play_id")) %>% 
  dplyr::mutate(dist_to_ball = sqrt((x - ball_x)^2 + (y - ball_y)^2),
                special_teams_result = factor(special_teams_result)) %>% 
  # Find distances to sideline and distances to endzone
  dplyr::group_by(game_id, play_id, frame_id) %>% 
  dplyr::mutate(returner_to_sideline = min(abs(c(y - 0, y - 160/3))),
                ball_to_sideline = min(abs(c(ball_y - 0, ball_y - 160/3))),
                ball_to_ez = min(abs(ball_x - 110))) %>% 
  dplyr::ungroup()

# Aggregate field control within the returner's vision cone
agg_control <- control %>% 
  dplyr::left_join(returners %>% 
                     dplyr::select(game_id, play_id, frame_id, 
                                   returner_x = x, 
                                   returner_y = y),
                   by = c("game_id", "play_id", "frame_id")) %>% 
  dplyr::mutate(dist_to_returner = sqrt((x-returner_x)^2 + (y-returner_y)^2)) %>%
  dplyr::group_by(play, frame_id) %>% 
  dplyr::summarise(control_wt = sum(control*dist_to_returner)/sum(dist_to_returner), # Weight control by how close point is to returner
                   control = mean(control),
                   .groups = "drop")

# Find all non-returner players
non_returners <- punts %>% 
  dplyr::anti_join(partial_blocks, by = "play") %>% 
  dplyr::filter(returner == 0,
                team != "football") %>% 
  dplyr::select(time:nfl_id, frame_id:play_id, display_name, team, 
                defense, dplyr::contains("_role"), o_x:a_y) 

# Just the punt team
punt_team <- non_returners %>% 
  dplyr::filter(defense == 0) %>% 
  dplyr::left_join(ball %>% 
                     dplyr::select(game_id, play_id, frame_id,
                                   ball_x, ball_y),
                   by = c("frame_id", "game_id", "play_id")) %>% 
  dplyr::mutate(dist_to_ball = sqrt((x-ball_x)^2 + (y-ball_y)^2))

punt_team_closest_to_ball <- punt_team %>% 
  dplyr::filter(pre_snap_role == "gunners") %>% 
  dplyr::group_by(game_id, play_id, frame_id) %>% 
  dplyr::slice_min(dist_to_ball, n = 1, with_ties = FALSE) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(game_id, play_id, frame_id, gunner_to_ball = dist_to_ball)

# Just the return team (minus returner)
return_team <- non_returners %>% 
  dplyr::filter(defense == 1)

# Find distances of closest punt team players to returner
closest_punt_team <- returners %>% 
  dplyr::select(game_id, play_id, frame_id, 
                returner_x = x, returner_y = y, returner_dir = dir) %>% 
  dplyr::left_join(punt_team,
                   by = c("game_id", "play_id", "frame_id")) %>% 
  dplyr::mutate(dist_to_returner = sqrt((x-returner_x)^2 + (y-returner_y)^2)) %>% 
  dplyr::group_by(game_id, play_id, frame_id) %>% 
  # Take 3 closest players
  dplyr::slice_min(dist_to_returner, n = 3, with_ties = FALSE) %>% 
  dplyr::mutate(rank = dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dir_diff = dir - returner_dir,
                slope = atan2(returner_y - y, returner_x - x),
                diff = slope - o,
                dplyr::across(c(diff), 
                              ~ dplyr::case_when(
                                . >= -1*pi & . <= pi ~ .,
                                . > pi ~ . - (2*pi),
                                . < -1*pi ~ . + (2*pi))),
                dplyr::across(c(diff), abs)) %>% 
  dplyr::select(game_id, play_id, frame_id, rank, closest_punt_team_id = nfl_id,
                closest_punt_team_role = pre_snap_role, dist_to_returner, dist_to_ball,
                dir_diff, slope, diff, s) %>% 
  tidyr::pivot_wider(names_from = rank,
                     values_from = c(closest_punt_team_id, 
                                     closest_punt_team_role, 
                                     dist_to_returner, dist_to_ball,
                                     s, dir_diff, slope, diff))

# Compute average distances between non-returners and returner within each team
avg_distances <- non_returners %>% 
  dplyr::left_join(returners %>% 
                     dplyr::select(game_id, play_id, frame_id, 
                                   returner_x = x, returner_y = y),
                   by = c("frame_id", "game_id", "play_id")) %>% 
  dplyr::mutate(dist_to_returner = sqrt((x-returner_x)^2 + (y-returner_y)^2)) %>% 
  # select(game_id, play_id, frame_id, nfl_id, defense, dplyr::contains("_role"), dist_to_returner) %>% 
  dplyr::group_by(game_id, play_id, frame_id) %>% 
  dplyr::summarise(avg_dist_punt_team = mean(dist_to_returner[defense == 0]),
                   avg_dist_ret_team = mean(dist_to_returner[defense == 1]),
                   .groups = "drop")

# avg_distances %>% 
#   count(game_id, play_id, frame_id, sort = T)
# 
# agg_control %>% 
#   count(play, frame_id, sort = T)

model_dataset <- returners %>% 
  # Only keep frames after the punt is made
  dplyr::filter(frame_id >= punt_frame) %>% 
  # Join in average distances to returner by team
  dplyr::left_join(avg_distances, by = c("game_id", "play_id", "frame_id")) %>%
  # Join in closest players to returner on punt team
  dplyr::left_join(closest_punt_team, by = c("game_id", "play_id", "frame_id")) %>%
  # Join in closest gunner to ball on punt team
  dplyr::left_join(punt_team_closest_to_ball, by = c("game_id", "play_id", "frame_id")) %>%
  # Join in field control
  dplyr::left_join(agg_control, by = c("frame_id", "play")) %>% 
  # Join in play-level return predictions
  dplyr::left_join(play_level_preds, by = c("play")) %>% 
  # Fill in field control values down within play
  dplyr::group_by(play) %>% 
  tidyr::fill(control, control_wt, .direction = "down") %>% 
  dplyr::ungroup() %>% 
  tidyr::replace_na(list(control = 0, control_wt = 0)) %>% 
  # Create binary target variable
  dplyr::mutate(result = dplyr::case_when(
    special_teams_result == "Return" ~ "Return",
    special_teams_result %in% c("Fair Catch", "Muffed") ~ "Fair Catch",
    TRUE ~ "Poison")) %>% 
  # Get rid of muffs
  dplyr::filter(special_teams_result != "Muffed") %>% 
  dplyr::mutate(result = factor(result),
                special_teams_result = stringr::str_to_lower(stringr::str_replace_all(special_teams_result, " ", "_")),
                special_teams_result = factor(special_teams_result))

# Save model data
saveRDS(model_dataset, "data/proc/model_dataset.rds")
saveRDS(ball, "data/proc/ball_only.rds")

rm(returners, punts, control, agg_control, avg_distances, ball)

cat(paste0("Completed processing punt data from ", min(sns), " to ", max(sns), "\n"))

# model_dataset %>% 
#   mutate(target = ifelse(special_teams_result == "downed", 1, 0),
#          diff = dist_to_ball - gunner_to_ball) %>% 
#   # glm(target ~ dist_to_ball_1, family = "binomial", data = .) %>% 
#   # # fmsb::NagelkerkeR2()
#   # summary()
#     # tidyr::pivot_longer(cols = starts_with("slope_")) %>%
#     group_by(value = (dist_to_ball_1 %/% 1) * 1) %>%
#     summarise_target() %>%
#     plot_continuous(metric = value)

# model_dataset %>%
#   select(-c(s_x, s_y)) %>% 
#   mutate(target = ifelse(special_teams_result == "return", 1, 0)) %>%
#   tidyr::pivot_longer(cols = starts_with("s_")) %>%
#   group_by(name, value = (value %/% 1) * 1) %>%
#   summarise_target() %>%
#   plot_continuous(metric = value, color = name)
# model_dataset %>% 
#   distinct(play, punt_frame) %>% 
#   summarise_na()
# 
# model_dataset %>% 
#   tidyr::pivot_longer(cols = starts_with("dir_diff_")) %>% 
#   group_by(name, value = abs((value %/% 2) * 2)) %>% 
#   summarise_target() %>% 
#   plot_continuous(metric = value, color = name)
# 
# model_dataset %>%
#   mutate(target = ifelse(special_teams_result == "Return", 1, 0)) %>% 
#   tidyr::pivot_longer(cols = starts_with("slope_")) %>%
#   group_by(name, value = abs((value %/% 0.1) * 0.1)) %>%
#   summarise_target() %>%
#   plot_continuous(metric = value, color = name)
# 
# model_dataset %>%
#   mutate(target = ifelse(special_teams_result == "Return", 1, 0)) %>% 
#   tidyr::pivot_longer(cols = starts_with("dist_to_returner_")) %>%
#   group_by(name, value = (value %/% 1) * 1) %>%
#   summarise_target() %>%
#   plot_continuous(metric = value, color = name)
# 
# model_dataset %>%
#   mutate(target = ifelse(special_teams_result == "Return", 1, 0)) %>% 
#   tidyr::pivot_longer(cols = starts_with("control")) %>%
#   group_by(name, value = (value %/% 0.001) * 0.001) %>%
#   summarise_target() %>%
#   plot_continuous(metric = value, color = name)
# 
# model_dataset %>%
#   mutate(target = ifelse(special_teams_result == "Return", 1, 0)) %>% 
#   group_by(ball_to_sideline = (ball_to_sideline %/% 0.5) * 0.5) %>%
#   summarise_target() %>%
#   plot_continuous(metric = ball_to_sideline)
# 
# model_dataset %>%
#   mutate(target = ifelse(special_teams_result == "Touchback", 1, 0)) %>%
#   group_by(ball_to_ez = (ball_to_ez %/% 0.5) * 0.5) %>%
#   summarise_target() %>%
#   plot_continuous(metric = ball_to_ez)
# 
# 
# model_dataset %>% 
#   filter(play %in% sample(unique(model_dataset$play), size = 9)) %>% 
#   mutate(label = paste0(play, ": ", special_teams_result)) %>% 
#   ggplot(aes(frame_id, control)) +
#   geom_line() +
#   geom_text(aes(label = ifelse(event == "None", NA_character_, event)),
#             hjust = 1, vjust = 1, size = 3) +
#   facet_wrap(~ label, scales = "free_x")
# 
# model_dataset %>% 
#   count(special_teams_result)
# 
# model_dataset %>% count(hang_time) %>% View()
# count(ball_land_frame) %>% View()
# 
# model_dataset %>% 
#   group_by(event) %>% 
#   summarise(n = dplyr::n(),
#             mean(sec))
# 
# 
# model_dataset %>% 
#   filter(event == "fumble") %>% 
#   distinct(play)
# model_dataset %>% 
#   group_by(sec) %>% 
#   summarise(n = n()) %>% View()
# 
# plays %>% 
#   filter(play == "2018102107_1126") %>% View()
# 
# model_dataset %>% 
#   group_by(play) %>% 
#   summarise(frames = n_distinct(frame_id)) %>% 
#   arrange(desc(frames)) %>% View()
# 
# punts %>% glimpse()
# 
# vars <- c("dist_to_ball", "avg_dist_punt_team", "avg_dist_ret_team",
#           "control", "ball_to_ez")
# 
# paste(vars, collapse = " + ")
# 
# binary_mod <- model_dataset %>% 
#   glm(target ~ dist_to_ball + avg_dist_punt_team + avg_dist_ret_team + control +
#         dist_to_returner_1 + dist_to_returner_2 + dist_to_returner_3 + 
#         abs(slope_1) + abs(slope_2) +
#         ball_to_ez + ball_to_sideline, 
#       data = ., family = "binomial")
# 
# binary_mod %>% 
#   # fmsb::NagelkerkeR2()
#   summary()
# 
# model_dataset %>% 
#   filter(play == "2018090600_1850") %>% 
#   ggplot(aes(x, y)) +
#   geom_path() +
#   geom_path(aes(ball_x, ball_y), color = "brown")
# 
# mod <- model_dataset %>% 
#   nnet::multinom(special_teams_result ~ dist_to_ball + control, data = .)
# 
# mod %>% summary()
# 
# mod %>% broom::tidy() %>% View()

# impute `ball_land_frame` as time of `punt` event plus time of `hang_time` for all plays

# only keep frames one second before `punt_land`

# avg distance between returner and own team vs avg distance between returner and other team

# field control




