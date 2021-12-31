### Modeling return yards
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(tune)))
suppressMessages(suppressWarnings(library(broom)))

# Source helpers
source("R/helpers.R")

cat("Punt return yardage model...\n")

# Set metric as rmse, mae
mset <- yardstick::metric_set(yardstick::rmse, yardstick::mae)

# Read in frame-level data
model_dataset <- readRDS(paste0("data/proc/model_dataset.rds"))

# Play level data
play_info <- purrr::map_df(2018:2020, function(sn) {
  readRDS(paste0("data/proc/punt_plays", sn, ".rds")) %>% 
    dplyr::mutate(season = sn)
}) 

play_info <- play_info %>% 
  tidyr::replace_na(list(snap_time = mean(play_info$snap_time, na.rm = TRUE),
                         operation_time = mean(play_info$operation_time, na.rm = TRUE),
                         hang_time = mean(play_info$hang_time, na.rm = TRUE),
                         punt_rushers = median(play_info$punt_rushers, na.rm = TRUE),
                         gunners = median(play_info$gunners, na.rm = TRUE),
                         vises = median(play_info$vises, na.rm = TRUE),
                         special_teams_safeties = median(play_info$special_teams_safeties, na.rm = TRUE))) %>% 
  dplyr::mutate(target = factor(ifelse(special_teams_result == "Return", 1, 0)),
                time_to_land = snap_time + operation_time + hang_time,
                misskick = dplyr::if_else(kick_direction_intended != kick_direction_actual, 
                                          1, 0, missing = 1))

# Punt return probability predictions
return_prob_preds <- dplyr::bind_rows(readRDS("data/models/train_preds.rds") %>% 
                                        dplyr::mutate(type = "train"),
                                      readRDS("data/models/test_preds.rds") %>% 
                                        dplyr::mutate(type = "test"))

# Take frame 0.5 second before catch (modeling at time of decision) or at 7 seconds
decision_frames <- model_dataset %>% 
  # Find max frame for each play
  dplyr::group_by(play) %>% 
  dplyr::mutate(max_frame = max(frame_id)) %>% 
  dplyr::ungroup() %>% 
  # Find the decision frame as 0.5 seconds before ball lands
  dplyr::mutate(decision_frame = pmin(max_frame, ball_land_frame - 5)) %>% 
  dplyr::filter(frame_id <= decision_frame) %>% 
  dplyr::filter(sec <= 7) %>% 
  dplyr::group_by(play) %>% 
  dplyr::slice_max(sec) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(game_id, play, frame_id, nfl_id, display_name, jersey_number, position, team_name,
                x, y, s, a, o, los_x, ball_to_ez, sec:control, return_scale, kick_return_yardage) %>% 
  # Join in return probabilities
  dplyr::left_join(return_prob_preds %>% 
                     dplyr::select(play, game_id, play_id, frame_id, result, 
                                   dplyr::starts_with(".pred_"), model) %>% 
                     tidyr::pivot_wider(names_from = model,
                                        values_from = dplyr::starts_with(".pred_")),
                   by = c("game_id", "play", "frame_id")) %>% 
  # Join in some play-level information
  dplyr::left_join(play_info %>% 
                     dplyr::select(play, roof, surface, wind, temp, gunners:special_teams_safeties,
                                   kick_length, gain_loss, time_to_land, special_teams_result),
                   by = "play") %>% 
  dplyr::mutate(kick_return_yardage = ifelse(special_teams_result == "Return" & is.na(kick_return_yardage), 0, kick_return_yardage),
                ball_x_dist = abs(los_x - ball_x),
                time_left_to_land = time_to_land - sec,
                outdoors = ifelse(roof %in% c("dome", "closed", "open"), 0, 1),
                grass = ifelse(surface %in% c("grass"), 1, 0))


decision_frames %>% 
  tidyr::pivot_longer(cols = c(ball_x_dist, ball_speed, ball_accel, sec, time_left_to_land,
                               grass, outdoors, temp, wind),
                      names_to = "metric") %>% 
  group_by(metric) %>% 
  summarise(broom::tidy(cor.test(value, kick_length))) %>% View()


# Holdout data from 2020+
holdout <- decision_frames %>% dplyr::filter(season >= 2020)
model_dataset <- decision_frames %>% dplyr::filter(season < 2020)

# Split the data into train and test
set.seed(123)
spl <- rsample::initial_split(model_dataset)
train <- rsample::training(spl)
test <- rsample::testing(spl)

kick_length_left_mod <- train %>% 
  filter(special_teams_result %in% c("Downed", "Touchback", "Out of Bounds")) %>%
  # ggplot(aes(ball_speed, kick_length-ball_x_dist)) +
  # geom_point()
  lm(kick_length ~ dist_to_ball + ball_x_dist + ball_speed*sec,
     data = .)

kick_length_caught_mod <- train %>% 
  filter(!special_teams_result %in% c("Downed", "Touchback", "Out of Bounds")) %>% 
  # ggplot(aes(ball_speed, kick_length-ball_x_dist)) +
  # geom_point()
  lm(kick_length ~ dist_to_ball + ball_x_dist + ball_speed*sec,
     data = .)

# Create 5 folds for cross-validation
train_fold <- train %>%
  rsample::vfold_cv(v = 5)

# Naive RMSE
naive <- train %>% 
  dplyr::summarise(naive = mean(kick_return_yardage, na.rm = TRUE)) %>% 
  dplyr::pull(naive)

# Naive RMSE: 11.740
naive_rmse <- test %>% 
  dplyr::mutate(naive = naive) %>% 
  yardstick::rmse(kick_return_yardage, naive) 

cat(paste0("Punt return yards naive RMSE: ", naive_rmse$.estimate, "\n"))

train %>% summarise_na()

train %>% 
  group_by(grass, outdoors) %>% 
  summarise_kick_return_yards()

train %>% 
  # ggplot(aes(log(kick_return_yardage+20))) +
  ggplot(aes(kick_return_yardage)) +
  geom_histogram()

train %>% 
  group_by(sec) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = sec)

train %>% 
  group_by(time_left_to_land = (time_left_to_land %/% 0.1) * 0.1) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = time_left_to_land)

train %>% 
  group_by(return_scale = (return_scale %/% 0.05) * 0.05) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = return_scale)

train %>% 
  tidyr::pivot_longer(cols = c(control, control_wt),
                      names_to = "type",
                      values_to = "value") %>% 
  group_by(type, value = (value %/% 0.001) * 0.001) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = value, color = type)

train %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with(".pred_Return"),
                      names_to = "type",
                      values_to = "value") %>% 
  group_by(type, value = (value %/% 0.05) * 0.05) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = value, color = type)

train %>% 
  group_by(s = (s %/% 0.05) * 0.05) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = s)

train %>% 
  group_by(los_x = (los_x %/% 5) * 5,
           # x = (x %/% 1) * 1
  ) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = los_x, 
                         # color = factor(los_x)
  ) 

train %>% 
  group_by(dir_x = (dir_x %/% 0.05) * 0.05) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = dir_x)

train %>% 
  tidyr::pivot_longer(cols = c(gunners:special_teams_safeties),
                      names_to = "type",
                      values_to = "value") %>% 
  group_by(type, value = (value %/% 1) * 1) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = value, color = type)

train %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("dist_to_returner"),
                      names_to = "type",
                      values_to = "value") %>% 
  group_by(type, value = (value %/% 1) * 1) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = value, color = type)

train %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("dir_diff"),
                      names_to = "type",
                      values_to = "value") %>% 
  group_by(type, value = (value %/% 2) * 2) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = value, color = type)

train %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("slope_"),
                      names_to = "type",
                      values_to = "value") %>% 
  group_by(type, value = abs((value %/% 0.05) * 0.05)) %>% 
  summarise_kick_return_yards() %>% View()
plot_kick_return_yards(metric = value, color = type)

train %>%
  mutate(across(dplyr::starts_with("slope_"), abs)) %>% 
  tidyr::pivot_longer(cols = c(dplyr::starts_with("dist_to_returner_"),
                               dplyr::starts_with("dir_diff_"),
                               dplyr::starts_with("diff_"),
                               dplyr::starts_with("slope_"),
                               dplyr::starts_with(".pred_Return"),
                               dplyr::starts_with("control"),
                               dplyr::starts_with("avg_dist_"),
                               dplyr::starts_with("s_"),
                               grass, outdoors, los_x,
                               dist_to_ball, returner_to_sideline, time_left_to_land,
                               x, s, a, s_x, a_x, o_x, dir_x, dir_y, return_scale),
                      names_to = "metric") %>% 
  # ggplot(aes(value, kick_return_yardage)) +
  # geom_smooth() +
  # facet_wrap(~ metric, scales = "free_x")
  group_by(metric) %>% 
  summarise(broom::tidy(cor.test(value, kick_return_yardage))) %>% View()

begin_frames %>% 
  lm(kick_return_yardage ~ control + .pred_return_ensemble +  
       s_x + a_x + dir_x + dist_to_returner_1 + dist_to_returner_2 + s_1 + 
       abs(slope_1) + abs(slope_2) + los_x +
       avg_dist_punt_team + avg_dist_ret_team + returner_to_sideline + dist_to_ball, 
     data = .) %>% 
  summary()

train %>% 
  distinct(play, kick_return_yardage) %>% 
  # tidyr::replace_na(list(kick_return_yardage = 0)) %>% 
  ggplot(aes(kick_return_yardage)) +
  geom_histogram()

train %>% 
  mutate(a = log2(kick_return_yardage + 1)) %>% 
  select(a, kick_return_yardage) %>% View

# Model specification
return_yards_rec <- recipes::recipe(kick_return_yardage ~ control + return_scale + los_x +
                                      .pred_return_ensemble + outdoors + grass +
                                      s_x + a_x + dir_x +
                                      dist_to_returner_1 + dist_to_returner_2 + dist_to_returner_3 +
                                      s_1 +
                                      slope_1 + slope_2 + slope_3 +
                                      avg_dist_punt_team + avg_dist_ret_team + returner_to_sideline,
                                    data = train) %>% 
  recipes::step_filter(!is.na(kick_return_yardage)) %>% 
  recipes::step_mutate(slope_1 = abs(slope_1),
                       slope_2 = abs(slope_2),
                       slope_3 = abs(slope_3)) %>% 
  recipes::step_log(kick_return_yardage, base = 2, offset = 20, skip = TRUE)

return_yards_rec_lin <- return_yards_rec %>% 
  recipes::step_rm(dist_to_returner_3, slope_3) %>% 
  recipes::step_ns(.pred_return_ensemble, s_x)

return_yards_rec_lin %>% prep_juice() %>% View()

# Create a workflow
lin_wf <- workflows::workflow() %>%
  workflows::add_recipe(return_yards_rec_lin) %>% 
  workflows::add_model(parsnip::linear_reg(mode = "regression",
                                           penalty = tune::tune()) %>% 
                         parsnip::set_engine("glmnet"))

# Tune hyperparamaters
lin_tune <- lin_wf %>%
  tune::tune_grid(train_fold,
                  grid = tidyr::crossing(penalty = 10 ^ seq(-6, -0.5, .05)),
                  metrics = mset,
                  control = grid_control)

tune::autoplot(lin_tune)

# 9.50 CV, 10.8 log scale(11.8)
lin_wf_best <- lin_wf %>%
  tune::finalize_workflow(parameters = lin_tune %>% 
                            tune::collect_metrics() %>%
                            dplyr::filter(.metric == "rmse") %>%
                            dplyr::arrange(mean) %>%
                            head(1))

# Fit to the training data using best parameters
lin_fit_best <- lin_wf_best %>%
  parsnip::fit(train)

lin_test_preds <- lin_fit_best %>%
  augment(test) %>% 
  dplyr::mutate(.pred = 2^.pred - 20)

lin_fit_best$fit$fit %>% 
  broom::tidy() %>% View()

# 11.6 test rmse/11.6 logged (8.62)
lin_test_preds %>% 
  yardstick::rmse(kick_return_yardage, .pred)

lin_test_preds %>% 
  filter(!is.na(kick_return_yardage)) %>% 
  group_by(.pred = (.pred %/% 1) * 1) %>% 
  summarise_kick_return_yards() %>% 
  plot_kick_return_yards(metric = .pred) +
  geom_abline()

# Fit to all data
lin_return_yards_preds <- lin_fit_best %>%
  augment(decision_frames) %>% 
  # 'un-log'
  dplyr::mutate(.pred = 2^.pred - 20,
                type = dplyr::case_when(
                  season == 2020 ~ "holdout",
                  play %in% train$play ~ "train",
                  TRUE ~ "test"))

pr_returns <- lin_return_yards_preds %>% 
  mutate(yardline_100 = los_x - 10,
         returner_x = x - 10,
         # .pred = .pred + 1.5, # add 1.5 to punt return yard prediction
         .pred_kick_length_left = predict(kick_length_left_mod, newdata = .),
         .pred_kick_length_caught = predict(kick_length_caught_mod, newdata = .),
         .pred_ball_land_left = yardline_100 + .pred_kick_length_left,
         .pred_ball_land_caught = yardline_100 + .pred_kick_length_caught,
         dplyr::across(dplyr::starts_with(".pred_ball_land_"), ~ pmin(99, .))) %>% 
  select(nflfastr_game_id, play_id, frame_id, play, season, 
         type, nfl_id, display_name, jersey_number, position, team_name,
         dplyr::ends_with("_ensemble"), special_teams_result, 
         yardline_100, returner_x, kick_length, 
         .pred_ball_land_left, .pred_ball_land_caught,
         kick_return_yardage, gain_loss, ball_x_dist, 
         .pred_return_yardage = .pred) %>% 
  mutate(ball_land = yardline_100 + kick_length,
         net_touchback = 80,
         net_out_of_bounds = .pred_ball_land_left,
         net_downed = .pred_ball_land_left,
         net_fair_catch = .pred_ball_land_caught,
         net_return = .pred_ball_land_caught - .pred_return_yardage,
         exp_net = .pred_touchback_ensemble*net_touchback +
           .pred_out_of_bounds_ensemble*net_out_of_bounds +
           .pred_downed_ensemble*net_downed +
           .pred_fair_catch_ensemble*net_fair_catch +
           .pred_return_ensemble*net_return,
         net = ball_land - dplyr::coalesce(kick_return_yardage, 0),
         net = ifelse(special_teams_result == "Touchback", 80, net),
         net = pmin(99, net))

# `nflfastR` play-by-play for expected points
ep <- nflreadr::load_pbp(seasons = 2018:2020) %>% 
  dplyr::filter(!is.na(yardline_100)) %>% 
  dplyr::select(game_id, play_id, epa_orig = epa, desc, penalty, fumble, fumble_lost,
                season, yardline_100, down, posteam, defteam, home_team, 
                posteam_timeouts_remaining, defteam_timeouts_remaining,
                roof, half_seconds_remaining, ydstogo) %>% 
  dplyr::inner_join(pr_returns %>% 
                      dplyr::select(nflfastr_game_id, play_id, net, exp_net),
                    by = c("game_id" = "nflfastr_game_id", "play_id")) %>% 
  nflfastR::calculate_expected_points() %>% 
  dplyr::select(-c(td_prob:no_score_prob)) %>% 
  dplyr::as_tibble()

# Calculate expected points of actual punt return assuming no penalties and no fumbles
ep_real <- dplyr::bind_rows(
  # Touchdowns keep as is
  ep %>% 
    dplyr::filter(net == 0 | fumble == 1) %>% 
    dplyr::transmute(game_id, play_id, desc, epa = epa_orig, epa_orig, 
                     fumble, fumble_lost, penalty, touchdown = ifelse(net == 0, 1, 0)),
  # Calculate epa without penalties otherwise
  ep %>% 
    dplyr::filter(net != 0, fumble == 0) %>% 
    dplyr::mutate(down = 1,
                  ydstogo = 10,
                  yardline_100 = net,
                  posteam = defteam,
                  temp = posteam_timeouts_remaining,
                  posteam_timeouts_remaining = defteam_timeouts_remaining,
                  defteam_timeouts_remaining = temp,
                  half_seconds_remaining = half_seconds_remaining - 5.065401) %>% 
    dplyr::rename(ep_orig = ep) %>% 
    dplyr::select(-temp) %>% 
    nflfastR::calculate_expected_points() %>% 
    dplyr::select(-c(td_prob:no_score_prob)) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(epa = -(ep_orig + ep)) %>% 
    dplyr::transmute(game_id, play_id, desc, epa, epa_orig, 
                     fumble, fumble_lost, penalty, touchdown = 0)
) %>% 
  dplyr::mutate(type = "act_epa")

# Calculate expected points based on expected return
ep_exp <- ep %>% 
  dplyr::mutate(down = 1,
                ydstogo = 10,
                yardline_100 = exp_net,
                posteam = defteam,
                temp = posteam_timeouts_remaining,
                posteam_timeouts_remaining = defteam_timeouts_remaining,
                defteam_timeouts_remaining = temp,
                half_seconds_remaining = half_seconds_remaining - 5.065401) %>% 
  dplyr::rename(ep_orig = ep) %>% 
  dplyr::select(-temp) %>% 
  nflfastR::calculate_expected_points() %>% 
  dplyr::select(-c(td_prob:no_score_prob)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(epa = -(ep_orig + ep)) %>% 
  dplyr::select(game_id, play_id, desc, epa, epa_orig, fumble, fumble_lost, penalty) %>% 
  dplyr::mutate(type = "exp_epa")

expected_points <- dplyr::bind_rows(ep_real, ep_exp) %>% 
  # Convert EPA to terms of return team
  dplyr::mutate(epa = -epa) %>% 
  dplyr::group_by(game_id, play_id) %>% 
  tidyr::fill(touchdown, .direction = "downup") %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = type,
                     values_from = epa) %>% 
  dplyr::mutate(diff_epa = act_epa - exp_epa)

pr_returns_epa <- pr_returns %>% 
  left_join(play_info %>% 
              select(play, punt_team = posteam),
            by = "play") %>% 
  left_join(expected_points %>% 
              select(game_id, play_id, fumble, fumble_lost, dplyr::ends_with("_epa")),
            by = c("nflfastr_game_id" = "game_id", "play_id")) %>% 
  mutate(diff_net = exp_net - net,
         dplyr::across(c(net, exp_net), ~ 100 - .))

saveRDS(pr_returns_epa, "output/pr_returns.rds")


