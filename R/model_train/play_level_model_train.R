### Modeling return probability
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(tune)))
suppressMessages(suppressWarnings(library(broom)))

# option_list <- list(
#   make_option("--tune_lin", type="logical", default = FALSE,
#               help="Tune linear model?")
# )
# opt <- parse_args(OptionParser(option_list=option_list))
# Rscript R/model_train/play_level_model_train.R

# Source helpers
source("R/helpers.R")

cat("Punt return play-level return outcome model...\n")

# Set metric as log loss, accuracy
mset <- yardstick::metric_set(yardstick::mn_log_loss, yardstick::accuracy)

# Returner starting points
returner_starting_points <- purrr::map_df(2018:2020, function(sn) {
  readRDS(paste0("data/proc/punts", sn, ".rds")) %>% 
    dplyr::filter(returner == 1) %>% 
    dplyr::group_by(game_id, play_id) %>% 
    dplyr::slice_min(frame_id) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(game_id, play_id, frame_id, event, returner_x = x, returner_y = y)
})

# Play level data
play_info <- purrr::map_df(2018:2020, function(sn) {
  readRDS(paste0("data/proc/punt_plays", sn, ".rds")) %>% 
    dplyr::mutate(season = sn)
}) 

play_info <- play_info %>% 
  tidyr::replace_na(list(kick_direction_intended = "C",
                         snap_time = mean(play_info$snap_time, na.rm = TRUE),
                         operation_time = mean(play_info$operation_time, na.rm = TRUE),
                         hang_time = mean(play_info$hang_time, na.rm = TRUE),
                         punt_rushers = median(play_info$punt_rushers, na.rm = TRUE),
                         gunners = median(play_info$gunners, na.rm = TRUE),
                         vises = median(play_info$vises, na.rm = TRUE),
                         special_teams_safeties = median(play_info$special_teams_safeties, na.rm = TRUE))) %>% 
  dplyr::mutate(target = factor(ifelse(special_teams_result == "Return", 1, 0)),
                time_to_land = snap_time + operation_time + hang_time,
                misskick = dplyr::if_else(kick_direction_intended != kick_direction_actual, 
                                          1, 0, missing = 1),
                intended_kick = ifelse(kick_direction_intended == "C", 1, 0),
                intended_kick_side = dplyr::case_when(
                  kick_direction_intended == "C" ~ 0,
                  kick_direction_intended == "L" ~ 1,
                  kick_direction_intended == "R" ~ -1),
                outdoors = ifelse(roof %in% c("dome", "closed", "open"), 0, 1),
                grass = ifelse(surface %in% c("grass"), 1, 0)) %>% 
  # Join in returner starting coordinates
  dplyr::inner_join(returner_starting_points %>% 
                     dplyr::select(game_id, play_id, returner_x, returner_y),
                   by = c("game_id", "play_id")) %>% 
  dplyr::mutate(side = ifelse(returner_y < 25, -1, 1))

play_info %>% summarise_na()

# Split the data into train and test
set.seed(123)
spl <- rsample::initial_split(play_info %>% dplyr::filter(season < 2020), strata = target)
train <- rsample::training(spl)
test <- rsample::testing(spl)

# Create 5 folds for cross-validation
train_fold <- train %>%
  rsample::vfold_cv(v = 5, strata = target)

# Naive RMSE (predict mean for test as baseline)
naive <- train %>% 
  dplyr::summarise(naive = mean(as.numeric(target)-1)) %>% 
  dplyr::pull(naive)

# Naive log loss: 0.7726
naive_logloss <- test %>% 
  dplyr::mutate(naive = naive) %>% 
  yardstick::mn_log_loss(target, naive) 

cat(paste0("Punt outcome naive log loss: ", naive_logloss$.estimate, "\n"))

# good predictors: 
# misskick, time_to_land, kick_length, punt_rushers, vises

train %>% 
  mutate(target = as.numeric(target) - 1) %>% 
  group_by(outdoors) %>% 
  summarise_target()

train %>% 
  mutate(target = as.numeric(target) - 1) %>% 
  group_by(side, intended_kick_side) %>% 
  summarise_target()

train %>% 
  ggplot(aes(returner_y)) +
  geom_histogram()

train %>% 
  mutate(target = as.numeric(target) - 1) %>% 
  tidyr::pivot_longer(cols = c(time_to_land, kick_length, intended_kick, side,
                               punt_rushers, yardline_100, returner_x, returner_y,
                               wind, presnap_wp, temp, grass, outdoors)) %>% 
  group_by(name) %>%
  summarise(broom::tidy(cor.test(value, target))) %>% View()
  ggplot(aes(value, target)) +
  # geom_abline() +
  geom_smooth() +
  facet_wrap(~ name, scales = "free_x")

play_rec <- recipes::recipe(target ~ time_to_land + kick_length +
                              punt_rushers + vises +
                              yardline_100 + returner_x +
                              temp + grass + outdoors + wind + 
                              presnap_wp + intended_kick + misskick,
                            data = train) %>% 
  recipes::step_ns(kick_length,
                   punt_rushers, yardline_100, 
                   time_to_land, returner_x, deg_free = tune::tune())

play_rec %>% prep_juice() %>% View()

# Create a workflow
lin_wf <- workflows::workflow() %>%
  workflows::add_recipe(play_rec) %>% 
  workflows::add_model(parsnip::logistic_reg(mode = "classification",
                                             penalty = tune::tune()) %>% 
                         parsnip::set_engine("glmnet"))

# Tune hyperparamaters
lin_tune <- lin_wf %>%
  tune::tune_grid(train_fold,
                  grid = tidyr::crossing(penalty = 10 ^ seq(-6, -0.5, .05),
                                         deg_free = c(2)),
                  metrics = mset,
                  control = grid_control)

tune::autoplot(lin_tune)

# 0.498 CV/0.533
lin_wf_best <- lin_wf %>%
  tune::finalize_workflow(parameters = lin_tune %>% 
                            tune::collect_metrics() %>%
                            dplyr::filter(.metric == "mn_log_loss") %>%
                            dplyr::arrange(mean) %>%
                            head(1))

# Fit to the training data using best parameters
lin_fit_best <- lin_wf_best %>%
  parsnip::fit(train)

lin_fit_best$fit$fit %>% 
  broom::tidy()

lin_test_preds <- lin_fit_best %>%
  augment(test, type = "prob")

# 0.484 test log loss/0.535
lin_test_preds %>%
  yardstick::mn_log_loss(target, .pred_0)

# Fit to all data for frame-level modelling
play_level_preds <- lin_fit_best %>%
  augment(play_info, type = "prob") %>% 
  dplyr::transmute(season, play, return = as.numeric(target)-1, return_scale = .pred_1)

play_level_preds %>% 
  mutate(return_scale = 1- return_scale) %>% 
  group_by(season) %>%
  yardstick::mn_log_loss(factor(return), return_scale)

# Save predictions
saveRDS(play_level_preds %>% dplyr::select(-c(season)), "data/proc/play_level_preds.rds")

# readRDS("data/proc/play_level_preds.rds") %>%
#   mutate(return_scale = 1- return_scale) %>%
#   yardstick::mn_log_loss(factor(return), return_scale)

play_level_preds %>% 
  group_by(season, return_scale = (return_scale %/% 0.05) * 0.05) %>% 
  summarise(plays = n(),
            return = mean(return)) %>% 
  ggplot(aes(return_scale, return, color = factor(season))) +
  geom_abline() +
  geom_point(aes(size = plays)) +
  geom_smooth(aes(weight = plays)) +
  scale_size_continuous(range = c(0.5, 3))



