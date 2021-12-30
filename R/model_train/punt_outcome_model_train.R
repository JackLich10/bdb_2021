### Modeling punt outcomes
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(tune)))
suppressMessages(suppressWarnings(library(broom)))
suppressMessages(suppressWarnings(library(stacks)))

option_list <- list(
  make_option("--tune_lin", type="logical", default = FALSE,
              help="Tune linear model?"),
  make_option("--tune_xgb", type="logical", default = FALSE,
              help="Tune xgb model?"),
  make_option("--blend", type="logical", default = FALSE,
              help="Ensemble?"),
  make_option("--num_cores", type = "integer", default = 2,
              help = "Number of cores")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript R/model_train/punt_outcome_model_train.R --tune_lin TRUE --tune_xgb TRUE --blend TRUE

if (isFALSE(opt$tune_lin) | isFALSE(opt$tune_xgb)) {
  cat("Cannot blend predictions without first tuning both models...\n")
}

# Source helpers
source("R/helpers.R")

cat("Punt return outcome model...\n")

# Set metric as log loss, accuracy
mset <- yardstick::metric_set(yardstick::mn_log_loss, yardstick::accuracy)

model_dataset <- readRDS(paste0("data/proc/model_dataset.rds")) %>% 
  dplyr::mutate(gunner_to_ball = ifelse(is.na(gunner_to_ball), dist_to_ball_1, gunner_to_ball))
# Holdout data from 2020+
holdout <- model_dataset %>% dplyr::filter(season >= 2020)
model_dataset <- model_dataset %>% dplyr::filter(season < 2020)

# Split the data into train and test
set.seed(123)
train_plays <- purrr::map_df(unique(model_dataset$special_teams_result), function(str) {
  # Filter for all plays with that specific `special_teams_result`
  play_type <- model_dataset %>%
    dplyr::filter(special_teams_result == str) %>%
    dplyr::distinct(play) %>%
    dplyr::pull()
  
  # Take 75% as training
  play_type_train <- sample(play_type,
                            size = floor(0.75*length(play_type))) %>% 
    dplyr::as_tibble()
  
  return(play_type_train)
})

train <- model_dataset %>% 
  dplyr::filter(play %in% train_plays$value)

test <- model_dataset %>% 
  dplyr::filter(!play %in% train_plays$value)

dplyr::tibble(play = unique(train$play)) %>% 
  readr::write_csv("data/models/train_csv_plays.csv")

# Create 5 folds for cross-validation (group by play)
train_fold <- train %>%
  rsample::group_vfold_cv(v = 5, group = play)

# Naive log loss (predict mean for each)
naive <- train %>% 
  dplyr::group_by(special_teams_result) %>% 
  dplyr::summarise(n = dplyr::n(),
                   .groups = "drop") %>% 
  dplyr::mutate(pct = n/sum(n)) %>% 
  dplyr::select(special_teams_result, pct) %>% 
  tidyr::pivot_wider(names_from = special_teams_result, values_from = pct)

# Naive log loss: 1.509(1.4141)/1.093 for aggregated
naive_logloss <- test %>% 
  dplyr::select(special_teams_result) %>% 
  dplyr::bind_cols(naive) %>% 
  yardstick::mn_log_loss(special_teams_result, downed, fair_catch, out_of_bounds, return, touchback) 

cat(paste0("Punt outcome naive log loss: ", naive_logloss$.estimate, "\n"))

train %>% summarise_na()

# train %>% 
#   mutate(target = ifelse(special_teams_result == "downed", 1, 0)) %>% 
#   group_by(gunner_to_ball = (gunner_to_ball %/% 1) * 1) %>% 
#   summarise_target() %>% 
#   plot_continuous(metric = gunner_to_ball)

# Create model recipe
rec <- recipes::recipe(special_teams_result ~ s + dist_to_ball + gunner_to_ball + control + 
                         avg_dist_punt_team + avg_dist_ret_team + 
                         dist_to_returner_1 + dist_to_returner_2 + dist_to_returner_3 +
                         s_1 + s_2 + s_3 +
                         slope_1 + slope_2 + slope_3 +
                         ball_to_ez + ball_to_sideline + sec + return_scale,
                       data = train) %>% 
  recipes::step_mutate(slope_1 = abs(slope_1),
                       slope_2 = abs(slope_2),
                       slope_3 = abs(slope_3)) %>% 
  # Get rid of frames before 2 seconds after snap, after 7 seconds
  recipes::step_filter(sec >= 2, sec <= 7)

# Get rid of third variables for multi-collinearity reasons
lin_rec <- rec %>% 
  recipes::step_rm(dist_to_returner_3, slope_3, s_3) %>% 
  recipes::step_interact(terms = ~ dist_to_returner_1:s_1) %>% 
  recipes::step_interact(terms = ~ dist_to_returner_2:s_2) %>% 
  recipes::step_ns(control, ball_to_ez, ball_to_sideline, 
                   dist_to_returner_1, dist_to_returner_2,
                   slope_1, slope_2)

xg_rec <- rec

# View model data
xg_rec %>% prep_juice()

# xg_rec %>% 
#   prep_juice() %>% 
#   tidyr::pivot_longer(cols = c(everything(), -special_teams_result)) %>% 
#   ggplot(aes((value))) +
#   geom_histogram() +
#   facet_wrap(~ name, scales = "free")
# 
# xg_rec %>% 
#   prep_juice() %>% 
#   mutate(return = ifelse(special_teams_result == "Return", 1, 0)) %>% 
#   tidyr::pivot_longer(cols = c(everything(), -c(return, special_teams_result))) %>% 
#   ggplot(aes(value, return)) +
#   geom_abline() +
#   geom_smooth() +
#   facet_wrap(~ name, scales = "free")

# Create workflows
lin_wf <- workflows::workflow() %>%
  workflows::add_recipe(lin_rec) %>% 
  workflows::add_model(parsnip::multinom_reg("classification",
                                             penalty = tune::tune()) %>% 
                         parsnip::set_engine("glmnet"))

xg_wf <- workflows::workflow() %>%
  workflows::add_recipe(xg_rec) %>%
  workflows::add_model(parsnip::boost_tree("classification",
                                           mtry = tune::tune(),
                                           # tree_depth = tune::tune(),
                                           trees = tune::tune(),
                                           learn_rate = tune::tune()) %>% 
                         parsnip::set_engine("xgboost",
                                             early_stopping_rounds = 50,
                                             nthread = opt$num_cores))

# Tune hyperparamaters
if (isTRUE(opt$tune_lin)) {
  lin_tune <- lin_wf %>%
    tune::tune_grid(train_fold,
                    grid = tidyr::crossing(penalty = 10 ^ seq(-6, -0.5, .05)),
                    metrics = mset,
                    control = grid_control)
}

if (isTRUE(opt$tune_xgb)) {
  xg_tune <- xg_wf %>%
    tune::tune_grid(train_fold,
                    # grid = tidyr::crossing(mtry = c(5),
                    #                        # tree_depth = c(5, 6, 7),
                    #                        trees = seq(200, 1300, 50),
                    #                        learn_rate = c(0.01)),
                    grid = tidyr::crossing(mtry = c(4, 5),
                                           # tree_depth = c(5, 6, 7),
                                           trees = seq(200, 1300, 50),
                                           learn_rate = c(0.01)),
                    metrics = mset,
                    control = grid_control)
}

# Analyze results
# tune::autoplot(lin_tune)
# tune::autoplot(xg_tune)

# XGBoost metrics
# xg_tune %>% 
#   tune::collect_metrics() %>% 
#   saveRDS("data/models/xg_tune_metrics.rds")

# xg_metrics <- readRDS("data/models/xg_tune_metrics.rds")
# 
# xg_metrics %>% 
#   dplyr::filter(.metric == "mn_log_loss") %>% 
#   dplyr::arrange(mean)
#   ggplot(aes(trees, mean, color = factor(mtry))) +
#   geom_point() +
#   facet_wrap(~ .metric, scales = "free_y")

# Best CV: 1.100(0.968)/0.769 aggregated
best_cv <- xg_tune %>% 
  tune::collect_metrics() %>% 
  dplyr::filter(.metric == "mn_log_loss") %>% 
  dplyr::arrange(mean) %>% 
  head(1)

xg_tune %>% 
  tune::collect_metrics() %>% 
  dplyr::filter(.metric == "mn_log_loss") %>% 
  dplyr::arrange(mean) %>% 
  transmute(mtry, trees, learn_rate, .metric, scales::number(mean, accuracy = 0.000001)) %>% 
  print(n = 30)

best_params <- tune::select_best(xg_tune, metric = "mn_log_loss")

cat(paste0("Best params: mtry = ", best_params$mtry, ", trees = ", best_params$trees, ", learn_rate = ", best_params$learn_rate,
           "\nBest CV log loss: ", best_cv$mean, "\n"))

# Select best cross validated log loss parameters
xg_wf_best <- xg_wf %>%
  tune::finalize_workflow(best_params)

saveRDS(xg_wf_best, "data/models/xg_wf.rds")

# Training log loss: 1.12(1.00)/0.844 for aggregated
lin_tune %>% 
  tune::collect_metrics() %>% 
  dplyr::filter(.metric == "mn_log_loss") %>% 
  dplyr::arrange(mean)

lin_wf_best <- lin_wf %>%
  tune::finalize_workflow(tune::select_best(lin_tune, metric = "mn_log_loss"))

saveRDS(lin_wf_best, "data/models/lin_wf.rds")

cat(paste0("Completed tuning for all model specifications...\n"))

if (isTRUE(opt$blend)) {
  # Ensemble both models
  # Chosse best penalty for lasso
  lin_chosen <- lin_tune %>%
    tune::filter_parameters(parameters = tune::select_best(lin_tune, 
                                                           metric = "mn_log_loss"))
  # Choose best parameters for XGB
  xg_chosen <- xg_tune %>%
    tune::filter_parameters(parameters = tune::select_best(xg_tune, 
                                                           metric = "mn_log_loss"))
  
  saveRDS(lin_chosen, "data/models/lin_chosen.rds")
  saveRDS(xg_chosen, "data/models/xg_chosen.rds")
  
  # Ensemble using `stacks`
  ensemble <- stacks::stacks() %>%
    stacks::add_candidates(lin_chosen) %>%
    stacks::add_candidates(xg_chosen)
  
  ensemble_wf <- ensemble %>%
    stacks::blend_predictions(metric = yardstick::metric_set(yardstick::mn_log_loss))
  
  # Save ensemble workflow
  saveRDS(ensemble_wf, "data/models/ensemble_wf.rds")
  cat(paste0("Completed ensemble\n"))
}


