# Predict on holdout

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(tune)))
suppressMessages(suppressWarnings(library(broom)))

option_list <- list(
  make_option("--retrain", type="logical", default = TRUE,
              help="re-train all models?"),
  make_option("--num_cores", type = "integer", default = 2,
              help = "Number of cores")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript R/model_predict.R --retrain TRUE

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

# If needed to re-train all models
if (isTRUE(opt$retrain)) {
  # Read in best workflows from tuning
  lin_wf_best <- readRDS("data/models/lin_wf.rds")
  xg_wf_best <- readRDS("data/models/xg_wf.rds")
  ensemble_wf <- readRDS("data/models/ensemble_wf.rds")
  
  # Training data from initial model fitting
  train <- ensemble_wf$train
  
  # Fit workflows to all of 2018-2019 data
  lin_fit_best_cv <- lin_wf_best %>% 
    parsnip::fit(train)
  xg_fit_best_cv <- xg_wf_best %>% 
    parsnip::fit(train)
  ensemble_fit_cv <- ensemble_wf %>%
    stacks::fit_members()
  
  # Fit workflows to all of 2018-2019 data
  lin_fit_best <- lin_wf_best %>% 
    parsnip::fit(model_dataset)
  xg_fit_best <- xg_wf_best %>% 
    parsnip::fit(model_dataset)
  
  # Replace training data from ensemble with all of 2018-2019 data
  ensemble_wf$train <- model_dataset
  
  ensemble_fit <- ensemble_wf %>%
    stacks::fit_members()
  
  # Save in model fits
  saveRDS(lin_fit_best_cv, "data/models/fits/lin_fit_best_cv.rds")
  saveRDS(xg_fit_best_cv, "data/models/fits/xg_fit_best_cv.rds")
  saveRDS(ensemble_fit_cv, "data/models/fits/ensemble_fit_cv.rds")

  saveRDS(lin_fit_best, "data/models/fits/lin_fit_best.rds")
  saveRDS(xg_fit_best, "data/models/fits/xg_fit_best.rds")
  saveRDS(ensemble_fit, "data/models/fits/ensemble_fit.rds")
  
} else {
  # Read in model fits
  lin_fit_best_cv <- readRDS("data/models/fits/lin_fit_best_cv.rds")
  xg_fit_best_cv <- readRDS("data/models/fits/xg_fit_best_cv.rds")
  ensemble_fit_cv <- readRDS("data/models/fits/ensemble_fit_cv.rds")
  
  lin_fit_best <- readRDS("data/models/fits/lin_fit_best.rds")
  xg_fit_best <- readRDS("data/models/fits/xg_fit_best.rds")
  ensemble_fit <- readRDS("data/models/fits/ensemble_fit.rds")
}

# Predict return probabilities
### Predicting on train -> use only 75% of train data
### Predicting on test -> use all train data
predict_return_prob <- function(type) {
  if (type == "train") {
    data <- model_dataset
    lin_model <- lin_fit_best_cv
    xg_model <- xg_fit_best_cv
    ens_model <- ensemble_fit_cv
    path <- "data/models/train_preds.rds"
  } else if (type == "test") {
    data <- holdout
    lin_model <- lin_fit_best
    xg_model <- xg_fit_best
    ens_model <- ensemble_fit
    path <- "data/models/test_preds.rds"
  }
  
  # Predict
  lin_preds <- lin_model %>% 
    augment(data, type = "prob") %>% 
    dplyr::left_join(lin_model %>% 
                       augment(data) %>% 
                       dplyr::select(play, frame_id, .pred_class),
                     by = c("play", "frame_id")) %>% 
    dplyr::select(game_id, play_id, frame_id, sec, event, 
                  dplyr::starts_with(".pred_"), special_teams_result, result)
  
  xg_preds <- xg_model %>% 
    augment(data, type = "prob") %>% 
    dplyr::left_join(xg_model %>% 
                       augment(data) %>% 
                       dplyr::select(play, frame_id, .pred_class),
                     by = c("play", "frame_id")) %>% 
    dplyr::select(game_id, play_id, frame_id, sec, event, 
                  dplyr::starts_with(".pred_"), special_teams_result, result)
  
  ensemble_preds <- predict(ens_model, data, type = "prob") %>%
    dplyr::bind_cols(data) %>% 
    dplyr::left_join(predict(ens_model, data) %>%
                       dplyr::bind_cols(data) %>% 
                       dplyr::select(play, frame_id, .pred_class),
                     by = c("play", "frame_id")) %>% 
    dplyr::select(game_id, play_id, frame_id, sec, event, 
                  dplyr::starts_with(".pred_"), special_teams_result, result)
  
  # Bind predictions together
  preds <- dplyr::bind_rows(lin_preds %>% 
                              dplyr::mutate(model = 'linear'),
                            xg_preds %>% 
                              dplyr::mutate(model = 'xgboost'),
                            ensemble_preds %>% 
                              dplyr::mutate(model = 'ensemble')) %>% 
    tidyr::unite(play, game_id, play_id, remove = FALSE) %>% 
    dplyr::mutate(result = factor(result)) %>% 
    dplyr::filter(sec >= 2, sec <= 7)
  
  saveRDS(preds, path)
  
  return(preds)
}

train_preds <- predict_return_prob(type = "train")
test_preds <- predict_return_prob(type = "test")


