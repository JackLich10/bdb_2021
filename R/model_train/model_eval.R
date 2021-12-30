### Model evaluation

lin_fit <- readRDS("data/models/fits/lin_fit_best.rds")
xg_fit <- readRDS("data/models/fits/xg_fit_best.rds")

clean_feature_names <- function(feat) {
  dplyr::case_when(
    feat == "Avg Dist Ret Team" ~ "Average distance of return team to returner",
    feat == "Avg Dist Punt Team" ~ "Average distance of punt team to returner",
    feat == "Ball To Ez" ~ "Distance of ball to endzone",
    feat == "Ball To Sideline" ~ "Distance of ball to sideline",
    feat == "Dist To Ball" ~ "Distance of returner to ball",
    feat == "Return Scale" ~ "Play-level return probability",
    feat == "Control" ~ "Team field control",
    feat == "Gunner To Ball" ~ "Distance of closest gunner to ball",
    feat == "Sec" ~ "Seconds into snap",
    feat == "S" ~ "Speed of returner",
    feat == "S 1" ~ "Speed of defender 1",
    feat == "S 2" ~ "Speed of defender 2",
    feat == "S 3" ~ "Speed of defender 3",
    feat == "Slope 1" ~ "Slope of defender 1",
    feat == "Slope 2" ~ "Slope of defender 2",
    feat == "Slope 3" ~ "Slope of defender 3",
    feat == "Dist To Returner 1" ~ "Distance of defender 1 to returner",
    feat == "Dist To Returner 2" ~ "Distance of defender 2 to returner",
    feat == "Dist To Returner 3" ~ "Distance of defender 3 to returner",
    TRUE ~ feat
  )
}

lin_fit$fit$fit %>% 
  broom::tidy(conf.int = TRUE) %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  mutate(term = stringr::str_remove(term, "_ns.*")) %>% 
  group_by(term) %>% 
  summarise(estimate = mean(abs(estimate))) %>% 
  mutate(term = stringr::str_to_title(stringr::str_replace_all(term, "_", " ")),
         term = clean_feature_names(term),
         term = forcats::fct_reorder(term, estimate)) %>% 
  filter(!stringr::str_detect(term, " X ")) %>% 
  ggplot(aes(estimate, term)) +
  # geom_vline(xintercept = 0,  lty = 2) +
  geom_point() + 
  jacklich::theme_jack() +
  labs(title = "Average effect size of term in shrunken GAM",
       subtitle = "Punt return outcome model",
       x = "Average absolute effect size among five classes",
       y = NULL)
ggsave("output/lin_coef.png", .Last.value, dpi = 700, height = 5, width = 8)

# XGBoost importance
importances <- xgboost::xgb.importance(model = xg_fit$fit$fit$fit)

importances %>%
  dplyr::mutate(Feature = stringr::str_to_title(stringr::str_replace_all(Feature, "_", " ")),
                Feature = clean_feature_names(Feature),
                Feature = forcats::fct_reorder(Feature, Gain)) %>%
  ggplot(aes(Gain, Feature)) +
  geom_point() +
  jacklich::theme_jack() +
  labs(title = "Importance of each term in xgboost",
       subtitle = "Punt return outcome model | Gini impurity index",
       x = "Gain",
       y = NULL)
ggsave("output/xg_importance.png", .Last.value, dpi = 700, height = 5, width = 8)


xg_test_preds <- xg_fit_best %>% 
  augment(test, type = "prob") %>% 
  dplyr::left_join(xg_fit_best %>% 
                     augment(test) %>% 
                     dplyr::select(play, frame_id, .pred_class),
                   by = c("play", "frame_id"))

# Test log loss: 1.10(0.955)/ 0.784 aggregated
xg_test_preds %>%
  yardstick::mn_log_loss(special_teams_result,
                         .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback)

# Testing accuracy: 0.599(0.632)/0.651 aggregated
xg_test_preds %>% 
  yardstick::accuracy(special_teams_result, .pred_class)

xg_test_preds %>% 
  yardstick::conf_mat(special_teams_result, .pred_class) %>% 
  autoplot()


lin_fit_best <- lin_wf_best %>% 
  parsnip::fit(train)

lin_fit_best$fit$fit %>% 
  broom::tidy(conf.int = TRUE) %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  ggplot(aes(estimate, term, color = class)) +
  geom_vline(xintercept = 0,  lty = 2) +
  geom_point() + 
  facet_wrap(~ class, scales = "free_x")

lin_test_preds <- lin_fit_best %>% 
  augment(test, type = "prob") %>% 
  dplyr::left_join(lin_fit_best %>% 
                     augment(test) %>% 
                     dplyr::select(play, frame_id, .pred_class),
                   by = c("play", "frame_id"))

# Testing log loss: 1.11(1.01)/0.831 for aggregated
lin_test_preds %>% 
  yardstick::mn_log_loss(special_teams_result, 
                         .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback) 

# Testing accuracy: 0.581(0.611)/0.612 for aggregated
lin_test_preds %>% 
  group_by(special_teams_result) %>% 
  summarise(frames = n(),
            accuracy = mean(special_teams_result == .pred_class))

lin_test_preds %>% 
  yardstick::conf_mat(special_teams_result, .pred_class)

lin_test_preds %>% 
  select(-.pred_class) %>% 
  filter(play %in% sample(unique(lin_test_preds$play), size = 9)) %>% 
  mutate(play = paste0(play, ": ", special_teams_result)) %>% 
  tidyr::pivot_longer(cols = starts_with(".pred_"),
                      names_prefix = ".pred") %>% 
  ggplot(aes(sec, value, color = name)) +
  geom_line() +
  geom_text(aes(label = ifelse(event == "None", NA_character_, event))) +
  facet_wrap(~ play)



### Ensemble
autoplot(ensemble_wf, type = "members")

autoplot(ensemble_wf, type = "weights")

stacks::collect_parameters(ensemble_wf, "xg_chosen")

stacks::collect_parameters(ensemble_wf, "lin_chosen")

ensemble_fit_cv <- ensemble_wf %>%
  stacks::fit_members()

ensemble_test_preds_cv <- predict(ensemble_fit_cv, test, type = "prob") %>%
  dplyr::bind_cols(test) %>% 
  dplyr::bind_cols(.pred_class = predict(ensemble_fit_cv, test))

ensemble_test_preds_cv$.pred_class <- ensemble_test_preds_cv$.pred_class$.pred_class

# 0.974 test log loss
ensemble_test_preds_cv %>% 
  # group_by(sec) %>% 
  yardstick::mn_log_loss(special_teams_result, 
                         .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback)

ensemble_fit_cv$coefs %>% 
  broom::tidy()

# 63.4% test accuracy
ensemble_test_preds_cv %>% 
  # group_by(special_teams_result) %>% 
  # summarise(frames = n(),
  #           accuracy = mean(special_teams_result == .pred_class))
  yardstick::accuracy(special_teams_result, .pred_class)

# Testing log loss: 0.785 for aggregated
ensemble_test_preds %>% 
  yardstick::mn_log_loss(result, 
                         `.pred_Fair Catch`, .pred_Poison, .pred_Return) 

# Testing accuracy: 0.646 for aggregated
ensemble_test_preds %>% 
  yardstick::accuracy(result, .pred_class)



###### Full predictions and model fits

train_preds <- readRDS("data/models/train_preds.rds")
test_preds <- readRDS("data/models/test_preds.rds")

train_cv_plays <- readr::read_csv("data/models/train_csv_plays.csv")

preds <- dplyr::bind_rows(train_preds %>% dplyr::mutate(type = "train"),
                          test_preds %>% dplyr::mutate(type = "test"))

preds <- return_prob_preds

return_prob_preds %>% 
  filter(type == "test",
         special_teams_result == "downed") %>% 
  group_by(model, .pred_class) %>% 
  summarise(predictions = dplyr::n()) %>% 
  mutate(pct = predictions/sum(predictions))

preds <- preds %>% 
  mutate(type = ifelse(type == "test", "holdout", type),
         type = ifelse(type == "train" & !play %in% train_cv_plays$play, "test", type))

# 1.12 log loss(0.977)/0.852, 0.781 aggregated
preds %>% 
  dplyr::group_by(type, model) %>% 
  yardstick::mn_log_loss(special_teams_result,
                         .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback)

# 0.606(0.633), 0.667 accuracy aggregated
preds %>% 
  dplyr::group_by(type, model) %>% 
  yardstick::accuracy(special_teams_result, .pred_class)

preds %>% 
  group_by(type, model, special_teams_result) %>% 
  summarise(frames = dplyr::n(),
            plays = dplyr::n_distinct(play),
            accuracy = mean(special_teams_result == .pred_class),
            .groups = "drop") %>% 
  mutate(special_teams_result = stringr::str_to_title(stringr::str_replace_all(special_teams_result, "_", " ")),
         special_teams_result = paste0(special_teams_result, " (", plays, " plays)"),
         special_teams_result = forcats::fct_reorder(special_teams_result, accuracy),
         model = forcats::fct_reorder(model, accuracy, sum)) %>% 
  filter(type == "holdout") %>% 
  ggplot(aes(accuracy, special_teams_result, fill = model)) +
  geom_col(position = position_dodge(0.9)) +
  geom_text(aes(label = scales::percent(accuracy, accuracy = 0.1)),
            size = 3, hjust = 1, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set2", guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  # facet_wrap(~ type) +
  jacklich::theme_jack() +
  labs(title = "Model evaluation",
       subtitle = "Test accuracy across all frames",
       x = "Accuracy",
       y = NULL,
       fill = "Model")

ggsave("output/test_class_accuracy.png", .Last.value, dpi = 700, height = 5, width = 8)




metrics_summary <- dplyr::bind_rows(
  
  # all frames prediction accuracy 
  preds %>% 
    dplyr::group_by(type, model) %>% 
    dplyr::summarise(frames = dplyr::n(),
                     .estimate = mean(special_teams_result == .pred_class),
                     .groups = "drop") %>% 
    dplyr::mutate(.metric = "accuracy",
                  frame = "all"),
  
  # last frame accuracy
  preds %>% 
    dplyr::group_by(type, model, play) %>% 
    dplyr::slice_max(sec) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(type, model) %>% 
    dplyr::summarise(plays = dplyr::n(),
                     .estimate = mean(special_teams_result == .pred_class),
                     .groups = "drop") %>% 
    dplyr::mutate(.metric = "accuracy",
                  frame = "last"),
  
  # first frame accuracy
  preds %>% 
    dplyr::group_by(type, model, play) %>% 
    dplyr::slice_min(sec) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(type, model) %>% 
    dplyr::summarise(plays = dplyr::n(),
                     .estimate = mean(special_teams_result == .pred_class),
                     .groups = "drop") %>% 
    dplyr::mutate(.metric = "accuracy",
                  frame = "first"),
  
  # modal prediction accuracy
  preds %>% 
    dplyr::group_by(type, model, play, special_teams_result) %>% 
    dplyr::summarise(.pred_class = collapse::fmode(.pred_class),
                     .groups = "drop") %>% 
    dplyr::group_by(type, model) %>% 
    dplyr::summarise(plays = dplyr::n(),
                     .estimate = mean(special_teams_result == .pred_class),
                     .groups = "drop") %>% 
    dplyr::mutate(.metric = "accuracy",
                  frame = "modal"),
  
  # all frames log loss
  preds %>% 
    dplyr::group_by(type, model) %>% 
    yardstick::mn_log_loss(special_teams_result,
                           .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback) %>% 
    dplyr::mutate(frame = "all"),
  
  # last frame log loss
  preds %>% 
    dplyr::group_by(type, model, play) %>% 
    dplyr::slice_max(sec) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(type, model) %>% 
    yardstick::mn_log_loss(special_teams_result,
                           .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback) %>% 
    dplyr::mutate(frame = "last"),
  
  # first frame log loss
  preds %>% 
    dplyr::group_by(type, model, play) %>% 
    dplyr::slice_min(sec) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(type, model) %>% 
    yardstick::mn_log_loss(special_teams_result,
                           .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback) %>% 
    dplyr::mutate(frame = "first")
) %>% 
  dplyr::select(-.estimator) %>% 
  dplyr::group_by(type) %>% 
  tidyr::fill(frames, plays, .direction = "downup") %>% 
  dplyr::ungroup()

model_eval_table <- metrics_summary %>% 
  tidyr::pivot_wider(names_from = frame,
                     values_from = .estimate) %>% 
  tidyr::pivot_wider(names_from = .metric,
                     values_from = c(first, last, all, modal)) %>% 
  dplyr::filter(type == "holdout") %>% 
  dplyr::mutate(type = paste0(type, " (", plays, " plays, ", scales::number(frames, accuracy = 0.1), " frames)")) %>% 
  dplyr::select(-c(modal_mn_log_loss, plays, frames)) %>% 
  gt::gt(groupname_col = "type", rowname_col = "model") %>% 
  # gt::row_group_order(groups = c("train (3959 plays, 175806 frames)", "test (1712 plays, 76208 frames)")) %>% 
  gt::tab_header(title = gt::md("**Punt outcome model evaluation**"),
                 subtitle = gt::md("*Punt returns from the 2020 NFL season, excluding muffs*")) %>%
  gt::cols_label(first_accuracy = "First",
                 first_mn_log_loss = "First",
                 last_accuracy = "Last",
                 last_mn_log_loss = "Last",
                 all_accuracy = "All",
                 all_mn_log_loss = "All",
                 modal_accuracy = "Modal") %>% 
  gt::tab_spanner(columns = dplyr::contains("_accuracy"),
                  label = "Accuracy") %>% 
  gt::tab_spanner(columns = dplyr::contains("_mn_log_loss"),
                  label = "Log Loss") %>% 
  gt::fmt_number(columns = c(dplyr::contains("_mn_log_loss")),
                 decimals = 2) %>% 
  gt::fmt_percent(columns = c(dplyr::contains("_accuracy")),
                  decimals = 1) %>% 
  gt::data_color(columns = c(dplyr::contains("_accuracy")),
                 colors = scales::col_numeric(
                   palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
                   domain = NULL)) %>%
  gt::data_color(columns = c(dplyr::contains("_mn_log_loss")),
                 colors = scales::col_numeric(
                   palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
                   domain = NULL,
                   reverse = TRUE)) %>%
  # gt::tab_footnote(footnote = "25% of train metrics are out-of-sample | 100% of test metrics are out-of-sample",
  #                  locations = gt::cells_row_groups(
  #                    groups = dplyr::everything()
  #                  )) %>% 
  # gt::tab_source_note("Table: Jack Lichtenstein @jacklich10 | Data: 2022 NFL Big Data Bowl") %>%
  gtExtras::gt_theme_espn()

gt::gtsave(model_eval_table, "output/model_eval_table.png")


preds


calculate_metrics_by_sec <- function(type) {
  
  if (type == "train") {
    data <- train_preds
  } else if (type == "test") {
    data <- test_preds
  }
  
  # Log loss and accuracy at each frame
  dplyr::bind_rows(data %>% 
                     dplyr::group_by(model, sec) %>% 
                     yardstick::mn_log_loss(special_teams_result, 
                                            .pred_downed, .pred_fair_catch, .pred_out_of_bounds, .pred_return, .pred_touchback),
                   data %>% 
                     dplyr::group_by(model, sec) %>% 
                     dplyr::summarise(n = dplyr::n(),
                                      .estimate = mean(.pred_class == special_teams_result),
                                      .groups = "drop") %>% 
                     dplyr::mutate(.metric = "accuracy")) %>% 
    dplyr::group_by(sec) %>% 
    tidyr::fill(n, .direction = "updown") %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(type = type)
}

metrics_sec <- dplyr::bind_rows(calculate_metrics_by_sec(type = "train"),
                                calculate_metrics_by_sec(type = "test"))

metrics_sec %>% 
  ggplot(aes(sec, .estimate, color = model)) +
  geom_line() +
  geom_point(aes(size = n)) +
  scale_size_continuous(range = c(0.5, 2.5)) +
  facet_wrap(.metric ~ type, scales = "free_y")

# Plot some examples
test_preds %>% 
  select(-.pred_class) %>% 
  filter(play %in% sample(unique(test_preds$play), size = 9)) %>% 
  mutate(play = paste0(play, ": ", special_teams_result)) %>% 
  tidyr::pivot_longer(cols = starts_with(".pred_"),
                      names_prefix = ".pred_",
                      names_to = "outcome",
                      values_to = "probability") %>% 
  ggplot(aes(sec, probability, color = outcome, lty = model)) +
  geom_line(aes(size = ifelse(outcome == result, 1.5, 0.8))) +
  geom_text(aes(sec, 0.2, label = ifelse(event == "None", NA_character_, event)),
            color = "black", inherit.aes = FALSE, stat = "unique") +
  scale_y_continuous(labels = scales::percent) +
  scale_size_identity() +
  facet_wrap(~ play) +
  jacklich::theme_jack() +
  labs(x = "Seconds into play")

punts %>% 
  filter(display_name == "football")






sample_ep <- tidyr::crossing(season = 2018,
                             yardline_100 = seq(80, 99, by = 1),
                             down = 1, 
                             ydstogo = 10,
                             posteam = "NYJ", 
                             home_team = c("NYJ", "MIA"), 
                             posteam_timeouts_remaining = c(0, 1, 2, 3), 
                             defteam_timeouts_remaining = c(0, 1, 2, 3), 
                             roof = c("closed", "dome", "open", "outdoors"), 
                             half_seconds_remaining = c(30, 60, 300, 600, 900, 1200, 1500, 1800)) %>% 
  nflfastR::calculate_expected_points() %>% 
  group_by(yardline_100) %>% 
  summarise(ep = mean(ep)) %>%
  mutate(diff = dplyr::lag(ep) - ep)

sample_ep %>% 
  ggplot(aes(yardline_100, ep)) +
  geom_line()

pr_returns_epa$ep

# determine if possibility of muff/fumbles outweighs cost of fair catch inside 5 yardline?
