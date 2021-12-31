### Visualizations

source("R/ggfield.R")

punts <- readRDS(paste0("data/proc/punts2020.rds"))
control <- readRDS(paste0("data/proc/control2020.rds"))
vision_cones <- readRDS(paste0("data/proc/vision_cones2020.rds"))

# Play level data
play_info <- purrr::map_df(2018:2020, function(sn) {
  readRDS(paste0("data/proc/punt_plays", sn, ".rds")) %>% 
    dplyr::mutate(season = sn)
}) 

pr_returns_epa <- readRDS("output/pr_returns.rds")

# Punt return probability predictions
return_prob_preds <- dplyr::bind_rows(readRDS("data/models/train_preds.rds") %>% 
                                        dplyr::mutate(type = "train"),
                                      readRDS("data/models/test_preds.rds") %>% 
                                        dplyr::mutate(type = "test"))

vision_cone_control <- vision_cones %>% 
  dplyr::rename(returner_id = nfl_id) %>% 
  dplyr::inner_join(control, by = c("game_id", "play_id", "frame_id", "x", "y")) %>% 
  dplyr::mutate(play = paste0(game_id, "_", play_id)) %>% 
  dplyr::select(game_id, play_id, frame_id, play, x, y, control)

rm(control, vision_cones)


# vision_cone_control %>% 
#   left_join(returners %>% 
#               select(game_id, play_id, frame_id, special_teams_result, ball_land_frame,
#                      returner_x = x, 
#                      returner_y = y),
#             by = c("game_id", "play_id", "frame_id")) %>% 
#   mutate(dist_to_returner = sqrt((x-returner_x)^2 + (y-returner_y)^2)) %>%
#   group_by(play, frame_id) %>% 
#   summarise(control_wt = sum(control*dist_to_returner)/sum(dist_to_returner),
#             control = mean(control),
#             across(c(ball_land_frame, special_teams_result), unique),
#             .groups = "drop") %>% 
#   tidyr::pivot_longer(cols = starts_with("control")) %>% 
#   ggplot(aes(frame_id, value, color = name)) +
#   geom_line() +
#   geom_text(aes(ball_land_frame, 0, label = special_teams_result),
#             stat = "unique", size = 3) +
#   facet_wrap(~ play)
# 

# Palette 
outcome_pal <- dplyr::tibble("Return" = "#66C2A5",
                             "Fair Catch" = "#FC8D62", 
                             "Downed" = "#8DA0CB", 
                             "Touchback" = "#E78AC3", 
                             "OOB" = "#A6D854",
                             "Actual" = "#FFD92F",
                             "Expected" = "#E5C494") %>% 
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "outcome",
                      values_to = "color")

plot_vision_control <- function(p, f = NULL, anim = TRUE, exp_return = NULL, pause = 13) {
  
  play <- play_info %>% 
    dplyr::filter(play == p)
  
  # title and subtitile
  title <- stringr::str_remove_all(unique(play$desc), "\\(.*?\\)") %>% 
    stringr::str_squish() %>% 
    stringr::str_replace(., " \\.", ".")
  
  title <- title %>% 
    stringr::str_remove(., ", .*") %>% 
    paste0(., ". ", title %>% 
             stringr::str_split("\\.") %>% 
             unlist() %>% 
             stringi::stri_remove_empty() %>% 
             dplyr::last())
  
  subtitle <- paste0("Quarter ", unique(play$qtr), ": ", unique(play$home_team), 
                     " vs. ", unique(play$away_team),
                     ", Week ", unique(play$week), " ", unique(play$season), " NFL season")
  
  punts_filtered <- punts %>% 
    dplyr::filter(play == p) %>% 
    dplyr::left_join(nflfastR::teams_colors_logos %>% 
                       dplyr::select(team_abbr, fill = team_color, color = team_color2),
                     by = c("team_name" = "team_abbr")) %>% 
    tidyr::replace_na(list(color = "#d9d9d9",
                           fill = "#935e38")) %>% 
    # size, shape, color, fill for scale_*_identity
    dplyr::mutate(
      size = dplyr::case_when(
        team %in% c("home", "away") ~ 4,
        team == "football" ~ 3),
      shape = dplyr::case_when(
        team %in% c("home", "away") ~ 21,
        team == "football" ~ 16))
  
  if (!is.null(f)) {
    punts_filtered <- punts_filtered %>% 
      dplyr::filter(frame_id == f)
    
    vision_cone_control <- vision_cone_control %>% 
      dplyr::filter(frame_id == f)
  }
  
  vision_cone_control_filtered <- vision_cone_control %>%
    dplyr::filter(play == p) %>%
    dplyr::left_join(punts_filtered %>% dplyr::distinct(frame_id, sec),
                     by = "frame_id") %>% 
    dplyr::arrange(frame_id)
  
  if (!is.null(exp_return)) {
    # Expected return yards
    exp_return <- exp_return %>% 
      dplyr::filter(play == p) %>% 
      dplyr::transmute(play, frame_id,
                       net_touchback, net_out_of_bounds, net_downed, net_fair_catch, net_return,
                       exp_net = 100 - exp_net, net = 100 - net) %>% 
      tidyr::pivot_longer(cols = -c(play, frame_id),
                          names_to = "result",
                          names_prefix = "net_",
                          values_to = "net") %>% 
      dplyr::mutate(result = stringr::str_to_title(stringr::str_replace_all(result, "_", " ")),
                    result = ifelse(result == "Out Of Bounds", "OOB", result),
                    result = dplyr::case_when(
                      result == "Net" ~ "Actual",
                      result == "Exp Net" ~ "Expected",
                      TRUE ~ result),
                    vjust = ifelse(result %in% c("OOB", "Expected"), -0.5, 1.1),
                    hjust = ifelse(result %in% c("Actual", "Expected"), 0, 1),
                    x = ifelse(result %in% c("Actual", "Expected"), 53+1.1, 0-1.1)) %>% 
      dplyr::left_join(outcome_pal,
                       by = c("result" = "outcome")) 
    
    # Find frame to repeat
    repeat_frame <- vision_cone_control_filtered %>% 
      dplyr::filter(frame_id == min(exp_return$frame_id))
    
    # Repeat vision cones pause times
    repeated_vision_control <- purrr::map(seq_len(pause), ~ repeat_frame) %>% 
      dplyr::bind_rows(.id="id") %>% 
      dplyr::mutate(frame_id = frame_id + as.numeric(id))
    
    vision_cone_control_filtered <- vision_cone_control_filtered %>% 
      dplyr::mutate(frame_id = ifelse(frame_id > min(exp_return$frame_id), frame_id + pause, frame_id)) %>% 
      dplyr::bind_rows(repeated_vision_control) %>% 
      dplyr::arrange(frame_id)
    
    # Repeat expected return yards pause+6 times
    exp_return <- purrr::map(seq_len(pause+6), ~ exp_return) %>% 
      dplyr::bind_rows(.id="id") %>% 
      dplyr::mutate(frame_id = frame_id + as.numeric(id) - 1)
    
    # Repeat player locations pause times
    punts_filtered_frame <- punts_filtered %>% 
      dplyr::filter(frame_id == min(exp_return$frame_id))
    
    repeated_players <- purrr::map(seq_len(pause), ~ punts_filtered_frame) %>% 
      dplyr::bind_rows(.id="id") %>% 
      dplyr::mutate(frame_id = frame_id + as.numeric(id))
    
    punts_filtered <- punts_filtered %>% 
      dplyr::mutate(frame_id = ifelse(frame_id > min(exp_return$frame_id), frame_id + pause, frame_id)) %>% 
      dplyr::bind_rows(repeated_players) %>% 
      dplyr::arrange(frame_id)
    
    min_frame <- min(vision_cone_control_filtered$frame_id)
    max_frame <- max(vision_cone_control_filtered$frame_id)
    
    exp_return <- exp_return %>% 
      dplyr::bind_rows(tidyr::crossing(play = p,
                                       frame_id = seq(min_frame, max_frame, by = 1),
                                       result = unique(exp_return$result)) %>% 
                         dplyr::filter(!frame_id %in% unique(exp_return$frame_id)))
    
  }
  
  plot <- vision_cone_control_filtered %>%
    # filter(frame_id == i) %>% 
    ggplot(aes(x, y)) +
    # plot field
    gg_field(field_color = "white", line_color = "black",
             sideline_color = "white", endzone_color = "white",
             buffer_y = 10, buffer_x = 10, direction = "vert",
             yardmin = pmax(0, pmin(unique(play$yardline_100), 100 - unique(play$yardline_100))-15),
             yardmax = pmin(120, max(punts_filtered$x, na.rm = T)+10)) +
    stat_summary_2d(aes(z = 100*control), alpha = 0.6, binwidth = 1) +
    # Expected return yards annotations
    geom_segment(data = exp_return,
                 aes(x = net, xend = net,
                     y = 0, yend = 53,
                     color = color),
                 size = 0.9, lty = 5) +
    geom_text(data = exp_return,
              aes(net, x, label = result, 
                  vjust = vjust, hjust = hjust, color = color),
              size = 2.8, fontface = "bold.italic") +
    geom_segment(data = punts_filtered,
                 aes(x, y, xend = x + s_x, yend = y + s_y,
                     # fill = fill,
                     color = color),
                 size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    geom_point(data = punts_filtered,
               aes(x, y, color = fill,
                   # fill = fill,
                   # shape = shape,
                   size = size),
               # stroke = 0.8,
               alpha = 0.8) +
    geom_text(data = punts_filtered,
              aes(label = jersey_number,
                  group = nfl_id),
              size = 2.25, fontface = "bold", color = "white") +
    scale_size_identity() +
    scale_shape_identity() +
    scale_color_identity() +
    # scale_fill_identity() +
    scale_fill_distiller(direction = 1, 
                         palette = "YlGnBu",
                         limits = c(-0.3, 0.3),
                         breaks = seq(-0.3, 0.3, by = 0.1),
                         labels = c("More punt\nteam control", "", "", "Neutral", "", "", "More return\nteam control"),
                         oob = scales::squish) +
    guides(fill = guide_legend(label.position = 'bottom',
                               title.position = 'top',
                               keywidth = 0.75,
                               keyheight = 0.175,
                               default.unit = "inch",
                               title.hjust = 0.5,
                               title.vjust = -0.5,
                               # label.vjust = 3,
                               nrow = 1)) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30/.pt, margin = margin(0, 0, 5, 0)),
          plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 24/.pt),
          plot.caption = element_text(face = "italic", hjust = 1, size = 20/.pt, margin = margin(0, 0, 0, 0)),
          legend.spacing.x = grid::unit(0, 'cm'),
          legend.title = element_text(size = 30/.pt, face = "bold"),
          legend.key = element_rect(fill = NA, color = NA),
          legend.text = element_text(size = 24/.pt),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = 'bottom',
          legend.box.margin = margin(-35, 0, 0, 0),
          plot.margin = margin(5, 0, 5, 0)) +
    labs(title = title,
         subtitle = subtitle,
         fill = "Field Control") +
    NULL
  
  if (isTRUE(anim)) {
    plot <- plot +     
      gganimate::transition_time(frame_id) +
      gganimate::ease_aes('linear')
  }
  
  return(plot)
}

ex_field_control_plot <- plot_vision_control(p = "2020111505_3874", f = 66, anim = FALSE)
ggsave("output/ex_field_control_plot.png", .Last.value, dpi = 700, height = 5, width = 8)

punt_anim <- plot_vision_control(p = "2020100501_500", anim = TRUE, 
                                 exp_return = pr_returns_epa, pause = 13)

# plot_vision_control(p = "2020091308_1801", anim = TRUE)


probs_over_time <- return_prob_preds %>% 
  dplyr::filter(play == "2020100501_500") %>%
  # filter(frame_id == 45) %>% 
  dplyr::select(-.pred_class) %>% 
  dplyr::bind_rows(tidyr::crossing(sec = seq(0, 2, by = 0.1),
                                   model = unique(return_prob_preds$model))) %>% 
  dplyr::arrange(sec) %>% 
  tidyr::fill(.pred_return, .pred_downed, .pred_out_of_bounds, .pred_fair_catch, .pred_touchback,
              play, special_teams_result, result, .direction = "up") %>% 
  dplyr::filter(sec >= 3, sec < 7) %>% 
  tidyr::pivot_longer(cols = starts_with(".pred_"),
                      names_prefix = ".pred_",
                      names_to = "outcome",
                      values_to = "probability") %>% 
  dplyr::group_by(sec, outcome) %>% 
  dplyr::mutate(agg_prob = mean(probability)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(outcome = stringr::str_to_title(stringr::str_replace_all(outcome, "_", " ")),
                outcome = ifelse(outcome == "Out Of Bounds", "OOB", outcome),
                pos_x = dplyr::case_when(
                  outcome == "Return" ~ 3.25,
                  outcome == "Fair Catch" ~ 4 + 0.1,
                  outcome == "Downed" ~ 4.75 + 2*0.1,
                  outcome == "Touchback" ~ 5.5 + 3*0.1,
                  TRUE ~ 6.25 + 4*0.1))

repeat_frame <- probs_over_time %>% 
  dplyr::filter(frame_id == 75)

repeated_frames <- purrr::map(seq_len(13), ~ repeat_frame) %>% 
  dplyr::bind_rows(.id="id") %>% 
  dplyr::mutate(frame_id = frame_id + as.numeric(id))

probs_over_time <- probs_over_time %>% 
  dplyr::mutate(frame_id = ifelse(frame_id > min(repeat_frame$frame_id), frame_id + 13, frame_id)) %>% 
  dplyr::bind_rows(repeated_frames) %>% 
  dplyr::arrange(frame_id) %>% 
  dplyr::left_join(outcome_pal,
                   by = c("outcome")) %>% 
  tidyr::fill(sec, .direction = "down")

summary(vision_cone_control_filtered$frame_id)
summary(probs_over_time$frame_id)

probs_over_time_anim <- probs_over_time %>% 
  ggplot(aes(sec, probability, color = color, lty = model)) +
  geom_line(aes(size = 0.8)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_size_identity() +
  scale_color_identity() +
  geom_text(aes(x = pos_x, y = 0.95,
                label = outcome),
            stat = "unique", show.legend = FALSE,
            size = 4.1, fontface = "bold.italic") +
  geom_text(aes(x = pos_x, y = 0.89,
                label = scales::percent(agg_prob, accuracy = 0.1)),
            stat = "unique", show.legend = FALSE,
            size = 4.1, fontface = "bold.italic") +
  # setting animation parameters
  gganimate::transition_reveal(frame_id)  +
  gganimate::ease_aes('linear') +
  jacklich::theme_jack() +
  labs(x = "Seconds from snap",
       y = "Predicted probability",
       lty = "Model",
       color = "Outcome")

library(gganimate)
# save animation
anim_save(filename = "output/sample_punt_over_time.gif",
          animation = animate(punt_anim, 
                              fps = 10, res = 240,
                              height = 900, width = 1600,
                              start_pause = 10, end_pause = 10,
                              # renderer = ffmpeg_renderer(), # for mp4
                              renderer = gifski_renderer()))

anim_save(filename = "output/sample_return_probs_over_time.gif",
          animation = animate(probs_over_time_anim, 
                              fps = 10, res = 240,
                              height = 900, width = 1600,
                              start_pause = 10, end_pause = 10,
                              # renderer = ffmpeg_renderer(), # for mp4
                              renderer = gifski_renderer()))

# Combine gifs into one gif
library(magick)

a_mgif <- image_read(path = "output/sample_punt_over_time.gif")
b_mgif <- image_read(path = "output/sample_return_probs_over_time.gif")

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = FALSE)
for(i in 2:length(a_mgif)){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = FALSE)
  new_gif <- c(new_gif, combined)
}

anim_save(filename = "output/punt_combined.gif",
          new_gif)
