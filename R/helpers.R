### Helpers


# Function to summarize NAs
summarise_na <- function(tbl) {
  tbl %>% 
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.)))) %>% t()
}

# Save predictions and workflow
grid_control <- tune::control_grid(
  save_pred = TRUE,
  save_workflow = TRUE,
  extract = tune::extract_model,
  verbose = TRUE)

# Helper function for looking at model recipe pre-processing
prep_juice <- function(x) recipes::juice(recipes::prep(x))

# Helper function for binding predictions onto data
augment.workflow <- function(x, newdata, ...) {
  stats::predict(x, newdata, ...) %>%
    dplyr::bind_cols(newdata)
}

# EDA
summarise_target <- function(tbl) {
  tbl %>%
    dplyr::summarise(frames = dplyr::n(),
                     target = sum(target),
                     .groups = "drop") %>%
    dplyr::mutate(target_pct = target / frames,
                  low = qbeta(.025, target + .5, frames - target + .5),
                  high = qbeta(.975, target + .5, frames - target + .5))
}

plot_continuous <- function(tbl, metric, ...) {
  tbl %>% 
    ggplot(aes({{ metric }}, target_pct, ...)) +
    geom_point(aes(size = frames)) +
    geom_errorbar(aes(ymin = low, ymax = high)) +
    scale_size_continuous(range = c(0.5, 3),
                          labels = scales::comma) +
    scale_y_continuous(labels = scales::percent)
}

summarise_kick_return_yards <- function(tbl) {
  tbl %>% 
    dplyr::summarise(n = dplyr::n(),
                     no_return = sum(is.na(kick_return_yardage)),
                     kick_return_yardage = mean(kick_return_yardage, na.rm = TRUE),
                     .groups = "drop") %>% 
    dplyr::mutate(pct_no_return = no_return/n)
}

plot_kick_return_yards <- function(metric, tbl, ...) {
  tbl %>% 
    ggplot(aes({{ metric }}, kick_return_yardage, ...)) +
    geom_point(aes(size = n-no_return)) +
    geom_smooth(aes(weight = n-no_return)) +
    scale_size_continuous(range = c(0.5, 3))
}


#### Field control helpers (https://www.kaggle.com/adamsonty/nfl-big-data-bowl-a-basic-field-control-model)

# Grid of points on NFL field
field_grid <- tidyr::expand_grid(x = seq(0, 120, length.out = 120),
                                 y = seq(0, 160/3, length.out = 160/3))

# 1. compute player's distance from ball
compute_dist_from_ball <- function(tracking_data) {
  tracking_data %>%
    dplyr::inner_join(tracking_data %>%
                        dplyr::filter(team == "football") %>%
                        dplyr::select(game_id, play_id, frame_id, ball_x = x, ball_y = y),
                      by = c("game_id", "play_id", "frame_id")) %>%
    dplyr::mutate(dist_from_ball = sqrt((x-ball_x)^2 + (y-ball_y)^2)) %>% 
    dplyr::select(-c(ball_x, ball_y))
}

# 2. compute each player's speed ratio
#    here we're using a max speed of 13 yds/s, 
#    which about lines up with the max speeds seen in 
#    the Next Gen Stats Fastest Ballcarrier tables
compute_speed_ratio <- function(tracking_data, s_max = 13.00) {
  tracking_data %>%
    dplyr::mutate(s_ratio = s / s_max)
}

# 3. compute each player's next location
compute_next_loc <- function(tracking_data, delta_t = 0.50) {
  tracking_data %>%
    dplyr::mutate(x_next = x + s_x * delta_t,
                  y_next = y + s_y * delta_t)
}

# 4. compute each player's radius of influence for a given frame
#    here we're using a model that approximates the plot shown in
#    the appendix of Wide Open Spaces. this original function was
#    found by Will Thomson. the modification that I'll make is that
#    I'll add a few parameters to the equation, so we can alter the
#    min/max radius of influence a player can have, as well as the
#    rate at which that radius changes (based on their proximity 
#    to the ball)
compute_radius_of_influence <- function(tracking_data,
                                        min_radius = 4.00,
                                        max_radius = 10.00,
                                        max_dist_from_ball = 20.00) {
  tracking_data %>%
    dplyr::mutate(radius_of_influence = min_radius + dist_from_ball^3 * (max_radius-min_radius) / max_dist_from_ball,
                  radius_of_influence = dplyr::case_when(
                    radius_of_influence > max_radius ~ max_radius,
                    TRUE ~ radius_of_influence))
}

compute_rotation_matrix <- function(v_theta) {
  matrix(
    c(cos(v_theta), -sin(v_theta),
      sin(v_theta),  cos(v_theta)),
    nrow = 2,
    byrow = TRUE
  )
}

compute_scaling_matrix <- function(radius_of_influence, s_ratio) {
  matrix(
    c(radius_of_influence * (1 + s_ratio), 0,
      0, radius_of_influence * (1 - s_ratio)),
    nrow = 2,
    byrow = TRUE
  )
}

compute_covariance_matrix <- function(field_grid, game_id, play_id,
                                      frame_id, nfl_id, team, defense,
                                      zoi_center_x, zoi_center_y, 
                                      v_theta, radius_of_influence, s_ratio) {
  # prev_p <- 0
  # future::plan(future::multisession)
  # furrr::future_pmap_dfr(list(game_id, play_id, frame_id, nfl_id, team, defense, v_theta, radius_of_influence, s_ratio, zoi_center_x, zoi_center_y),
  #                        function(g, p, f, nfl_id, team, defense, v, rad, s, x, y) {
  purrr::pmap_dfr(list(game_id, play_id, frame_id, nfl_id, team, defense, v_theta, radius_of_influence, s_ratio, zoi_center_x, zoi_center_y),
                  function(g, p, f, nfl_id, team, defense, v, rad, s, x, y) {
                    
                    # g = game_id[1]
                    # p = play_id[1]
                    # f = frame_id[1]
                    # x <- zoi_center_x[1]
                    # y <- zoi_center_y[1]
                    # s <- s_ratio[1]
                    # rad <- radius_of_influence[1]
                    # v <- v_theta[1]
                    # nfl_id <- nfl_id[1]
                    # team <- team[1]
                    # defense <- defense[1]
                    
                    # Filter for points in vision cone for that game, play, frame
                    # field_grid <- field_grid %>% 
                    #   dplyr::filter(game_id == g, play_id == p, frame_id == f)
                    
                    # if (prev_p != p) {
                    #   cat(paste0("GameID: ", g, ", PlayID: ", p, "...\n"))
                    # }
                    # prev_p <- p
                    
                    field_grid <- field_grid[game_id == g & play_id == p & frame_id == f] %>% 
                      dplyr::as_tibble() %>% 
                      dplyr::select(x, y)
                    
                    mu <- c(x, y)
                    R <- compute_rotation_matrix(v)
                    S <- compute_scaling_matrix(rad, s)
                    Sigma <- R %*% S %*% S %*% solve(R)
                    
                    field_grid %>% 
                      dplyr::mutate(influence = mvtnorm::dmvnorm(x = field_grid, mean = mu, sigma = Sigma),
                                    # influence = influence/max(influence),
                                    game_id = g,
                                    play_id = p,
                                    frame_id = f, #)
                                    nfl_id = nfl_id,
                                    team = team,
                                    defense = defense)
                  })
  
}


compute_player_zoi <- function(player_frame_tracking_data, field_grid = NULL) {
  if (is.null(field_grid)) {
    field_grid <- tidyr::expand_grid(x = seq(0, 120, length.out = 120),
                                     y = seq(0, 160/3, length.out = 160/3))
  }
  
  game_id <- player_frame_tracking_data %>% pull(game_id)
  play_id <- player_frame_tracking_data %>% pull(play_id)
  frame_id <- player_frame_tracking_data %>% pull(frame_id)
  nfl_id <- player_frame_tracking_data %>% pull(nfl_id) 
  team <- player_frame_tracking_data %>% pull(team) 
  defense <- player_frame_tracking_data %>% pull(defense) 
  
  zoi_center_x <- player_frame_tracking_data %>% pull(x_next)
  zoi_center_y <- player_frame_tracking_data %>% pull(y_next)
  v_theta <- player_frame_tracking_data %>% pull(v_theta)
  radius_of_influence <- player_frame_tracking_data %>% pull(radius_of_influence)
  s_ratio <- player_frame_tracking_data %>% pull(s_ratio)
  
  player_zoi <- compute_covariance_matrix(field_grid, game_id, play_id,
                                          frame_id, nfl_id, team, defense,
                                          zoi_center_x, zoi_center_y, 
                                          v_theta, radius_of_influence, s_ratio)
  
  return(player_zoi)
}

# Find the three vertices for the vision cone of the returner
find_vision_cone_vertices <- function(returner, tot_angle = 90, radius = 10) {
  returner %>% 
    dplyr::mutate(angle = (360 - o - tot_angle/2)/180*pi,
                  delta_x1 = -radius*cos(angle),
                  delta_y1 = -radius*sin(angle),
                  delta_x2 = -radius*sin(angle),
                  delta_y2 = radius*cos(angle),
                  bot_point_x = x + delta_x1,
                  bot_point_y = y + delta_y1,
                  top_point_x = x + delta_x2,
                  top_point_y = y + delta_y2) %>% 
    dplyr::select(game_id, play_id, frame_id, nfl_id,
                  orig_x = x, orig_y = y, bot_point_x, bot_point_y,
                  top_point_x, top_point_y)
}

# Helper function for finding if a point lies within a triangle
sign <- Vectorize(function(p1_x, p1_y, p2_x, p2_y, p3_x, p3_y) {
  (p1_x - p3_x) * (p2_y - p3_y) - (p2_x - p3_x) * (p1_y - p3_y)
})

# Function to compute if a point lies within a triangle (what part of )
point_in_triangle <- function(vision_vertices, field_grid) {
  
  # cat(vision_vertices$game_id)
  
  p1_x <- vision_vertices$orig_x
  p1_y <- vision_vertices$orig_y
  p2_x <- vision_vertices$bot_point_x
  p2_y <- vision_vertices$bot_point_y
  p3_x <- vision_vertices$top_point_x
  p3_y <- vision_vertices$top_point_y
  
  vision_points <- purrr::map_dfr(seq_len(nrow(field_grid)), function(row) {
    
    if (row %in% seq(1, nrow(field_grid), by = 500)) {
      cat(paste0("Completing field_grid row: ", row, "...\n"))
    }
    cur_grid <- field_grid %>% 
      dplyr::slice(row)
    
    d1 <- sign(cur_grid$x, cur_grid$y, p1_x, p1_y, p2_x, p2_y)
    d2 <- sign(cur_grid$x, cur_grid$y, p2_x, p2_y, p3_x, p3_y)
    d3 <- sign(cur_grid$x, cur_grid$y, p3_x, p3_y, p1_x, p1_y)
    
    has_neg <- (d1 < 0) | (d2 < 0) | (d3 < 0)
    has_pos <- (d1 > 0) | (d2 > 0) | (d3 > 0)
    
    vision_vertices %>% 
      dplyr::mutate(in_vision = !(has_neg & has_pos)) %>% 
      dplyr::filter(in_vision == TRUE) %>% 
      dplyr::bind_cols(cur_grid)
  })
  
  return(vision_points)
}

