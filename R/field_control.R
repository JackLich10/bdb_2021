### Field control model

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
  make_option("--season", type="integer", default = 2018,
              help="Season of tracking data to read"),
  make_option("--rerun_cones", type="logical", default = FALSE,
              help="Whether to re-run vision cone calclation"),
  make_option("--rerun_control", type="logical", default = FALSE,
              help="Whether to re-run field control calclation")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript R/field_control.R --season 2019 --rerun_cones FALSE --rerun_control FALSE

# Source helpers
source("R/helpers.R")

cat("Loading in processed BDB punt data...\n")

# Construct path to data
punts <- readRDS(paste0("data/proc/punts", opt$season, ".rds"))

# Vertices of returners vision triangle
vision_vertices <- punts %>% 
  dplyr::filter(returner == 1) %>% 
  find_vision_cone_vertices(tot_angle = 90, radius = 15)

cat("Finding points in the returner's vision...\n")
if (isTRUE(opt$rerun_cones)) {
  # Find all points on field within a returner's vision cone for each play at each frame
  vision_cones <- point_in_triangle(vision_vertices, field_grid)
  saveRDS(vision_cones, paste0("data/proc/vision_cones", opt$season, ".rds"))
} else {
  vision_cones <- readRDS(paste0("data/proc/vision_cones", opt$season, ".rds"))
}

cat("Coercing `vision_cones` to `data.table`...\n")

data.table::setDTthreads(threads = 2)

# Coerce to data.table for indexing on filters
vc_dt <- data.table::as.data.table(vision_cones)

# Set keys of data
data.table::setkeyv(vc_dt, c("game_id", "play_id", "frame_id"))

# Process non-returners 
non_returners_proc <- punts %>% 
  dplyr::filter(returner == 0,
                # play == "2018123000_1267"
  ) %>% 
  dplyr::select(time:nfl_id, frame_id:play_id, display_name, team, defense, o_x:a_y) %>% 
  dplyr::mutate(v_theta = atan(s_y / s_x),
                v_theta = ifelse(is.nan(v_theta), 0, v_theta)) %>% 
  compute_dist_from_ball() %>% 
  compute_speed_ratio() %>% 
  compute_next_loc() %>% 
  compute_radius_of_influence()

# Find players who are within 20 yards of any vertex of returner vision triangle
relevant_players <- non_returners_proc %>% 
  dplyr::filter(team != "football") %>% 
  dplyr::inner_join(vision_cones %>% 
                      dplyr::select(-c(x, y, nfl_id)) %>% 
                      dplyr::distinct(), 
                    by = c("frame_id", "game_id", "play_id")) %>% 
  dplyr::mutate(dist_orig = sqrt((x-orig_x)^2 + (y-orig_y)^2),
                dist_bot = sqrt((x-bot_point_x)^2 + (y-bot_point_y)^2),
                dist_top = sqrt((x-top_point_x)^2 + (y-top_point_y)^2)) %>% 
  dplyr::filter(!(dist_orig >= 20 & dist_bot >= 20 & dist_top >= 20)) %>% 
  dplyr::select(-c(orig_x, orig_y, bot_point_x, bot_point_y, top_point_x, top_point_y))

cat("Finding points in the returner's vision...\n")
# if (isTRUE(opt$rerun_control)) {
  # Field control of vision cone among relevant players
  # One game_id (8 punts): 29.136 sec elapsed
  
  control <- purrr::map_df(unique(relevant_players$game_id), function(g) {
    tictoc::tic()
    c <- relevant_players %>% 
      dplyr::filter(game_id == g) %>%
      compute_player_zoi(., field_grid = vc_dt) %>% 
      # Punt team's influence is negative
      dplyr::mutate(influence = ifelse(defense == 0, -influence, influence)) %>%
      dplyr::group_by(game_id, play_id, frame_id, x, y) %>%
      dplyr::summarise(control = sum(influence), 
                       .groups = "drop")
    tictoc::toc()
    return(c)
  })
  
  # control <- relevant_players %>% 
  #   filter(game_id == 2018123000) %>%
  #   compute_player_zoi(., field_grid = vc_dt) %>% 
  #   # Punt team's influence is negative
  #   dplyr::mutate(influence = ifelse(defense == 0, -influence, influence)) %>%
  #   dplyr::group_by(game_id, play_id, frame_id, x, y) %>%
  #   dplyr::summarise(control = sum(influence), 
  #                    .groups = "drop")
  # tictoc::toc()
  
  # Field control of just vision of returner
  vision_cone_control <- vision_cones %>% 
    dplyr::rename(returner_id = nfl_id) %>% 
    dplyr::inner_join(control, by = c("game_id", "play_id", "frame_id", "x", "y")) %>% 
    dplyr::mutate(play = paste0(game_id, "_", play_id)) %>% 
    dplyr::select(game_id, play_id, frame_id, play, x, y, control)
  
  saveRDS(vision_cone_control, paste0("data/proc/control", opt$season, ".rds"))
# } else {
#   control <- readRDS(paste0("data/proc/control", opt$season, ".rds"))
# }

cat(paste0("Completed field control calculation/processing for", opt$season, "\n"))

