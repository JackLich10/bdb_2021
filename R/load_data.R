### Load data

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(janitor)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(readr)))
suppressMessages(suppressWarnings(library(arrow)))
suppressMessages(suppressWarnings(library(tidyr)))

option_list <- list(
  make_option("--season", type="integer", default = 2018,
              help="season of tracking data to read")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript R/load_data.R --season 2019

# Source helpers
source("R/helpers.R")

cat("Loading in BDB data...\n")

# Load in nflfastR play-by-play
pbp <- nflreadr::load_pbp(seasons = opt$season)

# Game data
games <- readr::read_csv("data/games.csv", col_types = readr::cols()) %>% 
  janitor::clean_names()

# Players data
players <- readr::read_csv("data/players.csv", col_types = readr::cols()) %>% 
  janitor::clean_names()

# PBP data
plays <- readr::read_csv("data/plays.csv", col_types = readr::cols()) %>% 
  janitor::clean_names() %>% 
  tidyr::unite(play, game_id, play_id, remove = FALSE)

# PFF scouting data
pff_scout <- readr::read_csv("data/PFFScoutingData.csv", col_types = readr::cols()) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    snap_detail = dplyr::case_when(
      snap_detail == "H" ~ "High",
      snap_detail == "L" ~ "Low",
      snap_detail == "<" ~ "Left",
      snap_detail == ">" ~ "Right",
      snap_detail == "OK" ~ "Accurate",
      TRUE ~ NA_character_),
    kick_type = dplyr::case_when(
      kick_type == "D" ~ "Deep",
      kick_type == "F" ~ "Flat",
      kick_type == "K" ~ "Free Kick",
      kick_type == "O" ~ "Obvious Onside",
      kick_type == "P" ~ "Pooch Kick",
      kick_type == "Q" ~ "Squib",
      kick_type == "S" ~ "Surprise Onside",
      kick_type == "B" ~ "Deep Direct OOB",
      TRUE ~ NA_character_),
    kick_contact_type = dplyr::case_when(
      is.na(kick_contact_type) ~ NA_character_,
      kick_contact_type == "BB" ~ "Bounced Backwards",
      kick_contact_type == "BC" ~ "Bobbled Catch from Air",
      kick_contact_type == "BF" ~ "Bounced Forwards",
      kick_contact_type == "BOG" ~ "Bobbled on Ground",
      kick_contact_type == "CC" ~ "Clean Catch from Air",
      kick_contact_type == "CFF" ~ "Clean Field From Ground",
      kick_contact_type == "DEZ" ~ "Direct to Endzone",
      kick_contact_type == "ICC" ~ "Incidental Coverage Team Contact",
      kick_contact_type == "KTB" ~ "Kick Team Knocked Back",
      kick_contact_type == "KTC" ~ "Kick Team Catch",
      kick_contact_type == "KTF" ~ "Kick Team Knocked Forward",
      kick_contact_type == "MBC" ~ "Muffed by Contact with Non-Designated Returner",
      kick_contact_type == "MBD" ~ "Muffed by Designated Returner",
      kick_contact_type == "OOB" ~ "Directly Out Of Bounds"))

cat("Finding all punt plays...\n")

# Get all punt plays
punt_plays <- plays %>% 
  # Get rid of fakes/blocks
  dplyr::filter(special_teams_play_type == "Punt",
                !special_teams_result %in% c("Non-Special Teams Result", "Blocked Punt")) %>% 
  dplyr::mutate(gain_loss = play_result - dplyr::coalesce(penalty_yards, 0)) %>% 
  dplyr::distinct(play, game_id, play_id, special_teams_result, 
                  kick_length, kick_return_yardage, gain_loss, 
                  kick_blocker_id, kicker_id, returner_id) %>% 
  dplyr::left_join(pff_scout, by = c("game_id", "play_id")) %>% 
  janitor::remove_empty(which = c("rows", "cols"))

# Tracking data
tracking_punts <- purrr::map_df(opt$season, function(year) {
  
  cat(paste0("Reading in tracking data from ", year, "..\n"))
  
  arrow::read_parquet(paste0("data/tracking", year, ".parquet")) %>% 
    janitor::clean_names() %>% 
    # Keep only punts
    dplyr::inner_join(punt_plays %>% 
                        dplyr::filter(!is.na(hang_time)) %>% 
                        dplyr::select(game_id, play_id, 
                                      # kick_blocker_id, 
                                      kicker_id, returner_id, 
                                      hang_time, snap_time, operation_time,
                                      special_teams_result, kick_return_yardage), 
                      by = c("game_id", "play_id")) %>% 
    dplyr::mutate(
      # kick_blocker = if_else(kick_blocker_id == nfl_id, 1, 0, missing = 0),
      kicker = if_else(kicker_id == nfl_id, 1, 0, missing = 0),
      returner = if_else(returner_id == nfl_id, 1, 0, missing = 0)) %>% 
    dplyr::select(-dplyr::any_of(c("kick_blocker_id", "kicker_id", "returner_id")))
})

# Find how much the ball moves within first 5 seconds (some plays have bad tracking data)
ball_distances <- tracking_punts %>% 
  dplyr::filter(display_name == "football",
                frame_id <= 50) %>% 
  dplyr::select(game_id, play_id, frame_id, 
                ball_x = x, ball_y = y, dis) %>% 
  dplyr::group_by(game_id, play_id) %>% 
  dplyr::summarise(frames = dplyr::n_distinct(frame_id),
                   ball_dist = sum(dis),
                   .groups = "drop") %>% 
  dplyr::mutate(ball_dist_frame = ball_dist/frames)

# Ball must travel at least 25 yards within first 5 seconds
bad_tracking_plays <- ball_distances %>% 
  dplyr::filter(ball_dist_frame < 0.5) %>% 
  dplyr::distinct(game_id, play_id)

# Get rid of bad tracking plays where ball does not move
tracking_punts <- tracking_punts %>% 
  dplyr::anti_join(bad_tracking_plays,
                   by = c("game_id", "play_id"))

# Keep only punt plays in the tracking data
punt_plays <- punt_plays %>% 
  dplyr::inner_join(tracking_punts %>% 
                      dplyr::distinct(game_id, play_id),
                    by = c("game_id", "play_id"))

# Quick NA check
punt_plays %>% summarise_na()

# punt_plays %>% filter(is.na(kick_contact_type)) %>% count(special_teams_result)

cat("Mapping PFF roles to players in tracking data..\n")

# Make a jersey map for PFF joining
jersey_map <- tracking_punts %>% 
  dplyr::distinct(game_id, team, jersey_number, nfl_id) %>%
  dplyr::filter(!is.na(nfl_id)) %>% 
  dplyr::inner_join(games, by = "game_id") %>%
  dplyr::mutate(team = ifelse(team == 'home', home_team_abbr, visitor_team_abbr),
                # Adjusting jersey number so that it includes 0 when < 10
                jersey_number = ifelse(jersey_number < 10,
                                       paste0("0", jersey_number),
                                       as.character(jersey_number)),
                team_jersey = paste(team, jersey_number)) %>%
  dplyr::select(game_id, nfl_id, team_jersey)

# Find player roles on each play to join to tracking data
player_roles <- punt_plays %>% 
  dplyr::select(game_id, play_id, missed_tackler:vises) %>% 
  tidyr::pivot_longer(cols = missed_tackler:vises,
                      names_to = "role",
                      values_to = "team_jersey",
                      values_drop_na = TRUE) %>% 
  dplyr::mutate(type = ifelse(role %in% c("gunners", "punt_rushers", 
                                          "special_teams_safeties", "vises"), 
                              "pre_play", "post_play")) %>% 
  tidyr::separate_rows(team_jersey, sep = "; ") %>% 
  dplyr::left_join(jersey_map, by = c("game_id", "team_jersey"))

# Number of pre-play player roles on each play
n_player_roles <- player_roles %>% 
  dplyr::filter(type == "pre_play") %>% 
  dplyr::group_by(game_id, play_id, role) %>% 
  dplyr::summarise(players = dplyr::n(),
                   .groups = "drop") %>% 
  tidyr::pivot_wider(names_from = role,
                     values_from = players,
                     values_fill = 0)

# Plays with no pre-play roles marked
# punt_plays %>% 
#   anti_join(n_player_roles %>% select(-c(gunners:special_teams_safeties))) %>% View()

player_roles %>% summarise_na() # fine because not all tracking data is loaded right now
# player_roles %>% filter(is.na(nfl_id)) %>% View()

# Pre-play player roles (these are distinct)
pre_play_roles <- player_roles %>% 
  dplyr::filter(!is.na(nfl_id), type == "pre_play") %>% 
  dplyr::select(-c(team_jersey, type)) %>% 
  dplyr::rename(pre_snap_role = role)

# Pre-play player roles (can have multiple per player)
post_play_roles <- player_roles %>% 
  dplyr::filter(!is.na(nfl_id), type == "post_play") %>% 
  dplyr::select(-c(team_jersey, type)) %>% 
  dplyr::group_by(game_id, play_id, nfl_id) %>% 
  dplyr::mutate(value = paste0("post_snap_role_", dplyr::row_number())) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = value,
                     values_from = role)

cat("Finding start and end frames of each play\n(ball snap and ball snap plus hang time)..\n")

# Find bounds for ball snap
ball_snaps <- tracking_punts %>% 
  dplyr::group_by(game_id, play_id) %>% 
  dplyr::summarise(ball_snap_frame = min(frame_id[event == "ball_snap"]),
                   punt_frame = min(frame_id[event == "punt"]),
                   dplyr::across(dplyr::ends_with("_time"), ~ collapse::fmode(., na.rm = TRUE)),
                   .groups = "drop") %>% 
  # If no ball_snap, just keep all rows
  dplyr::mutate(ball_snap_frame = ifelse(is.infinite(ball_snap_frame), 0, ball_snap_frame),
                # pff_punt_frame = ball_snap_frame + ceiling(10*snap_time + 10*operation_time),
                punt_frame = ifelse(ball_snap_frame > punt_frame, NA_integer_, punt_frame),
                # ball land is punt frame plus hang time
                ball_land_frame = punt_frame + ceiling(10*hang_time))

ball_snaps %>% summarise_na()

# ball_snaps %>% 
#   ggplot(aes(punt_frame, pff_punt_frame)) +
#   geom_abline() +
#   geom_point()

# Plays with no tagged punt frame after ball is snapped
no_punt_frames <- ball_snaps %>% 
  dplyr::filter(is.na(punt_frame)) %>% 
  dplyr::mutate(play = paste0(game_id, "_", play_id))

# Join in information to the tracking data
tracking_punt_append <- tracking_punts %>% 
  # Filter for only frames between ball snap
  dplyr::inner_join(ball_snaps %>% 
                      dplyr::select(game_id, play_id, ball_snap_frame, 
                                    punt_frame, ball_land_frame),
                    by = c("game_id", "play_id")) %>% 
  # Keep frames only after the ball is snapped
  dplyr::filter(frame_id >= ball_snap_frame) %>% 
  dplyr::mutate(sec = 0.1 * (frame_id - ball_snap_frame)) %>% 
  # Join in pre_play_roles
  dplyr::left_join(pre_play_roles, by = c("game_id", "play_id", "nfl_id")) %>% 
  # Join in post_play_roles
  dplyr::left_join(post_play_roles, by = c("game_id", "play_id", "nfl_id")) %>% 
  # Clean plays
  ngscleanR::clean_and_rotate() %>% 
  # Clean up some tagging errors
  dplyr::mutate(event = ifelse(play == "2018123015_2651" & event == "kickoff_land",
                               "punt_land", event),
                event = ifelse(event == "kick_received", "punt_received", event))

# Impute missing punt frames
if (nrow(no_punt_frames) > 0) {
  
  cat(paste0("Imputing punt frame for ", nrow(no_punt_frames), " play(s)...\n"))
  
  to_impute_punt_frame <- tracking_punt_append %>% 
    dplyr::inner_join(no_punt_frames %>% 
                        dplyr::select(play), 
                      by = c("play")) 
  
  ball <- to_impute_punt_frame %>% 
    dplyr::filter(display_name == "football") %>% 
    dplyr::select(play, frame_id, 
                  ball_x = x, ball_y = y,
                  ball_speed = s)
  
  imputed_punt_frames <- to_impute_punt_frame %>% 
    # Punt must *conservatively* be at least a second after the snap
    # Punt must *conservatively* be made before 4 seconds after the snap
    dplyr::filter(kicker == 1,
                  frame_id >= 10 + ball_snap_frame, 
                  frame_id <= ball_snap_frame + 50) %>% 
    # filter(game_id == 2018112900,
    #        play_id == 432) %>%
    dplyr::left_join(ball,
                     by = c("play", "frame_id")) %>% 
    dplyr::mutate(punt_to_ball = sqrt((x - ball_x)^2 + (y - ball_y)^2),
                  event = ifelse(event == "None", NA_character_, event),
                  pff_punt_frame = ball_snap_frame + round(10*snap_time + 10*operation_time),
                  event = ifelse(frame_id == pff_punt_frame, "pff_punt_frame", event)) %>% 
    # tidyr::pivot_longer(cols = c(ball_speed, punt_to_ball)) %>%
    # ggplot(aes(frame_id, value, color = name)) +
    # geom_point() +
    # geom_text(aes(label = event))
    dplyr::group_by(play) %>% 
    dplyr::mutate(diff_ball_s = ball_speed - lag(ball_speed),
                  diff_punt_to_ball = punt_to_ball - lag(punt_to_ball)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(speed_punt = ifelse(diff_ball_s >= 4, 1, 0), # Punt must *conservatively* increase speed by 5 ft/s
                  dis_punt = ifelse(diff_punt_to_ball > 0, 1, 0), # Punt must increase distance from punter to ball
                  possible = dplyr::case_when(
                    speed_punt == 1 & dis_punt == 1 ~ 1,
                    speed_punt == 1 & lead(dis_punt) == 1 ~ 1,
                    TRUE ~ 0)) %>% 
    dplyr::filter(possible == 1) %>%
    dplyr::group_by(play) %>% 
    dplyr::summarise(imputed_punt_frame = min(frame_id),
                     .groups = "drop")
  
  tracking_punt_append <- tracking_punt_append %>% 
    dplyr::filter(!play %in% no_punt_frames$play) %>% 
    dplyr::bind_rows(tracking_punt_append %>% 
                       dplyr::filter(play %in% no_punt_frames$play) %>% 
                       dplyr::select(-punt_frame) %>% 
                       dplyr::left_join(imputed_punt_frames %>% 
                                          dplyr::rename(punt_frame = imputed_punt_frame),
                                        by = "play") %>% 
                       dplyr::mutate(ball_land_frame = punt_frame + ceiling(10*hang_time)))
  
  rm(to_impute_punt_frame, ball)
}

# ball_snaps %>% 
#   select(game_id, play_id, punt_frame) %>% 
#   left_join(imputed_punt_frames) %>% 
#   ggplot(aes(punt_frame, imputed_punt_frame)) +
#   geom_point() +
#   geom_abline()

# Save some play-level-columns
punt_plays <- punt_plays %>% 
  dplyr::left_join(tracking_punt_append %>% 
                     dplyr::select(game_id, play_id, week:play_type_nfl) %>% 
                     dplyr::distinct(),
                   by = c("game_id", "play_id"))

# Other game information
other_info <- tracking_punt_append %>%
  dplyr::distinct(play, play_id, nflfastr_game_id) %>% 
  dplyr::left_join(pbp %>% 
                     dplyr::transmute(play_id, game_id, roof, surface, temp, wind, presnap_wp = wp, 
                                      score_differential, half_seconds_remaining, game_seconds_remaining),
                   by = c("play_id", "nflfastr_game_id" = "game_id")) %>% 
  tidyr::replace_na(list(wind = 0, temp = 70))

# Join in other game information
punt_plays <- punt_plays %>% 
  dplyr::select(-c(missed_tackler:vises, air_yards, pass, rush, play_type_nfl)) %>% 
  dplyr::left_join(n_player_roles,
                   by = c("game_id", "play_id")) %>% 
  dplyr::left_join(other_info,
                   by = c("play", "play_id"))

cat("Cut plays when the ball lands...\n")

# Get rid of some play-level columns
punts <- tracking_punt_append %>% 
  # Keep only frames before the ball lands
  dplyr::filter(frame_id <= ball_land_frame) %>% 
  dplyr::select(-c(week:team_logo_espn, ball_snap_frame))

# tracking_punt_append %>% 
#   filter(play == "2018112507_560") %>% 
#   mutate(event = ifelse(event == "None", NA_character_, event),
#          event = ifelse(display_name != "football", NA_character_, event)) %>% 
#   ggplot(aes(x, y, color = team)) +
#   geom_point(alpha = 0.5) +
#   geom_text(aes(label = event), hjust = 1, vjust = 1, size = 3)
# 
# tracking_punts %>% 
#   filter(game_id == 2018112507,
#          play_id == 560,
#          team == "football") %>% 
#   mutate(event = ifelse(event == "None", NA_character_, event),
#          ball_snap_frame = min(frame_id[event == "ball_snap"]),
#          pff_punt_frame = ball_snap_frame + round(10*snap_time + 10*operation_time),
#          event = ifelse(frame_id == pff_punt_frame, "pff_punt_frame", event)) %>% 
#   ggplot(aes(frame_id, s)) +
#   geom_point() +
#   geom_text(aes(label = event), hjust = 1, vjust = 1, size = 3)

# # Cut plays at certain end events
# end_events <- c("punt_downed", "punt_received", "punt_blocked", "punt_muffed", "out_of_bounds", "touchback")
# 
# punts <- tracking_punt_append %>% 
#   ngscleanR::cut_plays(end_events = end_events)

# Kicker only data
kicker <- punts %>% 
  dplyr::filter(kicker == 1) %>% 
  dplyr::select(game_id, play_id, frame_id, 
                kicker_x = x, kicker_y = y, 
                kicker_speed = s)

# Ball only data
ball <- punts %>% 
  dplyr::filter(display_name == "football") %>% 
  dplyr::select(game_id, play_id, frame_id, 
                ball_x = x, ball_y = y,
                ball_speed = s)

# Plays with no `returner_id`
no_returner <- punt_plays %>% 
  dplyr::filter(is.na(returner_id)) %>% 
  dplyr::distinct(play, special_teams_result)

# Impute returners for plays where `returner_id` is NA
imputed_returners <- punts %>% 
  dplyr::filter(play %in% no_returner$play,
                # play == "2018090903_3982", # play with all punt rushers
                sec == 0, is.na(pre_snap_role), # no pre-snap role
                defense == 1, kicker == 0, team_name != "football") %>% 
  # Find player furthest away from line of scrimmage as returner
  dplyr::group_by(play) %>% 
  dplyr::summarise(nfl_id_returner = nfl_id[abs(dist_from_los) == max(abs(dist_from_los))],
                   .groups = "drop") %>% 
  dplyr::mutate(returner = 1)

# Plays with no returner and no imputed returner (fine since most are 2019 plus)
delete_plays <- no_returner %>% 
  dplyr::anti_join(imputed_returners, by = "play")

# 2018090903_3982 is not in this since no returner
punts_df <- punts %>% 
  dplyr::filter(play %in% no_returner$play) %>% 
  dplyr::select(-returner) %>% 
  dplyr::left_join(imputed_returners, by = c("play", "nfl_id" = "nfl_id_returner")) %>% 
  dplyr::bind_rows(punts %>% 
                     dplyr::filter(!play %in% no_returner$play)) %>% 
  dplyr::filter(!play %in% delete_plays$play) %>% # Get rid of plays with no returner/imputed returner
  tidyr::replace_na(list(returner = 0))

# Remove unnecessary data
rm(plays, delete_plays, imputed_punt_frames, imputed_returners,
   no_punt_frames, no_returner, post_play_roles, pre_play_roles,
   punts, tracking_punt_append, tracking_punts, ball, kicker)

# Save some processed data
saveRDS(punt_plays, paste0("~/Desktop/RStudio/bdb_2021/data/proc/punt_plays", opt$season, ".rds"))
saveRDS(punts_df, paste0("~/Desktop/RStudio/bdb_2021/data/proc/punts", opt$season, ".rds"))
saveRDS(jersey_map, paste0("~/Desktop/RStudio/bdb_2021/data/proc/jersey_map", opt$season, ".rds"))

cat(paste0("Completed processing and cleaning of punts for", opt$season, "\n"))

# punts_df %>% 
#   filter(defense == 0) %>% 
#   count(pre_snap_role, post_snap_role_1, post_snap_role_2)
# 
# 
# punts %>% 
#   filter(play == "2018111809_145", sec == 0) %>% View()
# ggplot(aes(x, y)) +
#   geom_point() +
#   geom_text(aes(label = nfl_id),
#             hjust = 1, vjust = 1, size = 3)
# 
# 
# tracking_punt_append %>% 
#   mutate(`down` = 0,
#          `ydstogo` = 0,
#          `qtr` = 0,
#          `desc` = 0,
#          `team_color` = ifelse(display_name == "football", "brown", NA),
#          `team_color2` = NA,
#          jersey_number = 0) %>% 
#   filter(play == "2018090903_407") %>% 
#   ngscleanR::plot_play(animated = FALSE)


# %>% 
#   # Join in play-level information
#   dplyr::left_join(punt_plays %>% 
#                      dplyr::select(play:special_teams_result,
#                                    snap_detail:return_direction_actual, 
#                                    kick_contact_type) %>% 
#                      dplyr::left_join(n_player_roles, by = c("game_id", "play_id")),
#                    by = c("game_id", "play_id"))

