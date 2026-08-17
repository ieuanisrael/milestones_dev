# Generate a local sample innings table for laptop testing.
# Run from the project root:
#   Rscript data/generate_sample_data.R

set.seed(20260817)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
})

out_dir <- file.path("data", "sample")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

not_out_id <- 1L

teams_m <- tibble(
  team_id = 1:6,
  team_name = c("NSW Blues M", "Victoria M", "Queensland M", "WA M", "SA M", "Tasmania M")
)

teams_f <- tibble(
  team_id = 11:14,
  team_name = c("NSW Breakers F", "QLD Fire F", "Victoria F", "WA F")
)

roles_12 <- c(
  "batter", "allrounder", "bowler", "keeper",
  "opener", "bowler", "batter", "bowler",
  "allrounder", "allrounder", "batter", "allrounder"
)

nsw_m <- tibble(
  player_id = 1001:1012,
  name = c(
    "Steve Smith", "Moises Henriques", "Pat Cummins", "Josh Philippe",
    "Daniel Hughes", "Sean Abbott", "Kurtis Patterson", "Nathan Lyon",
    "Jack Edwards", "Chris Green", "Oliver Davies", "Hayden Kerr"
  ),
  team_id = 1L,
  role = roles_12
)

make_squad <- function(team_id, start_id, names, roles) {
  tibble(
    player_id = start_id + seq_along(names) - 1L,
    name = names,
    team_id = as.integer(team_id),
    role = roles
  )
}

other_squads <- lapply(2:6, function(tid) {
  make_squad(tid, tid * 1000L + 1L, paste(c("VIC", "QLD", "WA", "SA", "TAS")[tid - 1], "Player", 1:12), roles_12)
})

players_m <- bind_rows(nsw_m, bind_rows(other_squads)) %>%
  group_by(team_id) %>%
  mutate(squad_rank = dplyr::row_number()) %>%
  ungroup()

players_f <- bind_rows(
  make_squad(11L, 11001L, paste("NSW W Player", 1:11), roles_12[-12]),
  make_squad(12L, 12001L, paste("QLD W Player", 1:11), roles_12[-12]),
  make_squad(13L, 13001L, paste("VIC W Player", 1:11), roles_12[-12]),
  make_squad(14L, 14001L, paste("WA W Player", 1:11), roles_12[-12])
) %>%
  group_by(team_id) %>%
  mutate(squad_rank = dplyr::row_number()) %>%
  ungroup()

venues <- tibble(
  venue_id = 1:6,
  venue_name = c(
    "Sydney Cricket Ground",
    "Melbourne Cricket Ground",
    "The Gabba",
    "Adelaide Oval",
    "WACA Ground",
    "Bellerive Oval"
  )
)

series <- tibble(
  series_id = 1:4,
  series_name = c(
    "Aus Domestic OD M",
    "Aus Domestic T20 M",
    "Sheffield Shield M",
    "Aus Domestic T20 F"
  ),
  match_length_id = c(2L, 3L, 1L, 3L),
  format_name = c("One Day", "T20", "First Class", "T20")
)

seasons <- tibble(
  season_id = 1:10,
  season_name = paste0(2016:2025, "-", sprintf("%02d", 17:26))
)

round_robin <- function(team_ids) {
  pairs <- t(combn(team_ids, 2))
  tibble(home_team_id = as.integer(pairs[, 1]), away_team_id = as.integer(pairs[, 2]))
}

build_matches <- function(series_row, team_tbl, season_tbl, n_target = 30L) {
  fixtures <- round_robin(team_tbl$team_id)
  while (nrow(fixtures) < n_target) {
    fixtures <- bind_rows(fixtures, round_robin(team_tbl$team_id))
  }
  fixtures <- fixtures[seq_len(n_target), ]

  expand_grid(season_id = season_tbl$season_id, fixture_row = seq_len(nrow(fixtures))) %>%
    mutate(
      home_team_id = fixtures$home_team_id[.data$fixture_row],
      away_team_id = fixtures$away_team_id[.data$fixture_row]
    ) %>%
    left_join(season_tbl, by = "season_id") %>%
    group_by(.data$season_id) %>%
    mutate(
      match_date = as.Date(sprintf("%s-10-15", substr(.data$season_name[1], 1, 4))) +
        (dplyr::row_number() - 1L) * 4L,
      venue_id = ((dplyr::row_number() - 1L) %% nrow(venues)) + 1L
    ) %>%
    ungroup() %>%
    mutate(
      series_id = series_row$series_id,
      series_name = series_row$series_name,
      match_length_id = series_row$match_length_id,
      format_name = series_row$format_name
    ) %>%
    left_join(venues, by = "venue_id")
}

expand_innings <- function(matches, team_tbl, player_tbl, n_innings) {
  sides <- bind_rows(
    matches %>% transmute(match_id_tmp = dplyr::row_number(), team_id = .data$home_team_id, is_home = TRUE),
    matches %>% transmute(match_id_tmp = dplyr::row_number(), team_id = .data$away_team_id, is_home = FALSE)
  )

  match_meta <- matches %>%
    mutate(match_id_tmp = dplyr::row_number()) %>%
    select(
      match_id_tmp, match_date, match_length_id, format_name, series_id, series_name,
      season_id, season_name, venue_id, venue_name
    )

  squad_n <- player_tbl %>%
    count(.data$team_id, name = "n_squad")

  lineup <- sides %>%
    left_join(player_tbl, by = "team_id", relationship = "many-to-many") %>%
    left_join(squad_n, by = "team_id") %>%
    mutate(
      extra_rank = .data$squad_rank - 8L,
      n_extras = pmax(.data$n_squad - 8L, 0L),
      keep = .data$squad_rank <= 8L |
        .data$n_squad <= 11L |
        (.data$extra_rank != ((.data$match_id_tmp - 1L) %% pmax(.data$n_extras, 1L)) + 1L)
    ) %>%
    filter(.data$keep)

  innings_grid <- expand_grid(
    match_innings_id_when_team_batted = seq_len(n_innings)
  )

  df <- lineup %>%
    left_join(match_meta, by = "match_id_tmp") %>%
    left_join(team_tbl, by = "team_id") %>%
    crossing(innings_grid)

  n <- nrow(df)
  run_mean <- dplyr::case_when(
    df$role == "opener" ~ 32,
    df$role == "batter" ~ 28,
    df$role == "keeper" ~ 22,
    df$role == "allrounder" ~ 18,
    TRUE ~ 8
  )
  wkt_mean <- dplyr::case_when(
    df$role == "bowler" ~ 1.6,
    df$role == "allrounder" ~ 0.9,
    TRUE ~ 0.05
  )
  catch_mean <- ifelse(df$role == "keeper", 0.9, 0.35)
  stump_mean <- ifelse(df$role == "keeper", 0.15, 0)

  t20 <- df$format_name == "T20"
  fc <- df$format_name == "First Class"
  run_mean[t20] <- run_mean[t20] * 0.7
  wkt_mean[t20] <- wkt_mean[t20] * 0.8
  run_mean[fc] <- run_mean[fc] * 1.15
  wkt_mean[fc] <- wkt_mean[fc] * 1.1

  runs <- pmax(0L, as.integer(round(rnorm(n, run_mean, pmax(run_mean * 0.7, 1)))))
  boost <- runif(n) < 0.08
  if (any(boost)) {
    runs[boost] <- runs[boost] + sample(c(50L, 60L, 80L, 110L), sum(boost), replace = TRUE)
  }

  wickets <- pmax(0L, as.integer(round(rpois(n, wkt_mean))))
  fivefer <- df$role == "bowler" & runif(n) < 0.04
  wickets[fivefer] <- pmax(wickets[fivefer], 5L)

  batting_position <- dplyr::case_when(
    df$role == "opener" ~ sample(1:2, n, replace = TRUE),
    df$role == "batter" ~ sample(3:6, n, replace = TRUE),
    df$role == "keeper" ~ sample(5:7, n, replace = TRUE),
    df$role == "allrounder" ~ sample(6:8, n, replace = TRUE),
    TRUE ~ sample(8:11, n, replace = TRUE)
  )

  all_out <- (df$match_id_tmp %% 7L) == 0L
  closure <- ifelse(all_out, 2L, 3L)
  result_id <- ifelse((df$is_home & (df$match_id_tmp %% 2L == 0L)) | (!df$is_home & (df$match_id_tmp %% 2L == 1L)), 1L, 2L)

  df %>%
    transmute(
      match_id = .data$match_id_tmp,
      match_date = .data$match_date,
      match_length_id = .data$match_length_id,
      format_name = .data$format_name,
      series_id = .data$series_id,
      series_name = .data$series_name,
      season_id = .data$season_id,
      season_name = .data$season_name,
      venue_id = .data$venue_id,
      venue_name = .data$venue_name,
      player_id = .data$player_id,
      name = .data$name,
      team_id = .data$team_id,
      team_name = .data$team_name,
      match_count = 1L,
      is_keeper = as.integer(.data$role == "keeper"),
      batter_score = runs,
      bowler_wickets = wickets,
      fielder_catches = pmax(0L, as.integer(rpois(n, catch_mean))),
      fielder_stumpings = pmax(0L, as.integer(rpois(n, stump_mean))),
      batting_position = as.integer(batting_position),
      batter_how_out_id = ifelse(runif(n) < 0.12, not_out_id, sample(2:4, n, replace = TRUE)),
      match_innings_id_when_team_batted = as.integer(.data$match_innings_id_when_team_batted),
      team_innings_1_closure_id = ifelse(.data$match_innings_id_when_team_batted == 1L, closure, 1L),
      team_innings_2_closure_id = ifelse(.data$match_innings_id_when_team_batted == 2L, closure, 1L),
      team_match_result_id = as.integer(result_id)
    )
}

offset_ids <- function(df, offset) {
  df$match_id <- df$match_id + offset
  df
}

message("Generating sample matches...")

od_matches <- build_matches(series[1, ], teams_m, seasons)
t20_matches <- build_matches(series[2, ], teams_m, seasons)
shield_matches <- build_matches(series[3, ], teams_m, seasons)
t20f_matches <- build_matches(series[4, ], teams_f, seasons)

od_m <- expand_innings(od_matches, teams_m, players_m, 1L)
t20_m <- expand_innings(t20_matches, teams_m, players_m, 1L)
shield_m <- expand_innings(shield_matches, teams_m, players_m, 2L)
t20_f <- expand_innings(t20f_matches, teams_f, players_f, 1L)

innings <- bind_rows(
  od_m,
  offset_ids(t20_m, max(od_m$match_id)),
  offset_ids(shield_m, max(od_m$match_id) + max(t20_m$match_id)),
  offset_ids(t20_f, max(od_m$match_id) + max(t20_m$match_id) + max(shield_m$match_id))
)

# Put NSW's latest 2025-26 matches into calendar 2026 so email "recent form" cards populate.
shift_recent_dates <- function(df, team_name, series_name, n_matches = 4L, start = as.Date("2026-01-10")) {
  idx <- which(
    df$team_name == team_name &
      df$series_name == series_name &
      df$season_name == "2025-26"
  )
  match_ids <- tail(sort(unique(df$match_id[idx])), n_matches)
  new_dates <- start + (seq_along(match_ids) - 1L) * 7L
  for (i in seq_along(match_ids)) {
    df$match_date[df$match_id == match_ids[i]] <- new_dates[i]
  }
  df
}

innings <- innings %>%
  shift_recent_dates("NSW Blues M", "Aus Domestic OD M") %>%
  shift_recent_dates("NSW Blues M", "Sheffield Shield M")

tune_last_match <- function(df, player_name, series_name, column, last_value, min_date = as.Date("2026-01-01")) {
  idx <- which(df$name == player_name & df$series_name == series_name & df$match_date >= min_date)
  if (length(idx) == 0) {
    idx <- which(df$name == player_name & df$series_name == series_name)
  }
  if (length(idx) == 0) {
    return(df)
  }
  last_idx <- idx[which.max(df$match_date[idx])]
  df[[column]][last_idx] <- last_value
  df
}

force_carried_bat <- function(df, player_name, n = 3L) {
  idx <- which(df$name == player_name & df$series_name == "Aus Domestic OD M")
  take <- head(idx, n)
  df$batting_position[take] <- 1L
  df$batter_how_out_id[take] <- not_out_id
  df$match_innings_id_when_team_batted[take] <- 1L
  df$team_innings_1_closure_id[take] <- 2L
  df
}

force_match_haul <- function(df, player_name, series_name, wickets = c(6L, 5L)) {
  idx <- which(df$name == player_name & df$series_name == series_name & df$match_date >= as.Date("2026-01-01"))
  if (length(idx) == 0) {
    idx <- which(df$name == player_name & df$series_name == series_name)
  }
  match_id <- df$match_id[idx[which.max(df$match_date[idx])]]
  rows <- which(df$name == player_name & df$match_id == match_id)
  rows <- rows[order(df$match_innings_id_when_team_batted[rows])]
  # Innings-level filters require one innings itself to reach the haul.
  if (length(rows) > 0) {
    df$bowler_wickets[rows[1]] <- max(wickets, na.rm = TRUE)
  }
  if (length(rows) > 1 && length(wickets) > 1) {
    df$bowler_wickets[rows[2]] <- wickets[2]
  }
  df
}

innings <- innings %>%
  tune_last_match("Steve Smith", "Aus Domestic OD M", "batter_score", 112L) %>%
  tune_last_match("Moises Henriques", "Aus Domestic OD M", "batter_score", 42L) %>%
  tune_last_match("Pat Cummins", "Aus Domestic OD M", "bowler_wickets", 5L) %>%
  force_carried_bat("Daniel Hughes", n = 4L) %>%
  force_match_haul("Sean Abbott", "Aus Domestic OD M", 10L) %>%
  force_match_haul("Sean Abbott", "Sheffield Shield M", c(10L, 4L))

keep_idx <- which(
  innings$name == "Josh Philippe" &
    innings$series_name == "Aus Domestic OD M" &
    innings$match_date >= as.Date("2026-01-01")
)
if (length(keep_idx) > 0) {
  last_keep <- keep_idx[which.max(innings$match_date[keep_idx])]
  innings$fielder_catches[last_keep] <- 5L
  innings$fielder_stumpings[last_keep] <- 1L
  innings$is_keeper[last_keep] <- 1L
}

saveRDS(list(innings = innings), file.path(out_dir, "tables.rds"))

summary_tbl <- innings %>%
  filter(.data$series_name == "Aus Domestic OD M", .data$team_name == "NSW Blues M") %>%
  group_by(.data$name) %>%
  summarise(
    matches = n_distinct(.data$match_id),
    runs = sum(.data$batter_score),
    wickets = sum(.data$bowler_wickets),
    dismissals = sum(.data$fielder_catches + .data$fielder_stumpings),
    last_date = max(.data$match_date),
    .groups = "drop"
  ) %>%
  arrange(desc(.data$runs))

print(summary_tbl)
message("Wrote ", file.path(out_dir, "tables.rds"), " with ", nrow(innings), " innings rows.")
