# Local sample-data helpers so the app and email workflow can run without Azure SQL.

.sample_env <- new.env(parent = emptyenv())

load_sample_data <- function() {
  if (!is.null(.sample_env$innings)) {
    return(invisible(.sample_env))
  }

  path <- sample_data_path()
  if (!file.exists(path)) {
    stop("Sample data not found at ", path, ". Run data/generate_sample_data.R first.")
  }

  tables <- readRDS(path)
  list2env(tables, envir = .sample_env)
  invisible(.sample_env)
}

sample_innings <- function() {
  load_sample_data()
  .sample_env$innings
}

local_value <- function(df, value_column) {
  if (identical(value_column, "fielder_stumpings + fielder_catches")) {
    df$fielder_stumpings + df$fielder_catches
  } else {
    df[[value_column]]
  }
}

local_ids_equal <- function(left, right) {
  as.character(left) == as.character(right)
}

local_apply_filters <- function(
  df,
  filters = NULL,
  player_id = NULL,
  definition = NULL,
  include_tier_cutoff = TRUE
) {
  df <- df[df$match_count == 1, , drop = FALSE]

  if (!is.null(filters) && has_filter_value(filters$format)) {
    df <- df[local_ids_equal(df$match_length_id, filters$format), , drop = FALSE]
  }

  if (!is.null(filters) && has_filter_value(filters$series)) {
    df <- df[local_ids_equal(df$series_id, filters$series), , drop = FALSE]
    season_year <- as.integer(substr(df$season_name, 1, 4))
    if (identical(as.character(filters$series), as.character(series_choices[["Aus Domestic T20 M"]]))) {
      df <- df[season_year >= 2011, , drop = FALSE]
    }
    if (identical(as.character(filters$series), as.character(series_choices[["Aus Domestic T20 F"]]))) {
      df <- df[season_year >= 2015, , drop = FALSE]
    }
  }

  if (!is.null(filters) && has_filter_value(filters$venue)) {
    df <- df[local_ids_equal(df$venue_id, filters$venue), , drop = FALSE]
  }

  if (!is.null(filters) && has_filter_value(filters$team)) {
    df <- df[local_ids_equal(df$team_id, filters$team), , drop = FALSE]
  }

  if (!is.null(player_id) && length(player_id) > 0 && !identical(as.character(player_id), "")) {
    df <- df[local_ids_equal(df$player_id, player_id), , drop = FALSE]
  }

  if (
    include_tier_cutoff &&
      !is.null(definition) &&
      definition$query_strategy %in% c("tiered_innings", "tiered_match")
  ) {
    vals <- local_value(df, definition$value_column)
    cutoff <- event_cutoff(definition)
    if (!is.na(cutoff)) {
      df <- df[vals >= cutoff, , drop = FALSE]
    }
  }

  if (!is.null(definition) && identical(definition$definition_id, "carried_bat")) {
    df <- df[
      df$batting_position %in% carried_bat_batting_position &
        df$batter_how_out_id == not_out_id &
        (
          (df$match_innings_id_when_team_batted == 1 & df$team_innings_1_closure_id == 2) |
            (df$match_innings_id_when_team_batted == 2 & df$team_innings_2_closure_id == 2)
        ),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(definition) && identical(definition$category, "fielding_wk")) {
    df <- df[df$is_keeper == 1, , drop = FALSE]
  }

  if (!is.null(definition)) {
    df <- df[!df$team_match_result_id %in% invalid_match_result_ids, , drop = FALSE]
  }

  df
}

local_assign_tier <- function(values, event_values) {
  if (length(values) == 0) {
    return(numeric())
  }

  tiers <- sort(unique(as.numeric(event_values)))
  tiers <- tiers[!is.na(tiers)]
  if (length(tiers) == 0) {
    return(rep(NA_real_, length(values)))
  }

  vapply(
    values,
    function(v) {
      hit <- tiers[v >= tiers]
      if (length(hit) == 0) NA_real_ else max(hit)
    },
    numeric(1)
  )
}

local_milestone_query <- function(definition, player_id = NULL, filters = NULL) {
  career_df <- local_apply_filters(
    sample_innings(),
    filters = filters,
    player_id = player_id,
    definition = definition,
    include_tier_cutoff = FALSE
  )

  match_counts <- career_df %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(n_matches = dplyr::n_distinct(.data$match_id), .groups = "drop")

  df <- career_df
  cutoff <- event_cutoff(definition)
  if (definition$query_strategy %in% c("tiered_innings", "tiered_match") && !is.na(cutoff)) {
    vals <- local_value(df, definition$value_column)
    df <- df[vals >= cutoff, , drop = FALSE]
  }

  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  df$stat_column <- local_value(df, definition$value_column)

  attach_match_rate <- function(tbl) {
    tbl %>%
      dplyr::left_join(match_counts, by = "player_id") %>%
      dplyr::mutate(
        n_matches = dplyr::coalesce(.data$n_matches, 0L),
        avg_value = ifelse(.data$n_matches > 0, .data$current_value / .data$n_matches, 0)
      )
  }

  if (identical(definition$query_strategy, "cumulative")) {
    grouped <- df %>%
      dplyr::group_by(.data$player_id, .data$name) %>%
      dplyr::summarise(
        last_match_date = max(.data$match_date),
        last_value = dplyr::nth(.data$stat_column, which.max(.data$match_date)),
        current_value = if (identical(definition$aggregation, "count")) {
          dplyr::n_distinct(.data$stat_column)
        } else {
          sum(.data$stat_column, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      attach_match_rate()

    if (identical(definition$aggregation, "count")) {
      grouped$last_value <- 1
    }

    progress <- progress_from_thresholds(
      grouped$current_value,
      thresholds_for(definition$display_name, if (is.null(filters)) NULL else filters$series)
    )

    return(
      grouped %>%
        dplyr::transmute(
          display_name = definition$display_name,
          player_id = .data$player_id,
          name = .data$name,
          last_match_date = .data$last_match_date,
          last_value = .data$last_value,
          avg_value = .data$avg_value,
          n_matches = .data$n_matches,
          current_value = .data$current_value,
          current_tier = progress$current_tier,
          next_threshold = progress$next_threshold,
          milestone_type = "cumulative"
        ) %>%
        dplyr::arrange(dplyr::desc(.data$current_value))
    )
  }

  if (identical(definition$query_strategy, "tiered_match")) {
    df <- df %>%
      dplyr::group_by(.data$player_id, .data$name, .data$match_id) %>%
      dplyr::summarise(
        match_date = max(.data$match_date),
        stat_column = sum(.data$stat_column, na.rm = TRUE),
        .groups = "drop"
      )
  }

  df$score_tier <- local_assign_tier(
    df$stat_column,
    related_event_values(definition)
  )
  df <- df[!is.na(df$score_tier), , drop = FALSE]
  if (!is.na(event_cutoff(definition))) {
    df <- df[df$score_tier == event_cutoff(definition), , drop = FALSE]
  }

  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  summarised <- df %>%
    dplyr::group_by(.data$player_id, .data$name) %>%
    dplyr::summarise(
      last_match_date = max(.data$match_date),
      current_value = dplyr::n(),
      .groups = "drop"
    ) %>%
    attach_match_rate()

  progress <- progress_from_thresholds(
    summarised$current_value,
    thresholds_for(definition$display_name, if (is.null(filters)) NULL else filters$series)
  )

  summarised %>%
    dplyr::transmute(
      display_name = definition$display_name,
      player_id = .data$player_id,
      name = .data$name,
      last_match_date = .data$last_match_date,
      last_value = 1,
      avg_value = .data$avg_value,
      n_matches = .data$n_matches,
      current_value = .data$current_value,
      current_tier = progress$current_tier,
      next_threshold = progress$next_threshold,
      milestone_type = definition$query_strategy
    ) %>%
    dplyr::arrange(dplyr::desc(.data$current_value))
}

execute_milestone_query <- function(definition, player_id = NULL, filters = NULL) {
  if (is_local_data()) {
    return(local_milestone_query(definition, player_id, filters))
  }

  QueryDBFunction(
    con = get_db_connection(),
    query = build_query(
      definition = definition,
      player_id = player_id,
      filters = filters
    )
  )
}

local_lookup_table <- function(df, id_col, name_col) {
  rows <- df %>%
    dplyr::distinct(ids = .data[[id_col]], names = .data[[name_col]]) %>%
    dplyr::arrange(.data$names)

  dplyr::bind_rows(
    data.frame(ids = 0, names = "All", stringsAsFactors = FALSE),
    rows
  )
}

local_get_venues <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  local_lookup_table(df, "venue_id", "venue_name")
}

local_get_teams <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  local_lookup_table(df, "team_id", "team_name")
}

local_get_players <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  df %>%
    dplyr::distinct(.data$player_id, .data$name) %>%
    dplyr::arrange(.data$name)
}

local_get_formats <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  local_lookup_table(df, "match_length_id", "format_name")
}

local_get_min_season_year <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  years <- as.integer(substr(df$season_name, 1, 4))
  years <- years[!is.na(years)]
  if (length(years) == 0) {
    return(series_min_year(if (is.null(filters)) NULL else filters$series))
  }
  min(years)
}

local_get_players_for <- function(team, series, season) {
  df <- sample_innings()
  series_match <- local_ids_equal(df$series_id, series) | df$series_name == series
  team_match <- local_ids_equal(df$team_id, team) | df$team_name == team
  df <- df[series_match & team_match & df$season_name == season, , drop = FALSE]

  df %>%
    dplyr::distinct(.data$player_id, .data$name) %>%
    dplyr::arrange(.data$name)
}
