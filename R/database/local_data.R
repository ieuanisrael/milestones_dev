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

local_apply_filters <- function(df, filters = NULL, player_id = NULL, definition = NULL) {
  df <- df[df$match_count == 1, , drop = FALSE]

  if (!is.null(filters) && has_filter_value(filters$format)) {
    df <- df[df$match_length_id == filters$format, , drop = FALSE]
  }

  if (!is.null(filters) && has_filter_value(filters$series)) {
    df <- df[df$series_name == filters$series, , drop = FALSE]
    season_year <- as.integer(substr(df$season_name, 1, 4))
    if (identical(filters$series, "Aus Domestic T20 M")) {
      df <- df[season_year >= 2011, , drop = FALSE]
    }
    if (identical(filters$series, "Aus Domestic T20 F")) {
      df <- df[season_year >= 2015, , drop = FALSE]
    }
  }

  if (!is.null(filters) && has_filter_value(filters$venue)) {
    df <- df[df$venue_name == filters$venue, , drop = FALSE]
  }

  if (!is.null(filters) && has_filter_value(filters$team)) {
    df <- df[df$team_name == filters$team, , drop = FALSE]
  }

  if (!is.null(player_id) && length(player_id) > 0 && !identical(player_id, "")) {
    df <- df[as.character(df$player_id) == as.character(player_id), , drop = FALSE]
  }

  if (!is.null(definition) && definition$query_strategy %in% c("tiered_innings", "tiered_match")) {
    vals <- local_value(df, definition$value_column)
    df <- df[vals >= definition$first_value, , drop = FALSE]
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

local_assign_tier <- function(values, first_value, multiple, max_value) {
  if (length(values) == 0) {
    return(numeric())
  }

  if (is.na(multiple) || multiple == 0) {
    return(ifelse(values >= first_value, first_value, NA_real_))
  }

  tiers <- seq(first_value, max_value, by = multiple)
  vapply(
    values,
    function(v) {
      hit <- tiers[v >= tiers]
      if (length(hit) == 0) NA_real_ else max(hit)
    },
    numeric(1)
  )
}

local_step_progress <- function(value, first_value, multiple) {
  if (is.na(multiple) || multiple == 0) {
    multiple <- first_value
  }

  current_tier <- ifelse(
    value < first_value,
    0,
    floor((value - first_value) / multiple) * multiple + first_value
  )
  next_threshold <- ifelse(value < first_value, first_value, current_tier + multiple)

  list(current_tier = current_tier, next_threshold = next_threshold)
}

local_milestone_query <- function(definition, player_id = NULL, filters = NULL) {
  df <- local_apply_filters(
    sample_innings(),
    filters = filters,
    player_id = player_id,
    definition = definition
  )

  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  df$stat_column <- local_value(df, definition$value_column)

  if (identical(definition$query_strategy, "cumulative")) {
    grouped <- df %>%
      dplyr::group_by(.data$player_id, .data$name) %>%
      dplyr::summarise(
        last_match_date = max(.data$match_date),
        last_value = dplyr::nth(.data$stat_column, which.max(.data$match_date)),
        avg_value = mean(.data$stat_column, na.rm = TRUE),
        current_value = if (identical(definition$aggregation, "count")) {
          dplyr::n_distinct(.data$stat_column)
        } else {
          sum(.data$stat_column, na.rm = TRUE)
        },
        .groups = "drop"
      )

    if (identical(definition$aggregation, "count")) {
      grouped$last_value <- 1
      grouped$avg_value <- 1
    }

    progress <- local_step_progress(
      grouped$current_value,
      definition$first_value,
      definition$multiple
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
    definition$first_value,
    definition$multiple,
    definition$max_value
  )
  df <- df[!is.na(df$score_tier), , drop = FALSE]

  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::group_by(.data$player_id, .data$name, .data$score_tier) %>%
    dplyr::summarise(
      last_match_date = max(.data$match_date),
      current_value = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      display_name = paste0(definition$display_name, " - ", .data$score_tier),
      player_id = .data$player_id,
      name = .data$name,
      last_match_date = .data$last_match_date,
      last_value = 1,
      avg_value = 1,
      current_value = .data$current_value,
      current_tier = floor(.data$current_value / 50) * 50,
      next_threshold = floor(.data$current_value / 50) * 50 + 50,
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

local_get_venues <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  sort(unique(df$venue_name))
}

local_get_teams <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  sort(unique(df$team_name))
}

local_get_players <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  df %>%
    dplyr::distinct(.data$player_id, .data$name) %>%
    dplyr::arrange(.data$name)
}

local_get_formats <- function(filters = NULL) {
  df <- local_apply_filters(sample_innings(), filters = filters)
  formats <- df %>%
    dplyr::distinct(format_id = .data$match_length_id, name = .data$format_name) %>%
    dplyr::arrange(.data$format_id)

  rbind(data.frame(format_id = "All", name = "All"), formats)
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
  df <- df[
    df$team_name == team &
      df$series_name == series &
      df$season_name == season,
    ,
    drop = FALSE
  ]

  df %>%
    dplyr::distinct(.data$player_id, .data$name) %>%
    dplyr::arrange(.data$name)
}
