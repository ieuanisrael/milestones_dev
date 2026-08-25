# SQL filter helpers for milestone queries.
# These functions normalize filter values and turn them into safe WHERE clauses.

has_filter_value <- function(value) {
  !is.null(value) &&
    length(value) > 0 &&
    !is.na(value) &&
    !identical(as.character(value), all_id) &&
    !(is.character(value) && !nzchar(value))
}

sql_literal <- function(value) {
  
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("NULL")
  }

  as.integer(value)
}

sql_list <- function(values) {
  paste0("(", paste(values, collapse = ","), ")")
}

build_filter_clause <- function(filters, player_id = NULL, definition = NULL, include_tier_cutoff = TRUE) {
  clauses <- c()

  if (!is.null(filters) && has_filter_value(filters$format)) {
    clauses <- c(clauses, paste0("m.match_length_id = ", sql_literal(filters$format)))
  }

  if (!is.null(filters) && has_filter_value(filters$series)) {
    clauses <- c(clauses, paste0("series.series_id = ", sql_literal(filters$series)))
    if (identical(as.character(filters$series), as.character(series_choices[["Aus Domestic T20 M"]]))) {
      clauses <- c(clauses, paste0("CAST(LEFT(season.name, 4) AS INT) >= 2011"))
    }
    if (identical(as.character(filters$series), as.character(series_choices[["Aus Domestic T20 F"]]))) {
      clauses <- c(clauses, paste0("CAST(LEFT(season.name, 4) AS INT) >= 2015"))
    }
  }

  if (!is.null(filters) && has_filter_value(filters$venue)) {
    clauses <- c(clauses, paste0("venue.venue_id = ", sql_literal(filters$venue)))
  }

  if (!is.null(filters) && has_filter_value(filters$team)) {
    clauses <- c(clauses, paste0("team.team_id = ", sql_literal(filters$team)))
  }

  if (!is.null(player_id)) {
    clauses <- c(clauses, paste0("p.player_id = ", sql_literal(player_id)))
  }

  if (
    include_tier_cutoff &&
      !is.null(definition) &&
      definition$query_strategy %in% c("tiered_innings", "tiered_match")
  ) {
    clauses <- c(clauses, glue::glue("pi.{definition$value_column} >= {definition$first_value}"))
  }

  if (!is.null(definition) && definition$definition_id == "carried_bat") {
    clauses <- c(
      clauses,
      glue::glue(
        "pi.batting_position IN {sql_list(carried_bat_batting_position)} AND 
        pi.batter_how_out_id = {not_out_id} AND 
        ((pi.match_innings_id_when_team_batted = 1 AND pi.team_innings_1_closure_id = 2)
        OR (pi.match_innings_id_when_team_batted = 2 AND pi.team_innings_2_closure_id = 2))"
      )
    )
  }
  
  if (!is.null(definition) && definition$category == 'fielding_wk') {
    clauses <- c(clauses, "mp.is_keeper = 1")
  }

  if (!is.null(definition)) {
    clauses <- c(clauses, glue::glue("pi.team_match_result_id NOT IN {sql_list(invalid_match_result_ids)}"))
  }

  if (length(clauses) == 0) {
    return("")
  }

  paste("WHERE", paste(clauses, collapse = " AND "))
}

series_min_year <- function(series) {
  series_id <- as.character(series)
  if (identical(series_id, as.character(series_choices[["Aus Domestic T20 M"]]))) {
    2011L
  } else if (identical(series_id, as.character(series_choices[["Aus Domestic T20 F"]]))) {
    2015L
  } else {
    NA_integer_
  }
}

series_date_range_label <- function(series, min_year = NULL) {
  year <- min_year
  if (is.null(year) || length(year) == 0 || is.na(year)) {
    year <- series_min_year(series)
  }

  if (!is.null(year) && length(year) == 1 && !is.na(year)) {
    paste0("Min year: ", year)
  } else {
    "Min year: all available seasons"
  }
}

