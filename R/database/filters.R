# SQL filter helpers for milestone queries.
# These functions normalize filter values and turn them into safe WHERE clauses.

has_filter_value <- function(value) {
  !is.null(value) &&
    length(value) > 0 &&
    !is.na(value) &&
    (!is.character(value) || (nzchar(value) && !identical(value, "All")))
}

sql_literal <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("NULL")
  }

  if (is.numeric(value) || is.integer(value)) {
    return(as.character(value))
  }

  paste0("'", gsub("'", "''", as.character(value)), "'")
}

sql_list <- function(values) {
  paste0("(", paste(values, collapse = ","), ")")
}

build_filter_clause <- function(filters, player_id = NULL, definition = NULL) {
  clauses <- c()

  if (!is.null(filters) && has_filter_value(filters$format)) {
    clauses <- c(clauses, paste0("m.match_length_id = ", sql_literal(filters$format)))
  }

  if (!is.null(filters) && has_filter_value(filters$series)) {
    clauses <- c(clauses, paste0("series.name = ", sql_literal(filters$series)))
    if (filters$series == "Aus Domestic T20 M") {
      clauses <- c(clauses, paste0("CAST(LEFT(season.name, 4) AS INT) >= 2011"))
    }
    if (filters$series == "Aus Domestic T20 F") {
      clauses <- c(clauses, paste0("CAST(LEFT(season.name, 4) AS INT) >= 2015"))
    }
  }

  if (!is.null(filters) && has_filter_value(filters$venue)) {
    clauses <- c(clauses, paste0("venue.name = ", sql_literal(filters$venue)))
  }

  if (!is.null(filters) && has_filter_value(filters$team)) {
    clauses <- c(clauses, paste0("team.team_name = ", sql_literal(filters$team)))
  }

  if (!is.null(player_id)) {
    clauses <- c(clauses, paste0("p.player_id = ", sql_literal(player_id)))
  }

  if (!is.null(definition) && definition$query_strategy %in% c("tiered_innings", "tiered_match")) {
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

