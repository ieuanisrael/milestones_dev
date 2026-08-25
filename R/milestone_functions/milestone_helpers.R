# Shared helper functions for milestone queries.
# These utilities build tier expressions and progress summaries for the query layer.

build_tier_case <- function(value_column, first_value, multiple, max_value = 500) {
  if (is.na(multiple) || multiple == 0) {
    tiers <- first_value
  } else {
    tiers <- seq(first_value, max_value, by = multiple)
  }

  case_lines <- purrr::map_chr(
    rev(tiers),
    ~ glue::glue("WHEN {value_column} >= {.x} THEN {.x}")
  )

  paste0(
    "CASE\n",
    paste(case_lines, collapse = "\n"),
    "\nEND AS current_tier"
  )
}

build_step_progress <- function(value_expr, first_value, multiple) {
  list(
    current_tier = glue::glue(
      "
      CASE
        WHEN {value_expr} < {first_value}
        THEN 0
        ELSE
          FLOOR(({value_expr}-{first_value})/{multiple}) * {multiple}
          + {first_value}
      END
      "
    ),

    next_threshold = glue::glue(
      "
      CASE
        WHEN {value_expr} < {first_value}
        THEN {first_value}
        ELSE
          FLOOR(({value_expr}-{first_value})/{multiple}) * {multiple}
          + {first_value}
          + {multiple}
      END
      "
    )
  )
}

build_milestone_state <- function(current_value, next_threshold) {
  current_value <- suppressWarnings(as.numeric(current_value))
  if (length(current_value) == 0 || all(is.na(current_value))) {
    current_value <- 0
  }

  achieved <- FALSE
  remaining <- next_threshold - current_value
  progress_pct <- round(100 * current_value / next_threshold, 1)

  list(
    achieved = achieved,
    remaining = remaining,
    progress_pct = progress_pct,
    next_target = next_threshold,
    current_value = current_value
  )
}


series_match_total <- function(series) {
  n <- series_matches[[as.character(series)]]
  if (is.null(n) || length(n) == 0 || is.na(n)) {
    10
  } else {
    as.numeric(n)
  }
}

current_season_window <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  month <- as.integer(format(today, "%m"))

  if (month >= 9) {
    list(
      start = as.Date(sprintf("%d-09-01", y)),
      end = as.Date(sprintf("%d-04-01", y + 1)),
      in_season = TRUE
    )
  } else if (month < 4) {
    list(
      start = as.Date(sprintf("%d-09-01", y - 1)),
      end = as.Date(sprintf("%d-04-01", y)),
      in_season = TRUE
    )
  } else {
    list(
      start = as.Date(sprintf("%d-09-01", y)),
      end = as.Date(sprintf("%d-04-01", y + 1)),
      in_season = FALSE
    )
  }
}

season_remaining <- function(today = Sys.Date(), series = series_choices[[1]]) {
  window <- current_season_window(today)
  total_days <- as.numeric(window$end - window$start)

  if (!isTRUE(window$in_season) || today < window$start) {
    remaining_frac <- 1
  } else if (today >= window$end) {
    remaining_frac <- 0
  } else {
    remaining_frac <- as.numeric(window$end - today) / total_days
  }

  remaining_frac <- max(0, min(1, remaining_frac))

  list(
    start = window$start,
    end = window$end,
    in_season = isTRUE(window$in_season) && today >= window$start && today < window$end,
    remaining_frac = remaining_frac,
    remaining_matches = series_match_total(series) * remaining_frac
  )
}

assess_season_reach <- function(current_value, next_target, avg_value, today = Sys.Date(), series = series_choices[[1]]) {
  season <- season_remaining(today, series)

  current_value <- suppressWarnings(as.numeric(current_value))[1]
  next_target <- suppressWarnings(as.numeric(next_target))[1]
  avg_value <- suppressWarnings(as.numeric(avg_value))[1]

  if (length(current_value) == 0 || is.na(current_value)) {
    current_value <- 0
  }
  if (length(next_target) == 0 || is.na(next_target)) {
    next_target <- 0
  }
  if (length(avg_value) == 0 || is.na(avg_value) || avg_value < 0) {
    avg_value <- 0
  }

  projected <- current_value + avg_value * season$remaining_matches
  achievable <- projected + 1e-9 >= next_target

  season_label <- if (season$in_season) "this season" else "next season"
  status <- paste0(if (achievable) "On track " else "Unlikely ", season_label)

  list(
    remaining_frac = season$remaining_frac,
    remaining_matches = season$remaining_matches,
    in_season = season$in_season,
    avg_value = avg_value,
    projected = projected,
    achievable = achievable,
    status = status
  )
}

build_aggregation <- function(aggregation_function, value_column, threshold_value = NULL) {
  switch(
    aggregation_function,
    sum = glue::glue("SUM(pi.{value_column})"),
    count = glue::glue("COUNT(DISTINCT pi.{value_column})"),
    stop("Unknown aggregation function")
  )
}
