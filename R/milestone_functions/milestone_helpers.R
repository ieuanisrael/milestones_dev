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

build_aggregation <- function(aggregation_function, value_column, threshold_value = NULL) {
  switch(
    aggregation_function,
    sum = glue::glue("SUM(pi.{value_column})"),
    count = glue::glue("COUNT(DISTINCT pi.{value_column})"),
    stop("Unknown aggregation function")
  )
}
