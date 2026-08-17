build_tier_case <- function(
    value_column,
    first_value,
    multiple,
    max_value = 500
){
  
  tiers <- seq(
    first_value,
    max_value,
    by = multiple
  )
  
  case_lines <- purrr::map_chr(
    rev(tiers),
    ~ glue::glue(
      "WHEN {value_column} >= {.x} THEN {.x}"
    )
  )
  
  paste0(
    "CASE\n",
    paste(case_lines, collapse = "\n"),
    "\nEND"
  )
}

build_step_progress <- function(
    value_expr,
    first_value,
    multiple
){
  
  list(
    
    current_tier =
      glue::glue("
      CASE
        WHEN {value_expr} < {first_value}
        THEN 0
        ELSE
          FLOOR(({value_expr}-{first_value})/{multiple}) * {multiple}
          + {first_value}
      END
      "),
    
    next_threshold =
      glue::glue("
      CASE
        WHEN {value_expr} < {first_value}
        THEN {first_value}
        ELSE
          FLOOR(({value_expr}-{first_value})/{multiple}) * {multiple}
          + {first_value}
          + {multiple}
      END
      ")
    
  )
  
}


build_milestone_state <- function(current_value, next_threshold) {
  current_value <- suppressWarnings(as.numeric(current_value))
  if (length(current_value) == 0 || all(is.na(current_value))) {
    current_value <- 0
  }
  
  achieved <- F
  remaining <- next_threshold - current_value
  progress_pct <- round(100 * current_value / next_threshold, 1)
  
  
  list(achieved = achieved, remaining = remaining, progress_pct = progress_pct, next_target = next_threshold, current_value = current_value)
}

build_select_aggregation <- function(
    aggregation_function,
    value_column,
    threshold_value = NULL
){
  
  switch(
    
    aggregation_function,
    
    sum =
      glue::glue(
        "max(lr.stat_column)
          AS last_value,
        avg(rr.stat_column)
          AS avg_value,"
      ),
    
    count =
      glue::glue(
        "1 AS last_value,
        1 AS avg_value,"
      ),
    
    stop("Unknown aggregation function")
    
  )
  
}

build_aggregation <- function(
    aggregation_function,
    value_column,
    threshold_value = NULL
){
  
  switch(
    
    aggregation_function,
    
    sum =
      glue::glue(
        "SUM({value_column})"
      ),
    
    count =
      glue::glue(
        "COUNT(DISTINCT {value_column})"
      ),
    
    stop("Unknown aggregation function")
    
  )
  
}


build_value_expression <- function(
    aggregation_function,
    value_column,
    threshold_value = NULL
){
  
  switch(
    
    aggregation_function,
    
    sum =
      glue::glue(
        "SUM(pi.{value_column})"
      ),
    
    count =
      glue::glue(
        "SUM(1)"
      ),
    
    stop("Unknown aggregation function")
    
  )
  
}


