library(glue)
library(lubridate)
library(dplyr)
library(odbc)

team <- 'NSW Blues M'
series <- 'Aus Domestic OD M'
season <- '2025-26'

filters <- list(
  series = series,
  venue = NULL,
  team = NULL
)

setwd("C:/Users/ieuan.israel/Documents/milestones")

source("./R/config/milestone_def.R")
source("./R/config/select_choices.R")
source("./R/config/constants.R")

source("./R/database/connection.R")
source("./R/database/filters.R")
source("./R/database/lookups.R")

source("./email/email_milestone_helpers.R")
source("./email/email_query_builder.R")

results <- purrr::map_df(seq_len(nrow(milestones)), function(i) {
  
  definition <- milestones[i,]
  
  query_data <- build_query(
    definition = definition,
    filters = filters
  )
  
  res <- tryCatch(
    QueryDBFunction(con = con, query = query_data),
    error = function(e) NULL
  )
  
  # Skip if no result returned
  if (is.null(res) ||
      nrow(res) == 0 ||
      is.null(res$current_value) ||
      all(is.na(res$current_value))) {
    return(NULL)
  }
  
  current_value <- as.numeric(res$current_value)
  
  state <- build_milestone_state(
    current_value,
    res$next_threshold
  )
  
  display_name_ui <- select_choices[res$display_name[1] == select_choices$display_name,]$display_name_ui
  
  tibble::tibble(
    display_name = res$display_name,
    display_name_ui = display_name_ui,
    current_value = current_value,
    threshold_value = res$current_tier,
    remaining = state$remaining,
    progress_pct = state$progress_pct,
    next_target = res$next_threshold,
    last_match_date = res$last_match_date,
    last_value = res$last_value,
    avg_value = res$avg_value,
    player = res$name,
    milestone_achieved = current_value - last_value <  threshold_value,
    within_threshold = 10 * avg_value + current_value > next_target
  ) 
  
})

query <- get_players_query(team,series,season)

team_list <- tryCatch(
  QueryDBFunction(con = con, query = query),
  error = function(e) NULL
)

