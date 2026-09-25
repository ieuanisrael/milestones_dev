library(glue)
library(lubridate)
library(dplyr)

Sys.setenv("MILESTONES_USE_SAMPLE" = 0) # uncomment for database
Sys.setenv("MILESTONES_USE_LUDIS" = 1)

if (!file.exists("app.R")) {
  stop("Run this script from the milestones_dev project root.")
}

team <- "'NSW Blues M'"
series <- 4
series_name <- "Aus Domestic OD M"
season <- "2025-26"

source("./R/config/milestone_def.R")
source("./R/config/select_choices.R")
if (file.exists("./R/config/constants.R")) {
  source("./R/config/constants.R")
} else {
  source("./R/config/local_constants.R")
}

filters <- list(
  series = series,
  venue = NULL,
  team = NULL
)

source("./R/database/connection.R")
source("./R/database/auth.R")
source("./R/database/filters.R")
source("./R/database/lookups.R")
source("./R/database/local_data.R")
source("./R/milestone_functions/milestone_helpers.R")
source("./email/email_milestone_helpers.R")
source("./email/email_query_builder.R")

enabled <- milestones_for_series(filters$series)

if(Sys.getenv("MILESTONES_USE_LUDIS") == 1) {
  con <- get_connection_ludis()
} else {
  con <- get_connection_local()
}


results <- purrr::map_df(seq_len(nrow(enabled)), function(i) {
  definition <- enabled[i, ]

  res <- tryCatch(
    execute_milestone_query(
      definition = definition,
      filters = filters,
      con = con
    ),
    error = function(e) NULL
  )

  res

})

source("./R/milestone_functions/query_builders.R")

progress <- purrr::map_df(seq_len(nrow(enabled)), function(i) {
  definition <- enabled[i, ]
  
  res <- tryCatch(
    execute_milestone_query(
      definition = definition,
      filters = filters,
      con = con
    ),
    error = function(e) NULL
  )
  
  res
  
})



if (Sys.getenv("MILESTONES_USE_LUDIS") == 1) {
  query <- glue(
    "SELECT 
            [ams_id]
          FROM 
            [elite].[LISTS_contract_lists]
          WHERE
            team_id in {series} AND season = '{this_year}'"
  )
  
  team_list <- tryCatch(
    QueryDBFunction(con = con, query = query),
    error = function(e) NULL
  )
} else {
  query <- get_players_query(team, series, season)
  team_list <- tryCatch(
    QueryDBFunction(con = con, query = query),
    error = function(e) NULL
  )
}


source("./email/email_html.R")
message("Wrote player_milestone_update.html")


