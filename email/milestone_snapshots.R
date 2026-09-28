library(glue)
library(lubridate)
library(dplyr)
library(odbc)

Sys.setenv("MILESTONES_USE_SAMPLE" = 0) # uncomment for database
Sys.setenv("MILESTONES_USE_LUDIS" = 0)

prefix <- ifelse(Sys.getenv("MILESTONES_USE_LUDIS") == 0, ".", "/srv/shiny-server")


source(glue("{prefix}/R/config/milestone_def.R"))
source(glue("{prefix}/R/config/select_choices.R"))
source(glue("{prefix}/R/config/constants.R"))

source(glue("{prefix}/R/database/connection.R"))
source(glue("{prefix}/R/database/auth.R"))
source(glue("{prefix}/R/database/filters.R"))
source(glue("{prefix}/R/database/lookups.R"))
source(glue("{prefix}/R/database/local_data.R"))
source(glue("{prefix}/R/milestone_functions/milestone_helpers.R"))
source(glue("{prefix}/email/email_milestone_helpers.R"))

if(Sys.getenv("MILESTONES_USE_LUDIS") == 1) {
  con <- get_connection_ludis()
} else {
  con <- get_connection_local()
}


email_types <- data.frame(
  team_name = c("NSW Blues M",
                "NSW Blues M",
                "NSW Breakers F",
                "Sydney Sixers M and Sydney Thunder M",
                "Sydney Sixers F and Sydney Thunder F"),
  team_id = c("'NSW Blues M'",
              "'NSW Blues M'",
              "'NSW Breakers F'",
              "'Sydney Sixers M','Sydney Thunder M'",
              "'Sydney Sixers F','Sydney Thunder F'"),
  series_id = c(3,
                4,
                690007,
                950002,
                220008),
  series_name = c("Aus Domestic 1st Class M",
                  "Aus Domestic OD M",
                  "Aus Domestic OD F",
                  "Aus Domestic T20 M",
                  "Aus Domestic T20 F"),
  file_name = c("1stclass","ODM","ODF","T20M","T20F"),
  current_year = "2025-26"
)

for(j in 1:nrow(email_types)) {
  vars <- email_types[j,]
  
  vars %>% glimpse
  
  filters <- list(
    series = vars$series_id,
    venue = NULL,
    team = NULL
  )
  
  source(glue("{prefix}/email/email_query_builder.R"))
  
  enabled <- milestones_for_series(filters$series)
  
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
  
  source(glue("{prefix}/R/milestone_functions/query_builders.R"))
  
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
            team_id in {vars$team_id} AND season = '{this_year}'"
    )
    
    team_list <- tryCatch(
      QueryDBFunction(con = con, query = query),
      error = function(e) NULL
    )
  } else {
    query <- get_players_query(vars$team_id, vars$series_id, vars$current_year)
    team_list <- tryCatch(
      QueryDBFunction(con = con, query = query),
      error = function(e) NULL
    )
  }
  
  
  source(glue("{prefix}/email/email_html.R"))
  message("Wrote player_milestone_update.html")
  
}





