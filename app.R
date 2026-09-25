# Main Shiny application entry point for the milestone studio.
# This file loads the supporting modules and launches the dashboard UI.

library(shiny)
library(bslib)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(DBI)
library(odbc)
library(DT)

Sys.setenv("MILESTONES_USE_SAMPLE" = 0) # uncomment for database
Sys.setenv("MILESTONES_USE_LUDIS" = 1) # uncomment for database

prefix <- ifelse(Sys.getenv("MILESTONES_USE_LUDIS") == 0, ".", "/srv/shiny-server")

# Configuration and lookup definitions
source(glue("{prefix}/R/config/milestone_def.R"))
source(glue("{prefix}/R/config/select_choices.R"))
if (file.exists(glue("{prefix}/R/config/constants.R"))) {
  source(glue("{prefix}/R/config/constants.R"))
} else {
  source(glue("{prefix}/R/config/local_constants.R"))
}

# Database helpers and query utilities
source(glue("{prefix}/R/database/connection.R"))
source(glue("{prefix}/R/database/filters.R"))
source(glue("{prefix}/R/database/lookups.R"))
source(glue("{prefix}/R/database/local_data.R"))
source(glue("{prefix}/R/database/auth.R"))

# Milestone computation helpers
source(glue("{prefix}/R/milestone_functions/leaderboard.R"))
source(glue("{prefix}/R/milestone_functions/milestone_helpers.R"))
source(glue("{prefix}/R/milestone_functions/player_progress.R"))
source(glue("{prefix}/R/milestone_functions/query_builders.R"))

# UI modules
source(glue("{prefix}/R/modules/mod_player_dashboard.R"))
source(glue("{prefix}/R/modules/mod_milestone_explorer.R"))
source(glue("{prefix}/R/modules/mod_conditional_filters.R"))
source(glue("{prefix}/R/utils/headerFooter.R"))
# source("/srv/shiny-server/R/modules/mod_milestone_builder.R"))

ui <- page_navbar(
  
  fillable = FALSE,
  
  navbar_options = navbar_options(bg = "#002855"),
  nav_panel("Player Milestones", playerDashboardUI("player")),
  nav_panel("Milestone Leaderboard", milestoneExplorerUI("explorer")),

  title = "Cricket NSW Milestones",
  
  footer = footer_row
)

server <- function(input, output, session) {
  if (Sys.getenv("MILESTONES_USE_LUDIS") == 0) {
    con <- get_connection_local()
    con_internal <- NULL
  } else {
    con <- get_connection_ludis()
    #con_internal <- get_connection_internal_ludis()
  }
  playerDashboardServer("player", con = con, internal_con = con_internal)
  milestoneExplorerServer("explorer", con = con, internal_con = con_internal)
}

# Run the application
options(shiny.host = '0.0.0.0')
options(shiny.port = 80)
shinyApp(ui = ui, server = server)

shinyApp(ui, server)

