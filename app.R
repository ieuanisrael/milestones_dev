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

# Configuration and lookup definitions
source("/srv/shiny-server/R/config/milestone_def.R")
source("/srv/shiny-server/R/config/select_choices.R")
if (file.exists("/srv/shiny-server/R/config/constants.R")) {
  source("/srv/shiny-server/R/config/constants.R")
} else {
  source("/srv/shiny-server/R/config/local_constants.R")
}

# Database helpers and query utilities
source("/srv/shiny-server/R/database/connection.R")
source("/srv/shiny-server/R/database/filters.R")
source("/srv/shiny-server/R/database/lookups.R")
source("/srv/shiny-server/R/database/local_data.R")
source("/srv/shiny-server/R/database/auth.R")

# Milestone computation helpers
source("/srv/shiny-server/R/milestone_functions/leaderboard.R")
source("/srv/shiny-server/R/milestone_functions/milestone_helpers.R")
source("/srv/shiny-server/R/milestone_functions/player_progress.R")
source("/srv/shiny-server/R/milestone_functions/query_builders.R")

# UI modules
source("/srv/shiny-server/R/modules/mod_player_dashboard.R")
source("/srv/shiny-server/R/modules/mod_milestone_explorer.R")
source("/srv/shiny-server/R/modules/mod_conditional_filters.R")
source("/srv/shiny-server/R/utils/headerFooter.R")
# source("/srv/shiny-server/R/modules/mod_milestone_builder.R")

ui <- page_navbar(
  
  fillable = FALSE,
  
  navbar_options = navbar_options(bg = "#002855"),
  nav_panel("Player Milestones", playerDashboardUI("player")),
  nav_panel("Milestone Leaderboard", milestoneExplorerUI("explorer")),

  title = "Cricket NSW Milestones",
  
  footer = footer_row
)

server <- function(input, output, session) {
  con <- get_connection_ludis()
  
  playerDashboardServer("player", con = con)
  milestoneExplorerServer("explorer", con = con)
}

# Run the application
options(shiny.host = '0.0.0.0')
options(shiny.port = 80)
shinyApp(ui = ui, server = server)

shinyApp(ui, server)

