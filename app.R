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

Sys.setenv("MILESTONES_USE_SAMPLE" = 0) # uncomment for database

# Configuration and lookup definitions
source("./R/config/milestone_def.R")
source("./R/config/select_choices.R")
if (file.exists("./R/config/constants.R")) {
  source("./R/config/constants.R")
} else {
  source("./R/config/local_constants.R")
}

# Database helpers and query utilities
source("./R/database/connection.R")
source("./R/database/filters.R")
source("./R/database/lookups.R")
source("./R/database/local_data.R")

# Milestone computation helpers
source("./R/milestone_functions/leaderboard.R")
source("./R/milestone_functions/milestone_helpers.R")
source("./R/milestone_functions/player_progress.R")
source("./R/milestone_functions/query_builders.R")

# UI modules
source("./R/modules/mod_player_dashboard.R")
source("./R/modules/mod_milestone_explorer.R")
source("./R/modules/mod_conditional_filters.R")
source("./R/utils/headerFooter.R")
# source("./R/modules/mod_milestone_builder.R")

ui <- page_navbar(
  
  fillable = FALSE,
  
  navbar_options = navbar_options(bg = "#002855"),
  nav_panel("Player Milestones", playerDashboardUI("player")),
  nav_panel("Milestone Leaderboard", milestoneExplorerUI("explorer")),

  title = "Cricket NSW Milestones",
  
  footer = footer_row
)

server <- function(input, output, session) {
  playerDashboardServer("player")
  milestoneExplorerServer("explorer")
}

options(shiny.launch.browser = TRUE)

shinyApp(ui, server)

