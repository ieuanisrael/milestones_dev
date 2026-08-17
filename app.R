# Main Shiny application entry point for the milestone studio.
# This file loads the supporting modules and launches the dashboard UI.

library(shiny)
library(bslib)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(DBI)
library(odbc)

# Configuration and lookup definitions
source("./R/config/milestone_def.R")
source("./R/config/select_choices.R")
source("./R/config/constants.R")

# Database helpers and query utilities
source("./R/database/connection.R")
source("./R/database/filters.R")
source("./R/database/lookups.R")

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

  title = "Player Milestone Studio",
  
  footer = footer_row
)

server <- function(input, output, session) {
  playerDashboardServer("player")
  milestoneExplorerServer("explorer")
}

options(shiny.launch.browser = TRUE)

shinyApp(ui, server,options = list(port = 5432))

