# Milestone explorer UI and server logic.
# This module lets the user pick a milestone definition and view its leaderboard.

milestoneExplorerUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    conditionalFiltersUI(ns("milestone_conditional_filters")),
    
    card_body(
      fluidRow(
        column(
          3,
          radioButtons(
            ns("milestone"),
            "Milestone",
            choices = setNames(
              seq_len(nrow(select_choices)),
              select_choices$display_name_ui
            ),
            #options = list(dropdownParent = "body", `live-search` = TRUE),
            selected = 1
          )
        ),
        column(
          9,
          DT::DTOutput(ns("leaderboard")) %>% withSpinner()
        )
      )
    )
  )
  
}

milestoneExplorerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    filters <- conditionalFiltersServer("milestone_conditional_filters")

    selected_definition <- reactive({
      req(input$milestone)

      milestones[
        milestones$definition_id == select_choices[input$milestone, ]$definition_id,
      ]
    })

    leaderboard_data <- reactive({
      get_milestone_leaderboard(
        new_display_name = select_choices[input$milestone, ]$display_name,
        definition = selected_definition(),
        filters = filters()
      )
    })

    output$leaderboard <- DT::renderDT(
      {
        leaderboard_data()
      }, 
      options = list(
        scrollY = "500px",  # Sets the fixed vertical scroll height
        paging = FALSE       # Disables pagination to scroll all data
      )
    )
  })
}
