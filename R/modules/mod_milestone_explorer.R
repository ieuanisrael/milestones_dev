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
            choices = setNames(milestones$definition_id, milestones$display_name_ui),
            selected = milestones$definition_id[[1]]
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

    observeEvent(filters(), {
      enabled <- milestones_for_series(filters()$series)
      if (nrow(enabled) == 0) {
        return()
      }

      selected <- input$milestone
      if (is.null(selected) || !selected %in% enabled$definition_id) {
        selected <- enabled$definition_id[[1]]
      }

      updateRadioButtons(
        session,
        "milestone",
        choices = setNames(enabled$definition_id, enabled$display_name_ui),
        selected = selected
      )
    })

    selected_definition <- reactive({
      req(input$milestone)
      milestones[milestones$definition_id == input$milestone, ]
    })

    leaderboard_data <- reactive({
      definition <- selected_definition()
      req(nrow(definition) == 1)

      get_milestone_leaderboard(
        new_display_name = definition$display_name,
        definition = definition,
        filters = filters()
      )
    })

    output$leaderboard <- DT::renderDT(
      {
        leaderboard_data()
      },
      options = list(
        scrollY = "500px",
        paging = FALSE
      )
    )
  })
}
