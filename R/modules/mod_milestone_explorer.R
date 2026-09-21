# Milestone explorer UI and server logic.
# This module lets the user pick a milestone definition and view its leaderboard.

milestoneExplorerUI <- function(id) {
  
  ns <- NS(id)
  card(
    card_body(
      conditionalFiltersUI(ns("milestone_conditional_filters")),
      
      fluidRow(
        column(
          width = 12,
          uiOutput(ns('warningText'))
        )
      ),
      
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
          dataTableOutput(ns("tab")) %>% withSpinner()
        )
      )
    )
  )
}

milestoneExplorerServer <- function(id, con, internal_con) {
  moduleServer(id, function(input, output, session) {
    
    filters <- conditionalFiltersServer("milestone_conditional_filters", con = con)
    
    observeEvent(filters, {
      output$warningText <- renderUI({
        min_year <- get_min_season_year(
          filters = list(series = filters()$series, venue = filters()$venue, team = filters()$team),
          conn = con
        )
        if (!(stringr::str_detect(filters()$series, "T20"))) {
          tags$div(
            tags$div(
            class = "alert alert-danger d-flex align-items-center",
            role = "alert",
            style = "padding: 12px 16px; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
            
            # Message body
            tags$div(
              glue("Data only available from {min_year}")
            )
          ),
          
          tags$div(
            style = "background-color:#fff2cc; padding: 12px 16px; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
            
            # Message body
            tags$div(
              glue("Highlighted rows denote NSW contracted players")
            )
          )
          )
          
        } else {
          tags$div(
            tags$div(
              style = "background-color:#fff2cc; padding: 12px 16px; border-radius: 6px; box-shadow: 0 2px 4px  rgba(0,0,0,0.05);",
              
              # Message body
              tags$div(
                glue("Highlighted rows denote NSW contracted players")
              )
            )
          )
        }
        
      })
    })
    

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
        filters = filters(),
        con = con
      )
    })

    output$tab <- renderDataTable(
      {
        req(filters()$series)
        
        query <- glue(
          "SELECT 
            [ams_id]
          FROM 
            [elite].[LISTS_contract_lists]
          WHERE
            team_id = '{filters()$series}' AND season = '{this_year}'"
        )
        
        query %>% print

        team_list <- tryCatch(
          QueryDBFunction(con = internal_con, query = query),
          error = function(e) NULL
        )
        
        team_list %>% glimpse
        
        datatable(
          leaderboard_data(),
          style = "default"
        ) %>%
          formatStyle(
            'player_id',
            target = 'row',
            backgroundColor = styleEqual(
              team_list$ams_id, # The exact names to look for
              rep('#fff2cc', length(team_list$ams_id)),  # Highlights matches in light yellow
              default = '#ffffff'
            )
          )
      }
    ) 
  })
}
