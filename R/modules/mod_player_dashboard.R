# Player dashboard UI and server logic.
# This module presents milestone snapshots and progress tracking for the selected player.

playerDashboardUI <- function(id) {
  ns <- NS(id)

  card(
    card_header("Player Overview"),

    conditionalFiltersUI(ns("conditional_filters")),

    card_body(
      pickerInput(
        ns("player"),
        "Player",
        choices = NULL,
        options = list(placeholder = "Choose a player", `live-search` = TRUE)
      ),

      hr(),

      h4("Milestone Snapshot"),
      uiOutput(ns("milestone_cards")) %>%
        withSpinner(proxy.height = "400px", color.background = "gray"),

      hr(),

      h4("Progress Tracker"),
      DT::DTOutput(ns("progress_table")) %>% withSpinner()
    )
  )
}

playerDashboardServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    conn <- get_db_connection()

    filters <- conditionalFiltersServer("conditional_filters")

    players <- reactive({
      get_players(filters = filters(), conn = conn)
    })

    observe({
      player_tbl <- players()

      if (nrow(player_tbl) == 0) {
        updatePickerInput(
          session,
          "player",
          choices = character(),
          selected = character()
        )
        return()
      }

      selected_player <- input$player

      if (is.null(selected_player) || !selected_player %in% player_tbl$player_id) {
        selected_player <- player_tbl$player_id[1]
      }

      updatePickerInput(
        session,
        "player",
        choices = setNames(player_tbl$player_id, player_tbl$name),
        selected = selected_player
      )
    })

    player_progress <- reactive({
      req(input$player)

      get_player_progress(
        player_id = input$player,
        filters = filters()
      )
    })

    output$milestone_cards <- renderUI({
      req(input$player)

      progress_tbl <- player_progress()

      if (nrow(progress_tbl) == 0) {
        return(div("No milestone data available yet.", class = "text-muted"))
      }

      cards <- lapply(seq_len(nrow(progress_tbl)), function(i) {
        row <- progress_tbl[i, ]

        card(
          class = paste0(
            "border-",
            ifelse(row$progress_pct > 50, "success", "warning")
          ),

          card_header(row$display_name),

          p(paste("Current:", coalesce(row$current_value, 0))),
          p(paste("Next Target:", coalesce(row$next_target, 0))),
          p(paste("Status:", row$achieved)),

          div(
            class = "progress",
            div(
              class = paste0(
                "progress-bar bg-",
                ifelse(row$progress_pct > 50, "success", "warning")
              ),
              style = paste0("width:", row$progress_pct, "%;")
            )
          )
        )
      })

      rows <- split(cards, ceiling(seq_along(cards) / 4))

      tagList(
        lapply(rows, function(group) {
          fluidRow(
            lapply(group, function(card_item) {
              column(3, card_item)
            })
          )
        })
      )
    })

    output$progress_table <- DT::renderDT(
      {
        req(input$player)
        
        p_progress <- player_progress()
        
        validate(
          need(nrow(p_progress) > 0, "Waiting on data...")
        )
        
        p_progress %>%
          select(display_name, current_value, next_target, progress_pct) %>%
          mutate(
            progress_pct = paste(round(progress_pct, 1), "%")
          ) %>%
          rename(
            "Milestone" = "display_name", 
            "Value" = "current_value", 
            "Target" = "next_target", 
            "Percent Finished" = "progress_pct")
      },
      options = list(
        pageLength = 8,
        columnDefs = list(
          list(className = 'dt-left', targets = '_all')
        )
      )
    )
  })
}

