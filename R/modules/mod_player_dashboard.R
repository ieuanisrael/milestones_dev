# Player dashboard UI and server logic.
# This module presents milestone snapshots and progress tracking for the selected player.

legend_swatch <- function(colour) {
  tags$span(
    style = paste0(
      "display:inline-block;width:16px;height:16px;border-radius:3px;",
      "margin-right:8px;vertical-align:middle;background:", colour, ";"
    )
  )
}

milestoneSnapshotLegend <- function() {
  div(
    class = "milestone-legend",
    style = paste(
      "background:#f8fafc;",
      "border:1px solid #d9e2ec;",
      "border-radius:8px;",
      "padding:12px 16px;",
      "margin:0 0 16px 0;"
    ),
    tags$p(
      style = "margin:0 0 10px 0;",
      "Each card is that player's progress toward the ",
      tags$strong("next milestone"),
      " in the selected series. Cards only appear for milestones the player has started."
    ),
    fluidRow(
      column(
        6,
        legend_swatch("#198754"),
        tags$span("Green — more than halfway to the next target")
      ),
      column(
        6,
        legend_swatch("#ffc107"),
        tags$span("Yellow — halfway or less to the next target")
      )
    ),
    tags$ul(
      style = "margin:12px 0 0 18px;",
      tags$li(tags$strong("Current"), " — career total so far (runs, wickets, appearances, and so on)"),
      tags$li(tags$strong("Next Target"), " — the next threshold, such as 1,000 runs or 50 wickets"),
      tags$li(tags$strong("Status"), " — still in progress until that target is reached")
    )
  )
}

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

      navset_tab(
        id = ns("player_views"),
        nav_panel(
          "Milestone Snapshot",
          milestoneSnapshotLegend(),
          uiOutput(ns("milestone_cards")) %>%
            withSpinner(proxy.height = "400px", color.background = "gray")
        ),
        nav_panel(
          "Progress Tracker",
          DT::DTOutput(ns("progress_table")) %>% withSpinner()
        )
      )
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
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-left', targets = '_all')
        )
      )
    )
  })
}

