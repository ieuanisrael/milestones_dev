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

fmt_stat <- function(value, digits = 0) {
  value <- suppressWarnings(as.numeric(value))[1]
  if (length(value) == 0 || is.na(value)) {
    return("0")
  }
  format(round(value, digits), nsmall = digits, big.mark = ",", trim = TRUE)
}

milestoneSnapshotLegend <- function() {
  season <- season_remaining()
  remaining_pct <- round(100 * season$remaining_frac)
  remaining_matches <- round(season$remaining_matches, 1)

  season_copy <- if (isTRUE(season$in_season)) {
    paste0(
      "About ", remaining_pct, "% of the current season remains (~",
      remaining_matches, " of ", SEASON_MATCHES,
      " matches). Seasons run 1 September to 1 April."
    )
  } else {
    paste0(
      "The next season starts 1 September and ends 1 April (~",
      SEASON_MATCHES,
      " matches). Cards treat the upcoming season as fully remaining."
    )
  }

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
      " in the selected series. Colour shows whether their average for that category ",
      "is enough to get there with the matches left this season. ",
      "Cards only appear for milestones the player has started."
    ),
    tags$p(style = "margin:0 0 10px 0;color:#475569;", season_copy),
    fluidRow(
      column(
        6,
        legend_swatch("#198754"),
        tags$span("Green — on track to reach the next target this season")
      ),
      column(
        6,
        legend_swatch("#ffc107"),
        tags$span("Yellow — not expected to reach it at their current average")
      )
    ),
    tags$ul(
      style = "margin:12px 0 0 18px;",
      tags$li(tags$strong("Current"), " — career total so far (runs, wickets, appearances, and so on)"),
      tags$li(tags$strong("Next Target"), " — the next threshold, such as 1,000 runs or 50 wickets"),
      tags$li(tags$strong("Average"), " — that category per match in the selected series"),
      tags$li(tags$strong("Projected"), " — current total plus average times matches remaining"),
      tags$li(tags$strong("Status"), " — on track or unlikely based on that projection")
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
        on_track <- isTRUE(row$season_achievable)
        tone <- if (on_track) "success" else "warning"

        card(
          class = paste0("border-", tone),

          card_header(row$display_name),

          p(paste("Current:", fmt_stat(coalesce(row$current_value, 0)))),
          p(paste("Next Target:", fmt_stat(coalesce(row$next_target, 0)))),
          p(paste("Average:", fmt_stat(coalesce(row$avg_value, 0), 2), "per match")),
          p(paste(
            "Remaining:",
            fmt_stat(coalesce(row$remaining_matches, 0), 1),
            "matches"
          )),
          p(paste("Projected:", fmt_stat(coalesce(row$projected, 0)))),
          p(paste("Status:", row$achieved)),

          div(
            class = "progress",
            div(
              class = paste0("progress-bar bg-", tone),
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
          transmute(
            Milestone = display_name,
            Value = current_value,
            Target = next_target,
            `Avg / match` = round(avg_value, 2),
            `Matches left` = round(remaining_matches, 1),
            Projected = round(projected, 1),
            `On track` = ifelse(season_achievable, "Yes", "No")
          )
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

