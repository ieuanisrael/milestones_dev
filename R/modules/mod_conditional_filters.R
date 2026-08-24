# Shared filter UI and server logic for the Shiny modules.
# This component exposes series, venue, and team selectors that update each other.

filter_legend_item <- function(title, text) {
  div(
    tags$div(
      style = "font-weight:700;color:#002855;margin-bottom:4px;",
      title
    ),
    tags$div(
      style = "color:#334155;font-size:13px;line-height:1.35;",
      text
    )
  )
}

conditionalFiltersUI <- function(id) {
  ns <- NS(id)

  card(
    card_header("Conditional Filters"),

    card_body(
      fluidRow(
        column(
          3,
          pickerInput(
            ns("series"),
            "Series",
            selected = series_choices[1],
            choices = series_choices,
            options = list(container = "body", `live-search` = TRUE)
          )
        ),
        column(
          3,
          pickerInput(
            ns("venue"),
            "Venue",
            selected = "All",
            choices = "All",
            options = list(container = "body", `live-search` = TRUE)
          )
        ),
        column(
          3,
          pickerInput(
            ns("team"),
            "Team",
            selected = "All",
            choices = "All",
            options = list(container = "body", `live-search` = TRUE)
          )
        ),
        column(
          3,
          div(
            style = "display:flex;align-items:center;height:100%;min-height:72px;",
            actionButton(ns("apply"), "Apply Filters", class = "btn-primary")
          )
        )
      ),

      div(
        style = paste(
          "background:#f8fafc;",
          "border:1px solid #d9e2ec;",
          "border-radius:8px;",
          "padding:12px 16px;",
          "margin:0 0 12px 0;"
        ),
        fluidRow(
          column(12, uiOutput(ns("filter_legend")))
        )
      )
    )
  )
}

conditionalFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    conn <- get_db_connection()

    output$filter_legend <- renderUI({
      series <- input$series
      venue <- input$venue
      team <- input$team

      if (is.null(series) || !nzchar(series)) {
        series <- series_choices[1]
      }
      if (is.null(venue) || !nzchar(venue)) {
        venue <- "All"
      }
      if (is.null(team) || !nzchar(team)) {
        team <- "All"
      }

      min_year <- get_min_season_year(
        filters = list(series = series, venue = venue, team = team),
        conn = conn
      )

      date_text <- tagList(
        tags$div(
          style = "font-size:15px;margin-bottom:4px;",
          series_date_range_label(series, min_year)
        ),
        tags$div(
          if (identical(series, "Aus Domestic T20 M")) {
            "Men's domestic T20 is limited to the Big Bash era."
          } else if (identical(series, "Aus Domestic T20 F")) {
            "Women's domestic T20 is limited to the WBBL era."
          } else {
            "All seasons from this year onwards are included."
          }
        )
      )

      venue_text <- if (identical(venue, "All")) {
        "All grounds. Choose a venue to count only matches at that ground."
      } else {
        paste0("Only matches at ", venue, ".")
      }

      team_text <- if (identical(team, "All")) {
        "All teams. Choose a team to count only innings for that club."
      } else {
        paste0("Only innings for ", team, ".")
      }

      fluidRow(
        
        column(
          3,
          filter_legend_item(
            "Series",
            paste0(
              series,
              " — career totals are counted only in this competition."
            )
          )
        ),
        column(
          3,
          filter_legend_item("Venue", venue_text)
        ),
        column(
          3,
          filter_legend_item("Team", team_text)
        ),
        column(
          3,
          filter_legend_item("Date range", date_text)
        )
        
      )
    })

    filter_config <- list(
      series = list(getter = get_series),
      venue = list(getter = get_venues),
      team = list(getter = get_teams)
    )

    make_choices <- function(values, all) {
      values <- values |>
        unique() |>
        na.omit()

      values <- values[nzchar(values)]

      if (all) {
        values <- c("All", values)
      }

      values
    }

    get_current_filters <- function(input) {
      list(
        series = input$series,
        venue = input$venue,
        team = input$team
      )
    }

    update_single_filter <- function(target, filters, session, conn, current_value) {
      config <- filter_config[[target]]
      values <- config$getter(filters = filters, conn = conn)
      choices <- make_choices(values, target != "series")

      selected <- if (current_value %in% choices) current_value else "All"

      updatePickerInput(
        session,
        target,
        choices = choices,
        selected = selected
      )
    }

    refresh_filters <- function(changed_filter) {
      current <- get_current_filters(input)

      for (filter_name in names(filter_config)) {
        if (filter_name == changed_filter) {
          next
        }

        filter_args <- current
        filter_args[[changed_filter]] <- current[[changed_filter]]

        update_single_filter(
          target = filter_name,
          filters = filter_args[names(filter_args) != filter_name],
          current_value = current[[filter_name]],
          session = session,
          conn = conn
        )
      }
    }

    observeEvent(input$series, {
      refresh_filters("series")
    })

    observeEvent(input$venue, {
      refresh_filters("venue")
    })

    observeEvent(input$team, {
      refresh_filters("team")
    })

    init_filters <- function() {
      update_single_filter(
        "series",
        filters = NULL,
        session = session,
        conn = conn,
        current_value = "All"
      )

      update_single_filter(
        "venue",
        filters = NULL,
        session = session,
        conn = conn,
        current_value = "All"
      )

      update_single_filter(
        "team",
        filters = NULL,
        session = session,
        conn = conn,
        current_value = "All"
      )
    }

    initialised <- init_filters()

    filters <- eventReactive(c(initialised, input$apply), {
      list(
        series = input$series,
        venue = input$venue,
        team = input$team
      )
    })

    return(filters)
  })
}
