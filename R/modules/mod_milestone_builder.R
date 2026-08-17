milestoneBuilderUI <- function(id){

  ns <- NS(id)

  card(
    card_header("Milestone Builder"),

    textInput(ns("name"), "Milestone Name", placeholder = "e.g. 3000 runs"),

    selectInput(
      ns("stat_type"),
      "Stat Type",
      choices = c("Runs", "Wickets", "Strike Rate", "Win Contribution")
    ),

    selectInput(
      ns("aggregation"),
      "Aggregation Type",
      choices = c("Career", "Season", "Rolling", "Single Match")
    ),

    numericInput(ns("threshold"), "Threshold", value = 100, min = 1),

    actionButton(ns("add_condition"), "Add Condition", class = "btn-sm"),
    uiOutput(ns("conditions_ui")),
    actionButton(ns("save"), "Create Milestone", class = "btn-primary")
  )
}

milestoneBuilderServer <- function(id){

  moduleServer(id, function(input, output, session){

    conditions <- reactiveVal(list())

    observeEvent(input$add_condition, {
      current <- conditions()
      current[[length(current) + 1]] <- list(field = "", operator = "", value = "")
      conditions(current)
    })

    output$conditions_ui <- renderUI({
      req(length(conditions()) > 0)
      lapply(seq_along(conditions()), function(i) {
        tagList(
          fluidRow(
            column(4, textInput(session$ns(paste0("field_", i)), "Field", value = conditions()[[i]]$field)),
            column(4, textInput(session$ns(paste0("operator_", i)), "Operator", value = conditions()[[i]]$operator)),
            column(4, textInput(session$ns(paste0("value_", i)), "Value", value = conditions()[[i]]$value))
          )
        )
      })
    })

    observeEvent(input$save, {
      save_milestone_definition(
        name = input$name,
        stat_type = input$stat_type,
        aggregation = input$aggregation,
        threshold = input$threshold,
        conditions = conditions()
      )
      showNotification("Milestone definition saved.", type = "message")
    })

  })
}