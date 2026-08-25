library(dplyr)
library(glue)
library(htmltools)
library(stringr)
library(lubridate)

# -------------------------------
# Data preparation
# -------------------------------

milestone_achieved <- results %>%
  filter(milestone_achieved, player %in% team_list$name)

milestone_threshold <- results %>%
  filter(within_threshold, player %in% team_list$name)

top_10_thresholds <- results %>%
  group_by(display_name) %>%
  mutate(
    tenth = nth(current_value, 10),
    within_top_10 = current_value + 10 * avg_value > tenth & current_value > nth(current_value, 20),
    rank = dense_rank(desc(current_value))
  ) %>%
  filter(within_top_10, player %in% team_list$name)

single_performance_milestones <- results %>%
  filter(
    str_detect(display_name, "50s|Centuries|150s|200s|250s|Haul|Dismissals|Carried|Tiers|Innings|Match"),
    last_match_date > as.Date("2026-01-01"),
    player %in% team_list$name
  )

# -------------------------------
# Helper function
# -------------------------------

last_match_performance_label <- function(x) {
  display_name <- as.character(x$display_name)

  if (identical(display_name, "Centuries")) {
    return("a century")
  }
  if (display_name %in% c("50s", "150s", "200s", "250s")) {
    return(paste("a", sub("s$", "", display_name)))
  }

  as.character(x$display_name_ui)
}

create_cards <- function(data, text_fn, colour) {
  
  if (nrow(data) == 0) {
    return(
      div(
        class = "card",
        style = paste0("border-left:5px solid ", colour),
        p("No updates this week.")
      )
    )
  }
  
  tagList(
    lapply(
      seq_len(nrow(data)),
      function(i) {
        div(
          class = "card",
          style = paste0("border-left:5px solid ", colour),
          HTML(text_fn(data[i, ]))
        )
      }
    )
  )
}

# -------------------------------
# HTML email
# -------------------------------

email_html <- tagList(
  
  tags$head(
    tags$style(HTML("

      body {
        font-family: 'Segoe UI', Arial, sans-serif;
        background-color: #f3f6fa;
        margin: 0;
        padding: 0;
        color: #1f2937;
      }

      .container {
        max-width: 1000px;
        margin: auto;
        background: white;
      }

      .hero {
        background: #002B5C;
        color: white;
        text-align: center;
        padding: 20px;
        font-size: 12px;
      }

      .hero-table {
        width: 100%;
      }

      .logo {
        height: 90px;
      }

      .hero-title {
        font-size: 34px;
        font-weight: 700;
        margin-bottom: 8px;
      }

      .hero-subtitle {
        font-size: 18px;
        opacity: 0.9;
      }

      .section {
        padding: 25px 35px;
      }

      .section-title {
        font-size: 24px;
        color: #002B5C;
        font-weight: 700;
        margin-bottom: 15px;
        border-bottom: 3px solid #4FB3FF;
        padding-bottom: 8px;
      }

      .card {
        background: #fafafa;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      }

      .achieved {
        border-left: 6px solid #2E7D32;
      }

      .close {
        border-left: 6px solid #F57C00;
      }

      .top10 {
        border-left: 6px solid #1565C0;
      }

      .performance {
        border-left: 6px solid #6A1B9A;
      }

      .player {
        font-size: 18px;
        font-weight: bold;
        color: #002B5C;
      }

      .stat {
        color: #005EB8;
        font-weight: 600;
      }

      .footer {
        background: #002B5C;
        color: white;
        text-align: center;
        padding: 20px;
        font-size: 12px;
      }

      .summary {
        padding: 25px 35px;
        background: #eef5ff;
      }

      .summary-grid {
        width: 100%;
      }

      .summary-box {
        background: white;
        border-radius: 10px;
        padding: 15px;
        text-align: center;
      }

      .summary-number {
        font-size: 32px;
        font-weight: bold;
        color: #005EB8;
      }

      .summary-label {
        color: #666;
      }

    "))
  ),
  
  div(
    class = "container",
    
    # HERO HEADER -----------------------------
    
    div(
      class = "hero", 
      tags$div(
        class = "hero-title",
        glue("NSW Blues Men's {series} Milestone Report")
      ),
      
      tags$div(
        class = "hero-subtitle",
        paste("Performance Analysis Update |", Sys.Date())
      )
    ),
    
    # SUMMARY SECTION -------------------------
    
    div(
      class = "summary",
      
      tags$table(
        class = "summary-grid",
        width = "100%",
        
        tags$tr(
          
          tags$td(
            width = "25%",
            div(
              class = "summary-box",
              div(
                class = "summary-number",
                nrow(milestone_achieved)
              ),
              div(
                class = "summary-label",
                "Milestones Achieved"
              )
            )
          ),
          
          tags$td(
            width = "25%",
            div(
              class = "summary-box",
              div(
                class = "summary-number",
                nrow(single_performance_milestones)
              ),
              div(
                class = "summary-label",
                "Last Match"
              )
            )
          ),
          
          tags$td(
            width = "25%",
            div(
              class = "summary-box",
              div(
                class = "summary-number",
                nrow(milestone_threshold)
              ),
              div(
                class = "summary-label",
                "Approaching"
              )
            )
          ),
          
          tags$td(
            width = "25%",
            div(
              class = "summary-box",
              div(
                class = "summary-number",
                nrow(top_10_thresholds)
              ),
              div(
                class = "summary-label",
                "Top 10 Watch"
              )
            )
          )
        )
      )
    ),
    
    # ACHIEVED SECTION ------------------------
    
    div(
      class = "section",
      
      div(
        class = "section-title",
        "Milestones Achieved"
      ),
      
      create_cards(
        milestone_achieved,
        function(x) {
          glue("
          <strong>{x$player}</strong> has passed the milestone
          <strong>{x$display_name_ui}</strong> ({x$threshold_value}).
          <br>
          Current Total: <strong>{x$current_value}</strong><br>
          Achievement Date: {as_date(x$last_match_date)}
        ")
        },
        "#28a745"
      ),
    ),
    
    # LAST MATCH SECTION ----------------------
    
    div(
      class = "section",
      
      div(
        class = "section-title",
        "Last Match Performances"
      ),
      
      create_cards(
        single_performance_milestones,
        function(x) {
          glue("
          <strong>{x$player}</strong> achieved
          <strong>{last_match_performance_label(x)}</strong>
          on {as_date(x$last_match_date)}.
          <br>
          Career Total: <strong>{x$current_value}</strong>
        ")
        },
        "#9c27b0"
      ),
    ),
    
    # APPROACHING SECTION ---------------------
    
    div(
      class = "section",
      
      div(
        class = "section-title",
        "Milestones Approaching"
      ),
      
      create_cards(
        milestone_threshold,
        function(x) {
          glue("
          <strong>{x$player}</strong> is projected to pass
          <strong>{x$display_name_ui}</strong> ({x$next_target})
          within the next 10 matches.
          <br>
          Current Total: <strong>{x$current_value}</strong>
        ")
        },
        "#ff9800"
      ),
    ),
    
    # TOP 10 SECTION --------------------------
    
    div(
      class = "section",
      
      div(
        class = "section-title",
        "Top 10 Watch"
      ),
      
      create_cards(
        top_10_thresholds,
        function(x) {
          glue("
          <strong>{x$player}</strong>
          is currently ranked <strong>#{x$rank}</strong>
          for <strong>{x$display_name_ui}</strong>.
          <br>
          Current Total: <strong>{x$current_value}</strong>
        ")
        },
        "#0078D4"
      ),
      
    ),
    
    div(
      class = "footer",
      
      strong("NSW Blues Men's Program"),
      br(),
      "Cricket NSW | Performance Analysis",
      br(),
      "Automated Milestone Monitoring Report"
    )
  )
)

# -------------------------------
# Save HTML file
# -------------------------------

save_html(
  email_html,
  file = "player_milestone_update.html"
)

