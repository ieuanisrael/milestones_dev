library(glue)
library(lubridate)
library(dplyr)

if (!file.exists("app.R")) {
  stop("Run this script from the milestones_dev project root.")
}

team <- "NSW Blues M"
series <- 4
season <- "2025-26"

source("./R/config/milestone_def.R")
source("./R/config/select_choices.R")
if (file.exists("./R/config/constants.R")) {
  source("./R/config/constants.R")
} else {
  source("./R/config/local_constants.R")
}

filters <- list(
  series = series,
  venue = all_id,
  team = NULL
)

source("./R/database/connection.R")
source("./R/database/filters.R")
source("./R/database/lookups.R")
source("./R/database/local_data.R")

source("./email/email_milestone_helpers.R")
source("./email/email_query_builder.R")

results <- purrr::map_df(seq_len(nrow(milestones)), function(i) {
  definition <- milestones[i, ]

  res <- tryCatch(
    execute_milestone_query(
      definition = definition,
      filters = filters
    ),
    error = function(e) NULL
  )

  if (is.null(res) ||
      nrow(res) == 0 ||
      is.null(res$current_value) ||
      all(is.na(res$current_value))) {
    return(NULL)
  }

  current_value <- as.numeric(res$current_value)

  state <- build_milestone_state(
    current_value,
    res$next_threshold
  )

  display_name_ui <- select_choices$display_name_ui[match(res$display_name, select_choices$display_name)]
  batting <- grepl("Batting Tiers", res$display_name)
  if (any(batting)) {
    tier <- suppressWarnings(as.integer(as.numeric(sub(".*-\\s*", "", res$display_name[batting]))))
    display_name_ui[batting] <- ifelse(tier == 100, "Centuries", paste0(tier, "s"))
  }
  display_name_ui[is.na(display_name_ui)] <- res$display_name[is.na(display_name_ui)]

  tibble::tibble(
    display_name = res$display_name,
    display_name_ui = display_name_ui,
    current_value = current_value,
    threshold_value = res$current_tier,
    remaining = state$remaining,
    progress_pct = state$progress_pct,
    next_target = res$next_threshold,
    last_match_date = res$last_match_date,
    last_value = res$last_value,
    avg_value = res$avg_value,
    player = res$name,
    milestone_achieved = current_value - last_value < threshold_value,
    within_threshold = 10 * avg_value + current_value > next_target
  )
})

if (is_local_data()) {
  team_list <- local_get_players_for(team, series, season)
} else {
  query <- get_players_query(team, series, season)
  team_list <- tryCatch(
    QueryDBFunction(con = con, query = query),
    error = function(e) NULL
  )
}

if (is_local_data()) {
  source("./email/email_html.R")
  message("Wrote player_milestone_update.html")
}

