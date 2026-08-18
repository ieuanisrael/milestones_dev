# Player progress helpers.
# These functions assemble milestone progress summaries for a selected player.

get_player_progress <- function(player_id, filters = NULL) {
  get_player_milestone_summary(player_id = player_id, filters = filters)
}

get_player_milestone_summary <- function(
  player_id,
  filters = NULL,
  milestone_definitions = milestones,
  choice_definitions = select_choices
) {
  if (is.null(player_id) || !nzchar(player_id)) {
    return(tibble::tibble())
  }

  results <- purrr::map_df(seq_len(nrow(select_choices)), function(i) {
    definition <- milestone_definitions
    choices <- choice_definitions[i, ]

    definition <- definition[
      choices$definition_id == definition$definition_id,
    ]

    query_data <- execute_milestone_query(
      definition = definition,
      player_id = player_id,
      filters = filters
    )

    res <- tryCatch(
      query_data,
      error = function(e) NULL
    ) 

    if (is.null(res) || nrow(res) == 0 || is.null(res$current_value) || all(is.na(res$current_value))) {
      return(NULL)
    }
    
    res <- res %>%
      filter(display_name == select_choices[i, ]$display_name)

    if (nrow(res) == 0) {
      return(NULL)
    }

    current_value <- as.numeric(res$current_value)
    next_target <- as.numeric(res$next_threshold)
    avg_value <- if ("avg_value" %in% names(res)) {
      suppressWarnings(as.numeric(res$avg_value))
    } else {
      0
    }

    state <- build_milestone_state(
      current_value,
      next_target
    )
    season <- assess_season_reach(
      current_value = current_value,
      next_target = next_target,
      avg_value = avg_value
    )

    tibble::tibble(
      display_name = choices$display_name_ui,
      current_value = current_value,
      threshold_value = res$current_tier,
      remaining = state$remaining,
      progress_pct = state$progress_pct,
      next_target = next_target,
      avg_value = season$avg_value,
      remaining_matches = season$remaining_matches,
      remaining_frac = season$remaining_frac,
      projected = season$projected,
      season_achievable = season$achievable,
      in_season = season$in_season,
      achieved = season$status
    )
  })

  results
}

