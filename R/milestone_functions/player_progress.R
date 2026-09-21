# Player progress helpers.
# These functions assemble milestone progress summaries for a selected player.

get_player_progress <- function(player_id, filters = NULL) {
  get_player_milestone_summary(player_id = player_id, filters = filters)
}

get_player_milestone_summary <- function(
  player_id,
  filters = NULL,
  milestone_definitions = NULL
) {
  if (is.null(player_id) || !nzchar(as.character(player_id))) {
    return(tibble::tibble())
  }

  series <- if (is.null(filters)) NULL else filters$series
  if (is.null(milestone_definitions)) {
    milestone_definitions <- milestones_for_series(series)
  }

  if (nrow(milestone_definitions) == 0) {
    return(tibble::tibble())
  }

  purrr::map_df(seq_len(nrow(milestone_definitions)), function(i) {
    definition <- milestone_definitions[i, ]

    res <- tryCatch(
      execute_milestone_query(
        definition = definition,
        player_id = player_id,
        filters = filters
      ),
      error = function(e) NULL
    )

    if (is.null(res) || nrow(res) == 0 || is.null(res$current_value) || all(is.na(res$current_value))) {
      return(NULL)
    }

    res <- res %>%
      dplyr::filter(.data$display_name == definition$display_name)

    if (nrow(res) == 0) {
      return(NULL)
    }

    current_value <- as.numeric(res$current_value)[1]
    avg_value <- if ("avg_value" %in% names(res)) {
      suppressWarnings(as.numeric(res$avg_value))[1]
    } else {
      0
    }

    progress <- progress_from_thresholds(
      current_value,
      thresholds_for(definition$display_name, series)
    )
    state <- build_milestone_state(
      current_value,
      progress$next_threshold
    )
    season <- assess_season_reach(
      series = series,
      current_value = current_value,
      next_target = progress$next_threshold,
      avg_value = avg_value
    )

    tibble::tibble(
      display_name = definition$display_name_ui,
      current_value = current_value,
      threshold_value = progress$current_tier,
      remaining = state$remaining,
      progress_pct = state$progress_pct,
      next_target = progress$next_threshold,
      avg_value = season$avg_value,
      remaining_matches = season$remaining_matches,
      remaining_frac = season$remaining_frac,
      projected = season$projected,
      season_achievable = season$achievable,
      in_season = season$in_season,
      achieved = season$status
    )
  })
}
