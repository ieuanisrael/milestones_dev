# Leaderboard helper for milestone explorer views.
# This function runs the milestone query and returns a compact leaderboard table.

get_milestone_leaderboard <- function(new_display_name, definition, filters = NULL, con = NULL) {

  res <- tryCatch(
    execute_milestone_query(
      definition = definition,
      filters = filters,
      con = con
    ),
    error = function(e) NULL
  )

  if (is.null(res) || nrow(res) == 0) {
    return(tibble(player = NA, value = NA))
  }

  res %>%
    filter(display_name == new_display_name) %>%
    transmute(
      player_id = player_id,
      Player = name,
      Value = current_value
    ) %>%
    arrange(desc(Value))
}

