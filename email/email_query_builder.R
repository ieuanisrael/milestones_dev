# Email-oriented SQL builders. last_value is 1 for counts and hauls (one more
# event) and the last innings contribution for cumulative sums.

email_stat_sql <- function(definition, alias = "pi") {
  col <- definition$value_column
  if (grepl("+", col, fixed = TRUE)) {
    parts <- trimws(strsplit(col, "+", fixed = TRUE)[[1]])
    paste(paste0(alias, ".", parts), collapse = " + ")
  } else {
    paste0(alias, ".", col)
  }
}

email_from_sql <- function() {
  glue::glue(
    "
FROM {view}.[MatchPlayers] mp
JOIN {view}.[PlayerInnings] pi
  ON mp.match_id = pi.match_id AND mp.player_id = pi.player_id AND mp.match_count = 1
JOIN {view}.Players p
  ON p.player_id = pi.player_id
JOIN {view}.Matches m
  ON m.match_id = pi.match_id
JOIN {view}.Teams team
  ON pi.team_id = team.team_id
JOIN {view}.Venues venue
  ON m.venue_id = venue.venue_id
JOIN {view}.Series series
  ON m.series_id = series.series_id
JOIN [GA20260618].Seasons season
  ON m.season_id = season.season_id
"
  )
}

build_cumulative_query <- function(
    definition,
    player_id = NULL,
    filters) {

  stat_sql <- email_stat_sql(definition)
  agg_sql <- if (identical(definition$aggregation, "count")) {
    "COUNT(DISTINCT rr.stat_column)"
  } else {
    "SUM(rr.stat_column)"
  }

  series <- if (is.null(filters)) NULL else filters$series
  progress <- build_threshold_progress(
    value_expr = agg_sql,
    thresholds = thresholds_for(definition$display_name, series)
  )

  last_value_sql <- if (identical(definition$aggregation, "count")) {
    "1"
  } else {
    "MAX(CASE WHEN rr.rn = 1 THEN rr.stat_column END)"
  }
  avg_value_sql <- if (identical(definition$aggregation, "count")) {
    "1"
  } else {
    "AVG(CAST(rr.stat_column AS FLOAT))"
  }

  filter_sql <- build_filter_clause(filters, player_id, definition)
  from_sql <- email_from_sql()

  glue::glue(
    "
WITH RankedRows AS (
    SELECT
        p.player_id,
        p.name,
        {stat_sql} AS stat_column,
        m.match_date,
        ROW_NUMBER() OVER (PARTITION BY p.player_id ORDER BY m.match_date DESC) AS rn
    {from_sql}
    {filter_sql}
)
SELECT
    '{definition$display_name}' AS display_name,
    rr.player_id,
    rr.name,
    MAX(rr.match_date) AS last_match_date,
    {last_value_sql} AS last_value,
    {avg_value_sql} AS avg_value,
    {agg_sql} AS current_value,
    {progress$current_tier} AS current_tier,
    {progress$next_threshold} AS next_threshold,
    'cumulative' AS milestone_type
FROM RankedRows AS rr
GROUP BY
    rr.player_id,
    rr.name
ORDER BY
    current_value DESC
"
  )
}

build_tiered_innings_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
) {
  stat_sql <- email_stat_sql(definition)
  tier_case <- build_tier_case(
    value_column = "stat_column",
    event_values = related_event_values(definition)
  )

  series <- if (is.null(filters)) NULL else filters$series
  progress <- build_threshold_progress(
    value_expr = "COUNT(*)",
    thresholds = thresholds_for(definition$display_name, series)
  )

  filter_sql <- build_filter_clause(filters, player_id, definition)
  from_sql <- email_from_sql()
  event_value <- event_cutoff(definition)

  glue::glue(
    "
WITH events AS (
    SELECT
      p.player_id,
      p.name,
      m.match_date,
      {stat_sql} AS stat_column,
      {tier_case}
    {from_sql}
    {filter_sql}
)
SELECT
  '{definition$display_name}' AS display_name,
  player_id,
  name,
  MAX(match_date) AS last_match_date,
  1 AS last_value,
  CAST(COUNT(*) AS FLOAT) / NULLIF(COUNT(*), 0) AS avg_value,
  COUNT(*) AS current_value,
  {progress$current_tier} AS current_tier,
  {progress$next_threshold} AS next_threshold,
  'tiered_innings' AS milestone_type
FROM events
WHERE current_tier = {event_value}
GROUP BY
  player_id,
  name
ORDER BY
  current_value DESC
"
  )
}

build_tiered_match_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
) {
  stat_sql <- email_stat_sql(definition)
  tier_case <- build_tier_case(
    value_column = "match_value",
    event_values = related_event_values(definition)
  )

  series <- if (is.null(filters)) NULL else filters$series
  progress <- build_threshold_progress(
    value_expr = "COUNT(*)",
    thresholds = thresholds_for(definition$display_name, series)
  )

  filter_sql <- build_filter_clause(filters, player_id, definition)
  from_sql <- email_from_sql()
  event_value <- event_cutoff(definition)

  glue::glue(
    "
WITH match_summary AS (
    SELECT
      p.player_id,
      p.name,
      pi.match_id,
      MAX(m.match_date) AS match_date,
      SUM({stat_sql}) AS match_value
    {from_sql}
    {filter_sql}
    GROUP BY
      p.player_id,
      p.name,
      pi.match_id
),
events AS (
    SELECT
      player_id,
      name,
      match_date,
      match_value,
      {tier_case}
    FROM match_summary
)
SELECT
    '{definition$display_name}' AS display_name,
    player_id,
    name,
    MAX(match_date) AS last_match_date,
    1 AS last_value,
    CAST(COUNT(*) AS FLOAT) / NULLIF(COUNT(*), 0) AS avg_value,
    COUNT(*) AS current_value,
    {progress$current_tier} AS current_tier,
    {progress$next_threshold} AS next_threshold,
    'tiered_match' AS milestone_type
FROM events
WHERE current_tier = {event_value}
GROUP BY
  player_id,
  name
ORDER BY
  current_value DESC
"
  )
}

query_builders <- list(
  cumulative = build_cumulative_query,
  tiered_innings = build_tiered_innings_query,
  tiered_match = build_tiered_match_query
)

build_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
) {
  builder <- query_builders[[definition$query_strategy]]

  if (is.null(builder)) {
    stop(paste("Unknown query strategy:", definition$query_strategy))
  }

  builder(
    definition = definition,
    player_id = player_id,
    filters = filters
  )
}
