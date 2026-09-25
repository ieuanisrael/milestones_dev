create_threshold_sql <- function(thresholds) {
  thresholds <- sort(unique(thresholds))
  
  paste(
    c(
      "milestones AS (",
      paste0(
        c(
          paste0("    SELECT ", thresholds[1], " AS milestone"),
          paste0("    UNION ALL SELECT ", thresholds[-1])
        ),
        collapse = "\n"
      ),
      ")"
    ),
    collapse = "\n"
  )
}

build_cumulative_query <- function(
    definition,
    player_id = NULL,
    filters) {
  
  agg_sql <- build_aggregation(
    definition$aggregation,
    glue("pi.{definition$value_column}")
  )
  
  filter_sql <- build_filter_clause(filters, player_id, definition) 
  
  thresholds <- create_threshold_sql(thresholds_for(definition$display_name, series))
  
  glue::glue("
WITH innings_runs AS (
    SELECT
        p.player_id,
        p.name,
        m.match_id,
        m.match_date,
        {agg_sql} AS current_value
    FROM [GA20260618].[MatchPlayers] mp
    JOIN [GA20260618].[PlayerInnings] pi
        ON mp.match_id = pi.match_id
        AND mp.player_id = pi.player_id
        AND mp.match_count = 1
    JOIN [GA20260618].[Players] p
        ON p.player_id = pi.player_id
    JOIN [GA20260618].[Matches] m
        ON m.match_id = pi.match_id
    JOIN [GA20260618].[Series] series
        ON m.series_id = series.series_id
    {filter_sql}
    GROUP BY
        p.player_id,
        p.name,
        m.match_id,
        m.match_date
),

career_progression AS (
    SELECT
        *,
        SUM(current_value) OVER (
            PARTITION BY player_id
            ORDER BY match_date, match_id
            ROWS UNBOUNDED PRECEDING
        ) AS milestone_value
    FROM innings_runs
),

{thresholds},

milestone_dates AS (
    SELECT
        cp.player_id,
        cp.name,
        m.milestone,
        cp.match_date,
        cp.milestone_value,
        ROW_NUMBER() OVER (
            PARTITION BY cp.player_id, m.milestone
            ORDER BY cp.match_date, cp.match_id
        ) AS rn
    FROM career_progression cp
    JOIN milestones m
        ON cp.milestone_value >= m.milestone
)

SELECT
    '{definition$display_name}' as display_name,
    player_id,
    name,
    milestone,
    match_date AS milestone_date,
    milestone_value
FROM milestone_dates
WHERE rn = 1
ORDER BY
    name,
    milestone;
             ")
}


build_tiered_innings_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
){
  
  tier_case <- build_tier_case(
    value_column = definition$value_column,
    event_values = related_event_values(definition)
  )
  
  filter_sql <- build_filter_clause(filters, player_id, definition) 
  
  thresholds <- create_threshold_sql(thresholds_for(definition$display_name, series))
  
  glue::glue("
WITH milestone_innings AS (
    SELECT
        p.player_id,
        p.name,
        m.match_id,
        m.match_date,
        {tier_case},
        1 AS current_value
    FROM [GA20260618].[MatchPlayers] mp
    JOIN [GA20260618].[PlayerInnings] pi
        ON mp.match_id = pi.match_id
        AND mp.player_id = pi.player_id
        AND mp.match_count = 1
    JOIN [GA20260618].[Players] p
        ON p.player_id = pi.player_id
    JOIN [GA20260618].[Matches] m
        ON m.match_id = pi.match_id
    JOIN [GA20260618].[Series] series
        ON m.series_id = series.series_id
    {filter_sql}
),

career_progression AS (
    SELECT
        *,
        SUM(current_value) OVER (
            PARTITION BY player_id, current_tier
            ORDER BY match_date, match_id
            ROWS UNBOUNDED PRECEDING
        ) AS milestone_value
    FROM milestone_innings
),

{thresholds},

milestone_dates AS (
    SELECT
        cp.player_id,
        cp.name,
        cp.current_tier,
        m.milestone,
        cp.match_date,
        cp.milestone_value,
        ROW_NUMBER() OVER (
            PARTITION BY cp.player_id,
                         cp.current_tier,
                         m.milestone
            ORDER BY cp.match_date, cp.match_id
        ) AS rn
    FROM career_progression cp
    JOIN milestones m
        ON cp.milestone_value >= m.milestone
)

SELECT
    '{definition$display_name}' as display_name,
    player_id,
    name,
    milestone,
    match_date AS milestone_date,
    milestone_value
FROM milestone_dates
WHERE rn = 1
ORDER BY
    name,
    current_tier,
    milestone;
")
  
}

build_tiered_match_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
){
  
  tier_case <- build_tier_case(
    value_column = glue("sum({definition$value_column})"),
    event_values = related_event_values(definition)
  )
  
  filter_sql <- build_filter_clause(filters, player_id, definition) 
  
  thresholds <- create_threshold_sql(thresholds_for(definition$display_name, series))
  
  glue::glue("
WITH match_hauls AS (
    SELECT
        p.player_id,
        p.name,
        m.match_id,
        m.match_date,
        {tier_case},
        1 AS current_value
    FROM [GA20260618].[MatchPlayers] mp
    JOIN [GA20260618].[PlayerInnings] pi
        ON mp.match_id = pi.match_id
        AND mp.player_id = pi.player_id
        AND mp.match_count = 1
    JOIN [GA20260618].[Players] p
        ON p.player_id = pi.player_id
    JOIN [GA20260618].[Matches] m
        ON m.match_id = pi.match_id
    JOIN [GA20260618].[Series] series
        ON m.series_id = series.series_id
    {filter_sql}
    GROUP BY
        p.player_id,
        p.name,
        m.match_id,
        m.match_date
),

career_progression AS (
    SELECT
        *,
        SUM(current_value) OVER (
            PARTITION BY player_id
            ORDER BY match_date, match_id
            ROWS UNBOUNDED PRECEDING
        ) AS milestone_value
    FROM match_hauls
),

{thresholds},

milestone_dates AS (
    SELECT
        cp.player_id,
        cp.name,
        m.milestone,
        cp.match_date,
        cp.milestone_value,
        ROW_NUMBER() OVER (
            PARTITION BY cp.player_id, m.milestone
            ORDER BY cp.match_date, cp.match_id
        ) AS rn
    FROM career_progression cp
    JOIN milestones m
        ON cp.milestone_value >= m.milestone
)

SELECT
    '{definition$display_name}' as display_name,
    player_id,
    name,
    milestone,
    match_date AS milestone_date,
    milestone_value
FROM milestone_dates
WHERE rn = 1
ORDER BY
    name,
    milestone;
")

}

query_builders <- list(
  cumulative = build_cumulative_query,
  tiered_innings = build_tiered_innings_query,
  tiered_match = build_tiered_match_query
  # fixed_innings = build_fixed_query,
  # fixed_match = build_fixed_match_query
  # special = build_special_query
)

build_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
) {
  
  builder <- query_builders[[definition$query_strategy]]
  
  if (is.null(builder)) {
    stop(
      paste(
        "Unknown query strategy:",
        definition$query_strategy
      )
    )
  }
  
  builder(
    definition = definition,
    player_id = player_id,
    filters = filters
  )
}