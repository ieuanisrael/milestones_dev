

build_cumulative_query <- function(
    definition,
    player_id = NULL,
    filters) {
  
  agg_sql <- build_aggregation(
    definition$aggregation,
    "rr.stat_column"
  )
  
  agg_select_sql <- build_select_aggregation(
    definition$aggregation,
    "rr.stat_column"
  )
  
  progress <- build_step_progress(
    value_expr = agg_sql,
    first_value = definition$first_value,
    multiple = definition$multiple
  )
  
  filter_sql <- build_filter_clause(filters, player_id, definition) 
  
  glue::glue("
WITH RankedRows AS (
    SELECT 
        p.player_id,
        p.name,
        pi.{definition$value_column} as stat_column,
        m.match_date,
        ROW_NUMBER() OVER (PARTITION BY mp.player_id ORDER BY match_date DESC) as rn
    FROM [GA20260618].[MatchPlayers] mp

    JOIN [GA20260618].[PlayerInnings] pi
    ON mp.match_id = pi.match_id AND mp.player_id = pi.player_id and mp.match_count = 1
    JOIN [GA20260618].Players p
    ON p.player_id = pi.player_id
    JOIN [GA20260618].Matches m
    ON m.match_id = pi.match_id
    JOIN [GA20260618].Teams team 
    ON pi.team_id = team.team_id
    JOIN [GA20260618].Venues venue 
    ON m.venue_id = venue.venue_id
    JOIN [GA20260618].Series series 
    ON m.series_id = series.series_id
    JOIN [GA20260618].Seasons season
    ON m.season_id = season.season_id

    {filter_sql}
), lastRow AS (
    SELECT 
        player_id,
        name,
        stat_column,
        match_date
    FROM
        RankedRows
    WHERE
        rn = 1
)


SELECT

    '{definition$display_name}' AS display_name,

    rr.player_id,
    rr.name,

    max(lr.match_date) 
      AS last_match_date,
    
    {agg_select_sql}
    
    {agg_sql}
      AS current_value,

    {progress$current_tier}
      AS current_tier,

    {progress$next_threshold}
      AS next_threshold,

    'cumulative'
      AS milestone_type

FROM RankedRows as rr
JOIN lastRow as lr on lr.player_id = rr.player_id

GROUP BY
    rr.player_id,
    rr.name

ORDER BY
    current_value DESC
             ")
}


build_tiered_innings_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
){
  
  tier_case <- build_tier_case(
    value_column = definition$value_column,
    first_value = definition$first_value,
    multiple = definition$multiple,
    max_value = definition$max_value
  )
  
  filter_sql <- build_filter_clause(filters, player_id, definition) 
  
  glue::glue("

SELECT

  CONCAT(
    '{definition$display_name} - ',
    current_tier
  ) AS display_name,

  player_id,
  name,
  max(match_date) AS last_match_date,
  1 as last_value,
  1 as avg_value,
  COUNT(*) AS current_value,

  FLOOR(COUNT(*) / 50.0) * 50
      AS current_tier,

  FLOOR(COUNT(*) / 50.0) * 50 + 50
    AS next_threshold,

  'tiered_innings'
    AS milestone_type

FROM (

    SELECT

      p.player_id,
      p.name,
      m.match_date,
      {tier_case} as current_tier

    FROM {view}.[MatchPlayers] mp

    JOIN {view}.[PlayerInnings] pi
      ON mp.match_id = pi.match_id AND mp.player_id = pi.player_id and mp.match_count = 1
    JOIN {view}.Players p
      ON pi.player_id = p.player_id
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

    {filter_sql}

) x

GROUP BY

  player_id,
  name,
  current_tier
  
ORDER BY
  current_value DESC

")
  
}

build_tiered_match_query <- function(
    definition,
    player_id = NULL,
    filters = NULL
){
  
  tier_case <- build_tier_case(
    value_column = "match_value",
    first_value = definition$first_value,
    multiple = definition$multiple,
    max_value = definition$max_value
  )
  
  filter_sql <- build_filter_clause(filters, player_id, definition) 
  
  glue::glue("

WITH match_summary AS (

    SELECT

      p.player_id,
      p.name,

      pi.match_id,
      max(m.match_date) as match_date,
      SUM(pi.{definition$value_column})
        AS match_value

    FROM {view}.[MatchPlayers] mp

    JOIN {view}.[PlayerInnings] pi
      ON mp.match_id = pi.match_id AND mp.player_id = pi.player_id and mp.match_count = 1
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

    {filter_sql}
    
    GROUP BY

      p.player_id,
      p.name,
      pi.match_id

)

SELECT

    CONCAT(
      '{definition$display_name} - ',
      current_tier
    ) AS display_name,

    player_id,
    name,
    max(match_date) as last_match_date,
    1 as last_value,
    1 as avg_value,
    COUNT(*) AS current_value,

    FLOOR(COUNT(*) / 50.0) * 50
      AS current_tier,

    FLOOR(COUNT(*) / 50.0) * 50 + 50
      AS next_threshold,

    'tiered_match'
      AS milestone_type

FROM (

    SELECT

      player_id,
      name,
      match_date,
      {tier_case} as current_tier

    FROM match_summary

) x

WHERE current_tier IS NOT NULL

GROUP BY

  player_id,
  name,
  current_tier

ORDER BY
  current_value DESC

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