get_players_query <- function(team, series, season) {
  glue::glue("SELECT DISTINCT mp.player_id,
          p.name
          FROM [GA20260618].PlayerInnings mp
          JOIN [GA20260618].Matches m ON mp.match_id = m.match_id
          JOIN [GA20260618].Players p ON mp.player_id = p.player_id AND p.name NOT LIKE '(SUB%'
          JOIN [GA20260618].Teams team ON mp.team_id = team.team_id
          JOIN [GA20260618].Series series ON m.series_id = series.series_id
          JOIN [GA20260618].Seasons season ON m.season_id = season.season_id
          where 
            team.team_name = '{team}' AND 
            series.name = '{series}' AND 
            season.name = '{season}'
          ORDER BY p.name"
  )
}

get_min_season_year <- function(filters = NULL, conn = NULL) {
  if (is_local_data()) {
    return(local_get_min_season_year(filters))
  }

  if (!db_is_available(conn)) {
    return(series_min_year(if (is.null(filters)) NULL else filters$series))
  }

  if (is.null(conn)) {
    conn <- get_db_connection()
  }

  where_clause <- build_filter_clause(filters)

  query <- glue::glue("
    SELECT MIN(CAST(LEFT(season.name, 4) AS INT)) AS min_year
    FROM [GA20260618].MatchPlayers mp
    JOIN [GA20260618].Matches m ON mp.match_id = m.match_id
    JOIN [GA20260618].Teams team ON mp.team_id = team.team_id
    JOIN [GA20260618].Venues venue ON m.venue_id = venue.venue_id
    JOIN [GA20260618].Series series ON m.series_id = series.series_id
    JOIN [GA20260618].Seasons season ON m.season_id = season.season_id
    {where_clause}
  ")

  min_year <- QueryDBFunction(con = conn, query = query)$min_year
  if (length(min_year) == 0 || is.na(min_year[1])) {
    return(series_min_year(if (is.null(filters)) NULL else filters$series))
  }
  as.integer(min_year[1])
}

get_formats <- function(filters = NULL, conn = NULL) {
  if (is_local_data()) {
    return(local_get_formats(filters))
  }

  if (!db_is_available(conn)) {
    return(character())
  }
  
  if (is.null(conn)) {
    conn <- get_db_connection()
  }
  
  where_clause <- build_filter_clause(filters)
  
  query <- glue::glue("
    SELECT DISTINCT m.match_length_id as format_id,
        l.DESCRIPTION as name
    FROM [GA20260618].MatchPlayers mp
    JOIN [GA20260618].Matches m ON mp.match_id = m.match_id
    JOIN [GA20260618].Lookups l ON m.match_length_id = l.id AND l.lookup_type_id = 3
    JOIN [GA20260618].Teams team ON mp.team_id = team.team_id
    JOIN [GA20260618].Venues venue ON m.venue_id = venue.venue_id
    JOIN [GA20260618].Series series ON m.series_id = series.series_id
    JOIN [GA20260618].Seasons season
    ON m.season_id = season.season_id
    {where_clause}
    ORDER BY format_id
  ")
  
  top_row <- data.frame(format_id = "All", name = "All")
  
  rbind(top_row, dbGetQuery(conn, query))
}

get_series <- function(filters = NULL, conn = NULL) {
  # if (!db_is_available(conn)) {
  #   return(character())
  # }
  # 
  # if (is.null(conn)) {
  #   conn <- get_db_connection()
  # }
  # 
  # where_clause <- build_filter_clause(filters)
  # 
  # query <- glue::glue("
  #   SELECT DISTINCT series.name as series_name
  #   FROM [GA20260618].MatchPlayers mp
  #   JOIN [GA20260618].Matches m ON mp.match_id = m.match_id
  #   JOIN [GA20260618].Teams team ON mp.team_id = team.team_id
  #   JOIN [GA20260618].Venues venue ON m.venue_id = venue.venue_id
  #   JOIN [GA20260618].Series series ON m.series_id = series.series_id
  #   {where_clause}
  #   ORDER BY series_name
  # ")
  # 
  # dbGetQuery(conn, query)$series_name
  
  series_choices
}

get_venues <- function(filters = NULL, conn = NULL) {
  if (is_local_data()) {
    return(local_get_venues(filters))
  }

  if (!db_is_available(conn)) {
    return(character())
  }
  
  if (is.null(conn)) {
    conn <- get_db_connection()
  }
  
  where_clause <- build_filter_clause(filters)
  
  query <- glue::glue("
    SELECT DISTINCT venue.name as venue_name
    FROM [GA20260618].MatchPlayers mp
    JOIN [GA20260618].Matches m ON mp.match_id = m.match_id
    JOIN [GA20260618].Teams team ON mp.team_id = team.team_id
    JOIN [GA20260618].Venues venue ON m.venue_id = venue.venue_id
    JOIN [GA20260618].Series series ON m.series_id = series.series_id
    JOIN [GA20260618].Seasons season
    ON m.season_id = season.season_id
    {where_clause}
    ORDER BY venue_name
  ")
  
  dbGetQuery(conn, query)$venue_name
}

get_teams <- function(filters = NULL, conn = NULL) {
  if (is_local_data()) {
    return(local_get_teams(filters))
  }

  if (!db_is_available(conn)) {
    return(character())
  }
  
  if (is.null(conn)) {
    conn <- get_db_connection()
  }
  
  where_clause <- build_filter_clause(filters)
  
  query <- glue::glue("
    SELECT DISTINCT team.team_name
    FROM [GA20260618].MatchPlayers mp
    JOIN [GA20260618].Matches m ON mp.match_id = m.match_id
    JOIN [GA20260618].Teams team ON mp.team_id = team.team_id
    JOIN [GA20260618].Venues venue ON m.venue_id = venue.venue_id
    JOIN [GA20260618].Series series ON m.series_id = series.series_id
    JOIN [GA20260618].Seasons season ON m.season_id = season.season_id
    {where_clause}
    ORDER BY team.team_name
  ")
  
  dbGetQuery(conn, query)$team_name
}

get_players <- function(filters = NULL, conn = NULL) {
  if (is_local_data()) {
    return(local_get_players(filters))
  }

  if (!db_is_available(conn)) {
    return(character())
  }
  
  if (is.null(conn)) {
    conn <- get_db_connection()
  }
  
  where_clause <- build_filter_clause(filters)
  
  query <- glue::glue("
    SELECT DISTINCT mp.player_id,
      p.name
    FROM [GA20260618].PlayerInnings mp
    JOIN [GA20260618].Matches m ON mp.match_id = m.match_id
    JOIN [GA20260618].Players p ON mp.player_id = p.player_id AND p.name NOT LIKE '(SUB%'
    JOIN [GA20260618].Teams team ON mp.team_id = team.team_id
    JOIN [GA20260618].Venues venue ON m.venue_id = venue.venue_id
    JOIN [GA20260618].Series series ON m.series_id = series.series_id
    JOIN [GA20260618].Seasons season ON m.season_id = season.season_id
    {where_clause}
    ORDER BY p.name
  ")
  
  dbGetQuery(conn, query)
}