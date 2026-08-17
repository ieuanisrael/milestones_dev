# Database connection helpers for the milestone application.
# These functions centralize the ODBC connection lifecycle and query execution.

sample_data_path <- function() {
  Sys.getenv("MILESTONES_SAMPLE_DB", unset = file.path("data", "sample", "tables.rds"))
}

is_local_data <- function() {
  flag <- Sys.getenv("MILESTONES_USE_SAMPLE", unset = NA_character_)
  if (!is.na(flag) && nzchar(flag)) {
    return(tolower(flag) %in% c("1", "true", "yes"))
  }
  file.exists(sample_data_path())
}

QueryDBFunction <- function(con, query, query_param = NULL) {
  if (is.null(con) || inherits(con, "local_sample_connection") || !DBI::dbIsValid(con)) {
    return(tibble::tibble())
  }

  query_stmt <- DBI::dbSendQuery(con, query)
  on.exit(DBI::dbClearResult(query_stmt))

  if (!is.null(query_param)) {
    DBI::dbBind(query_stmt, list(query_param))
  }

  df <- DBI::dbFetch(query_stmt)
  return(df)
}

get_db_connection <- function(force_reconnect = FALSE) {
  if (is_local_data()) {
    con <<- structure(list(path = sample_data_path()), class = "local_sample_connection")
    return(con)
  }

  if (
    force_reconnect ||
      !exists("con", inherits = FALSE) ||
      is.null(con) ||
      !inherits(con, "DBIConnection") ||
      !DBI::dbIsValid(con)
  ) {
    con <<- safe_db_connect()
  }

  con
}

db_is_available <- function(conn = NULL) {
  if (is_local_data()) {
    return(TRUE)
  }

  if (is.null(conn)) {
    conn <- get_db_connection()
  }

  !is.null(conn) && inherits(conn, "DBIConnection") && DBI::dbIsValid(conn)
}

safe_db_connect <- function() {
  if (is_local_data()) {
    message("Using local sample data: ", sample_data_path())
    return(structure(list(path = sample_data_path()), class = "local_sample_connection"))
  }

  tryCatch(
    {
      DBI::dbConnect(
        odbc(),
        UID = "ieuan.israel@cricketnsw.com.au",
        Driver = "ODBC Driver 18 for SQL Server",
        Server = "auscricketams.database.windows.net",
        Database = "auscricketpdae",
        Authentication = "ActiveDirectoryInteractive"
      )
    },
    error = function(e) {
      message("Database connection unavailable: ", conditionMessage(e))
      NULL
    }
  )
}

con <- get_db_connection()