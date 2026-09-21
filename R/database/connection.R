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
