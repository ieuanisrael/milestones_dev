library(tidyverse)

query <- "SELECT [season]
      ,[first_name]
      ,[surname]
      ,[expiry]
      ,[team_id]
      ,[replaced_player_id]
      ,[ams_id]
  FROM [elite].[LISTS_contract_lists]
  WHERE season = '2025-26' AND
  team_id = '3'"

#' Create a connection to the internal database via Ludis
#' 
#' Creates a connection to the internal database using Azure authentication via Ludis
#' 
#' @return A database connection object
get_connection_internal_ludis <- function() {
  SQL_COPT_SS_ACCESS_TOKEN <- 1256
  AUTHORITY_HOST_URL <- "https://login.microsoftonline.com"
  SQL_SERVER_SCOPE <- "https://database.windows.net/.default"
  
  app_id <- Sys.getenv("app_internal")
  secret <- Sys.getenv("secret_internal")
  
  # Internal database parameters (from your Python code)
  scope <- SQL_SERVER_SCOPE
  tenant <- "australiancricket.onmicrosoft.com"
  authorityHostUrl <- AUTHORITY_HOST_URL
  clientId <- app_id
  serverName <- "data-cnsw-prod-sql"
  databaseName <- "data-cnsw-prod-db"
  clientSecret <- secret
  
  resource <- c("https://database.windows.net/.default")
  redirect <- Sys.getenv("redirect")
  
  # Get authentication token
  token <- AzureAuth::get_azure_token(
    resource, tenant, app_id,
    password = secret,
    authorize_args = list(redirect_uri = redirect),
    use_cache = FALSE,
    version = 2
  )
  
  print(token)
  
  # Create connection with token
  attrs_before <- list("azure_token" = token$credentials$access_token)
  
  con <- DBI::dbConnect(
    drv = odbc::odbc(),
    driver = "ODBC Driver 18 for SQL Server",
    server = "data-cnsw-prod-sql.database.windows.net",
    database = "data-cnsw-prod-db",
    attributes = attrs_before
  )
  
  return(con)
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


internal_con <- get_connection_internal_ludis()

res <- QueryDBFunction(internal_con, query)

res %>% glimpse