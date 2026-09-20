# Auth - Benchmarks

headers = c('X-api-key' = Sys.getenv('LUDIS_API_TOKEN'))


get_connection_local <- function() {
  
  ser <- "auscricketams.database.windows.net" # Establishing server
  db <- "auscricketpdae" # Establishing database
  ui <- "scott.peterson@cricketnsw.com.au"#write your work email address inside the quotation marks
  passwd <- ''
  
  con <- dbConnect(odbc(),
                   UID = ui,
                   #pwd = passwd,
                   Driver = "ODBC Driver 17 for SQL Server", # If you don't have this driver installed on your computer, email Stumped and ask to get it installed
                   Server = ser,
                   Database = db,
                   Authentication = "ActiveDirectoryInteractive")
  
  return(con)
  
}


get_connection_ludis <- function() {
  
  SQL_COPT_SS_ACCESS_TOKEN <- 1256
  AUTHORITY_HOST_URL <- "https://login.microsoftonline.com"
  SQL_SERVER_SCOPE <- "https://database.windows.net/.default"
  
  app_id <- Sys.getenv("app_id")
  secret <- Sys.getenv("secret")
  
  # These are defined in the Azure setup
  scope <- SQL_SERVER_SCOPE
  tenant <- "australiancricket.onmicrosoft.com"
  authorityHostUrl <- AUTHORITY_HOST_URL
  clientId = app_id
  serverName <- "auscricketams"
  databaseName <- "auscricketpdae"
  clientSecret <- secret
  
  resource <- c("https://database.windows.net/.default")
  tenant <- "australiancricket.onmicrosoft.com"
  app <- Sys.getenv("app_id")
  password <- Sys.getenv("secret")
  redirect <- Sys.getenv("redirect")
  
  token <- AzureAuth::get_azure_token(
    resource, tenant, app,
    password = password,
    #auth_type = "authorization_code",
    authorize_args = list(redirect_uri = redirect),
    use_cache = FALSE,
    #auth_code = code,   # function argument, code retrieved per the shiny vignette
    version = 2
  )
  
  connString = "Driver={ODBC Driver 18 for SQL Server};SERVER=auscricketams.database.windows.net;DATABASE=auscricketpdae"
  attrs_before = list("azure_token" = token$credentials$access_token)
  
  con <- dbConnect(drv = odbc::odbc(),
                   driver = "ODBC Driver 18 for SQL Server",
                   server = "auscricketams.database.windows.net",
                   database = "auscricketpdae",
                   attributes = attrs_before)
  
  return(con)
  
}