# Local/sample-data constants used when R/config/constants.R is absent.
# Keep these aligned with the database constants: series IDs, match totals, and lookup IDs.

view <- "[GA20260618]"

carried_bat_batting_position <- c(0, 1)
not_out_id <- 0
invalid_match_result_ids <- c(13)

series_db <- data.frame(
  ids = c(3, 4, 690007, 860011, 860005),
  names = c(
    "Aus Domestic 1st Class M",
    "Aus Domestic OD M",
    "Aus Domestic OD F",
    "Aus Domestic T20 M",
    "Aus Domestic T20 F"
  ),
  stringsAsFactors = FALSE
)

series_choices <- setNames(
  series_db$ids,
  series_db$names
)

series_matches <- c(
  "3" = 10,
  "4" = 7,
  "690007" = 12,
  "860011" = 10,
  "860005" = 10
)

all_id <- "0"

milestone_choices <- data.frame(
  Milestone = c(
    "Appearances",
    "Career Runs",
    "50s",
    "Centuries",
    "150s",
    "200s",
    "250s",
    "Career Wickets",
    "Wicket Innings Haul - 5",
    "Wicket Innings Haul - 10",
    "Wicket Match Haul - 10",
    "Career Dismissals",
    "Career Catches",
    "Dismissals In Innings - 5",
    "Dismissals In Innings - 6",
    "Carried Bat"
  ),
  `Aus Domestic 1st Class M` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  `Aus Domestic OD M` = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
  `Aus Domestic T20 M` = c(1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
  `Aus Domestic OD F` = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
  `Aus Domestic T20 F` = c(1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
