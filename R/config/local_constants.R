# Local/sample-data constants used when R/config/constants.R is absent.
# Production machines can keep a private constants.R (gitignored) that overrides these.

view <- "[GA20260618]"

series_choices <- c(
  "Aus Domestic OD M",
  "Aus Domestic T20 M",
  "Sheffield Shield M",
  "Aus Domestic T20 F"
)

carried_bat_batting_position <- c(1L, 2L)
not_out_id <- 1L
invalid_match_result_ids <- c(5L, 6L)
