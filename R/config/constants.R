view <- "[GA20260618]"
carried_bat_batting_position <- c(0, 1)
not_out_id <- 0
invalid_match_result_ids <- c(13)


series_db <- data.frame(
  ids = c(3,
          4,
          690007,
          950002,
          220008),
  names = c("Aus Domestic 1st Class M",
            "Aus Domestic OD M",
            "Aus Domestic OD F",
            "Aus Domestic T20 M",
            "Aus Domestic T20 F")
)

series_choices <- setNames(
  series_db$ids, series_db$names
)

series_matches <- c(
  '3' = 10,
  '4' = 7,
  '690007' = 12,
  '860011' = 10,
  '860005' = 10
)


all_id <- "0"
