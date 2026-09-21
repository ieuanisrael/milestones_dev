# Single milestone definition table, plus threshold lists from mikes_worksheet.xlsx.
# Query mechanics live here; numeric targets come from the odt20 / first-class sheets.

milestone_workbook_path <- function() {
  candidates <- c(
    "mikes_worksheet.xlsx",
    file.path("..", "mikes_worksheet.xlsx")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    return(NULL)
  }
  hit[[1]]
}

milestones <- dplyr::tribble(
  ~definition_id, ~display_name, ~query_strategy, ~category, ~value_column, ~aggregation, ~event_value,

  "appearance", "Appearances", "cumulative", "general", "match_id", "count", NA_real_,
  "career_runs", "Career Runs", "cumulative", "batting", "batter_score", "sum", NA_real_,
  "batting_50", "50s", "tiered_innings", "batting", "batter_score", NA_character_, 50,
  "batting_100", "Centuries", "tiered_innings", "batting", "batter_score", NA_character_, 100,
  "batting_150", "150s", "tiered_innings", "batting", "batter_score", NA_character_, 150,
  "batting_200", "200s", "tiered_innings", "batting", "batter_score", NA_character_, 200,
  "batting_250", "250s", "tiered_innings", "batting", "batter_score", NA_character_, 250,
  "career_wickets", "Career Wickets", "cumulative", "bowling", "bowler_wickets", "sum", NA_real_,
  "wicket_innings_5", "Wicket Innings Haul - 5", "tiered_innings", "bowling", "bowler_wickets", NA_character_, 5,
  "wicket_innings_10", "Wicket Innings Haul - 10", "tiered_innings", "bowling", "bowler_wickets", NA_character_, 10,
  "wicket_match_10", "Wicket Match Haul - 10", "tiered_match", "bowling", "bowler_wickets", NA_character_, 10,
  "career_dismissals", "Career Dismissals", "cumulative", "fielding_wk", "fielder_stumpings + fielder_catches", "sum", NA_real_,
  "career_catches", "Career Catches", "cumulative", "fielding", "fielder_catches", "sum", NA_real_,
  "wk_dismissals_5", "Dismissals In Innings - 5", "tiered_match", "fielding_wk", "fielder_stumpings + fielder_catches", NA_character_, 5,
  "wk_dismissals_6", "Dismissals In Innings - 6", "tiered_match", "fielding_wk", "fielder_stumpings + fielder_catches", NA_character_, 6,
  "carried_bat", "Carried Bat", "cumulative", "general", "match_id", "count", NA_real_
)

milestones$display_name_ui <- milestones$display_name

parse_threshold_cell <- function(value) {
  if (length(value) == 0 || is.null(value) || is.na(value)) {
    return(numeric())
  }
  if (is.numeric(value) && !is.na(value)) {
    return(as.numeric(value))
  }

  text <- trimws(as.character(value))
  if (!nzchar(text) || grepl("^<NA>$", text, ignore.case = TRUE)) {
    return(numeric())
  }

  as.numeric(unlist(regmatches(text, gregexpr("[0-9]+", text))))
}

parse_threshold_sheet <- function(path, sheet, family) {
  raw <- readxl::read_xlsx(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  body <- raw[-1, , drop = FALSE]
  names <- as.character(body[[1]])

  rows <- lapply(seq_len(nrow(body)), function(i) {
    cells <- unlist(body[i, -1, drop = TRUE], use.names = FALSE)
    thresholds <- unique(unlist(lapply(cells, parse_threshold_cell), use.names = FALSE))
    thresholds <- sort(thresholds[!is.na(thresholds)])
    if (length(thresholds) == 0) {
      return(NULL)
    }
    tibble::tibble(
      display_name = names[[i]],
      family = family,
      threshold = thresholds
    )
  })

  dplyr::bind_rows(rows)
}

fallback_milestone_thresholds <- function() {
  make <- function(family, mapping) {
    dplyr::bind_rows(lapply(names(mapping), function(nm) {
      tibble::tibble(display_name = nm, family = family, threshold = mapping[[nm]])
    }))
  }

  dplyr::bind_rows(
    make("odt20", list(
      Appearances = c(1, 50, 100, 150, 200, 250),
      `Career Runs` = c(500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000),
      `50s` = c(1, 5, 10, 20, 30, 40, 50),
      Centuries = c(1, 2, 5, 10, 20),
      `150s` = c(1, 2, 3, 4, 5, 10),
      `200s` = c(1, 2, 3, 4, 5, 6),
      `250s` = c(1, 2, 3, 4, 5, 6),
      `Career Wickets` = c(1, 50, 100, 200, 500, 1000),
      `Wicket Innings Haul - 5` = c(1, 2, 5, 10, 15, 20),
      `Wicket Innings Haul - 10` = c(1, 2, 3, 4, 5, 6),
      `Wicket Match Haul - 10` = c(1, 2, 3, 4, 5, 6),
      `Career Dismissals` = c(1, 50, 100, 150, 200, 250, 300, 350, 400, 500),
      `Career Catches` = c(1, 50, 100, 150, 200, 250),
      `Dismissals In Innings - 5` = c(1, 2, 5, 10, 15, 20),
      `Dismissals In Innings - 6` = c(1, 2, 3, 4, 5, 6),
      `Carried Bat` = c(1, 5, 10)
    )),
    make("firstclass", list(
      Appearances = c(1, 50, 100, 150, 200),
      `Career Runs` = c(500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000),
      `50s` = c(1, 10, 50, 100, 150),
      Centuries = c(1, 5, 10, 20, 50),
      `150s` = c(1, 5, 10, 20),
      `200s` = c(1, 5, 10),
      `250s` = c(1, 5, 10),
      `Career Wickets` = c(1, 50, 100, 200, 250, 500, 1000),
      `Wicket Innings Haul - 5` = c(1, 10, 25, 50, 100),
      `Wicket Innings Haul - 10` = c(1, 2, 3, 4, 5, 10),
      `Wicket Match Haul - 10` = c(1, 2, 3, 4, 5, 10),
      `Career Dismissals` = c(1, 50, 100, 200, 250, 500, 1000),
      `Career Catches` = c(1, 50, 100, 150, 200, 250),
      `Dismissals In Innings - 5` = c(1, 2, 5, 10, 15, 20),
      `Dismissals In Innings - 6` = c(1, 2, 3, 4, 5, 6),
      `Carried Bat` = c(1, 5, 10)
    ))
  )
}

fallback_series_matrix <- function() {
  data.frame(
    Milestone = milestones$display_name,
    `Aus Domestic 1st Class M` = 1,
    `Aus Domestic OD M` = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
    `Aus Domestic T20 M` = c(1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
    `Aus Domestic OD F` = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
    `Aus Domestic T20 F` = c(1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

load_milestone_workbook <- function(path = milestone_workbook_path()) {
  if (is.null(path) || !file.exists(path)) {
    return(list(
      thresholds = fallback_milestone_thresholds(),
      series_matrix = fallback_series_matrix()
    ))
  }

  thresholds <- dplyr::bind_rows(
    parse_threshold_sheet(path, "milestone_thresholds_odt20", "odt20"),
    parse_threshold_sheet(path, "milestone_thresholds_firstclass", "firstclass")
  )

  series_matrix <- as.data.frame(
    readxl::read_xlsx(path, sheet = "series_milestone_matrix")
  )

  list(thresholds = thresholds, series_matrix = series_matrix)
}

.milestone_book <- load_milestone_workbook()
milestone_thresholds <- .milestone_book$thresholds
series_milestone_matrix <- .milestone_book$series_matrix
milestone_choices <- series_milestone_matrix

milestone_family <- function(series) {
  series_id <- as.character(series)
  firstclass_id <- "3"
  if (exists("series_choices", inherits = TRUE)) {
    mapped <- series_choices[["Aus Domestic 1st Class M"]]
    if (!is.null(mapped) && !is.na(mapped)) {
      firstclass_id <- as.character(mapped)
    }
  }

  if (identical(series_id, firstclass_id)) {
    "firstclass"
  } else {
    "odt20"
  }
}

series_label <- function(series) {
  if (exists("series_choices", inherits = TRUE)) {
    labels <- names(series_choices)[as.character(series_choices) == as.character(series)]
    if (length(labels) == 1) {
      return(labels)
    }
  }
  as.character(series)
}

thresholds_for <- function(display_name, series = NULL) {
  family <- milestone_family(series)
  vals <- milestone_thresholds$threshold[
    milestone_thresholds$display_name == display_name &
      milestone_thresholds$family == family
  ]
  sort(unique(as.numeric(vals)))
}

milestones_for_series <- function(series = NULL) {
  if (is.null(series) || length(series) == 0 || is.na(series) || identical(as.character(series), "0")) {
    return(milestones)
  }

  label <- series_label(series)
  if (!label %in% names(series_milestone_matrix)) {
    return(milestones)
  }

  enabled <- series_milestone_matrix$Milestone[series_milestone_matrix[[label]] == 1]
  milestones[milestones$display_name %in% enabled, , drop = FALSE]
}

related_event_values <- function(definition) {
  vals <- milestones$event_value[
    milestones$query_strategy == definition$query_strategy &
      milestones$value_column == definition$value_column &
      !is.na(milestones$event_value)
  ]
  sort(unique(as.numeric(vals)))
}

event_cutoff <- function(definition) {
  if (!is.null(definition$event_value) && length(definition$event_value) > 0 && !is.na(definition$event_value)[1]) {
    return(definition$event_value[[1]])
  }
  NA_real_
}
