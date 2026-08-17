select_choices <- milestones %>%
  rowwise() %>%
  do({
    
    row <- .
    
    if (
      row$query_strategy %in%
      c(
        "tiered_innings",
        "tiered_match"
      )
    ) {
      
      if (is.na(row$multiple) || row$multiple == 0) {
        tiers <- row$first_value
      } else {
        tiers <- seq(
          row$first_value,
          row$max_value,
          by = row$multiple
        )
      }
      
      
      tibble(
        definition_id = row$definition_id,
        display_name =
          paste0(
            row$display_name,
            " - ",
            tiers
          ),
        tier = tiers
      )
      
    } else {
      tibble(
        definition_id = row$definition_id,
        display_name = row$display_name,
        tier = NA_real_
      )
      
    }
    
  }) %>% mutate(
     display_name_ui = ifelse(
      (definition_id == "batting_tiers"),
      ifelse(tier == 100, "Centuries", paste0(tier, "s")),
      display_name
     )
  ) %>%
  ungroup()
