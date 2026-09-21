query <- "SELECT [season]
      ,[first_name]
      ,[surname]
      ,[expiry]
      ,[team_id]
      ,[replaced_player_id]
  FROM [elite].[LISTS_contract_lists]"
  


team_list <- tryCatch(
          QueryDBFunction(con = internal_con, query = query),
          error = function(e) NULL
        )