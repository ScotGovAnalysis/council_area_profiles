check_expectations <- function(raw_data, expected) {
  
  # expected=expectations
  # READ ME -----------------------------------------------------------------
  # AUTHORS:            Rhiannon.Batstone@nrscotland.gov.uk
  #                     joseph.adams@nrscotland.gov.uk
  # PURPOSE OF SCRIPT:  Check data for Council Area Profiles
  
  result <- list()
  
  # Sheet and column names --------------------------------------------------
  result[["sheet_names"]] <-
    identical(names(raw_data), names(expected[["col_names"]]))
  
  if (identical(lapply(raw_data, names), expected[["col_names"]])) {
    message("Check passed: sheet and column names are correct")
    
    # First check if sheet names are correct
  } else if (isFALSE(result[["sheet_names"]])) {
    warning(
      paste("Check failed: unexpected Sheet Names: ", setdiff(
        names(raw_data), names(expected[["col_names"]])
      ), "\n"),
      paste("Check failed: missing Sheet Names: ", setdiff(
        names(expected[["col_names"]]), names(raw_data)
      ), "\n")
    )
    
    # If sheet names are correct then check column names
  } else if (result[["sheet_names"]]) {
    unexpected_col_names <-
      purrr::map2(lapply(raw_data, names),
                  expected[["col_names"]],
                  setdiff) %>%
      compact()
    
    missing_col_names <-
      purrr::map2(expected[["col_names"]],
                  lapply(raw_data, names),
                  setdiff) %>%
      compact()
    
    warning(paste(
      "Check failed: unexpected column names (above)",
      str(unexpected_col_names)
    ),
    immediate. = TRUE)
    
    warning(paste(
      "Check failed: missing column names (above)",
      str(missing_col_names)
    ),
    immediate. = TRUE)
  }
  
  # Realistic row limit -----------------------------------------------------
  # If the row limit is exactly 1,048,576 rows (the maximum for Excel) it is
  # likely there has been some data loss
  result[["max_row_limit"]] <- lapply(raw_data, nrow) == 1048575
  
  if (any(result[["max_row_limit"]])) {
    warning(
      paste(
        "Check failed: these datasets are at the maximum row limit for Excel ",
        "(suggesting possible data loss):",
        paste(names(result[["max_row_limit"]][result[["max_row_limit"]]]),
              collapse = ", ")
      )
    )
  } else {
    message("Check passed: all datasets are under the maximum row limit for ",
            "Excel")
  }
  
  
  # Values ------------------------------------------------------------------
  
  # * NAs -------------------------------------------------------------------
  result[["NAs"]] <-
    sapply(
      raw_data,
      FUN = function(x)
        any(is.na(x))
    )
  
  if (any(result[["NAs"]])) {
    warning(paste("Check failed: there are NAs in:", paste(names(
      result[["NAs"]][result[["NAs"]]]
    ), collapse = ", ")))
  } else {
    message("Check passed: no NAs found")
  }
  
  # * Missing ---------------------------------------------------------------
  result[["to_compare"]] <-
    map2(.x = raw_data[!(names(raw_data) %in% c("updates", "expected_ranges"))],
         .y = expected[["col_values"]][["tibble"]],
         ~ select(.x, names(.y)))
  
  result[["combinations"]] <-
    purrr::map(.x = expected[["col_values"]][["tibble"]],
               .f = expand.grid,
               stringsAsFactors = FALSE) %>%
    purrr::map(.f = as_tibble)
  
  # We know that some combinations won't be in certain datasets
  result[["combinations"]][["household_projections"]] <- 
    filter(result[["combinations"]][["household_projections"]],
           !(`Household type` == "All households" &
               `Age group` == "All ages"))
  
  result[["values_missing"]] <- purrr::map2(
    .x = result[["combinations"]],
    .y = result[["to_compare"]],
    .f = anti_join) %>%
    `[`(names(.) != "leading_causes_of_death") %>%
    purrr::discard(~ nrow(.x) == 0) %>% 
    # suppress the "joining by" console messages as there are far too many to be useful
    suppressMessages()
  # We cannot have different combinations of ICD code, Cause, and Cause label.
  # Also some rows in this table are suppressed. So we don't check this table for
  # missing values.
  
  # TODO by not looking for missing values in leading_causes_of_death it means all of the data
  # is labelled as unexpected during the checking stage. This is not helpful but it's a really big
  # edge case that is difficult to deal with.
  
  # TODO leading_causes_of_death needs slimming down to only the combinations of codes, causes and labels 
  # which are used together, not every possible combination. 
  
  message("Checking for missing values...")
  
  if (length(result[["values_missing"]]) == 0) {
    message("Check passed: no missing values found")
  } else {
    message("Check failed: there are rows are missing in the following tables.")
    message("Table summaries:")
    
    # so we have identified rows which are missing 
    # according to our expectations
    # sometimes there are far too many rows to print
    # eg. pop est has 18k rows 
    # here we summarise each table based on which columns are present
    # it's not a catch all, but will give an idea of how many pieces of
    # information are missing in the console which has a 10 row limit for 
    # printing tables
    
    summary_tables = lapply(result[["values_missing"]], function(df){
      
      # df = result[["values_missing"]][[4]] %>% names()
      
      sum_table = df %>% group_by(`Council area`) %>%
        summarize(Year = ifelse("Year" %in% names(df), paste(min(Year),"to",max(Year)), NA),
                  Sex = ifelse("Sex" %in% names(df), paste(unique(Sex), collapse = ", "), NA),
                  Age = ifelse("Age" %in% names(df), paste(min(Age),"to",max(Age)), NA),
                  `Registration Year` = ifelse("Registration Year" %in% names(df), paste(min(`Registration Year`),"to",max(`Registration Year`)), NA),
                  `Age group` = ifelse("Age group" %in% names(df), paste(first(`Age group`),"to",last(`Age group`)), NA),
                  Type = ifelse("Type" %in% names(df), paste(first(`Type`),"and",last(`Type`)), NA),
                  `Council Tax band` = ifelse("Council Tax band" %in% names(df), paste(first(`Council Tax band`),"to",last(`Council Tax band`)), NA),
                  n_rows = n()
                  ) %>% 
        select_if(~ !any(is.na(.)))
      
      # TODO add other summary functions for columns not in the set that already have issues
      
    })
    
    print(summary_tables)
    
    message("If any tables have not been summarised correctly there is a chance that there is a new problem with the data.")
    
    message("To see a detailed account of the missing rows, please inspect the 'check_results' object")
    
    # print(result[["values_missing"]])
  }
  
  # * Unexpected ------------------------------------------------------------
  result[["values_unexpected"]] <- purrr::map2(
    .x = result[["to_compare"]],
    .y = result[["combinations"]],
    .f = anti_join) %>%
    purrr::discard(~ nrow(.x) == 0) %>% 
    # suppress all of the join by messages that float up
    suppressMessages()
  
  # TODO leading_causes_of_death needs excluding from here too? 
  
  if (length(result[["values_unexpected"]]) == 0) {
    message("Check passed: no unexpected values found")
  } else {
    message("Check failed: these rows are unexpected:")
    print(result[["values_unexpected"]])
  }
  
  # * Implausible -----------------------------------------------------------
  check_range <- function(tibble,
                          column,
                          min_expected,
                          max_expected) {
    if (min(raw_data[[tibble]][[column]]) < min_expected)
      message(
        "Check failed: the table '",
        tibble,
        "' has values in column '",
        column,
        "' below the expected minimum of ",
        min_expected
      )
    if (max(raw_data[[tibble]][[column]]) > max_expected)
      message(
        "Check failed: the table '",
        tibble,
        "' has values in column '",
        column,
        "' above the expected maximum of ",
        max_expected
      )
  }
  
  # * * population_estimates ------------------------------------------------
  check_range(
    tibble = "population-estimates",
    column = "Population",
    min = 0,
    max = 43000
  )
  
  # * * population_projections ----------------------------------------------
  check_range(
    tibble = "population-projections",
    column = "Population",
    min = 0,
    max = 55000
  )
  
  # * * nature_of_population_change -----------------------------------------
  check_range(
    tibble = "nature-of-population-change",
    column = 2,
    min = 22000,
    max = 5500000
  )
  
  check_range(
    tibble = "nature-of-population-change",
    column = 3,
    min = 22000,
    max = 5600000
  )
  
  check_range(
    tibble = "nature-of-population-change",
    column = "Population change",
    min = -5500,
    max = 100000
  )
  
  check_range(
    tibble = "nature-of-population-change",
    column = "Births",
    min = 1500,
    max = 510000
  )
  
  check_range(
    tibble = "nature-of-population-change",
    column = "Deaths",
    min = 2400,
    max = 600000
  )
  
  check_range(
    tibble = "nature-of-population-change",
    column = "Natural change",
    min = -95000,
    max = 6500
  )
  
  check_range(
    tibble = "nature-of-population-change",
    column = "Net migration",
    min = -650,
    max = 190000
  )
  
  # * * births_by_sex -------------------------------------------------------
  check_range(
    tibble = "births-by-sex",
    column = "Number",
    min = 60,
    max = 70000
  )
  
  # * * standardised_birth_rates --------------------------------------------
  check_range(
    tibble = "standardised-birth-rates",
    column = "Standardised birth rate",
    min = 6,
    max = 17
  )
  
  # * * births_by_age_of_mother ---------------------------------------------
  check_range(
    tibble = "births-by-age-of-mother",
    column = "Number",
    min = 0,
    max = 67500
  )
  
  # * * fertility_rates -----------------------------------------------------
  check_range(
    tibble = "fertility-rates",
    column = "Total fertility rate",
    min = 0.9,
    max = 2.2
  )
  
  # * * deaths_by_sex -------------------------------------------------------
  check_range(
    tibble = "deaths-by-sex",
    column = "Number",
    min = 75,
    max = 64500
  )
  
  # * * standardised_death_rates --------------------------------------------
  check_range(
    tibble = "standardised-death-rates",
    column = "Standardised death rate",
    min = 7.5,
    max = 15
  )
  
  # * * deaths_by_sex_by_age ------------------------------------------------
  check_range(
    tibble = "deaths-by-sex-by-age",
    column = "Number",
    min = 0,
    max = 65000
  )
  
  # * * leading_causes_of_death ---------------------------------------------
  check_range(
    tibble = "leading-causes-of-death",
    column = "Number",
    min = 5,
    max = 4300
  )
  
  check_range(
    tibble = "leading-causes-of-death",
    column = "Percent",
    min = 3,
    max = 22
  )
  
  check_range(
    tibble = "leading-causes-of-death",
    column = "Total deaths",
    min = 90,
    max = 30000
  )
  
  # * * migration -----------------------------------------------------------
  check_range(
    tibble = "migration",
    column = "Number",
    min = -5000,
    max = 100000
  )
  
  
  # * * net_migration -------------------------------------------------------
  check_range(
    tibble = "net-migration",
    column = "Number",
    min = -2500,
    max = 17500
  )
  
  # * * net_migration_rates -------------------------------------------------
  check_range(
    tibble = "net-migration-rates",
    column = "Rate",
    min = -6.5,
    max = 17.5
  )
  
  # * * life_expectancy -----------------------------------------------------
  check_range(
    tibble = "life-expectancy",
    column = "Life expectancy",
    min = 1.5,
    max = 85
  )
  
  # * * marriages -----------------------------------------------------------
  check_range(
    tibble = "marriages",
    column = "Number of marriages",
    min = 60,
    max = 36000
  )
  
  # * * civil_partnerships --------------------------------------------------
  check_range(
    tibble = "civil-partnerships",
    column = "Number of civil partnerships",
    min = 0,
    max = 1100
  )
  
  # * * household_estimates -------------------------------------------------
  check_range(
    tibble = "household-estimates",
    column = "Number of households",
    min = 8300,
    max = 2600000
  )
  
  # * * household_projections -----------------------------------------------
  check_range(
    tibble = "household-projections",
    column = "Number",
    min = 0,
    max = 1930000
  )
  
  # * * dwellings -----------------------------------------------------------
  check_range(
    tibble = "dwellings",
    column = "Number of dwellings",
    min = 9000,
    max = 2700000
  )
  
  # * * dwellings_by_type ---------------------------------------------------
  check_range(
    tibble = "dwellings-by-type",
    column = "Number",
    min = 0,
    max = 2610000
  )
  
  # * * dwellings_by_council_tax_band ---------------------------------------
  check_range(
    tibble = "dwellings-by-council-tax-band",
    column = "Number",
    min = 0,
    max = 605000
  )
  
  return(result)
  
}

