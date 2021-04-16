# READ ME -----------------------------------------------------------------
# AUTHORS:            Rhiannon.Batstone@nrscotland.gov.uk
#                     joseph.adams@nrscotland.gov.uk
# PURPOSE OF SCRIPT:  Check data for Council Area Profiles

# Set expectations --------------------------------------------------------
source("source/expected_names_and_columns.R")
result <- list()

# Sheet and column names --------------------------------------------------
result[["sheet_names"]] <-
  identical(names(raw_data), names(expected[["col_names"]]))

if(identical(lapply(raw_data, names), expected[["col_names"]])) {
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
  
  warning(paste("Check failed: unexpected column names (above)", str(unexpected_col_names)),
          immediate. = TRUE)
  warning(paste("Check failed: missing column names (above)", str(missing_col_names)),
          immediate. = TRUE)
}

# Realistic row limit -----------------------------------------------------
# If the row limit is exactly 1,048,576 rows (the maximum for Excel) it is
# likely there has been some data loss
result[["max_row_limit"]] <- lapply(raw_data, nrow) == 1048575

if (any(result[["max_row_limit"]])) {
  warning(
    paste(
      "Check failed: these datasets are at the maximum row limit for Excel (suggesting possible data loss):",
      paste(names(result[["max_row_limit"]][result[["max_row_limit"]]]), collapse = ", ")
    )
  )
} else {
  message("Check passed: all datasets are under the maximum row limit for Excel")
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
  map2(.x = raw_data[names(raw_data) != "updates"],
       .y = expected[["col_values"]][["tibble"]],
       ~ select(.x, names(.y)))

result[["combinations"]] <-
  purrr::map(.x = expected[["col_values"]][["tibble"]],
             .f = expand.grid,
             stringsAsFactors = FALSE) %>% 
  purrr::map(.f = as_tibble)

result[["values_missing"]] <- purrr::map2(
  .x = result[["combinations"]],
  .y = result[["to_compare"]],
  .f = anti_join) %>%
  `[`(names(.) != "leading_causes_of_death") %>%
  purrr::discard(~ nrow(.x) == 0)
# We cannot have different combinations of ICD code, Cause, and Cause label.
# Also some rows in this table are suppressed. So we don't check this table for
# missing values.

if (length(result[["values_missing"]]) == 0) {
  message("Check passed: no missing values found")
} else {
  message("Check failed: these rows are missing:")
  print(result[["values_missing"]])
}

# * Unexpected ------------------------------------------------------------
result[["values_unexpected"]] <- purrr::map2(
  .x = result[["to_compare"]],
  .y = result[["combinations"]],
  .f = anti_join) %>%
  purrr::discard(~ nrow(.x) == 0)

if (length(result[["values_unexpected"]]) == 0) {
  message("Check passed: no unexpected values found")
} else {
  message("Check failed: these rows are unexpected:")
  print(result[["values_unexpected"]])
}

# * Implausible -----------------------------------------------------------
# * * population_estimates ------------------------------------------------
expect_gt(object = min(raw_data[["population-estimates"]][["Population"]]),
          expected = 0)
expect_lt(object = max(raw_data[["population-estimates"]][["Population"]]),
          expected = 43000)

# * * population_projections ----------------------------------------------
expect_gt(object = min(raw_data[["population-projections"]][["Population"]]),
          expected = 0)
expect_lt(object = max(raw_data[["population-projections"]][["Population"]]),
          expected = 55000)

# * * nature_of_population_change -----------------------------------------
expect_gt(object = min(raw_data[["nature-of-population-change"]][[2]]),
          expected = 22000)
expect_lt(object = max(raw_data[["nature-of-population-change"]][[2]]),
          expected = 5500000)


expect_gt(object = min(raw_data[["nature-of-population-change"]][[3]]),
          expected = 22000)
expect_lt(object = max(raw_data[["nature-of-population-change"]][[3]]),
          expected = 5600000)


expect_gt(object = min(raw_data[["nature-of-population-change"]][["Population change"]]),
          expected = -5500)
expect_lt(object = max(raw_data[["nature-of-population-change"]][["Population change"]]),
          expected = 100000)


expect_gt(object = min(raw_data[["nature-of-population-change"]][["Births"]]),
          expected = 1500)
expect_lt(object = max(raw_data[["nature-of-population-change"]][["Births"]]),
          expected = 510000)


expect_gt(object = min(raw_data[["nature-of-population-change"]][["Deaths"]]),
          expected = 2400)
expect_lt(object = max(raw_data[["nature-of-population-change"]][["Deaths"]]),
          expected = 600000)


expect_gt(object = min(raw_data[["nature-of-population-change"]][["Natural change"]]),
          expected = -95000)
expect_lt(object = max(raw_data[["nature-of-population-change"]][["Natural change"]]),
          expected = 6500)


expect_gt(object = min(raw_data[["nature-of-population-change"]][["Net migration"]]),
          expected = -650)
expect_lt(object = max(raw_data[["nature-of-population-change"]][["Net migration"]]),
          expected = 190000)

# * * births_by_sex -------------------------------------------------------
expect_gt(object = min(raw_data[["births-by-sex"]][["Number"]]),
          expected = 60)
expect_lt(object = max(raw_data[["births-by-sex"]][["Number"]]),
          expected = 70000)

# * * standardised_birth_rates --------------------------------------------
expect_gt(object = min(raw_data[["standardised-birth-rates"]][["Standardised birth rate"]]),
          expected = 6)
expect_lt(object = max(raw_data[["standardised-birth-rates"]][["Standardised birth rate"]]),
          expected = 17)

# * * births_by_age_of_mother ---------------------------------------------
expect_gte(object = min(raw_data[["births-by-age-of-mother"]][["Number"]]),
          expected = 0)
expect_lt(object = max(raw_data[["births-by-age-of-mother"]][["Number"]]),
          expected = 67500)

# * * fertility_rates -----------------------------------------------------
expect_gt(object = min(raw_data[["fertility-rates"]][["Total fertility rate"]]),
           expected = 1)
expect_lt(object = max(raw_data[["fertility-rates"]][["Total fertility rate"]]),
          expected = 2.2)

# * * deaths_by_sex -------------------------------------------------------
expect_gt(object = min(raw_data[["deaths-by-sex"]][["Number"]]),
          expected = 75)
expect_lt(object = max(raw_data[["deaths-by-sex"]][["Number"]]),
          expected = 64500)

# * * standardised_death_rates --------------------------------------------
expect_gt(object = min(raw_data[["standardised-death-rates"]][["Standardised death rate"]]),
          expected = 7.5)
expect_lt(object = max(raw_data[["standardised-death-rates"]][["Standardised death rate"]]),
          expected = 15)

# * * deaths_by_sex_by_age ------------------------------------------------
expect_gte(object = min(raw_data[["deaths-by-sex-by-age"]][["Number"]]),
          expected = 0)
expect_lt(object = max(raw_data[["deaths-by-sex-by-age"]][["Number"]]),
          expected = 59000)

# * * leading_causes_of_death ---------------------------------------------
expect_gte(object = min(raw_data[["leading-causes-of-death"]][["Number"]]),
          expected = 5)
expect_lt(object = max(raw_data[["leading-causes-of-death"]][["Number"]]),
          expected = 4300)

expect_gt(object = min(raw_data[["leading-causes-of-death"]][["Percent"]]),
          expected = 3)
expect_lt(object = max(raw_data[["leading-causes-of-death"]][["Percent"]]),
          expected = 22)

expect_gt(object = min(raw_data[["leading-causes-of-death"]][["Total deaths"]]),
          expected = 90)
expect_lt(object = max(raw_data[["leading-causes-of-death"]][["Total deaths"]]),
          expected = 30000)

# * * migration -----------------------------------------------------------
expect_gt(object = min(raw_data[["migration"]][["Number"]]),
          expected = -5000)
expect_lt(object = max(raw_data[["migration"]][["Number"]]),
          expected = 100000)

# * * net_migration -------------------------------------------------------
expect_gt(object = min(raw_data[["net-migration"]][["Number"]]),
          expected = -2500)
expect_lt(object = max(raw_data[["net-migration"]][["Number"]]),
          expected = 17500)

# * * net_migration_rates -------------------------------------------------
expect_gt(object = min(raw_data[["net-migration-rates"]][["Rate"]]),
          expected = -6.5)
expect_lt(object = max(raw_data[["net-migration-rates"]][["Rate"]]),
          expected = 17.5)


# * * life_expectancy -----------------------------------------------------
expect_gt(object = min(raw_data[["life-expectancy"]][["Life expectancy"]]),
          expected = 1.5)
expect_lt(object = max(raw_data[["life-expectancy"]][["Life expectancy"]]),
          expected = 85)

# * * marriages -----------------------------------------------------------
expect_gt(object = min(raw_data[["marriages"]][["Number of marriages"]]),
          expected = 60)
expect_lt(object = max(raw_data[["marriages"]][["Number of marriages"]]),
          expected = 36000)

# * * civil_partnerships --------------------------------------------------
expect_gte(object = min(raw_data[["civil-partnerships"]][["Number of civil partnerships"]]),
          expected = 0)
expect_lt(object = max(raw_data[["civil-partnerships"]][["Number of civil partnerships"]]),
          expected = 1100)

# * * household_estimates -------------------------------------------------
expect_gt(object = min(raw_data[["household-estimates"]][["Number of households"]]),
          expected = 8300)
expect_lt(object = max(raw_data[["household-estimates"]][["Number of households"]]),
          expected = 2500000)

# * * household_projections -----------------------------------------------
expect_gte(object = min(raw_data[["household-projections"]][["Number"]]),
          expected = 0)
expect_lt(object = max(raw_data[["household-projections"]][["Number"]]),
          expected = 1930000)

# * * dwellings -----------------------------------------------------------
expect_gt(object = min(raw_data[["dwellings"]][["Number of dwellings"]]),
          expected = 9000)
expect_lt(object = max(raw_data[["dwellings"]][["Number of dwellings"]]),
          expected = 2650000)

# * * dwellings_by_type ---------------------------------------------------
expect_gte(object = min(raw_data[["dwellings-by-type"]][["Number"]]),
          expected = 0)
expect_lt(object = max(raw_data[["dwellings-by-type"]][["Number"]]),
          expected = 2610000)

# * * dwellings_by_council_tax_band ---------------------------------------
expect_gt(object = min(raw_data[["dwellings-by-council-tax-band"]][["Number"]]),
          expected = 0)
expect_lt(object = max(raw_data[["dwellings-by-council-tax-band"]][["Number"]]),
          expected = 605000)



