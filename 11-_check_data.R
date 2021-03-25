# READ ME #####################################################################
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

# Missing values ----------------------------------------------------------
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


# Values ------------------------------------------------------------------
# To do: expand these checks to the rest of the datasets
check_values <- function(values_actual, values_expected) {
  map2(.x = values_actual,
       .y = values_expected,
       ~ setequal(.x, .y))
}

result[["values"]] <- map2(.x = raw_data[c("population-estimates",
                                           "population-projections",
                                           "nature-of-population-change")],
                           .y = expected[["col_values"]][["tibble"]][1:3],
                           ~ select(.x, names(.y))) %>%
  map(~ map(.x = ., unique)) %>%
  map2(.y = expected[["col_values"]][["tibble"]][1:3],
       .f = check_values)

result[["values_message"]] <- result[["values"]] %>%
  discard( ~ all(as.logical(.))) %>%
  map( ~ discard(., ~ .)) %>%
  map( ~ names(.))

if (result[["values"]] %>%
    flatten() %>%
    as.logical() %>%
    all()) {
  message("Check passed: all values as expected")
} else {
  warning("Check failed: these columns have missing and/or unexpected values:",
          immediate. = TRUE)
  result[["values_message"]]
}


