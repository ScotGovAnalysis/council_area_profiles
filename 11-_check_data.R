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

# Values ------------------------------------------------------------
result[["to_compare"]] <- 
  map2(.x = raw_data[names(raw_data) != "updates"],
       .y = expected[["col_values"]][["tibble"]],
       ~ select(.x, names(.y)))

result[["combinations"]] <-
  purrr::map(.x = expected[["col_values"]][["tibble"]],
             .f = expand.grid,
             stringsAsFactors = FALSE) %>% 
  purrr::map(.f = as_tibble)

# * Missing values --------------------------------------------------------
result[["values_missing"]] <- purrr::map2(
  .x = result[["combinations"]],
  .y = result[["to_compare"]],
  .f = anti_join) %>%
  `[`(names(.) != "leading_causes_of_death") %>%
  purrr::discard(~ nrow(.x) == 0)
# We cannot have different combinations of ICD code, Cause, and Cause label.
# Also some rows in this table are suppressed. So we don't check this table for
# missing values.

# * Unexpected values -----------------------------------------------------
result[["values_unexpected"]] <- purrr::map2(
  .x = result[["to_compare"]],
  .y = result[["combinations"]],
  .f = anti_join) %>%
  purrr::discard(~ nrow(.x) == 0)

# * Message ---------------------------------------------------------------
if (length(result[["values_missing"]]) == 0 &&
    length(result[["values_unexpected"]]) == 0) {
  message("Check passed: all values as expected")
} else {
  if (length(result[["values_missing"]]) != 0) {
    message("Check failed: these rows are missing:")
    print(result[["values_missing"]])
  }
  if (length(result[["values_unexpected"]]) != 0) {
    message("Check failed: these rows are unexpected:")
    print(result[["values_unexpected"]])
  }
}



