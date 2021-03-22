# READ ME #####################################################################
# AUTHORS:            Rhiannon.Batstone@nrscotland.gov.uk
#                     joseph.adams@nrscotland.gov.uk
# PURPOSE OF SCRIPT:  Check data for Council Area Profiles
###############################################################################

# Basics ------------------------------------------------------------------
source("source/expected_names_and_columns.R")

# Sheet and column names --------------------------------------------------
sheet_names_correct <-
  identical(names(raw_data), names(expected_names_and_columns))

if(identical(lapply(raw_data, names), expected_names_and_columns)) {
  message("Check passed: sheet and column names are correct")
  
  # First check if sheet names are correct
} else if (isFALSE(sheet_names_correct)) {
  warning(
    paste("Check failed: unexpected Sheet Names: ", setdiff(
      names(raw_data), names(expected_names_and_columns)
    ), "\n"),
    paste("Check failed: missing Sheet Names: ", setdiff(
      names(expected_names_and_columns), names(raw_data)
    ), "\n")
  )
  
  # If sheet names are correct then check column names
} else if (sheet_names_correct) {
  unexpected_col_names <-
    purrr::map2(lapply(raw_data, names),
                expected_names_and_columns,
                setdiff) %>%
    compact()
  
  missing_col_names <-
    purrr::map2(expected_names_and_columns,
                lapply(raw_data, names),
                setdiff) %>%
    compact()
  
  warning(paste("Check failed: unexpected column names (above)",str(unexpected_col_names)),
          immediate. = TRUE)
  warning(paste("Check failed: missing column names (above)", str(missing_col_names)),
          immediate. = TRUE)
}

# Realistic row limit -----------------------------------------------------
# If the row limit is exactly 1,048,576 rows (the maximum for Excel) it is
# likely there has been some data loss
data_sets_with_max_row_limit <- lapply(raw_data, nrow) == 1048575

if (any(data_sets_with_max_row_limit)) {
  warning(
    paste(
      "Check failed: these datasets are at the maximum row limit for Excel (suggesting possible data loss):",
      paste(names(data_sets_with_max_row_limit[data_sets_with_max_row_limit]), collapse = ", ")
    )
  )
} else {
  message("Check passed: all datasets are under the maximum row limit for Excel")
}

# No NAs ------------------------------------------------------------------
data_sets_with_NA <-
  sapply(
    raw_data,
    FUN = function(x)
      any(is.na(x))
  )

if (any(data_sets_with_NA)) {
  warning(paste("Check failed: there are NAs in:", paste(names(
    data_sets_with_NA[data_sets_with_NA]
  ), collapse = ", ")))
} else {
  message("Check passed: no NAs found")
}

# Scotland in Council Areas -----------------------------------------------
#TODO Find out if scotland is not meant to be in any tables (it's currently in
#all (except updates))
for(i in seq_along(raw_data)){
  if("Council area" %in% names(raw_data[[i]])){ 
    if(!("Scotland" %in% raw_data[[i]]$'Council area')){
      warning(paste("Scotland expected but missing in:", names(raw_data[i])))
    }
  }
}



