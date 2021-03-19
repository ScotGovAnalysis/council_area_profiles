# Test Raw Data  ==============================================================

# Creates a list of the sheets in the expected order
raw_data <- path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = path)

# Sources list of expected sheet names and the columns within them
source("source/expected_names_and_columns.R")

# Check for expected data sheets
if (identical(names(raw_data), names(expected_names_and_columns)) == F){ 
  stop(paste("Unexpected Sheet Names: ", setdiff(names(raw_data), names(expected_names_and_columns)), "\n"),
       paste("Missing Sheet Names: ", setdiff(names(expected_names_and_columns), names(raw_data)), "\n"))
}

# Check for NA's in data
for(i in seq_along(raw_data)){
  if(any(is.na(raw_data[[i]]))){
    warning(paste("Blank Data in:", names(raw_data[i])))
  }
}

# Check for excels row limit. 
# If it is exactly 1048576 rows it is likely there has been some data loss.
for(i in seq_along(raw_data)){
  if(nrow(raw_data[[i]]) == 1048575){ # 1 less than excel limit because R uses first row as column names
    warning(paste("Excel Row limit reached:", names(raw_data[i])))
  }
}

# Check for expected column names 
for(i in seq_along(raw_data)){
  if(identical(names(raw_data[[i]]), expected_names_and_columns[[i]]) == F){
    warning(paste("Unexpected Column Names", names(raw_data[i])))
  }
}

# Check scotland in Council Areas 
#TODO Find out if scotland is not meant to be in any tables (it's currently in all (except updates))
for(i in seq_along(raw_data)){
  if("Council area" %in% names(raw_data[[i]])){ 
    if(!("Scotland" %in% raw_data[[i]]$'Council area')){
      warning(paste("Scotland expected but missing in:", names(raw_data[i])))
    }
  }
}