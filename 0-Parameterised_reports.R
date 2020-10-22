library(readxl)
library(dplyr)

start_time <- Sys.time()

path <- "data/council-area-profiles-dataset.xlsx"

path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = path) %>%
  list2env(globalenv())

updates %>% 
  as.list() %>%
  list2env(globalenv())

# Test Raw Data  ==============================================================

# Creates a list of the sheets in the expected order
raw_data <- path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = path)

# Sources list of expected sheet names and the columns within them
source("sourced/expected_names_and_columns.R")



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

# Knit HTML documents =========================================================
Area <- c(
  
  "Aberdeen City",
  "Aberdeenshire",
  "Angus",
  "Argyll and Bute",
  "City of Edinburgh",
  "Clackmannanshire",
  "Dumfries and Galloway",
  "East Ayrshire",
  "East Dunbartonshire",
  "East Lothian",
  "East Renfrewshire",
  "Falkirk",
  "Fife",
  "Glasgow City",
  "Highland",
  "Inverclyde",
  "Midlothian",
  "Moray",
  "Na h-Eileanan Siar",
  "North Ayrshire",
  "North Lanarkshire",
  "Orkney Islands",
  "Perth and Kinross",
  "Renfrewshire",
  "Scottish Borders",
  "Shetland Islands",
  "South Lanarkshire",
  "West Dunbartonshire",
  "West Lothian"
  
  )

pb = txtProgressBar(min = 0, max = length(Area), initial = 0, style = 3)

for (i in seq_along(Area)) {
  rmarkdown::render("1-Report.Rmd",
                    output_dir = "output",
                    output_file = paste0(gsub(" ", "-", tolower(Area[[i]])),
                                         "-council-profile.html"),
                    params = list(area = Area[[i]]),
                    quiet = ifelse(length(Area) > 1, TRUE, FALSE))
  setTxtProgressBar(pb, i)
}
end_time <- Sys.time()

print(run_time <- end_time - start_time)

# All 32 council areas ========================================================
# "Aberdeen City",
# "Aberdeenshire",
# "Angus",
# "Argyll and Bute",
# "City of Edinburgh",
# "Clackmannanshire",
# "Dumfries and Galloway",
# "Dundee City",
# "East Ayrshire",
# "East Dunbartonshire",
# "East Lothian",
# "East Renfrewshire",
# "Falkirk",
# "Fife",
# "Glasgow City",
# "Highland",
# "Inverclyde",
# "Midlothian",
# "Moray",
# "Na h-Eileanan Siar",
# "North Ayrshire",
# "North Lanarkshire",
# "Orkney Islands",
# "Perth and Kinross",
# "Renfrewshire",
# "Scottish Borders",
# "Shetland Islands",
# "South Ayrshire",
# "South Lanarkshire",
# "Stirling",
# "West Dunbartonshire",
# "West Lothian"