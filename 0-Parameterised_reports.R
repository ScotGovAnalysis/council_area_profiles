library(readxl)
library(dplyr)

start_time <- Sys.time()

path <- "data/council-area-profiles-dataset_test.xlsx"

path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = path)  %>%
  list2env(globalenv())

updates %>% 
  as.list() %>%
  list2env(globalenv())

# Test Raw Data  ==============================================================

raw_data <- Filter(function(x) is(x, "data.frame"), mget(ls()))

expected_file_names <-  c("updates",
                           "population-estimates",
                           "population-projections",
                           "nature-of-population-change",
                           "births-by-sex",
                           "standardised-birth-rates",
                           "births-by-age-of-mother",
                           "fertility-rates",
                           "deaths-by-sex",
                           "standardised-death-rates",
                           "deaths-by-sex-by-age",
                           "leading-causes-of-death",
                           "migration",
                           "net-migration",
                           "net-migration-rates",
                           "life-expectancy",
                           "marriages",
                           "civil-partnerships",
                           "household-estimates",
                           "household-projections",
                           "dwelling",
                           "dwellings-by-type",
                           "dwellings-by-council-tax-band")

check_expected_data_files <- function(raw_data) {   #Check for expected data sheets
  if(identical(sort(names(raw_data)), sort(expected_file_names)) == F){
    stop("Unexpected Sheet Names")
  }
}


check_blank_data <- function(raw_data) { #Check for NA's in data
  
  for(df in seq_along(raw_data)){
    if(any(is.na(raw_data[[df]]))){
      warning(paste("Blank Data in:", names(raw_data[df])))
    }
  }
}


check_row_limit <- function(raw_data) { #Check for row limit
  for(df in seq_along(raw_data)){
    if(nrow(raw_data[[df]]) == 1048575){
      warning(paste("Excel Row limit reached:", names(raw_data[df])))
    }
  }
}


check_expected_data_files(raw_data)
check_blank_data(raw_data)
check_row_limit(raw_data)

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