readline(paste("Are you following the update instructions?",
               "\n https://github.com/DataScienceScotland/council_area_profiles",
               "\n Hit enter to continue:"))

library(readxl)
library(dplyr)
library(purrr)

start_time <- Sys.time()

path <- "data/council-area-profiles-dataset.xlsx"

raw_data <- path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = path)

source("11-_check_data.R")
  
list2env(raw_data, globalenv())

updates %>% 
  as.list() %>%
  list2env(globalenv())

# Knit HTML documents =========================================================
Area <- c(
  "Aberdeen City",
  "Aberdeenshire",
  "Angus",
  "Argyll and Bute",
  "City of Edinburgh",
  "Clackmannanshire",
  "Dumfries and Galloway",
  "Dundee City",
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
  "South Ayrshire",
  "South Lanarkshire",
  "Stirling",
  "West Dunbartonshire",
  "West Lothian"
  )

pb = txtProgressBar(min = 0, max = length(Area), initial = 0, style = 3)

for (i in seq_along(Area)) {
  rmarkdown::render("12-_create_profile.Rmd",
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