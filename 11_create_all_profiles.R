readline(paste("Are you following the update instructions?",
               "\n https://github.com/DataScienceScotland/council_area_profiles",
               "\n Hit enter to continue:"))

source("21_read_data.R")
source("22_set_expectations.R")
source("23_check_expectations.R")

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
  rmarkdown::render("31_create_profile.Rmd",
                    output_dir = "output",
                    output_file = paste0(gsub(" ", "-", tolower(Area[[i]])),
                                         "-council-profile.html"),
                    params = list(area = Area[[i]]),
                    quiet = ifelse(length(Area) > 1, TRUE, FALSE))
  setTxtProgressBar(pb, i)
}
