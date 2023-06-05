library(readxl)
library(tidyverse)
library(purrr)
library(testthat)
library(parallel)
library(rlist)


# 
# readline(paste("Are you following the update instructions?",
#                "\n https://github.com/DataScienceScotland/council_area_profiles",
#                "\n Hit enter to continue:"))

# start a timer 
st = Sys.time()

source("functions folder/source functions.R")

raw_data = read_CA_data("data/council-area-profiles-dataset.xlsx")

expectations = set_expectations()

check_expectations(raw_data, expectations)

# There is a later dependency in the Rmd file where having the 
# updates sublist at the top level of the global env is important.
raw_data = list.merge(raw_data, raw_data$updates)
# this is a bandaid and needs to be changed 
# TODO fix this shit

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


# find the number of cpu cores we have to available
n_cores = detectCores()
# create a compute cluster with this resource
cl = makeCluster(n_cores)

# export all the functions and data to distribute across the 
# cluster nodes
clusterExport(cl, varlist = c("produce_CA_data", 
                              "produce_CA_plots", 
                              "produce_CA_tables",
                              "produce_CA_text",
                              "raw_data"
                              ))
# call any specific R commands across the nodes
# here we call libraries to make sure the nodes can access tidyverse functions
clusterCall(cl, fun = function(){
  library(tidyverse)
  # Plot
  library(ggplot2)
  library(ggrepel)
  library(stringr)
  library(stringi)
  library(scales)
  
  # Tables
  library(reshape2)
  library(kableExtra)
  
  # Text
  library(glue)
  })

CA_data_list = parLapply(cl, Area, function(CA){
  
  
  # CA=Area[1]
  
  CA_data = produce_CA_data(CA, raw_data)
  
  CA_data = produce_CA_plots(CA_data)
  
  CA_data = produce_CA_tables(CA_data)
  
  CA_data = produce_CA_text(CA_data)
  
  write_rds(CA_data, file = paste0("temp/",CA_data$area,".rds"))
  
  return(1)
  
}
)

parLapply(cl, Area, function(area){
  
  # for debugging with a single CA
  # area = Area[1]
  
  rmarkdown::render("31_create_profile_new.Rmd",
                    output_dir = "output",
                    output_file = paste0(gsub(" ", "-", tolower(area)),
                                         "-council-profile.html"),
                    params = list(area = area),
                    quiet = ifelse(length(area) > 1, TRUE, FALSE))
  
})

stopCluster(cl)

timed_run = Sys.time() - st

print(paste("Code complete. Run time:",timed_run))
