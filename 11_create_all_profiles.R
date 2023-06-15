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

# read in the excel file with all tabs
raw_data = read_CA_data("data/council-area-profiles-dataset.xlsx")

# define the expected shape and characteristics of the data
expectations = set_expectations()

# check whether the raw data matches these expectations
check_expectations(raw_data, expectations)

# There is a later dependency in the Rmd file where having the 
# updates sublist at the top level of the global env is important.
raw_data = list.merge(raw_data, raw_data$updates)
# this is a bandaid and needs to be changed 
# TODO fix this shit

# Create Content =========================================================
# list of Council Areas to produce reports for
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
# # # # # # # # # # # # 
# parallel setup
# # # # # # # # # # # # 

# find the number of cpu cores we have available
n_cores = detectCores()
# create a compute cluster with this resource
cl = makeCluster(n_cores)

# export the data to distribute across the 
# cluster nodes
clusterExport(cl, varlist = c("raw_data"))

# call any specific R commands across the nodes
clusterCall(cl, fun = function(){
  # here we call libraries to make sure the nodes can access tidyverse functions
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

# source custom functions for producing the content across all the nodes
clusterEvalQ(cl, source("functions folder/plot_functions.R" ,local = T))
clusterEvalQ(cl, source("functions folder/table_functions.R" ,local = T))
clusterEvalQ(cl, source("functions folder/text_functions.R" ,local = T))
clusterEvalQ(cl, source("functions folder/produce_CA_content.R" ,local = T))

# # # # # # # # # # # # 
# parallel execution
# # # # # # # # # # # # 

# produce the content for all the areas 
CA_content_status = parLapply(cl, Area, function(CA){
  
  # CA=Area[1]
  
  # produce_CA_content function declared in functions folder/produce_CA_content.R
  CA_data = produce_CA_content(CA, raw_data)
  # TODO add QA steps in to check content 
  # check number of items
  # check for NAs
  
  # write the content to file.
  # the Rmd will load it later
  write_rds(CA_data, file = paste0("temp/",CA_data$area,"-content.rds"))
  
  # assuming nothing went wrong
  # return 1
  return(1)
  
  
  
}
)

# Knit HTML Files =========================================================

parLapply(cl, Area, function(area){
  
  # for debugging with a single CA
  # area = Area[1]
  
  rmarkdown::render("CA_profile.Rmd",
                    output_dir = "output",
                    output_file = paste0(gsub(" ", "-", tolower(area)),
                                         "-council-profile.html"),
                    params = list(area = area),
                    quiet = ifelse(length(area) > 1, TRUE, FALSE))
  
})

stopCluster(cl)

# # # # # # # # # # # # 
# parallel end
# # # # # # # # # # # # 

timed_run = Sys.time() - st

print(paste("Code complete. Run time:",timed_run))
