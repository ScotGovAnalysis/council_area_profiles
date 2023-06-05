library(readxl)
library(dplyr)
library(purrr)
library(testthat)

path <- "data/council-area-profiles-dataset.xlsx"

raw_data <- path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = path)

list2env(raw_data, globalenv())

# WTF is this doing?
updates %>%
  as.list() %>%
  list2env(globalenv())
# It's a shortcut to get the elements in the 
# updates list, which sits inside raw_data, to the top level
# of the global env so that in the Rmd file
# they don't have to have the prefix "data$"
# this is super hacky and shouldn't be done