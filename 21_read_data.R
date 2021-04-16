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

updates %>% 
  as.list() %>%
  list2env(globalenv())