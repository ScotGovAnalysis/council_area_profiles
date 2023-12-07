
# This function reads all the tables from the excel sheet and the 
# update schedule. Returns a list of datatables and text encoded dates
# for when the data was last and will be updated
read_CA_data <- function(path) {
  
    # path <- "data/council-area-profiles-dataset.xlsx"
  
  raw <- path %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    purrr::map(read_excel, path = path)
  
  
  # list2env(raw_data, globalenv())
  
  # updates %>%
  #   as.list() %>%
  # list2env(globalenv())
  
  return(raw)
  
}




