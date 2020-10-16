library(tidyr)
library(readxl)

path <- "data/council-area-profiles-dataset.xlsx"

sheet_names <- excel_sheets(path = path)

sheets <- sheet_names %>% 
  purrr::set_names() %>%
  purrr::map(read_excel, path = path)

expected_sheet_names <-  c("updates",
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
                  "dwellings",
                  "dwellings-by-type",
                  "dwellings-by-council-tax-band")



#TODO SEPARATE test, descriptive variable names,
#TODO check for column names, column classes, google common errors
#TODO does/should it include scotland?

check_data <- function(a, b) { 

  #Check for expected data sheets
if(identical(sort(a), sort(expected_sheet_names)) == F){
  stop("Unmatched Sheet Names")
} 
  
  #Check for NAs
  # # Problem with this is it will always stop even if there is intentional blanks
   for(tibble in b){
     if(any(is.na(i))){
     stop(paste("Warning: Blank Data", i, which(is.na(i))))
     }
     }
  
  #Check for row limit
   for(i in b){
     if(rowSums(as_tibble(i)) == 1048576){
       stop("Row limit reached")
   }
   }
  }
  



check_data(sheet_names, sheets)
  
  
  
  
  