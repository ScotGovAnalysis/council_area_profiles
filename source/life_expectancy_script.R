library(tidyverse)
library(readxl)

file_path <- "data/life-expectancy/life-expectancy-17-19-tab3.xlsx"

# Read in the sheets as a list --------------------------------------------

sheets <-
  file_path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = file_path)

# Transforming the sheets --------------------------------------------------------

sheets <- sheets[-c(1, 2)] # removing the metadata sheets

transposed_sheets <- lapply(sheets, function(x) {
  t(x) %>%  # Transposing the columns and rows so that The variables are in columns not rows.
    zoo::na.locf() %>% # Replacing each NA with the most recent non-NA prior to it.
    as.data.frame() %>% # Converting matrix to dataframe
    janitor::remove_empty("cols") # Removing columns that are empty
})

for (i in seq_along(transposed_sheets)) {
  row.names(transposed_sheets[[i]]) <-
    NULL # Remove row names left over from transpose
}

clean_sheets <- lapply(transposed_sheets, function(x)
  x %>%
    select(1:161) %>% # 161 is where the data stops and it becomes footnotes
    slice(-1)) # Row one is the age bands but these are added as headings later

names <-
  c(
    # creating a list of column names so that rbind can work on matching column names
    "council_area",
    "sex",
    "type",
    "0",
    "1 to 4",
    "5 to 9",
    "10 to 14",
    "15 to 19",
    "20 to 24",
    "25 to 29",
    "30 to 34",
    "35 to 39",
    "40 to 44",
    "45 to 59",
    "50 to 54",
    "55 to 59",
    "60 to 64",
    "65 to 69",
    "70 to 74",
    "75 to 79",
    "80 to 84",
    "85 to 89",
    "90 and over"
  )

final_clean <- list() # Empty list to add the new dataframes into

for (i in seq_along(clean_sheets)) {
  #TODO maybe this could be a list instead of 8 new objects
  
  # The tables are in multiples of 23 so every 23 columns is a new table (each tables contains 4 council areas in alphabetical order)
  x1 <- clean_sheets[[i]] %>%
    select(1:23)
  colnames(x1) <-
    names #adding names so that all subsets match for the rbind()
  
  x2 <- clean_sheets[[i]] %>%
    select(24:46)
  colnames(x2) <- names
  
  x3 <- clean_sheets[[i]] %>%
    select(47:69)
  colnames(x3) <- names
  
  x4 <- clean_sheets[[i]] %>%
    select(70:92)
  colnames(x4) <- names
  
  x5 <- clean_sheets[[i]] %>%
    select(93:115)
  colnames(x5) <- names
  
  x6 <- clean_sheets[[i]] %>%
    select(116:138)
  colnames(x6) <- names
  
  x7 <- clean_sheets[[i]] %>%
    select(139:161)
  colnames(x7) <- names
  
  all_combined <- x1 %>%
    rbind(x2, x3, x4, x5, x6, x7) %>%  # Take each subset of the data and combining into one dataframe (The data appears as horizontal tables, this turns it into one long table)
    mutate(year = names(clean_sheets[i])) %>% # Add in year column taken from sheet name
    gather("0":"90 and over", key = "age", value = "life_expectancy") %>% # Combine age columns into one
    distinct() # Remove duplicate rows (West Lothian, being at the end of the list, has dupes from zoo::na.locf() )
  
  final_clean[i] <-
    list(all_combined) # Create a new list of all the cleaned data for each year range
}

life_expectancy_list = list()

for (i in seq_along(final_clean)) {
  life_expectancy_list[[i]] <- final_clean[[i]] # Lists all cleaned sheets
}

# Final dataframe creation ------------------------------------------------

# Tidy the values to match CA_Profile format
life_expectancy <-  do.call(rbind, life_expectancy_list) %>%
  mutate(
    council_area = str_replace(council_area, pattern = "1", ""),# Some areas had a 1 from footnotes in the excel file
    type = str_replace(type, pattern = "exo", "Estimate"),
    type = str_replace(type, pattern = "upper CI", "Upper CI"),
    type = str_replace(type, pattern = "lower CI", "Lower CI"),
    year = str_replace(year, pattern = "x", ""),
    year = str_replace(year, pattern = "_20", "-"),
    year = str_replace(year, pattern = "_19", "-"),
    sex = str_replace(sex, pattern = "s", "")
  ) 

# Checks ------------------------------------------------------------------

# Check number of rows are as epected
council_areas = 33
sex = 2
type = 3
age_bands = 20

if (as.numeric(count(life_expectancy)) / length(clean_sheets) != council_areas *
    sex * type * age_bands) {
  stop("Unexpected row count in life_expectancy")
} else {
  "Row count OK"
} 
