set_expectations <- function() {
  # Creating list of expected sheet names and columns within those sheets for data
  # validation
  
  expected <- list()
  
  # Sheet & column names ----------------------------------------------------
  expected[["col_names"]] <-
    list(
      "updates" = c(
        "pop_est_last_update",
        "pop_est_next_update",
        "pop_proj_last_update",
        "pop_proj_next_update",
        "births_last_update",
        "births_next_update",
        "deaths_last_update",
        "deaths_next_update",
        "life_exp_last_update",
        "life_exp_next_update",
        "mig_last_update",
        "mig_next_update",
        "marr_cp_last_update",
        "marr_cp_next_update",
        "house_est_last_update",
        "house_est_next_update",
        "house_proj_last_update",
        "house_proj_next_update",
        "dwell_last_update",
        "dwell_next_update"
      ),
      "population-estimates" = c(
        "Council area",
        "Year",
        "Sex",
        "Age",
        "Population"
      ),
      "population-projections" = c(
        "Council area",
        "Year",
        "Sex",
        "Age",
        "Population"
      ),
      "nature-of-population-change" = c(
        "Council area",
        "2018",
        "2028",
        "Population change",
        "Births",
        "Deaths",
        "Natural change",
        "Net migration"
      ),
      "births-by-sex" = c(
        "Council area",
        "Registration Year",
        "Sex",
        "Number"
      ),
      "standardised-birth-rates" = c(
        "Council area",
        "Registration Year",
        "Standardised birth rate"
      ),
      "births-by-age-of-mother" = c(
        "Council area",
        "Registration Year",
        "Mother age group",
        "Number"
      ),
      "fertility-rates" = c(
        "Council area",
        "Registration Year",
        "Total fertility rate"
      ),
      "deaths-by-sex" = c(
        "Council area",
        "Registration Year",
        "Sex",
        "Number"
      ),
      "standardised-death-rates" = c(
        "Council area",
        "Registration Year",
        "Standardised death rate"
      ),
      "deaths-by-sex-by-age" = c(
        "Council area",
        "Registration Year",
        "Sex",
        "Age group",
        "Number"
      ),
      "leading-causes-of-death" = c(
        "Council area",
        "Year",
        "ICD codes",
        "Cause",
        "Cause label",
        "Number",
        "Percent",
        "Sex",
        "Total deaths"
      ),
      "migration" = c(
        "Council area",
        "Year",
        "Sex",
        "Type",
        "Number"
      ),
      "net-migration" = c(
        "Council area",
        "Year",
        "Sex",
        "Age group",
        "Number"
      ),
      "net-migration-rates" = c(
        "Council area",
        "Year",
        "Rate"
      ),
      "life-expectancy" = c(
        "Council area",
        "Year",
        "Sex",
        "Age group",
        "Type",
        "Life expectancy"
      ),
      "marriages" = c(
        "Council area",
        "Registration Year",
        "Number of marriages"
      ),
      "civil-partnerships" = c(
        "Council area",
        "Registration Year",
        "Sex",
        "Number of civil partnerships"
      ),
      "household-estimates" = c(
        "Council area",
        "Year",
        "Number of households"
      ),
      "household-projections" = c(
        "Council area",
        "Age group",
        "Household type",
        "Year",
        "Number"
      ),
      "dwellings" = c(
        "Council area",
        "Year",
        "Number of dwellings"
      ),
      "dwellings-by-type" = c(
        "Council area",
        "Year",
        "Type",
        "Number"
      ),
      "dwellings-by-council-tax-band" = c(
        "Council area",
        "Year",
        "Council Tax band",
        "Number"
      )
    )
  
  # Column values -----------------------------------------------------------
  # * Council areas ---------------------------------------------------------
  expected[["col_values"]][["council_area"]] <- c(
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
    "Scotland",
    "Scottish Borders",
    "Shetland Islands",
    "South Ayrshire",
    "South Lanarkshire",
    "Stirling",
    "West Dunbartonshire",
    "West Lothian"
  )
  
  # * population_estimates --------------------------------------------------
  expected[["col_values"]][["tibble"]][["population_estimates"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = as.numeric(1998:2021), # this needs changed every year
    Sex = c("Female", "Male"),
    Age = as.numeric(0:90)
  )
  
  # * population_projections ------------------------------------------------
  expected[["col_values"]][["tibble"]][["population_projections"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = as.numeric(2018:2043),
    Sex = c("Female", "Male"),
    Age = as.numeric(0:90)
  )
  
  # * nature_of_population_change -------------------------------------------
  expected[["col_values"]][["tibble"]][["nature_of_population_change"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]]
  )
  
  
  # * births_by_sex ---------------------------------------------------------
  expected[["col_values"]][["tibble"]][["births_by_sex"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(1991:2021), # this needs changed every year
    Sex = c("All people",
            "Female",
            "Male")
  )
  
  # * standardised_birth_rates ---------------------------------------------
  expected[["col_values"]][["tibble"]][["standardised_birth_rates"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(1991:2021) # this needs changed every year
  )
  
  # * births_by_age_of_mother -----------------------------------------------
  expected[["col_values"]][["tibble"]][["births_by_age_of_mother"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(1991:2021), # this needs changed every year
    `Mother age group` = c(
      "0 to 19",
      "20 to 24",
      "25 to 29",
      "30 to 34",
      "35 to 39",
      "40 and over",
      "All ages",
      "Not stated"
    )
  )
  
  # * fertility_rates -------------------------------------------------------
  expected[["col_values"]][["tibble"]][["fertility_rates"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(1991:2021) # this needs changed every year
  )
  
  # * deaths_by_sex ---------------------------------------------------------
  expected[["col_values"]][["tibble"]][["deaths_by_sex"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(1991:2021), # this needs changed every year
    Sex = c("All people",
            "Female",
            "Male")
  )
  
  # * standardised_death_rates ----------------------------------------------
  expected[["col_values"]][["tibble"]][["standardised_death_rates"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(1991:2021) # this needs changed every year
  )
  
  # * deaths_by_sex_by_age --------------------------------------------------
  expected[["col_values"]][["tibble"]][["deaths_by_sex_by_age"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(2021), # this needs changed every year
    Sex = c("All people",
            "Females",
            "Males"),
    `Age group` = c(
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
      "45 to 49",
      "50 to 54",
      "55 to 59",
      "60 to 64",
      "65 to 69",
      "70 to 74",
      "75 to 79",
      "80 to 84",
      "85 to 89",
      "90+",
      "All ages"
    )
  )
  
  # * leading_causes_of_death -----------------------------------------------
  expected[["col_values"]][["tibble"]][["leading_causes_of_death"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = as.numeric(2021), # this needs changed every year
    `ICD codes` = c(
      "I20-I25",
      "C33-C34",
      "F01, F03, G30",
      "J40-J47",
      "I60-I69",
      "C61",
      "C81-C96",
      "C18-C21",
      "X40-X49",
      "C50",
      "J09-J18"
    ),
    Cause = c(
      "Ischaemic heart diseases",
      "Malignant neoplasm of trachea, bronchus and lung",
      "Dementia and Alzheimer Disease",
      "Chronic lower respiratory diseases",
      "Cerebrovascular disease",
      "Malignant neoplasm of prostate",
      "Malignant neoplasms, stated or presumed to be primary, of lymphoid, haematopoietic and related tissue",
      "Malignant neoplasm of colon, sigmoid, rectum and anus",
      "Accidental poisoning",
      "Malignant neoplasm of breast",
      "Influenza and pneumonia"
    ),
    `Cause label` = c(
      "ischaemic heart diseases",
      "lung cancer",
      "dementia and Alzheimer's disease",
      "chronic lower respiratory diseases",
      "cerebrovascular disease",
      "prostate cancer",
      "Malignant neoplasms of lymphoid, hematopoietic and related tissue",
      "bowel cancer",
      "accidental poisoning",
      "breast cancer",
      "influenza and pneumonia"
    ),
    Sex = c("Female",
            "Male")
  )
  
  
  # * migration -------------------------------------------------------------
  expected[["col_values"]][["tibble"]][["migration"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = paste0(2001:2020,  # this needs changed every year 2001:lastyear
                  "-",
                  stringr::str_sub(2002:2021, start = 3, end = 4)),  # this needs changed every year 2002:thisyear
    Sex = c("Female",
            "Male",
            "Persons"),
    Type = c("In", "Net", "Out")
  )
  
  
  # * net_migration ---------------------------------------------------------
  expected[["col_values"]][["tibble"]][["net_migration"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = paste0(2001:2020,# this needs changed every year 2001:lastyear
                  "-",
                  stringr::str_sub(2002:2021, start = 3, end = 4)), # this needs changed every year 2002:thisyear
    Sex = c("Female",
            "Male"),
    `Age group` = c(
      "0 to 4",
      "5 to 9",
      "10 to 14",
      "15 to 19",
      "20 to 24",
      "25 to 29",
      "30 to 34",
      "35 to 39",
      "40 to 44",
      "45 to 49",
      "50 to 54",
      "55 to 59",
      "60 to 64",
      "65 to 69",
      "70 to 74",
      "75 to 79",
      "80 to 84",
      "85 to 89",
      "90 and over",
      "All ages"
    )
  )
  
  # * net_migration_rates ---------------------------------------------------
  expected[["col_values"]][["tibble"]][["net_migration_rates"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = paste0(2008:2020,# this needs changed every year 2001:lastyear
                  "-",
                  stringr::str_sub(2009:2021, start = 3, end = 4))# this needs changed every year 2002:thisyear
  )
  
  # * life_expectancy -------------------------------------------------------
  expected[["col_values"]][["tibble"]][["life_expectancy"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = paste0(2001:2018,# this needs changed every year
                  "-",
                  stringr::str_sub(2003:2020, start = 3, end = 4)),# this needs changed every year
    Sex = c("Female",
            "Male"),
    `Age group` = c(
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
      "45 to 49",
      "50 to 54",
      "55 to 59",
      "60 to 64",
      "65 to 69",
      "70 to 74",
      "75 to 79",
      "80 to 84",
      "85 to 89",
      "90+" # This replaces "90 and over" in life-expectancy starting with 2016-18
    ),
    Type = c(
      "Estimate",
      "Lower_CI",
      "Upper_CI"
    )
  )
  
  
  # * marriages -------------------------------------------------------------
  expected[["col_values"]][["tibble"]][["marriages"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(1991:2020)# this needs changed every year
  )
  
  
  # * civil_partnerships ----------------------------------------------------
  expected[["col_values"]][["tibble"]][["civil_partnerships"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Registration Year` = as.numeric(2005:2020),# this needs changed every year
    Sex = c(
      "All",
      "Female",
      "Male"
    )
  )
  
  # * household_estimates ---------------------------------------------------
  expected[["col_values"]][["tibble"]][["household_estimates"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = as.numeric(2001:2022)# this needs changed every year
  )
  
  # * household_projections -------------------------------------------------
  expected[["col_values"]][["tibble"]][["household_projections"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    `Age group` = c(
      "16 to 19",
      "20 to 24",
      "25 to 29",
      "30 to 34",
      "35 to 39",
      "40 to 44",
      "45 to 49",
      "50 to 54",
      "55 to 59",
      "60 to 64",
      "65 to 69",
      "70 to 74",
      "75 to 79",
      "80 to 84",
      "85 to 89",
      "90 and over",
      "All ages"
    ),
    `Household type` = c(
      "1 adult 1 child",
      "1 adult 2+ children",
      "1 person female",
      "1 person male",
      "2 person all adult",
      "2+ adult 1+ children",
      "3+ person all adult",
      "All households",
      "Non-heads"
    ),
    Year = as.numeric(2018:2043)
  )
  
  
  # * dwellings -------------------------------------------------------------
  expected[["col_values"]][["tibble"]][["dwellings"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = as.numeric(2001:2022)# this needs changed every year
  )
  
  
  # * dwellings_by_type -----------------------------------------------------
  expected[["col_values"]][["tibble"]][["dwellings_by_type"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = as.numeric(2017),
    Type = c(
      "Detached",
      "Flat",
      "Not known",
      "Semi-detached",
      "Terrace",
      "Total"
    )
  )
  
  # * dwellings_by_council_tax_band -----------------------------------------
  expected[["col_values"]][["tibble"]][["dwellings_by_council_tax_band"]] <- list(
    `Council area` = expected[["col_values"]][["council_area"]],
    Year = as.numeric(2022),
    `Council Tax band` = LETTERS[1:8]
  )
  
  return(expected)
}
