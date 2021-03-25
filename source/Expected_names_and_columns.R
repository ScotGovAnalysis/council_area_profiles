# Creating list of expected sheet names and columns within those sheets for data
# validation

expected <- list()

# Names of sheets and columns ---------------------------------------------
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
      "deaths_next_update" ,
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

# Values ------------------------------------------------------------------

# Values in MULTIPLE datasets ---------------------------------------------
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

# Values in SINGLE datasets -----------------------------------------------
# Values in SINGLE datasets population_estimates --------------------------
expected[["col_values"]][["tibble"]][["population_estimates"]] <- list(
  `Council area` = expected[["col_values"]][["council_area"]],
  Year = as.numeric(1998:2019),
  Sex = c("Female", "Male"),
  Age = as.numeric(0:90)
)

# Values in SINGLE datasets population_projections ------------------------
expected[["col_values"]][["tibble"]][["population_projections"]] <- list(
  `Council area` = expected[["col_values"]][["council_area"]],
  Year = as.numeric(2018:2043),
  Sex = c("Female", "Male"),
  Age = as.numeric(0:90)
)

# Values in SINGLE datasets nature_of_population_change -------------------
expected[["col_values"]][["tibble"]][["nature_of_population_change"]] <- list(
  `Council area` = expected[["col_values"]][["council_area"]]
)

# Values in SINGLE datasets births_by_age_of_mother -----------------------
expected[["col_values"]][["tibble"]][["births_by_age_of_mother"]] <- list(
  mother_age_group = c(
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

# Values in SINGLE datasets deaths_by_sex_by_age --------------------------
expected[["col_values"]][["tibble"]][["deaths_by_sex_by_age"]] <- list(
  age_group = c(
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
    "90 and over",
    "All ages"
  )
)

# Values in SINGLE datasets net_migration ---------------------------------
expected[["col_values"]][["tibble"]][["net_migration"]] <- list(
  age_group = c(
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

# Values in SINGLE datasets life_expectancy -------------------------------
expected[["col_values"]][["tibble"]][["life_expectancy"]] <- list(
  age_group = c(
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
  )
)

# Values in SINGLE datasets household_projections -------------------------
expected[["col_values"]][["tibble"]][["household_projections"]] <- list(
  age_group = c(
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
  )
)
