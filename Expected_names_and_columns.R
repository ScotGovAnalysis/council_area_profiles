#Creating list of expected sheet names and columns within those sheets for data validation in 0-Parameterised_reports.R

expected_names_and_columns <-
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