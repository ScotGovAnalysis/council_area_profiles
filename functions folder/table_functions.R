# Table: total and % change by year ============================================
table_total_and_perc_change_by_year <- function(dataset, Area_name){
  dataset %>%
    filter(Area == Area_name) %>%
    transform(Year = as.character(Year)) %>%
    mutate(Perc = round(Perc, 1))
}


# Table: Age group by gender  =============================================
table_age_gender_dataset <- function(dataset, Area_name){
  dataset %>%
    filter(Area == Area_name) %>%
    spread(key = Sex, value = Number) %>%
    mutate(`All people` = Female + Male,
           Perc_column = `All people` / sum(`All people`) * 100) %>%
    melt %>%
    dcast(Age_group ~ variable, sum, na.rm = FALSE, margins = "Age_group") %>%
    rename(`Age group` = Age_group) %>%
    mutate(Perc_column = round(Perc_column, 1)) %>%
    mutate(`Age group` = recode(`Age group`, "(all)" = "All people"))
}



# Table: Age group by start and end year with % change =========================
table_age_year_perc_change_1_dataset <- function(dataset, Area_name){
  dataset %>%
    spread(key = Year, value = Number) %>%
    filter(Area == Area_name) %>%
    select(-Area) %>%
    melt %>%
    dcast(Age_group ~ variable, sum, na.rm = FALSE, margins = "Age_group")
}

table_age_year_perc_change_2_dataset <- function(dataset, Area_name, start_year,
                                                 end_year){
  dataset %>%
    mutate(Perc = (dataset[[as.character(end_year)]] -
                     dataset[[as.character(start_year)]]) /
             dataset[[as.character(start_year)]] * 100) %>%
    rename(`Age group` = Age_group) %>%
    mutate(Perc = round(Perc, 1)) %>%
    mutate(`Age group` = recode(`Age group`, "(all)" = "All people"))
}



# Table: Sex by year and % change =================================
table_sex_by_year_perc_change_1_dataset <- function(dataset, Area_name){
  dataset %>%
    spread(key = Year, value = Number) %>%
    filter(Area == Area_name) %>%
    select(-Area) %>%
    melt %>%
    dcast(Sex ~ variable, sum, na.rm = FALSE, margins = "Sex")
}

table_sex_by_year_perc_change_2_dataset <- function(dataset, Area_name,
                                                    start_year, end_year){
  dataset %>%
    mutate(Perc = (dataset[[as.character(end_year)]] -
                     dataset[[as.character(start_year)]]) /
             dataset[[as.character(start_year)]] * 100) %>%
    mutate(Perc = round(Perc, 1)) %>%
    mutate(Sex = recode(Sex, "(all)" = "All people"))
}


# Table: Dwellings by type or tax band with % of total  ===================
table_dwell_by_type_or_tax_dataset <- function(dataset, Area_name){
  dataset %>%
    filter(Area == Area_name) %>%
    select(Type, Number) %>%
    mutate(Perc_column = Number / sum(Number) * 100) %>%
    melt %>%
    dcast(Type ~ variable, sum, na.rm = FALSE, margins = "Type") %>%
    mutate(Perc_column = round(Perc_column, 1)) %>%
    mutate(Type = recode(Type, "(all)" = "Total")) %>%
    select(-Number)
}


# Table: Household projection % change by household type ==================
table_house_proj_by_type_1 <- function(dataset, area_name){
  dataset %>%
    filter(Area == area_name) %>%
    spread(key = Year, value = Number) %>%
    ungroup()
}

table_house_proj_by_type_2 <- function(dataset, area_name, house_proj_start_year, house_proj_end_year){
  dataset %>%
    mutate(Perc = (dataset[[as.character(house_proj_end_year)]] -
                     dataset[[as.character(house_proj_start_year)]]) /
             dataset[[as.character(house_proj_start_year)]] * 100) %>%
    mutate(Perc = round(Perc, 1))
}


# Table: Migration by year by type (in, out, net) =========================
table_mig_by_type <- function(dataset, Area_name){
  dataset %>%
    filter(Area == Area_name) %>%
    spread(key = Type, value = Number) %>%
    select(-Net, everything())
}



# Table: Life expectancy by year by type with % change of estimate ================
table_life_exp <- function(dataset, Area_name, sex) {
  dataset  %>%
    filter(Sex == sex, Area == Area_name) %>%
    spread(key = Type, value = Number) %>%
    mutate(Perc = (Estimate - Estimate[Year == min(Year)]) /
             abs(Estimate[Year == min(Year)]) * 100) %>%
    mutate(Perc = round(Perc, 1),
           Lower_CI = round(Lower_CI, 1),
           Upper_CI = round(Upper_CI, 1),
           Estimate = round(Estimate, 1)) %>%
    mutate(Year = Year_2)
}


# Table: Total marriages by year with %  =====================================
table_total_and_perc_of_pop_by_year <- function(dataset, Area_name){
  dataset %>%
    filter(Area == Area_name) %>%
    select(-Area) %>%
    transform(Year = as.character(Year)) %>%
    rename(Total = Number)
}


# Table: Total number by year by sex =====================================
table_year_by_sex <- function(dataset, Area_name){
  dataset %>%
    filter(Area == Area_name) %>%
    spread(key = Sex, value = Number) %>%
    rename(Total = All) %>%
    transform(Year = as.character(Year)) %>%
    select(-Area)
}


# Table: Total number by year by sex with % change =====================================
table_total_and_sex_perc_change_by_year_ca <- function(dataset, Area_name, bir_dea_marr_est_start_year){
  dataset %>%
    filter(Area == Area_name) %>%
    transform(Year = as.character(Year)) %>%
    spread(key = Sex, value = Number) %>%
    rename(Total = `All people`) %>%
    select(-Total, everything()) %>%
    mutate(Perc = ((Total - Total[Year == bir_dea_marr_est_start_year]) /
                     Total[Year == bir_dea_marr_est_start_year]) * 100) %>%
    mutate(Perc = round(Perc, 1)) %>%
    select(-Area)
}

