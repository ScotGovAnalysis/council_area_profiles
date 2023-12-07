# Start and end years for each chapter

# Population estimates
pop_est_start_year <- min(`population-estimates`[["Year"]])
pop_est_end_year <- max(`population-estimates`[["Year"]])

# Population projections
pop_proj_start_year <- min(`population-projections`[["Year"]])
pop_proj_end_year <- min(`population-projections`[["Year"]]) + 10

# Births, deaths, marriages and civil partnerships
bir_dea_marr_est_start_year <- max(`standardised-birth-rates`[["Registration Year"]]) - 20
bir_dea_marr_cp_est_end_year <- max(`standardised-birth-rates`[["Registration Year"]])
cp_start_year <- min(`civil-partnerships`[["Registration Year"]])
cp_end_yr <- max(`civil-partnerships`[["Registration Year"]])

# Household estimates
house_est_start_year <- min(`household-estimates`[["Year"]])
house_est_end_year <- max(`household-estimates`[["Year"]])

# Dwellings estimates
dwell_est_start_year <- min(`dwellings`[["Year"]])
dwell_est_end_year <- max(`dwellings`[["Year"]])
dwell_type_end_year <- max(`dwellings-by-type`[["Year"]])

# Household projections
house_proj_start_year <- min(`household-projections`[["Year"]])
house_proj_end_year <- min(`household-projections`[["Year"]]) + 10


# Migration
mig_start_year <- min(`migration`[["Year"]])
mig_rate_start_year <- min(`net-migration-rates`[["Year"]])
mig_end_year <- max(`migration`[["Year"]])
mig_mid_year <- as.numeric(stringr::str_sub(max(`migration`[["Year"]]), start = 1, end = 4)) + 1
mig_penultimate_year <- min(tail(unique(`migration`[["Year"]]), n = 2))

# Life expectancy
life_exp_start_year <- min(`life-expectancy`[["Year"]])
life_exp_end_year <- max(`life-expectancy`[["Year"]])


# Colour codes ==========================================================================

source("content scripts/colour and formatting.R")


# Dataset preparation ====================================================================


# Population estimates ===================================================================

# Mutate initial population-estimates dataset to rename variables, define age groups and define factors
pop_est <- mutate(`population-estimates`,
                  Age_group = cut(Age, c(-1, 15, 24, 44, 64, 74, 130),
                                  labels = c("0 to 15", "16 to 24", "25 to 44",
                                             "45 to 64", "65 to 74",
                                             "75 and over")),
                  Area = `Council area`,
                  Number = Population) %>%
  mutate(Age_group = factor(Age_group, levels = c("All ages", "0 to 15",
                                                  "16 to 24", "25 to 44",
                                                  "45 to 64", "65 to 74",
                                                  "75 and over"))) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))

# Total population estimates for each council area and for each year (all ages and sexes)
total_pop_est <- pop_est %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Define a vector the same length as the number of rows of total_pop_est. Repeat the start year population 
# estimate for each council area once for each year. e.g. repeat 20 times for a 20 year period between start
# and end year.
base_pop_est <- rep(filter(total_pop_est, Year == min(Year))$Number,
                    each = pop_est_end_year - pop_est_start_year + 1)

# Use base_pop_est to compute the percentage change from the start year for each council area for each year
total_pop_est <- mutate(total_pop_est,
                        Perc = (Number - base_pop_est) / base_pop_est * 100)

# Population estimates for each year for paramsarea only
total_pop_est_CA <- total_pop_est %>%
  filter(Area == paramsarea)

# Population estimates for each year for all council areas and excluding Scotland
total_pop_est_all_CA <- total_pop_est %>%
  filter(Area != "Scotland")

# For use in R Markdown text. Rank the changes in population and rank the percentage changes. 
# 1 = highest/largest change.
total_pop_est_end_year <- total_pop_est %>%
  filter(Area != "Scotland", Year == pop_est_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           total_pop_est$Number[total_pop_est$Year == (pop_est_end_year - 1) &
                                  total_pop_est$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal"))) %>%
  mutate(Rank_change = min_rank(desc(Perc)))


# For use with population pyramid. 
# Population estimates for paramsarea only for end year only by sex by single year of age.
total_pop_est_gender <- pop_est %>%
  group_by(Area, Year, Sex, Age) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year == pop_est_end_year, Area == paramsarea) %>%
  ungroup()

# Population estimates for paramsarea only for start year and end year only by sex by single year of age.
total_pop_est_gender_comp <- pop_est %>%
  group_by(Area, Year, Sex, Age) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_est_start_year, pop_est_end_year),
         Area == paramsarea) %>%
  ungroup()

# Population estimates for Scotland only for end year only by sex by single year of age.
total_pop_est_gender_scotland <- pop_est %>%
  group_by(Area, Year, Sex, Age) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year == pop_est_end_year, Area == "Scotland") %>%
  ungroup()

# For use with age group by sex bar plot. 
# Population estimates for each council area and Scotland and for each year, by age group by sex
total_pop_est_age_sex_prelim <- pop_est %>%
  group_by(Area, Year, Sex, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Population estimates for paramsarea only and for end year only, by age group by sex
total_pop_est_age_sex <- total_pop_est_age_sex_prelim %>%
  filter(Year == pop_est_end_year, 
         Area == paramsarea
  )

# For use with automated text. 
# For each age group, is female population larger than male population?
total_pop_est_age_sex_comp <- total_pop_est_age_sex %>%
  spread(key = Sex, value = Number) %>%
  mutate(Comparison = ifelse(Female > Male, 1, 0)) %>%
  mutate(Total = Female + Male)

# For use with % change by age group bar plot
total_pop_est_age_perc <- pop_est %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_est_start_year, pop_est_end_year),
         Area == paramsarea) %>%
  ungroup() %>%
  spread(key = Year, value = Number)

# For paramsarea, compute percentage change between start year and end year, for each age group.
# Define variable to state whether the percentage change is positive, negative or equal.
total_pop_est_age_perc <- total_pop_est_age_perc %>%
  mutate(Perc = (total_pop_est_age_perc[[as.character(pop_est_end_year)]] -
                   total_pop_est_age_perc[[as.character(pop_est_start_year)]]) /
           total_pop_est_age_perc[[as.character(pop_est_start_year)]] * 100,
         Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal")))



# Population projections data ============================================================

# Mutate initial population-projections dataset to rename variables, define age groups and define factors
pop_proj <- mutate(`population-projections`,
                   Age_group = cut(Age, c(-1, 15, 24, 44, 64, 74, 130),
                                   labels = c("0 to 15", "16 to 24", "25 to 44",
                                              "45 to 64", "65 to 74",
                                              "75 and over")),
                   Area = `Council area`,
                   Number = Population) %>%
  filter(Year >= pop_proj_start_year, Year <= pop_proj_end_year) %>%
  mutate(Age_group = factor(Age_group, levels = c("All ages", "0 to 15",
                                                  "16 to 24", "25 to 44",
                                                  "45 to 64", "65 to 74",
                                                  "75 and over"))) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))

# Total population projections for each council area and for each year (all ages and sexes)
total_pop_proj <- pop_proj %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Define a vector the same length as the number of rows of total_pop_proj. Repeat the start year population 
# projection for each council area once for each year. e.g. repeat 20 times for a 20 year period between start
# and end year.
base_pop_proj <- rep(filter(total_pop_proj, Year == min(Year))$Number,
                     each = pop_proj_end_year - pop_proj_start_year + 1)

# Use base_pop_proj to compute the percentage change from the start year for each council area for each year
total_pop_proj <- mutate(total_pop_proj,
                         Perc = (Number - base_pop_proj) / base_pop_proj * 100)

# Population projections for each year for paramsarea only
total_pop_proj_CA <- total_pop_proj %>%
  filter(Area == paramsarea)

# Population projections for each year for all council areas and excluding Scotland
total_pop_proj_all_CA <- total_pop_proj %>%
  filter(Area != "Scotland")

# For use in R Markdown text. Rank the changes in population for each CA (not Scotland) and rank the 
# percentage changes. 1 = highest/largest change.
total_pop_proj_end_year <- total_pop_proj %>%
  filter(Area != "Scotland", Year == pop_proj_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           total_pop_proj$Number[total_pop_proj$Year == pop_proj_start_year &
                                   total_pop_proj$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal"))) %>%
  mutate(Rank_change = min_rank(desc(Perc)))

# Population projections for paramsarea only for start year and end year only by sex by single year of age.
total_pop_proj_sex <- pop_proj %>%
  group_by(Area, Year, Sex, Age) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_proj_start_year, pop_proj_end_year),
         Area == paramsarea) %>%
  ungroup()

# For use with start and end year by age group bar plot. 
# Population projections for paramsarea and for start year and end year only, by age group
total_pop_proj_age_year <- pop_proj %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_proj_start_year, pop_proj_end_year),
         Area == paramsarea) %>%
  ungroup()

# For use with % change between start and end year by age group bar plot
# Change layout of total_pop_proj_age_year above.
# Then, for paramsarea, compute percentage change between start year and end year, for each age group.
# Define variable to state whether the percentage change is positive, negative or equal and rank the % changes.
total_pop_proj_age_perc <- total_pop_proj_age_year %>%
  spread(key = Year, value = Number)

total_pop_proj_age_perc <- total_pop_proj_age_perc %>%
  mutate(Perc = (total_pop_proj_age_perc[[as.character(pop_proj_end_year)]] -
                   total_pop_proj_age_perc[[as.character(pop_proj_start_year)]]) /
           total_pop_proj_age_perc[[as.character(pop_proj_start_year)]] * 100,
         Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal"))) %>%
  group_by(Sign) %>%
  mutate(Rank_change = min_rank(desc(abs(Perc))))




# Population projections - Nature of change

# Mutate initial nature-of-population-change dataset to rename variables and deselect certain unnecessary columns
pop_proj_nature <- rename(`nature-of-population-change`,
                          Area = `Council area`,
                          Natural_change = `Natural change`,
                          Net_migration = `Net migration`,
                          Population_change = `Population change`) %>%
  select(-Births, -Deaths)

# Compute % changes between start and end year for natural change, net migration, population and total change
pop_proj_nature <- pop_proj_nature %>%
  mutate(Natural_perc = (Natural_change /
                           pop_proj_nature[[as.character(pop_proj_start_year)]]) * 100,
         Mig_perc = (Net_migration /
                       pop_proj_nature[[as.character(pop_proj_start_year)]]) * 100,
         Total_perc = (Population_change /
                         pop_proj_nature[[as.character(pop_proj_start_year)]]) * 100)

# Percentage change for natural change, net migration and population, for paramsarea and Scotland only.
# For each, define whether the change is positive, negative or equal.
pop_proj_nature_perc_plot <- pop_proj_nature %>%
  select(Area, Natural_perc, Mig_perc, Total_perc) %>%
  gather(key = Type, value = Perc, -Area) %>%
  mutate(Type = factor(Type,
                       levels = c("Natural_perc", "Mig_perc", "Total_perc"))) %>%
  arrange(Type) %>%
  group_by(Area) %>%
  filter(Area %in% c(paramsarea, "Scotland")) %>%
  ungroup() %>%
  mutate(Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal")))





# Migration data =========================================================================


# In-out migration

# Mutate initial migration dataset to rename variables and deselect certain unnecessary columns
# Because Year is a string e.g. "2014-16" rather than numeric, we define this to be Year_2 instead and only  
# use it after we have produced the plots, to re-label the axes with the string values. 
# We define Year to be the last two digits of Year_2, so it is numeric and can be easily used for plots.
data_mig <- mutate(`migration`, Area = `Council area`, Year_2 = Year) %>%
  select(-Year) %>%
  mutate(Year = as.numeric(substr(Year_2, nchar(Year_2) - 1, nchar(Year_2))))

# Total in, out and net migration for each council area for each year (no sex breakdown)
total_mig <- data_mig %>%
  filter(Sex == "Persons") %>%
  group_by(Area, Year, Year_2, Type) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Total in and out migration for each council area for each year (no sex breakdown and no net migration)
total_in_out_mig <- data_mig %>%
  filter(Sex == "Persons", Type %in% c("In", "Out")) %>%
  group_by(Area, Year, Year_2, Type) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Total in and out migration for each year for paramsarea only
total_in_out_mig_ca <- total_in_out_mig %>%
  filter(Area == paramsarea)


# Total net migration for each council area (and Scotland) for each year
total_net_mig <- data_mig %>%
  filter(Sex == "Persons", Type == "Net") %>%
  group_by(Area, Year, Year_2) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Total net migration for each council area (but not Scotland) for each year
total_net_mig_all_CA <- total_net_mig %>%
  filter(Area != "Scotland")

# For use in R Markdown text.
# Total net migration for each council area (but not Scotland) for end year only. Rank the changes, 
# 1 = highest change
net_mig_end_year <- total_net_mig %>%
  filter(Area != "Scotland", Year == max(Year)) %>%
  mutate(Rank = min_rank(desc(Number)))



# Net migration by age by sex

# Mutate initial net-migration dataset to rename variables, form age groups, define factors and deselect 
# certain unnecessary columns.
# Because Year is a string e.g. "2014-16" rather than numeric, we define this to be Year_2 instead and only  
# use it after we have produced the plots, to re-label the axes with the string values. 
# We define Year to be the last two digits of Year_2, so it is numeric and can be easily used for plots.
net_mig <- mutate(`net-migration`,
                  Area = `Council area`,
                  Age_group = `Age group`,
                  Year_2 = Year) %>%
  select(-Year) %>%
  mutate(Year = as.numeric(substr(Year_2,
                                  nchar(Year_2) - 1,
                                  nchar(Year_2)))) %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("All ages", "0 to 4", "5 to 9",
                                       "10 to 14", "15 to 19", "20 to 24",
                                       "25 to 29", "30 to 34", "35 to 39",
                                       "40 to 44", "45 to 49", "50 to 54",
                                       "55 to 59", "60 to 64", "65 to 69",
                                       "70 to 74", "75 to 79", "80 to 84",
                                       "85 to 89", "90 and over"))) %>%
  arrange(Age_group) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female", "Persons"))) %>%
  drop_na(Area)

# Net migration for each council area (and Scotland) for end year only by age group by sex
net_mig_age_sex <- net_mig %>%
  filter(Age_group != "All ages", Year == max(Year)) %>%
  group_by(Area, Year, Year_2, Age_group, Sex) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Net migration for paramsarea only for end year only by age group by sex
net_mig_age_sex_ca <- net_mig_age_sex %>%
  filter(Area == paramsarea)

# Net migration for each council area (and Scotland) for end year only by sex (no age breakdown)
net_mig_sex <- net_mig %>%
  filter(Age_group == "All ages", Year == max(Year)) %>%
  group_by(Area, Year, Year_2, Sex) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Net migration for each council area (and Scotland) for end year only by age group (no sex breakdown)
net_mig_age <- net_mig %>%
  filter(Age_group != "All ages", Year == max(Year)) %>%
  group_by(Area, Year, Year_2, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()


# Net migration rates

# Mutate initial net-migration-rates dataset to rename variables.
# Because Year is a string e.g. "2014-16" rather than numeric, we define this to be Year_2 instead and only  
# use it after we have produced the plots, to re-label the axes with the string values. 
# We define Year to be the last two digits of Year_2, so it is numeric and can be easily used for plots.
net_mig_rates <- mutate(`net-migration-rates`,
                        Area = `Council area`,
                        Year_2 = Year,
                        Number = Rate) %>%
  select(-Year) %>%
  mutate(Year = as.numeric(substr(Year_2, nchar(Year_2) - 1, nchar(Year_2))))

# For use in R Markdown text.
# Net migration rates for each council area (but not Scotland) for end year only. Rank the changes and define
# whether the change was a decrease, increase or equal.
# Rank 1 = highest change
net_mig_rates_end_year <- net_mig_rates %>%
  filter(Area != "Scotland", Year_2 == mig_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           net_mig_rates$Number[net_mig_rates$Year_2 == mig_penultimate_year &
                                  net_mig_rates$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal")))



# Marriages and civil partnerships data ==================================================


# Marriages

# Mutate initial marriages dataset to rename variables.
data_marr <- mutate(`marriages`,
                    Area = `Council area`,
                    Year = `Registration Year`,
                    Number = `Number of marriages`)

# Total marriages for each council area (and Scotland) for each year from start year to end year
total_marr <- data_marr %>%
  filter(Year >= bir_dea_marr_est_start_year) %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Total marriages for paramsarea only for each year from start year to end year
total_marr_CA <- total_marr %>%
  filter(Area == paramsarea)

# Total marriages for each council area (but not Scotland) for each year from start year to end year
total_marr_all_CA <- total_marr %>%
  filter(Area != "Scotland")

# For use in R Markdown text.
# Total marriages for each council area (but not Scotland) for end year only.
# Rank the councils, 1=highest number of marriages. Define whether the change between end year and the 
# previous year was a decrease, increase or equal.
marr_end_year <- total_marr %>%
  filter(Area != "Scotland", Year == bir_dea_marr_cp_est_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           total_marr$Number[total_marr$Year == (bir_dea_marr_cp_est_end_year - 1) &
                               total_marr$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal"))) #%>%



# Civil partnerships

# Mutate initial civil-partnerships dataset to rename variables and define factors.
data_cp <- mutate(`civil-partnerships`,
                  Area = `Council area`,
                  Year = `Registration Year`,
                  Number = `Number of civil partnerships`) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female", "All", "Mixed-sex", "Same-sex"))) %>%
  arrange(Sex)

# Total civil partnerships for each council area (and Scotland) for each year from start year to end year
total_cp <- data_cp %>%
  filter(Sex == "All") %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Total civil partnerships for paramsarea only for each year from start year to end year
total_cp_CA <- total_cp %>%
  filter(Area == paramsarea) 

# Total civil partnerships for each council area (but not Scotland) for each year from start year to end year
total_cp_all_CA <- total_cp %>%
  filter(Area != "Scotland")

# For use in R Markdown text.
# Total civil partnerships for each council area (but not Scotland) for end year only.
# Rank the councils, 1=highest number of civil partnerships. 
# Define whether the change between end year and the previous year was a decrease, increase or equal.
cp_end_year <- total_cp %>%
  filter(Area != "Scotland", Year == cp_end_yr) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           total_cp$Number[total_cp$Year == (cp_end_yr - 1) &
                             total_cp$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal")))

# Total civil partnerships for each council area (and Scotland) for each year from start year to end year,
# broken down by sex
data_cp_gender <- data_cp %>%
  group_by(Area, Year, Sex) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()




# Household estimates  ===================================================================


# Mutate initial household-estimates dataset to rename variables.
data_house_est <- mutate(`household-estimates`,
                         Area = `Council area`,
                         Number = `Number of households`)

# Total household estimates for each council area (and Scotland) and for each year
total_house_est <- data_house_est %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Define a vector the same length as the number of rows of total_house_est Repeat the start year household 
# estimate for each council area once for each year. e.g. repeat 20 times for a 20 year period between start
# and end year.
base_house_est <- rep(filter(total_house_est, Year == min(Year))$Number,
                      each = house_est_end_year - house_est_start_year + 1)

# Use base_house_est to compute the percentage change from the start year for each council area for each year
total_house_est <- mutate(total_house_est,
                          Perc = (Number - base_house_est) / base_house_est * 100)

# Household estimates for each year for paramsarea only
total_house_est_CA <- data_house_est %>%
  filter(Area == paramsarea)

# Household estimates for each year for all council areas and excluding Scotland
data_house_est_all_CA <- data_house_est %>%
  filter(Area != "Scotland")

# For use in R Markdown text. Rank the changes in households and rank the percentage changes. 
# 1 = highest/largest change.
# Define whether the change in number between end year and the previous year was a decrease, increase or equal.
house_est_end_year_data <- total_house_est %>%
  filter(Area != "Scotland", Year == house_est_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           total_house_est$Number[total_house_est$Year == (house_est_end_year - 1) &
                                    total_house_est$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal"))) %>%
  mutate(Rank_perc = min_rank(desc(Perc)))



# Dwellings ==============================================================================


# Number of dwellings


# Mutate initial dwellings dataset to rename variables and select certain columns.
data_dwell_num <- mutate(`dwellings`,
                         Area = `Council area`,
                         Number = `Number of dwellings`) %>%
  select(Area, Year, Number)

# Total dwellings estimates for each council area (and Scotland) and for each year
total_dwell_num <- data_dwell_num %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Define a vector the same length as the number of rows of total_dwell_num. Repeat the start year dwellings 
# estimate for each council area once for each year. e.g. repeat 20 times for a 20 year period between start
# and end year.
base_dwell_num <- rep(filter(total_dwell_num, Year == min(Year))$Number,
                      each = dwell_est_end_year - dwell_est_start_year + 1)

# Use base_dwell_num to compute the percentage change from the start year for each council area for each year
total_dwell_num <- mutate(total_dwell_num,
                          Perc = (Number - base_dwell_num) / base_dwell_num * 100)

# Dwellings estimates for each year for paramsarea only
total_dwell_num_CA <- data_dwell_num %>%
  filter(Area == paramsarea)

# For use in R Markdown text. Rank the changes in dwellings and rank the percentage changes. 
# 1 = highest/largest change.
# Define whether the change in number between end year and the previous year was a decrease, increase or equal.
dwell_end_year <- total_dwell_num %>%
  filter(Area != "Scotland", Year == dwell_est_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           total_dwell_num$Number[total_dwell_num$Year == (dwell_est_end_year - 1) &
                                    total_dwell_num$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal"))) %>%
  mutate(Rank_perc = min_rank(desc(Perc)))




# Dwellings by type, end year only


# Mutate initial dwellings-by-type dataset to rename variables, select certain columns and define factors.
# Dwellings estimates by type for all council areas (and Scotland)
data_dwell_type <- mutate(`dwellings-by-type`, Area = `Council area`) %>%
  select(Area, Year, Type, Number)
data_dwell_type <- data_dwell_type %>%
  filter(Type != "Total") %>%
  mutate(Type = factor(Type,
                       levels = c("Total", "Detached", "Semi-detached",
                                  "Terrace", "Flat", "Not known")))

# Dwellings estimates by type for paramsarea only, by type of dwelling.
# Compute proportion of each type.
data_dwell_type_ca <- data_dwell_type %>%
  filter(Area == paramsarea) %>%
  mutate(Perc = Number / sum(Number) * 100) %>%
  mutate(Perc = round(Perc, 1))

# Same as data_dwell_type_ca but define factors of the variable Type.
data_dwell_type_ca_plot <- data_dwell_type_ca %>%
  mutate(Type = factor(Type,
                       levels = c("Detached", "Semi-detached", "Terrace",
                                  "Flat", "Not known")))




# Dwellings by council tax band, end year only


# Mutate initial dwellings-by-council-tax-band dataset to rename variables.
data_dwell_tax <- mutate(`dwellings-by-council-tax-band`,
                         Area = `Council area`,
                         Type = `Council Tax band`)

# Dwellings estimates by council tax band for all council areas (and Scotland)
data_dwell_tax <- data_dwell_tax %>%
  select(Area, Year, Type, Number) %>%
  filter(Type != "Total")

# Group the council tax bands and define as factors of Type.
# Dwellings estimates by grouped council tax bands for all council areas (and Scotland)
data_dwell_tax_grouped <- data_dwell_tax %>%
  group_by(Area, Year, Type) %>%
  summarise(Number = sum(Number)) %>%
  spread(key = Type, value = Number) %>%
  mutate(
    `A - C` = A + B + C,
    `D - E` = D + E,
    `F - H` = F + G + H) %>%
  select(Area, Year, `A - C`, `D - E`, `F - H`) %>%
  gather(key = Type, value = Number, -Area, -Year) %>%
  mutate(Type = factor(Type, levels = c("A - C", "D - E", "F - H"))) %>%
  ungroup()

# Dwellings estimates by grouped council tax bands for paramsarea only.
# Compute proportion of each group.
data_dwell_tax_grouped_ca <- data_dwell_tax_grouped %>%
  filter(Area == paramsarea) %>%
  mutate(Perc = Number / sum(Number) * 100) %>%
  mutate(Perc = round(Perc, 1))

# Dwellings estimates by ungrouped council tax bands for paramsarea only.
# Compute proportion of each council tax band
data_dwell_tax_ca <- data_dwell_tax %>%
  filter(Area == paramsarea) %>%
  mutate(Perc = Number / sum(Number) * 100) %>%
  mutate(Perc = round(Perc, 1)) %>%
  arrange(Type) %>%
  mutate(Type = factor(Type, Type))




# Household projections ==================================================================


# Mutate initial household-projections dataset to rename variables, filter to start year until end year and
# select certain columns.
data_house_proj <- mutate(`household-projections`,
                          Area = `Council area`,
                          Age_group = `Age group`,
                          Type = `Household type`) %>%
  filter(Year >= house_proj_start_year, Year <= house_proj_end_year) %>%
  select(Area, Age_group, Type, Year, Number) %>%
  filter(Type != "Non-heads") %>%
  filter(Age_group != "All ages")

# Total household projections for each council area (and Scotland) and for each year
total_house_proj <- data_house_proj %>%
  filter(Type != "All households") %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Define a vector the same length as the number of rows of total_house_proj. Repeat the start year household 
# projection for each council area once for each year. e.g. repeat 20 times for a 20 year period between start
# and end year.
base_house_proj <- rep(filter(total_house_proj, Year == min(Year))$Number,
                       each = house_proj_end_year - house_proj_start_year + 1)

# Use base_house_proj to compute the percentage change from the start year for each council area for each year
total_house_proj <- mutate(total_house_proj,
                           Perc = (Number - base_house_proj) /
                             base_house_proj * 100) %>%
  mutate(Number = round(Number, 0))

# Household projections for each year for paramsarea only
total_house_proj_CA <- total_house_proj %>%
  filter(Area == paramsarea)

# Household projections for each year for all council areas and excluding Scotland
total_house_proj_all_CA <- total_house_proj %>%
  filter(Area != "Scotland")

# For use in R Markdown text. Rank the changes in households and rank the percentage changes. 
# 1 = highest/largest change.
# Define whether the change in number between end year and the previous year is projected to be a decrease, 
# increase or equal.
house_proj_end_year_dataset <- total_house_proj %>%
  filter(Area != "Scotland", Year == house_proj_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           total_house_proj$Number[total_house_proj$Year == (house_proj_end_year - 1) &
                                     total_house_proj$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal"))) %>%
  mutate(Rank_perc = min_rank(desc(Perc)))



# Household projections by age group of Household Reference Person

# Household projections for start year and end year only for all council areas and Scotland, broken down by 
# age group of the Household Reference Person.
house_proj_age_table <- data_house_proj %>%
  filter(Type != "All households") %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(house_proj_start_year, house_proj_end_year)) %>%
  ungroup() %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("All ages", "16 to 19", "20 to 24",
                                       "25 to 29", "30 to 34", "35 to 39",
                                       "40 to 44", "45 to 49", "50 to 54",
                                       "55 to 59", "60 to 64", "65 to 69",
                                       "70 to 74", "75 to 79", "80 to 84",
                                       "85 to 89", "90 and over")))

# Round the projections to 0 decimal places
house_proj_age <- house_proj_age_table %>%
  mutate(Number = round(Number, 0))

# Household projections for start year and end year only for paramsarea only, broken down by 
# age group of the Household Reference Person.
house_proj_age_plot <- house_proj_age %>%
  filter(Age_group != "All ages", Area == paramsarea)

# Reformat the table so we can compute percentage change using start year and end year columns.
house_proj_age_perc_spread <- house_proj_age_plot %>%
  spread(key = Year, value = Number) %>%
  ungroup()

# Compute the projected percentage change between start year and end year. Define whether the change is 
# positive, negative or equal.
house_proj_age_perc <- house_proj_age_perc_spread %>%
  mutate(Perc = (house_proj_age_perc_spread[[as.character(house_proj_end_year)]] -
                   house_proj_age_perc_spread[[as.character(house_proj_start_year)]]) /
           house_proj_age_perc_spread[[as.character(house_proj_start_year)]] * 100,
         Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal")))



# Household projections by household type

# Household projections for each year and for all council areas and Scotland, broken down by household type
house_proj_type <- data_house_proj %>%
  filter(Age_group != "All ages") %>%
  group_by(Area, Year, Type) %>%
  summarise(Number = sum(Number)) %>%
  spread(key = Type, value = Number) %>%
  mutate(
    `One adult` = `1 person female` + `1 person male`,
    `Two adults` = `2 person all adult`,
    `One adult, one or more children` = `1 adult 1 child` + `1 adult 2+ children`,
    `Two or more adults, one or more children` = `2+ adult 1+ children`,
    `Three or more adults` = `3+ person all adult`) %>%
  select(Area, Year, `One adult`, `Two adults`,
         `One adult, one or more children`,
         `Two or more adults, one or more children`, `Three or more adults`,
         `All households`) %>%
  gather(key = Type, value = Number, -Area, -Year) %>%
  mutate(Type = factor(Type,
                       levels = c("All households", "One adult", "Two adults",
                                  "One adult, one or more children",
                                  "Two or more adults, one or more children",
                                  "Three or more adults"))) %>%
  arrange(Type) %>%
  mutate(Number = round(Number, 0))

# Household projections for start year and end year only and for all council areas and Scotland, broken down 
# by household type
house_proj_type_comp <- house_proj_type %>%
  group_by(Area, Year, Type) %>%
  filter(Year %in% c(house_proj_start_year, house_proj_end_year))

# Household projections for end year only and for paramsarea only. Compute projected proportions of household 
# type in end year
house_proj_type_prop <- house_proj_type_comp %>%
  filter(Type != "All households",
         Area == paramsarea,
         Year == house_proj_end_year) %>%
  ungroup() %>%
  mutate(Prop = Number / sum(Number) * 100)


# For use in plot p_house_proj_type
# Household projections for start year and end year only and for all council areas and Scotland, broken down 
# by household type, excluding total "All households".
house_proj_type_plot <- house_proj_type_comp %>%
  filter(Type != "All households")

# Rearrange house_proj_type_comp so we have start year and end year columns, so we can compute projected 
# % change.
house_proj_type_perc_prelim <- house_proj_type_comp %>%
  filter(Type != "All households") %>%
  spread(key = Year, value = Number) %>%
  ungroup()

# Compute projected % change between start and end year and state whether the change is positive, negative 
# or equal.
house_proj_type_perc <- house_proj_type_perc_prelim %>%
  mutate(Perc = (house_proj_type_perc_prelim[[as.character(house_proj_end_year)]] -
                   house_proj_type_perc_prelim[[as.character(house_proj_start_year)]]) /
           house_proj_type_perc_prelim[[as.character(house_proj_start_year)]] * 100,
         Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal"))) %>%
  filter(Area == paramsarea)




# Births =================================================================================


# Number of births by sex

# Mutate initial births-by-sex dataset to rename variables.
births_sex <- mutate(`births-by-sex`,
                     Area = `Council area`,
                     Year = `Registration Year`,
                     Sex = factor(Sex, levels = c("Male", "Female", "All people")))

# Total number of births for each council area (and Scotland) and for each year from start year to end year
total_births <- births_sex %>%
  filter(Sex == "All people", Year >= bir_dea_marr_est_start_year) %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Define a vector the same length as the number of rows of total_births Repeat the start year number of births
# for each council area once for each year. e.g. repeat 20 times for a 20 year period between start and end year.
base_births <- rep(filter(total_births, Year == min(Year))$Number,
                   each = bir_dea_marr_cp_est_end_year -
                     bir_dea_marr_est_start_year + 1)

# Use base_births to compute the percentage change from the start year for each council area for each year
total_births <- mutate(total_births,
                       Perc = (Number - base_births) / base_births * 100)

# Births by sex for each year for all council areas (and Scotland)
total_births_sex_all <- births_sex %>%
  filter(Year >= bir_dea_marr_est_start_year) %>%
  group_by(Area, Year, Sex) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Births by sex for each year for paramsarea only
total_births_sex <- total_births_sex_all %>%
  filter(Area == paramsarea)



# Births by age of mother

# Mutate initial births-by-age-of-mother dataset to rename variables and define factors.
births_mothers_age <- mutate(`births-by-age-of-mother`,
                             Area = `Council area`,
                             Year = `Registration Year`,
                             Age_group = `Mother age group`) %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("All ages", "0 to 19", "20 to 24",
                                       "25 to 29", "30 to 34", "35 to 39",
                                       "40 and over", "Not stated")))


# For use with age group by year bar plot.
# Total number of births for each council area (and Scotland), by age group of mother, for start year and 
# end year only
total_births_mothers_age <- births_mothers_age %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(bir_dea_marr_est_start_year, bir_dea_marr_cp_est_end_year)) %>%
  ungroup()

# For use with % change year by age group bar plot.
# Total number of births for paramsarea only, by age group of mother, for start year and end year only. 
# Rearrange total_births_mothers_age so we have start year and end year columns, so we can compute % change.
total_births_mothers_age_perc_prelim <- total_births_mothers_age %>%
  filter(Area == paramsarea) %>%
  spread(key = Year, value = Number)

# Compute % change between start and end year and state whether the change is positive, negative or equal.
total_births_mothers_age_perc <- total_births_mothers_age_perc_prelim %>%
  mutate(Perc = (total_births_mothers_age_perc_prelim[[as.character(bir_dea_marr_cp_est_end_year)]] -
                   total_births_mothers_age_perc_prelim[[as.character(bir_dea_marr_est_start_year)]]) /
           total_births_mothers_age_perc_prelim[[as.character(bir_dea_marr_est_start_year)]] * 100,
         Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal")))


# For use in plot p_births_mothers_age
# Births for start year and end year only and for paramsarea only, by age group of mother, excluding total 
# "All ages" and "Not stated".
total_births_mothers_age_plot <- total_births_mothers_age %>%
  filter(Area == paramsarea) %>%
  filter(Age_group != "All ages",
         Age_group != "Not stated")

# For use in plot p_births_mothers_age_perc.
# Total number of births for paramsarea only, by age group of mother excluding "All ages" and "Not stated", 
# for start year and end year only and with % change.
total_births_mothers_age_perc_plot <- total_births_mothers_age_perc %>%
  filter(Age_group != "All ages",
         Age_group != "Not stated")

# Total number of births for Scotland only, by age group of mother, for start year and end year only. 
# Rearrange total_births_mothers_age so we have start year and end year columns, so we can compute % change.
total_births_mothers_age_perc_scotland_prelim <- total_births_mothers_age %>%
  filter(Area == "Scotland") %>%
  spread(key = Year, value = Number)

# Compute % change between start and end year and state whether the change is positive, negative or equal.
total_births_mothers_age_perc_scotland <- total_births_mothers_age_perc_scotland_prelim %>%
  mutate(Perc = (total_births_mothers_age_perc_scotland_prelim[[as.character(bir_dea_marr_cp_est_end_year)]] -
                   total_births_mothers_age_perc_scotland_prelim[[as.character(bir_dea_marr_est_start_year)]]) /
           total_births_mothers_age_perc_scotland_prelim[[as.character(bir_dea_marr_est_start_year)]] * 100,
         Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal")))



# Standardised birth rates

# Mutate initial standardised-birth-rates dataset to rename variables.
# Birth rates for all council areas (and Scotland) for each year from start year to end year
births_rate <- mutate(`standardised-birth-rates`,
                      Area = `Council area`,
                      Year = `Registration Year`,
                      Number = `Standardised birth rate`) %>%
  filter(Year >= bir_dea_marr_est_start_year) %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# For use in R Markdown text. Rank the changes in birth rates, 1 = highest/largest change.
# Define whether the change between end year and the previous year was a decrease, increase or equal.
births_rate_end_year <- births_rate %>%
  filter(Area != "Scotland", Year == bir_dea_marr_cp_est_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           births_rate$Number[births_rate$Year == (bir_dea_marr_cp_est_end_year - 1) &
                                births_rate$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal")))



# Fertility rates

# Mutate initial fertility-rates dataset to rename variables and select columns.
# Fertility rates for all council areas (and Scotland) for each year from start year to end year
fert_rate <- mutate(`fertility-rates`,
                    Area = `Council area`,
                    Year = `Registration Year`,
                    Number = `Total fertility rate`) %>%
  mutate(Number = as.numeric(Number)) %>%
  select(Area, Year, Number) %>%
  filter(Year >= bir_dea_marr_est_start_year)

# Define a vector the same length as the number of rows of fert_rate. Repeat the start year rate for each 
# council area once for each year. e.g. repeat 20 times for a 20 year period between start and end year.
base_fert_rate <- rep(filter(fert_rate, Year == min(Year))$Number,
                      each = bir_dea_marr_cp_est_end_year -
                        bir_dea_marr_est_start_year + 1)

# Use base_fert_rate to compute the percentage change from the start year for each council area for each year
fert_rate <- mutate(fert_rate,
                    Perc = (Number - base_fert_rate) / base_fert_rate * 100)

# For use in R Markdown text. Rank the changes in fertility rates, 1 = highest/largest change.
# Define whether the change between end year and the previous year was a decrease, increase or equal.
fert_rate_end_year <- fert_rate %>%
  filter(Area != "Scotland", Year == bir_dea_marr_cp_est_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           fert_rate$Number[fert_rate$Year == (bir_dea_marr_cp_est_end_year - 1) &
                              fert_rate$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal")))


# Deaths  ================================================================================


# Number of deaths

# Mutate initial deaths-by-sex dataset to rename variables and define factors.
deaths_sex <-
  mutate(`deaths-by-sex`,
         Area = `Council area`,
         Year = `Registration Year`,
         Sex = factor(Sex, levels = c("Male", "Female", "All people")))

# Total number of deaths for all council areas (and Scotland) for each year from start year to end year
total_deaths <- deaths_sex %>%
  filter(Sex == "All people",
         Year >= bir_dea_marr_est_start_year) %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Define a vector the same length as the number of rows of total_deaths. Repeat the start year total for each 
# council area once for each year. e.g. repeat 20 times for a 20 year period between start and end year.
base_deaths <- rep(filter(total_deaths, Year == min(Year))$Number,
                   each = bir_dea_marr_cp_est_end_year -
                     bir_dea_marr_est_start_year + 1)

# Use base_deaths to compute the percentage change from the start year for each council area for each year
total_deaths <- mutate(total_deaths,
                       Perc = (Number - base_deaths) / base_deaths * 100)

# Deaths by sex for each council area (and Scotland) for each year from start year to end year
total_deaths_sex_all <- deaths_sex %>%
  filter(Year >= bir_dea_marr_est_start_year) %>%
  group_by(Area, Year, Sex) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Deaths by sex for paramsarea only for each year from start year to end year
total_deaths_sex <- total_deaths_sex_all %>%
  filter(Area == paramsarea)

# For use with plot p_deaths_sex_comp
# Deaths by sex (excluding total "All people") for each council area (and Scotland) for start year and end year
total_deaths_sex_comp_table <- deaths_sex %>%
  group_by(Area, Year, Sex) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(bir_dea_marr_est_start_year, bir_dea_marr_cp_est_end_year),
         Sex != "All people") %>%
  ungroup()

# Deaths by sex (excluding total "All people") for paramsarea only for start year and end year
total_deaths_sex_comp <- total_deaths_sex_comp_table %>%
  filter(Area == paramsarea)


# For use with plot p_deaths_sex_perc
# Rearrange total_deaths_sex_comp so we have start year and end year columns, so we can compute % change.
total_deaths_sex_perc <- total_deaths_sex_comp %>%
  spread(key = Year, value = Number)

# Compute % change between start and end year and state whether the change is positive, negative or equal.
total_deaths_sex_perc <- total_deaths_sex_perc %>%
  mutate(Perc = (total_deaths_sex_perc[[as.character(bir_dea_marr_cp_est_end_year)]] -
                   total_deaths_sex_perc[[as.character(bir_dea_marr_est_start_year)]]) /
           total_deaths_sex_perc[[as.character(bir_dea_marr_est_start_year)]] * 100,
         Sign = ifelse(Perc > 0, "positive", ifelse(Perc < 0, "negative", "equal")))


# Deaths by sex (excluding total "All people") for Scotland only for start year and end year
# Rearrange total_deaths_sex_comp_table so we have start year and end year columns, so we can compute % change.
total_deaths_sex_perc_scotland <- total_deaths_sex_comp_table %>%
  filter(Area == "Scotland") %>%
  spread(key = Year, value = Number)

# Compute % change between start and end year and state whether the change is positive, negative or equal.
total_deaths_sex_perc_scotland <- total_deaths_sex_perc_scotland %>%
  mutate(Perc = (total_deaths_sex_perc_scotland[[as.character(bir_dea_marr_cp_est_end_year)]] -
                   total_deaths_sex_perc_scotland[[as.character(bir_dea_marr_est_start_year)]]) /
           total_deaths_sex_perc_scotland[[as.character(bir_dea_marr_est_start_year)]] * 100,
         Sign = ifelse(Perc > 0,
                       "positive",
                       ifelse(Perc < 0,
                              "negative",
                              "equal")))



# Deaths by sex by age

# Mutate initial deaths-by-sex-by-age dataset to rename variables define factors and arrange variables.
# Deaths by sex by age group for each council area and Scotland, for end year only
deaths_age_sex <-
  mutate(`deaths-by-sex-by-age`,
         Area = `Council area`,
         Year = `Registration Year`,
         Age_group = `Age group`) %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("All ages", "0", "1 to 4", "5 to 9",
                                       "10 to 14", "15 to 19", "20 to 24",
                                       "25 to 29", "30 to 34", "35 to 39",
                                       "40 to 44", "45 to 49", "50 to 54",
                                       "55 to 59", "60 to 64", "65 to 69",
                                       "70 to 74", "75 to 79", "80 to 84",
                                       "85 to 89", "90 and over"))) %>%
  mutate(Sex = replace(Sex, Sex == "Females", "Female"),
         Sex = replace(Sex, Sex == "Males", "Male")) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female", "All people"))) %>%
  arrange(Age_group)

# Total deaths by sex (not total "All people") by age group for each council area and Scotland
total_deaths_age_sex_comp <- deaths_age_sex %>%
  filter(Year == bir_dea_marr_cp_est_end_year) %>%
  group_by(Area, Sex, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Sex != "All people",
         Age_group != "All ages") %>%
  ungroup()

# Total deaths by sex (not total "All people") by age group for paramsarea only
total_deaths_age_sex <- total_deaths_age_sex_comp %>%
  filter(Area == paramsarea)


# For use with deaths by age group by sex table
# Total deaths by sex by age group for paramsarea and Scotland only
total_deaths_age_sex_table <- deaths_age_sex %>%
  group_by(Area, Sex, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Age_group != "All ages") %>%
  ungroup() %>%
  filter(Area %in% c(paramsarea, "Scotland"))

# Total deaths by age group for paramsarea only (no sex breakdown)
total_deaths_age <- total_deaths_age_sex_table %>%
  filter(Sex == "All people",
         Area == paramsarea)




# Standardised death rates

# Mutate initial standardised-death-rates dataset to rename variables.
# Death rates for each council area and Scotland, for each year from start year to end year
deaths_rate <- mutate(`standardised-death-rates`,
                      Area = `Council area`,
                      Year = `Registration Year`,
                      Number = `Standardised death rate`) %>%
  filter(Year >= bir_dea_marr_est_start_year) %>%
  group_by(Area, Year) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# For use in R Markdown text. Rank the changes in death rates, 1 = highest/largest change.
# Define whether the change between end year and the previous year was a decrease, increase or equal.
deaths_rate_end_year <- deaths_rate %>%
  filter(Area != "Scotland", Year == bir_dea_marr_cp_est_end_year) %>%
  mutate(Rank = min_rank(desc(Number))) %>%
  mutate(Difference = Number -
           deaths_rate$Number[deaths_rate$Year == (bir_dea_marr_cp_est_end_year - 1) &
                                deaths_rate$Area != "Scotland"]) %>%
  mutate(Change_type = ifelse(Difference < 0,
                              "decrease",
                              ifelse(Difference > 0,
                                     "increase",
                                     "equal")))



# Leading cause of death


# Mutate initial leading-causes-of-death dataset to rename variables and select certain columns.
# Leading causes of death for each council area and Scotland, for end year only.
deaths_cause <- mutate(`leading-causes-of-death`, Area = `Council area`, Total = `Total deaths`, ICD_code = `ICD codes`, Cause, Perc = Percent, Cause_label = `Cause label`) %>%
  mutate(Type = paste(ICD_code, Cause, sep = " ")) %>%
  select(Area, Year, Total, Type, Cause, Cause_label, Number, Perc, Sex)

# Leading causes of death for females for each council area and Scotland, arranged in descending order
deaths_cause_f_comp <- deaths_cause %>%
  filter(Sex == "Females") %>%
  arrange(Area, -Number)

# Leading causes of death for females for paramsarea only, arranged in descending order
deaths_cause_f <- deaths_cause_f_comp %>%
  filter(Area == paramsarea) %>%
  mutate(Type = factor(Type, Type))

# Leading causes of death for males for each council area and Scotland, arranged in descending order
deaths_cause_m_comp <- deaths_cause %>%
  filter(Sex == "Males") %>%
  arrange(Area, -Number)

# Leading causes of death for males for paramsarea only, arranged in descending order
deaths_cause_m <- deaths_cause_m_comp %>%
  filter(Area == paramsarea) %>%
  mutate(Type = factor(Type, Type))



# Life expectancy ========================================================================


# Mutate initial life-expectancy dataset to rename variables.
# Because Year is a string e.g. "2014-16" rather than numeric, we define this to be Year_2 instead and only  
# use it after we have produced the plots, to re-label the axes with the string values. 
# We define Year to be the last two digits of Year_2, so it is numeric and can be easily used for plots.
life_exp <-
  mutate(`life-expectancy`,
         Year_2 = Year,
         Number = `Life expectancy`,
         Area = `Council area`,
         Age_group = `Age group`) %>%
  select(-Year) %>%
  mutate(Year = as.numeric(substr(Year_2, nchar(Year_2) - 1, nchar(Year_2))))



# Life expectancy at birth


# Life expectancy at birth by type for each year from start year to end year
life_exp_from_birth <- life_exp %>%
  filter(Age_group == 0) %>%
  group_by(Area, Year, Year_2, Sex, Type) %>%
  summarise(Number = sum(Number)) %>%
  ungroup() %>%
  mutate(Type = factor(Type, levels = c("Lower_CI", "Estimate", "Upper_CI")))

# Female life expectancy estimate at birth for each year from start year to end year
life_exp_from_birth_f_unrounded <- life_exp_from_birth %>%
  filter(Type == "Estimate", Sex == "Female")

# Define a vector the same length as the number of rows of life_exp_from_birth_f_unrounded. Repeat the start 
# year life expectancy for each council area once for each year. e.g. repeat 20 times for a 20 year period between start and end year.
base_life_exp_from_birth_f <- rep(filter(life_exp_from_birth_f_unrounded,
                                         Year == min(Year))$Number,
                                  each = max(life_exp_from_birth_f_unrounded$Year) -
                                    min(life_exp_from_birth_f_unrounded$Year) + 1)

# Use base_life_exp_from_birth_f to compute the percentage change from the start year for each council area for each year
life_exp_from_birth_f_unrounded <- mutate(life_exp_from_birth_f_unrounded,
                                          Perc = (Number - base_life_exp_from_birth_f) /
                                            abs(base_life_exp_from_birth_f) * 100)

# Female life expectancy estimate at birth for each year from start year to end year, rounded to 1 decimal place
life_exp_from_birth_f <- life_exp_from_birth_f_unrounded %>%
  mutate(Perc = round(Perc, 1))

# For use in R Markdown text.
# Female life expectancy estimates at birth for each council area (not Scotland), for end year only
life_exp_from_birth_f_end_year <- life_exp_from_birth_f %>%
  filter(Area != "Scotland", Year_2 == max(Year_2)) %>%
  mutate(Rank = min_rank(desc(Perc)))



# Male life expectancy estimate at birth for each year from start year to end year
life_exp_from_birth_m_unrounded <- life_exp_from_birth %>%
  filter(Type == "Estimate", Sex == "Male")

# Define a vector the same length as the number of rows of life_exp_from_birth_m_unrounded. Repeat the start 
# year life expectancy for each council area once for each year. e.g. repeat 20 times for a 20 year period between start and end year.
base_life_exp_from_birth_m <- rep(filter(life_exp_from_birth_m_unrounded,
                                         Year == min(Year))$Number,
                                  each = max(life_exp_from_birth_m_unrounded$Year) -
                                    min(life_exp_from_birth_m_unrounded$Year) + 1)

# Use base_life_exp_from_birth_m to compute the percentage change from the start year for each council area for each year
life_exp_from_birth_m_unrounded <- mutate(life_exp_from_birth_m_unrounded,
                                          Perc = (Number - base_life_exp_from_birth_m) /
                                            abs(base_life_exp_from_birth_m) * 100)

# Male life expectancy estimate at birth for each year from start year to end year, rounded to 1 decimal place
life_exp_from_birth_m <- life_exp_from_birth_m_unrounded %>%
  mutate(Perc = round(Perc, 1))

# For use in R Markdown text.
# Male life expectancy estimates at birth for each council area (not Scotland), for end year only
life_exp_from_birth_m_end_year <- life_exp_from_birth_m %>%
  filter(Area != "Scotland", Year_2 == max(Year_2)) %>%
  mutate(Rank = min_rank(desc(Perc)))

# Life expectancy estimates at birth for males and females, for paramsarea and Scotland only, for each year 
# from start year to end year
life_exp_from_birth_comp <- life_exp_from_birth %>%
  filter(Area %in% c(paramsarea, "Scotland")) %>%
  spread(key = Type, value = Number)



# Life expectancy at age 65 to 69


# Life expectancy at age 65 to 69 by type for each year from start year to end year
life_exp_at_65 <- life_exp %>%
  filter(Age_group == "65 to 69") %>%
  group_by(Area, Year, Year_2, Sex, Type) %>%
  summarise(Number = sum(Number)) %>%
  ungroup()

# Female life expectancy estimate at 65 to 69 for each year from start year to end year
life_exp_at_65_f_unrounded <- life_exp_at_65 %>%
  filter(Type == "Estimate", Sex == "Female")

# Define a vector the same length as the number of rows of life_exp_at_65_f_unrounded. Repeat the start year
# life expectancy for each council area once for each year. e.g. repeat 20 times for a 20 year period between start and end year.
base_life_exp_at_65_f <- rep(filter(life_exp_at_65_f_unrounded,
                                    Year == min(Year))$Number,
                             each = max(life_exp_at_65_f_unrounded$Year) -
                               min(life_exp_at_65_f_unrounded$Year) + 1)

# Use base_life_exp_at_65_f to compute the percentage change from the start year for each council area for each year
life_exp_at_65_f_unrounded <- mutate(life_exp_at_65_f_unrounded,
                                     Perc = (Number - base_life_exp_at_65_f) /
                                       abs(base_life_exp_at_65_f) * 100)

# Female life expectancy estimate at 65 to 69 for each year from start year to end year, rounded to 1 decimal place
life_exp_at_65_f <- life_exp_at_65_f_unrounded %>%
  mutate(Perc = round(Perc, 1))

# For use in R Markdown text.
# Female life expectancy estimates at age 65 to 69 for each council area (not Scotland), for end year only
life_exp_at_65_f_end_year <- life_exp_at_65_f %>%
  filter(Area != "Scotland", Year_2 == max(Year_2)) %>%
  mutate(Rank = min_rank(desc(Perc)))

# Male life expectancy estimate at 65 to 69 for each year from start year to end year
life_exp_at_65_m_unrounded <- life_exp_at_65 %>%
  filter(Type == "Estimate", Sex == "Male")

# Define a vector the same length as the number of rows of life_exp_at_65_m_unrounded. Repeat the start year
# life expectancy for each council area once for each year. e.g. repeat 20 times for a 20 year period between start and end year.
base_life_exp_at_65_m <- rep(filter(life_exp_at_65_m_unrounded,
                                    Year == min(Year))$Number,
                             each = max(life_exp_at_65_m_unrounded$Year) -
                               min(life_exp_at_65_m_unrounded$Year) + 1)

# Use base_life_exp_at_65_m to compute the percentage change from the start year for each council area for each year
life_exp_at_65_m_unrounded <- mutate(life_exp_at_65_m_unrounded,
                                     Perc = (Number - base_life_exp_at_65_m) /
                                       abs(base_life_exp_at_65_m) * 100)

# Male life expectancy estimate at 65 to 69 for each year from start year to end year, rounded to 1 decimal place
life_exp_at_65_m <- life_exp_at_65_m_unrounded %>%
  mutate(Perc = round(Perc, 1))

# For use in R Markdown text.
# Male life expectancy estimates at age 65 to 69 for each council area (not Scotland), for end year only
life_exp_at_65_m_end_year <- life_exp_at_65_m %>%
  filter(Area != "Scotland", Year_2 == max(Year_2)) %>%
  mutate(Rank = min_rank(desc(Perc)))

# Life expectancy estimates at age 65 to 69 for males and females, for paramsarea and Scotland only, for each year 
# from start year to end year
life_exp_at_65_comp <- life_exp_at_65 %>%
  filter(Area %in% c(paramsarea, "Scotland")) %>%
  spread(key = Type, value = Number)