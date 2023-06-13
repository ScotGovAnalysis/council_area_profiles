# Population estimates =========================================================



# Table 1: Total and % change by year ==========================================

# Paramsarea
data_table_pop_est <- table_total_and_perc_change_by_year(total_pop_est,
                                                          paramsarea) %>%
  select(-Area) %>%
  rename(Population = Number)
names(data_table_pop_est)[which(colnames(data_table_pop_est) == "Perc")] <- paste("% change from", pop_est_start_year)

# Scotland
data_table_pop_est_scotland <- table_total_and_perc_change_by_year(total_pop_est,
                                                                   "Scotland") %>%
  select(Year, Perc)
names(data_table_pop_est_scotland)[which(colnames(data_table_pop_est_scotland) == "Perc")] <- paste("Scotland % change from", pop_est_start_year)

# Merge the two datasets
data_table_pop_est <- merge(data_table_pop_est, data_table_pop_est_scotland)


# Table 2: Age group (detailed) by gender with % of total ================


pop_est_sex_age <- pop_est %>%
  group_by(Area, Year, Sex, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year == pop_est_end_year) %>%
  ungroup()

# Paramsarea
data_table_pop_est_sex_age <- table_age_gender_dataset(pop_est_sex_age,
                                                       Area_name = paramsarea) %>%
  mutate(`% of population` = Perc_column) %>%
  select(-Year, -Perc_column)

# Scotland
data_table_pop_est_sex_age_scotland <- table_age_gender_dataset(pop_est_sex_age,
                                                                Area_name = "Scotland") %>%
  mutate(`Scotland % of population` = Perc_column) %>%
  select(`Age group`, `Scotland % of population`)

# Merge the two datasets
data_table_pop_est_sex_age <- merge(data_table_pop_est_sex_age, data_table_pop_est_sex_age_scotland) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)



# Table 3: Age group by gender with % of total  ===========================


pop_est_age_small <- mutate(pop_est,
                            Age_group = cut(Age,
                                            c(-1, 15, 64, 130),
                                            labels = c("0 to 15", "16 to 64",
                                                       "65 and over"))) %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("All ages", "0 to 15", "16 to 64",
                                       "65 and over")))

pop_est_sex_age_small <- pop_est_age_small %>%
  group_by(Area, Year, Age_group, Sex) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year == pop_est_end_year) %>%
  ungroup()

# Paramsarea
data_table_pop_est_sex_age_small <- table_age_gender_dataset(pop_est_sex_age_small,
                                                             Area_name = paramsarea) %>%
  mutate(`% of population` = Perc_column) %>%
  select(-Year, -Perc_column)

# Scotland
data_table_pop_est_sex_age_scotland_small <- table_age_gender_dataset(pop_est_sex_age_small,
                                                                      Area_name = "Scotland") %>%
  mutate(`Scotland % of population` = Perc_column) %>%
  select(`Age group`, `Scotland % of population`)

# Merge the two datasets
data_table_pop_est_sex_age_small <- merge(data_table_pop_est_sex_age_small,
                                          data_table_pop_est_sex_age_scotland_small) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)



# Table 4: change by age group (detailed) =========================


data_pop_est_age <- pop_est %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_est_start_year, pop_est_end_year)) %>%
  ungroup()

# Paramsarea
data_table_pop_est_age <- table_age_year_perc_change_1_dataset(data_pop_est_age,
                                                               paramsarea)
data_table_pop_est_age <- table_age_year_perc_change_2_dataset(data_table_pop_est_age,
                                                               paramsarea,
                                                               start_year = pop_est_start_year,
                                                               end_year = pop_est_end_year) %>%
  mutate(`% change` = Perc) %>%
  select(-Perc)

# Scotland
data_table_pop_est_age_scotland <- table_age_year_perc_change_1_dataset(data_pop_est_age,
                                                                        "Scotland")
data_table_pop_est_age_scotland <- table_age_year_perc_change_2_dataset(data_table_pop_est_age_scotland,
                                                                        "Scotland",
                                                                        start_year = pop_est_start_year,
                                                                        end_year = pop_est_end_year) %>%
  mutate(`Scotland % change` = Perc) %>%
  select(`Age group`, `Scotland % change`)

# Merge the two datasets
data_table_pop_est_age <- merge(data_table_pop_est_age,
                                data_table_pop_est_age_scotland) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)



# Table 5: change by age group ==================================


data_pop_est_age_small <- pop_est_age_small %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_est_start_year, pop_est_end_year)) %>%
  ungroup()

# Paramsarea
data_table_pop_est_age_small <- table_age_year_perc_change_1_dataset(data_pop_est_age_small,
                                                                     paramsarea)
data_table_pop_est_age_small <- table_age_year_perc_change_2_dataset(data_table_pop_est_age_small,
                                                                     paramsarea,
                                                                     start_year = pop_est_start_year,
                                                                     end_year = pop_est_end_year) %>%
  mutate(`% change` = Perc) %>%
  select(-Perc)

# Scotland
data_table_pop_est_age_scotland_small <- table_age_year_perc_change_1_dataset(data_pop_est_age_small,
                                                                              "Scotland")
data_table_pop_est_age_scotland_small <- table_age_year_perc_change_2_dataset(data_table_pop_est_age_scotland_small,
                                                                              "Scotland",
                                                                              start_year = pop_est_start_year,
                                                                              end_year = pop_est_end_year) %>%
  mutate(`Scotland % change` = Perc) %>%
  select(`Age group`, `Scotland % change`)

# Merge the two datasets
data_table_pop_est_age_small <- merge(data_table_pop_est_age_small,
                                      data_table_pop_est_age_scotland_small) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)


# Population projections =======================================================



# Table 1: Total and % change by year ===============================


# Paramsarea
data_table_pop_proj <- table_total_and_perc_change_by_year(total_pop_proj,
                                                           paramsarea) %>%
  select(-Area) %>%
  rename(Population = Number)
names(data_table_pop_proj)[which(colnames(data_table_pop_proj) == "Perc")] <- paste("% change from",
                                                                                    pop_proj_start_year)

# Scotland
data_table_pop_proj_Scotland <- table_total_and_perc_change_by_year(total_pop_proj,
                                                                    "Scotland") %>%
  select(Year, Perc)
names(data_table_pop_proj_Scotland)[which(colnames(data_table_pop_proj_Scotland) == "Perc")] <- paste("Scotland % change from",
                                                                                                      pop_proj_start_year)

# Merge the two datasets
data_table_pop_proj <- merge(data_table_pop_proj, data_table_pop_proj_Scotland)



# Table 2: change by age group (detailed) =======================


data_pop_proj_age <- pop_proj %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_proj_start_year, pop_proj_end_year)) %>%
  ungroup()

# Paramsarea
data_table_pop_proj_age <- table_age_year_perc_change_1_dataset(data_pop_proj_age,
                                                                paramsarea)
data_table_pop_proj_age <- table_age_year_perc_change_2_dataset(data_table_pop_proj_age,
                                                                paramsarea,
                                                                start_year = pop_proj_start_year,
                                                                end_year = pop_proj_end_year) %>%
  mutate(`% change` = Perc) %>%
  select(-Perc)

# Scotland
data_table_pop_proj_age_scotland <- table_age_year_perc_change_1_dataset(data_pop_proj_age,
                                                                         "Scotland")
data_table_pop_proj_age_scotland <- table_age_year_perc_change_2_dataset(data_table_pop_proj_age_scotland,
                                                                         "Scotland",
                                                                         start_year = pop_proj_start_year,
                                                                         end_year = pop_proj_end_year) %>%
  mutate(`Scotland % change` = Perc) %>%
  select(`Age group`, `Scotland % change`)

# Merge the two datasets
data_table_pop_proj_age <- merge(data_table_pop_proj_age,
                                 data_table_pop_proj_age_scotland) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)



# Table 3: change by age group =================================================


pop_proj_age_small <- mutate(pop_proj,
                             Age_group = cut(Age,
                                             c(-1, 15, 64, 130),
                                             labels = c("0 to 15", "16 to 64",
                                                        "65 and over"))) %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("All ages", "0 to 15", "16 to 64",
                                       "65 and over")))

data_pop_proj_age_small <- pop_proj_age_small %>%
  group_by(Area, Year, Age_group) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_proj_start_year, pop_proj_end_year)) %>%
  ungroup()

# Paramsarea
data_table_pop_proj_age_small <- table_age_year_perc_change_1_dataset(data_pop_proj_age_small,
                                                                      paramsarea)
data_table_pop_proj_age_small <- table_age_year_perc_change_2_dataset(data_table_pop_proj_age_small,
                                                                      paramsarea,
                                                                      start_year = pop_proj_start_year,
                                                                      end_year = pop_proj_end_year) %>%
  mutate(`% change` = Perc) %>%
  select(-Perc)

# Scotland
data_table_pop_proj_age_scotland_small <- table_age_year_perc_change_1_dataset(data_pop_proj_age_small,
                                                                               "Scotland")
data_table_pop_proj_age_scotland_small <- table_age_year_perc_change_2_dataset(data_table_pop_proj_age_scotland_small,
                                                                               "Scotland",
                                                                               start_year = pop_proj_start_year,
                                                                               end_year = pop_proj_end_year) %>%
  mutate(`Scotland % change` = Perc) %>%
  select(`Age group`, `Scotland % change`)

# Merge the two datasets
data_table_pop_proj_age_small <- merge(data_table_pop_proj_age_small,
                                       data_table_pop_proj_age_scotland_small) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)



# Table 4: change by sex ========================================


pop_proj_sex <- pop_proj %>%
  group_by(Area, Year, Sex) %>%
  summarise(Number = sum(Number)) %>%
  filter(Year %in% c(pop_proj_start_year, pop_proj_end_year)) %>%
  ungroup()

# Paramsarea
data_table_pop_proj_sex <- table_sex_by_year_perc_change_1_dataset(pop_proj_sex,
                                                                   paramsarea)
data_table_pop_proj_sex <- table_sex_by_year_perc_change_2_dataset(data_table_pop_proj_sex,
                                                                   paramsarea,
                                                                   start_year = pop_proj_start_year,
                                                                   end_year = pop_proj_end_year) %>%
  mutate(`% change` = Perc) %>%
  select(-Perc)

# Scotland
data_table_pop_proj_sex_scotland <- table_sex_by_year_perc_change_1_dataset(pop_proj_sex, "Scotland")
data_table_pop_proj_sex_scotland <- table_sex_by_year_perc_change_2_dataset(data_table_pop_proj_sex_scotland,
                                                                            "Scotland",
                                                                            start_year = pop_proj_start_year,
                                                                            end_year = pop_proj_end_year) %>%
  mutate(`Scotland % change` = Perc) %>%
  select(Sex, `Scotland % change`)

# Merge the two datasets
data_table_pop_proj_sex <- merge(data_table_pop_proj_sex,
                                 data_table_pop_proj_sex_scotland) %>%
  mutate(Sex = factor(Sex, levels = c("All people", "Male", "Female"))) %>%
  arrange(Sex)


# Table 5: Components of change =================================


data_table_pop_proj_nature <- pop_proj_nature %>%
  filter(Area %in% c(paramsarea, "Scotland")) %>%
  select(-Population_change, -Natural_perc, -Mig_perc, -Population_change,
         -Total_perc, everything()) %>%
  rename(`Natural change` = Natural_change, `Net migration` = Net_migration,
         `Total change` = Population_change, `Natural % change` = Natural_perc,
         `Net migration % change` = Mig_perc, `Total % change` = Total_perc) %>%
  arrange(
    if (which(pop_proj_nature$Area == "Scotland") <
        which(pop_proj_nature$Area == paramsarea))
      rev(Area)
    else
      Area)



# Household estimates ==========================================================



# Table 1: Total by year with % change ========================================

# Paramsarea
data_table_house_est <- table_total_and_perc_change_by_year(total_house_est,
                                                            paramsarea) %>%
  select(-Area) %>%
  rename(`All households` = Number)
names(data_table_house_est)[which(colnames(data_table_house_est) == "Perc")] <-
  paste("% change from", house_est_start_year)

# Scotland
data_table_house_est_Scotland <- table_total_and_perc_change_by_year(total_house_est,
                                                                     "Scotland") %>%
  select(Year, Perc)
names(data_table_house_est_Scotland)[which(colnames(data_table_house_est_Scotland) == "Perc")] <-
  paste("Scotland % change from", house_est_start_year)

# Merge the two datasets
data_table_house_est <- merge(data_table_house_est, data_table_house_est_Scotland)



# Household projections ========================================================


# Table 1: Total by year with % change =========================================

# Paramsarea
data_table_house_proj <- table_total_and_perc_change_by_year(total_house_proj,
                                                             paramsarea) %>%
  select(-Area)
names(data_table_house_proj)[which(colnames(data_table_house_proj) == "Perc")] <-
  paste("% change from", house_proj_start_year)

# Scotland
data_table_house_proj_Scotland <- table_total_and_perc_change_by_year(total_house_proj,
                                                                      "Scotland") %>%
  select(Year, Perc)
names(data_table_house_proj_Scotland)[which(colnames(data_table_house_proj_Scotland) == "Perc")] <-
  paste("Scotland % change from", house_proj_start_year)

# Merge the two datasets
data_table_house_proj <- merge(data_table_house_proj,
                               data_table_house_proj_Scotland) %>%
  mutate(Number = round(Number, 0)) %>%
  rename(`All households` = Number)


# Table 2: change by age group of the Household Reference Person (HRP)

# Paramsarea
data_table_house_proj_age <- table_age_year_perc_change_1_dataset(house_proj_age_table,
                                                                  paramsarea)
data_table_house_proj_age <- table_age_year_perc_change_2_dataset(data_table_house_proj_age,
                                                                  paramsarea,
                                                                  start_year = house_proj_start_year,
                                                                  end_year = house_proj_end_year) %>%
  mutate(`% change` = Perc) %>%
  select(-Perc)

data_table_house_proj_age[as.character(house_proj_start_year)] <-
  round(data_table_house_proj_age[as.character(house_proj_start_year)], 0)
data_table_house_proj_age[as.character(house_proj_end_year)] <-
  round(data_table_house_proj_age[as.character(house_proj_end_year)], 0)

# Scotland
data_table_house_proj_age_scotland <-
  table_age_year_perc_change_1_dataset(house_proj_age_table, "Scotland")
data_table_house_proj_age_scotland <-
  table_age_year_perc_change_2_dataset(data_table_house_proj_age_scotland,
                                       "Scotland",
                                       start_year = house_proj_start_year,
                                       end_year = house_proj_end_year) %>%
  mutate(`Scotland % change` = Perc) %>%
  select(`Age group`, `Scotland % change`)

# Merge the two datasets
data_table_house_proj_age <- merge(data_table_house_proj_age,
                                   data_table_house_proj_age_scotland) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All HRPs")) %>%
  mutate(`Age group` = recode(`Age group`, "All people" = "All HRPs")) %>%
  arrange(`Age group`)


# Table 3: change by household type =============================


# Paramsarea
data_table_house_proj_type <- table_house_proj_by_type_1(house_proj_type_comp,
                                                         paramsarea)
data_table_house_proj_type <- table_house_proj_by_type_2(data_table_house_proj_type,
                                                         paramsarea,
                                                         house_proj_start_year, 
                                                         house_proj_end_year) %>%
  mutate(`% change` = Perc) %>%
  select(-Perc, -Area)

# Scotland
data_table_house_proj_type_scotland <- table_house_proj_by_type_1(house_proj_type_comp,
                                                                  "Scotland")
data_table_house_proj_type_scotland <- table_house_proj_by_type_2(data_table_house_proj_type_scotland,
                                                                  "Scotland",
                                                                  house_proj_start_year, 
                                                                  house_proj_end_year) %>%
  mutate(`Scotland % change` = Perc) %>%
  select(`Scotland % change`, Type)

# Merge the two datasets
data_table_house_proj_type <- merge(data_table_house_proj_type,
                                    data_table_house_proj_type_scotland,
                                    sort = FALSE)



# Dwellings ====================================================================


# Table 1: Total by year with % change ==============================

# Paramsarea
data_table_dwell <- table_total_and_perc_change_by_year(total_dwell_num,
                                                        paramsarea) %>%
  select(-Area) %>%
  rename(`All dwellings` = Number)
names(data_table_dwell)[which(colnames(data_table_dwell) == "Perc")] <-
  paste("% change from", dwell_est_start_year)

# Scotland
data_table_dwell_Scotland <- table_total_and_perc_change_by_year(total_dwell_num,
                                                                 "Scotland") %>%
  select(Year, Perc)
names(data_table_dwell_Scotland)[which(colnames(data_table_dwell_Scotland) == "Perc")] <-
  paste("Scotland % change from", dwell_est_start_year)

# Merge the two datasets
data_table_dwell <- merge(data_table_dwell, data_table_dwell_Scotland)


# Table 2: Dwellings by type  =============================================

# Paramsarea
data_table_dwell_type <- table_dwell_by_type_or_tax_dataset(data_dwell_type,
                                                            paramsarea) %>%
  mutate(`% of all dwellings` = Perc_column) %>%
  select(-Perc_column)

# Scotland
data_table_dwell_type_scotland <- table_dwell_by_type_or_tax_dataset(data_dwell_type,
                                                                     "Scotland") %>%
  mutate(`Scotland % of all dwellings` = Perc_column) %>%
  select(Type, `Scotland % of all dwellings`)

# Merge the two datasets
data_table_dwell_type <- merge(data_table_dwell_type,
                               data_table_dwell_type_scotland) %>%
  arrange(Type) %>%
  mutate(Type = recode(Type, "Total" = "All dwellings"))


# Table 3: Dwellings by council tax band (detailed)  ======================


# Paramsarea
data_table_dwell_tax <- table_dwell_by_type_or_tax_dataset(data_dwell_tax,
                                                           paramsarea) %>%
  mutate(`% of all dwellings` = Perc_column) %>%
  select(-Perc_column)

# Scotland
data_table_dwell_tax_scotland <- table_dwell_by_type_or_tax_dataset(data_dwell_tax,
                                                                    "Scotland") %>%
  mutate(`Scotland % of all dwellings` = Perc_column) %>%
  select(Type, `Scotland % of all dwellings`)

# Merge the two datasets
data_table_dwell_tax <- merge(data_table_dwell_tax, data_table_dwell_tax_scotland) %>%
  mutate(Type = factor(Type,
                       levels = c("Total", "A", "B", "C", "D", "E", "F", "G",
                                  "H"))) %>%
  mutate(Type = recode(Type, "Total" = "All dwellings")) %>%
  arrange(Type) %>%
  rename(`Tax band` = Type)


# Table 4: Dwellings by council tax band  =================================


# Paramsarea
data_table_dwell_tax_grouped <- table_dwell_by_type_or_tax_dataset(data_dwell_tax_grouped,
                                                                   paramsarea) %>%
  mutate(`% of all dwellings` = Perc_column) %>%
  select(-Perc_column)

# Scotland
data_table_dwell_tax_grouped_scotland <-
  table_dwell_by_type_or_tax_dataset(data_dwell_tax_grouped, "Scotland") %>%
  mutate(`Scotland % of all dwellings` = Perc_column) %>%
  select(Type, `Scotland % of all dwellings`)

# Merge the two datasets
data_table_dwell_tax_grouped <- merge(data_table_dwell_tax_grouped,
                                      data_table_dwell_tax_grouped_scotland) %>%
  mutate(Type = factor(Type, levels = c("Total", "A - C", "D - E", "F - H"))) %>%
  mutate(Type = recode(Type, "Total" = "All dwellings")) %>%
  arrange(Type) %>%
  rename(`Tax band` = Type)



# Marriages ====================================================================


# Table 1: Total by year with %  =====================================

data_table_marr <- table_total_and_perc_of_pop_by_year(total_marr,
                                                       paramsarea) %>%
  rename(`All marriages` = Total)



# Civil partnerships ===========================================================


# Table 1: Total by year by sex =====================================


data_table_cp_sex <- table_year_by_sex(data_cp_gender,
                                       paramsarea) %>%
  rename(`All civil partnerships` = Total)




# Births =======================================================================


# Table 1: Total by year by sex  =====================================

# Paramsarea
data_table_births_sex <- table_total_and_sex_perc_change_by_year_ca(total_births_sex_all,
                                                                    paramsarea,
                                                                    bir_dea_marr_est_start_year) %>%
  rename(`All births` = Total)
names(data_table_births_sex)[which(colnames(data_table_births_sex) == "Perc")] <-
  paste("% change from", bir_dea_marr_est_start_year)

# Scotland
data_table_births_sex_scotland <- table_total_and_sex_perc_change_by_year_ca(total_births_sex_all,
                                                                             "Scotland",
                                                                             bir_dea_marr_est_start_year) %>%
  select(Year, Perc)
names(data_table_births_sex_scotland)[which(colnames(data_table_births_sex_scotland) == "Perc")] <-
  paste("Scotland % change from", bir_dea_marr_est_start_year)

# Merge the two datasets
data_table_births_sex <- merge(data_table_births_sex,
                               data_table_births_sex_scotland)



# Table 2: Birth rate by year,  =====================================

# Paramsarea
data_table_births_rate <- births_rate %>%
  filter(Area == paramsarea) %>%
  transform(Year = as.character(Year)) %>%
  rename(`Standardised birth rate` = Number)

# Scotland
data_table_births_rate_scotland <- births_rate %>%
  filter(Area == "Scotland") %>%
  transform(Year = as.character(Year)) %>%
  select(-Area) %>%
  rename(`Scottish birth rate` = Number)

# Merge the two datasets
data_table_births_rate <- merge(data_table_births_rate,
                                data_table_births_rate_scotland) %>%
  select(-Area)



# Table 3: change by age group of mother =======================

# Paramsarea
data_table_total_births_mothers_age_perc <- total_births_mothers_age_perc %>%
  select(-Area, -Sign) %>%
  rename(`% change` = Perc)

# Scotland
data_table_total_births_mothers_age_perc_scotland <-
  total_births_mothers_age_perc_scotland %>%
  select(Age_group, Perc) %>%
  rename(`Scotland % change` = Perc)

# Merge the two datasets
data_table_total_births_mothers_age_perc <-
  merge(data_table_total_births_mothers_age_perc,
        data_table_total_births_mothers_age_perc_scotland) %>%
  arrange(Age_group) %>%
  rename(`Age group of mother` = Age_group)


# Table 4: Fertility rates ==========================================

# Paramsarea
data_table_fert_rate <- table_total_and_perc_change_by_year(fert_rate,
                                                            paramsarea) %>%
  select(Year, Number, Perc) %>%
  rename(`Approximated total fertility rate` = Number)
names(data_table_fert_rate)[which(colnames(data_table_fert_rate) == "Perc")] <-
  paste("% change from", bir_dea_marr_est_start_year)

# Scotland
data_table_fert_rate_scotland <- table_total_and_perc_change_by_year(fert_rate,
                                                                     "Scotland") %>%
  select(Year, Perc)
names(data_table_fert_rate_scotland)[which(colnames(data_table_fert_rate_scotland) == "Perc")] <-
  paste("Scotland % change from", bir_dea_marr_est_start_year)

# Merge the two datasets
data_table_fert_rate <- merge(data_table_fert_rate,
                              data_table_fert_rate_scotland)



# Deaths =======================================================================


# Table 1: Total by year by sex ================================================

# Paramsarea
data_table_deaths_sex <- table_total_and_sex_perc_change_by_year_ca(total_deaths_sex_all,
                                                                    paramsarea, 
                                                                    bir_dea_marr_est_start_year) %>%
  rename(`All people` = Total)
names(data_table_deaths_sex)[which(colnames(data_table_deaths_sex) == "Perc")] <-
  paste("% change from", bir_dea_marr_est_start_year)

# Scotland
data_table_deaths_sex_scotland <-
  table_total_and_sex_perc_change_by_year_ca(total_deaths_sex_all,
                                             "Scotland", 
                                             bir_dea_marr_est_start_year) %>%
  select(Year, Perc)
names(data_table_deaths_sex_scotland)[which(colnames(data_table_deaths_sex_scotland) == "Perc")] <-
  paste("Scotland % change from", bir_dea_marr_est_start_year)

# Merge the two datasets
data_table_deaths_sex <- merge(data_table_deaths_sex,
                               data_table_deaths_sex_scotland)


# Table 2: Death rate by year ==================================================

# Paramsarea
data_table_deaths_rate <- deaths_rate %>%
  filter(Area == paramsarea) %>%
  transform(Year = as.character(Year)) %>%
  rename(`Standardised death rate` = Number)

# Scotland
data_table_deaths_rate_scotland <- deaths_rate %>%
  filter(Area == "Scotland") %>%
  transform(Year = as.character(Year)) %>%
  select(-Area) %>%
  rename(`Scottish death rate` = Number)

# Merge the two datasets
data_table_deaths_rate <- merge(data_table_deaths_rate,
                                data_table_deaths_rate_scotland) %>%
  select(-Area)


# Table 3: Total by sex and % change  ==========================================

# Paramsarea
data_table_total_deaths_sex_comp_table <-
  table_sex_by_year_perc_change_1_dataset(total_deaths_sex_comp_table,
                                          paramsarea)
data_table_total_deaths_sex_comp_table <-
  table_sex_by_year_perc_change_2_dataset(data_table_total_deaths_sex_comp_table,
                                          paramsarea,
                                          start_year = bir_dea_marr_est_start_year,
                                          end_year = bir_dea_marr_cp_est_end_year) %>%
  rename(`% change` = Perc)

# Scotland
data_table_total_deaths_sex_comp_table_scotland <-
  table_sex_by_year_perc_change_1_dataset(total_deaths_sex_comp_table,
                                          "Scotland")
data_table_total_deaths_sex_comp_table_scotland <-
  table_sex_by_year_perc_change_2_dataset(data_table_total_deaths_sex_comp_table_scotland,
                                          "Scotland",
                                          start_year = bir_dea_marr_est_start_year,
                                          end_year = bir_dea_marr_cp_est_end_year) %>%
  rename(`Scotland % change` = Perc) %>%
  select(Sex, `Scotland % change`)

# Merge the two datasets for table6
data_table_total_deaths_sex_comp_table <-
  merge(data_table_total_deaths_sex_comp_table,
        data_table_total_deaths_sex_comp_table_scotland) %>%
  mutate(Sex = factor(Sex, levels = c("All people", "Male", "Female"))) %>%
  arrange(Sex)



# Table 4: Total by age group by sex ===========================================

# Paramsarea
data_table_total_deaths_age_sex <-
  table_age_gender_dataset(total_deaths_age_sex_comp,
                           Area_name = paramsarea) %>%
  rename(`% of all deaths` = Perc_column)

# Scotland
data_table_total_deaths_age_sex_scotland <-
  table_age_gender_dataset(total_deaths_age_sex_comp,
                           Area_name = "Scotland") %>%
  mutate(`Scotland % of all deaths` = Perc_column) %>%
  select(`Age group`, `Scotland % of all deaths`)

# Merge the two datasets
data_table_total_deaths_age_sex <-
  merge(data_table_total_deaths_age_sex,
        data_table_total_deaths_age_sex_scotland) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)



# Tables 5 and 6: Leading cause of death by sex ==================================


# Female
data_table_deaths_cause_f <- deaths_cause_f_comp %>%
  filter(Area == paramsarea) %>%
  rename(`% of all female deaths` = Perc) %>%
  select(-Area, -Cause) %>%
  arrange(-Number) %>%
  rename(`Female deaths` = Number, `Leading causes of death` = Type) %>%
  select(-Year, -Total, -Sex, -Cause_label)

# Male
data_table_deaths_cause_m <- deaths_cause_m_comp %>%
  filter(Area == paramsarea) %>%
  rename(`% of all male deaths` = Perc) %>%
  select(-Area, -Cause) %>%
  arrange(-Number) %>%
  rename(`Male deaths` = Number, `Leading causes of death` = Type) %>%
  select(-Year, -Total, -Sex, -Cause_label)



# Migration ====================================================================



# Table 1: Total by year by type (in, out, net) =====================

data_table_mig_by_type <- table_mig_by_type(total_mig,
                                            paramsarea) %>%
  mutate(Year = Year_2) %>%
  select(-Area, -Year_2)


# Table 2: Net migration rate by year, 2008-09 to 2015-16 ======================

# Paramsarea
data_table_net_mig_rates <- net_mig_rates %>%
  filter(Area == paramsarea) %>%
  transform(Year = as.character(Year)) %>%
  select(Year_2, Number) %>%
  rename(`Net migration rate` = Number,
         Year = Year_2)

# Scotland
data_table_net_mig_rates_scotland <- net_mig_rates %>%
  filter(Area == "Scotland") %>%
  transform(Year = as.character(Year)) %>%
  select(Year_2, Number) %>%
  rename(`Scottish Net migration rate` = Number,
         Year = Year_2)

# Merge the two datasets
data_table_net_mig_rates <- merge(data_table_net_mig_rates,
                                  data_table_net_mig_rates_scotland)



# Table 3: Net migration by age group by sex ==========================

data_table_net_mig_age_sex <- table_age_gender_dataset(net_mig_age_sex,
                                                       paramsarea) %>%
  select(-Year, -Perc_column) %>%
  mutate(`Age group` = recode(`Age group`, "All ages" = "All people")) %>%
  arrange(`Age group`)



# Life expectancy =============================================================


# Table 1: Life expectancy at birth by sex =================


# Females at birth

# Paramsarea
data_table_life_exp_birth_f <- table_life_exp(life_exp_from_birth,
                                              paramsarea,
                                              "Female") %>%
  select(Year, Lower_CI, Estimate, Upper_CI, Perc) %>%
  rename(`Lower CI` = Lower_CI, `Upper CI` = Upper_CI)
names(data_table_life_exp_birth_f)[which(colnames(data_table_life_exp_birth_f) == "Perc")] <-
  paste("Estimate % change from", life_exp_start_year)

# Scotland
data_table_life_exp_birth_f_scotland <- table_life_exp(life_exp_from_birth,
                                                       "Scotland",
                                                       "Female") %>%
  select(Year, Perc)
names(data_table_life_exp_birth_f_scotland)[which(colnames(data_table_life_exp_birth_f_scotland) == "Perc")] <-
  paste("Scotland Estimate % change from", life_exp_start_year)

# Merge the two datasets
data_table_life_exp_birth_f <- merge(data_table_life_exp_birth_f,
                                     data_table_life_exp_birth_f_scotland)


# Males at birth

# Paramsarea
data_table_life_exp_birth_m <- table_life_exp(life_exp_from_birth,
                                              paramsarea,
                                              "Male") %>%
  select(Year, Lower_CI, Estimate, Upper_CI, Perc) %>%
  rename(`Lower CI` = Lower_CI, `Upper CI` = Upper_CI)
names(data_table_life_exp_birth_m)[which(colnames(data_table_life_exp_birth_m) == "Perc")] <-
  paste("Estimate % change from", life_exp_start_year)

# Scotland
data_table_life_exp_birth_m_scotland <- table_life_exp(life_exp_from_birth,
                                                       "Scotland",
                                                       "Male") %>%
  select(Year, Perc)
names(data_table_life_exp_birth_m_scotland)[which(colnames(data_table_life_exp_birth_m_scotland) == "Perc")] <-
  paste("Scotland Estimate % change from", life_exp_start_year)

# Merge the two datasets
data_table_life_exp_birth_m <- merge(data_table_life_exp_birth_m,
                                     data_table_life_exp_birth_m_scotland)



# Table 2: Life expectancy at age 65-69 by sex =============


# Females at age 65-69

# Paramsarea
data_table_life_exp_at_65_f <- table_life_exp(life_exp_at_65,
                                              paramsarea,
                                              "Female") %>%
  select(Year, Lower_CI, Estimate, Upper_CI, Perc) %>%
  rename(`Lower CI` = Lower_CI, `Upper CI` = Upper_CI)
names(data_table_life_exp_at_65_f)[which(colnames(data_table_life_exp_at_65_f) == "Perc")] <-
  paste("Estimate % change from", life_exp_start_year)

# Scotland
data_table_life_exp_at_65_f_scotland <- table_life_exp(life_exp_at_65,
                                                       "Scotland",
                                                       "Female") %>%
  select(Year, Perc)
names(data_table_life_exp_at_65_f_scotland)[which(colnames(data_table_life_exp_at_65_f_scotland) == "Perc")] <-
  paste("Scotland Estimate % change from", life_exp_start_year)

# Merge the two datasets
data_table_life_exp_at_65_f <- merge(data_table_life_exp_at_65_f,
                                     data_table_life_exp_at_65_f_scotland)


# Males at age 65-69

# Paramsarea
data_table_life_exp_at_65_m <- table_life_exp(life_exp_at_65,
                                              paramsarea,
                                              "Male") %>%
  select(Year, Lower_CI, Estimate, Upper_CI, Perc) %>%
  rename(`Lower CI` = Lower_CI, `Upper CI` = Upper_CI)
names(data_table_life_exp_at_65_m)[which(colnames(data_table_life_exp_at_65_m) == "Perc")] <-
  paste("Estimate % change from", life_exp_start_year)

# Scotland
data_table_life_exp_at_65_m_scotland <- table_life_exp(life_exp_at_65,
                                                       "Scotland",
                                                       "Male") %>%
  select(Year, Perc)
names(data_table_life_exp_at_65_m_scotland)[which(colnames(data_table_life_exp_at_65_m_scotland) == "Perc")] <-
  paste("Scotland Estimate % change from", life_exp_start_year)

# Merge the two datasets
data_table_life_exp_at_65_m <- merge(data_table_life_exp_at_65_m,
                                     data_table_life_exp_at_65_m_scotland)



# Functions for table creation ================================================


table <- function(dataset){
  dataset %>%
    kable("html",
          format.args = list(big.mark = ",", keep.trailing.zeros = TRUE),
          digits = 1,
          align = c("l", rep("c", ncol(dataset) - 1))) %>% #, padding = -1
    kable_styling(bootstrap_options = "striped",
                  position = "left",
                  full_width = F) %>%
    column_spec(1, bold = T, width = "7em") %>%
    column_spec(2:ncol(dataset), width = "8em")
}


# Two digit rounding, used for fertility rates table
table_two_digits <- function(dataset){
  dataset %>%
    kable("html",
          format.args = list(big.mark = ",", keep.trailing.zeros = TRUE),
          digits = 2,
          align = c("l", rep("c", ncol(dataset) - 1))) %>% #, padding = -1
    kable_styling(bootstrap_options = "striped",
                  position = "left",
                  full_width = F) %>%
    column_spec(1, bold = T, width = "7em") %>%
    column_spec(2:ncol(dataset), width = "8em")
}

# Wide first column, used for leading cause of death tables
table_wide_left <- function(dataset){
  dataset %>%
    kable("html",
          format.args = list(big.mark = ",", keep.trailing.zeros = TRUE),
          digits = 1,
          align = c("l", rep("c", ncol(dataset) - 1))) %>% #, padding = -1
    kable_styling(bootstrap_options = "striped",
                  position = "left",
                  full_width = F) %>%
    column_spec(1, bold = T, width = "26em") %>%
    column_spec(2:ncol(dataset), width = "8em")
}



# All tables ===================================================================


# Population estimates =========================================================


# Total population
table_pop_est <- table(dataset = data_table_pop_est)

# Age group (detailed) by sex
table_pop_est_sex_age <- table(dataset = data_table_pop_est_sex_age)

# Age group by sex
table_pop_est_sex_age_small <- table(dataset = data_table_pop_est_sex_age_small)

# change by age group (detailed)
table_pop_est_age <- table(dataset = data_table_pop_est_age)

# Change by age group
table_pop_est_age_small <- table(data_table_pop_est_age_small)


# Population projections =========================================================


# Total projected population
table_pop_proj <- table(dataset = data_table_pop_proj)

# Components of population projection
table_pop_proj_nature <- table(dataset = data_table_pop_proj_nature)

# Change by age group (detailed)
table_pop_proj_age <- table(dataset = data_table_pop_proj_age)

# Change by age group
table_pop_proj_age_small <- table(data_table_pop_proj_age_small)

# Change by sex
table_pop_proj_sex <- table(data_table_pop_proj_sex)


# Household estimates =========================================================


# Total households
table_house_est <- table(dataset = data_table_house_est)


# Household projections =======================================================


# Total projected households
table_house_proj <- table(dataset = data_table_house_proj)

# Change by age group of the Household Reference Person (HRP)
table_house_proj_age <- table(dataset = data_table_house_proj_age)

# Change by household type
table_house_proj_type <- table(dataset = data_table_house_proj_type)


# Dwellings ===================================================================


# Total dwellings
table_dwell <- table(dataset = data_table_dwell)

# Dwellings by type
table_dwell_type <- table(dataset = data_table_dwell_type)

# Dwellings by council tax band (detailed)
table_dwell_tax <- table(dataset = data_table_dwell_tax)

# Dwellings by council tax band
table_dwell_tax_grouped <- table(dataset = data_table_dwell_tax_grouped)


# Marriages ===================================================================


# Total marriages
table_marr <- table(dataset = data_table_marr)


# Civil partnerships ==========================================================


# Number of CPs by year by sex
table_cp_sex <- table(dataset = data_table_cp_sex)


# Births ======================================================================


# Total births by year by sex
table_births_sex <- table(dataset = data_table_births_sex)

# Birth rates by year
table_births_rate <- table(dataset = data_table_births_rate)

# Total births by mother's age group and % change
table_births_mothers_age_perc <-
  table(dataset = data_table_total_births_mothers_age_perc)

# Fertility rates
table_fert_rate <- table_two_digits(dataset = data_table_fert_rate)


# Deaths ======================================================================


# Total deaths by year by sex
table_deaths_sex <- table(dataset = data_table_deaths_sex)

# Death rates by year
table_deaths_rate <- table(dataset = data_table_deaths_rate)

# Total deaths by sex and % change
table_deaths_sex_comp <- table(dataset = data_table_total_deaths_sex_comp_table)

# Total deaths by age group by sex and % of all deaths
table_deaths_age_sex <- table(dataset = data_table_total_deaths_age_sex)

# Total female deaths by cause and % of all female deaths
table_deaths_cause_f <- table_wide_left(dataset = data_table_deaths_cause_f)

# Total male deaths by cause and % of all male deaths
table_deaths_cause_m <- table_wide_left(dataset = data_table_deaths_cause_m)


# Migration ===================================================================


# Total migration by year by type (in, out, net)
table_mig_by_type <- table(data_table_mig_by_type)

# Net migrations rate by year
table_net_mig_rates <- table(data_table_net_mig_rates)

# Net migration by age group by sex
table_net_mig_age_sex <- table(data_table_net_mig_age_sex)


paramsarea <- area
# Life expectancy =============================================================


# Female life expectancy at birth
table_life_exp_from_birth_f <- table(data_table_life_exp_birth_f)

# Male life expectancy at birth
table_life_exp_from_birth_m <- table(data_table_life_exp_birth_m)

# Female life expectancy at age 65-69
table_life_exp_at_65_f <- table(data_table_life_exp_at_65_f)

# Male life expectancy at age 65-69
table_life_exp_at_65_m <- table(data_table_life_exp_at_65_m)

