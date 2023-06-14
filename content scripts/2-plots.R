# All plots using function plot_one_CA_num ====================================

p_pop_est_one_CA <- plot_one_CA_num(
  dataset = total_pop_est_CA,
  col = pop,
  title = paramsarea,
  subtitle = paste0("Total population, ", pop_est_start_year,
                    "-", pop_est_end_year)) +
  labs(x = "Year to 30 June")


p_pop_proj_one_CA <- plot_one_CA_num(
  dataset = total_pop_proj_CA,
  col = pop,
  title = paramsarea,
  subtitle = paste0("Projected population, ", pop_proj_start_year,
                    "-", pop_proj_end_year))


p_house_est_num_one_CA <- plot_one_CA_num(
  dataset = total_house_est_CA,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Number of households, ", house_est_start_year,
                    "-", house_est_end_year)) +
  scale_x_continuous(
    breaks = seq(min(total_house_est_CA$Year), max(total_house_est_CA$Year), 4),
    expand = c(0.2, 0.2))


p_dwell_num_one_CA <- plot_one_CA_num(
  dataset = total_dwell_num_CA,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Number of dwellings, ", dwell_est_start_year,
                    "-", dwell_est_end_year)) +
  scale_x_continuous(
    breaks = seq(min(total_dwell_num_CA$Year) + 1,
                 max(total_dwell_num_CA$Year), 4),
    expand = c(0.2, 0.2))


p_marr_one_CA <- plot_one_CA_num(
  dataset = total_marr_CA,
  col = mar,
  title = paramsarea,
  subtitle = paste0("Number of marriages, ", bir_dea_marr_est_start_year,
                    "-", bir_dea_marr_cp_est_end_year))


p_cp_one_CA <- plot_one_CA_num(
  dataset = total_cp_CA,
  col = mar,
  title = paramsarea,
  subtitle = paste0("Number of civil partnerships, ", cp_start_year,
                    "-", cp_end_yr)) 


p_house_proj_one_CA <- plot_one_CA_num(
  dataset = total_house_proj_CA,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Projected number of households, ", house_proj_start_year,
                    "-", house_proj_end_year))



# All plots using function plot_one_CA_sex ====================================


p_births_sex <- plot_one_CA_sex(
  dataset = total_births_sex,
  col = bir,
  title = paramsarea,
  subtitle = paste0("Births by sex, ", bir_dea_marr_est_start_year,
                    "-", bir_dea_marr_cp_est_end_year))

p_deaths_sex <- plot_one_CA_sex(
  dataset = total_deaths_sex,
  col = dea,
  title = paramsarea,
  subtitle = paste0("Deaths by sex, ", bir_dea_marr_est_start_year,
                    "-", bir_dea_marr_cp_est_end_year))



# All plots using function plot_one_CA_type ===================================


p_mig_type <- plot_one_CA_type(
  dataset = total_in_out_mig_ca,
  col = mig,
  title = paramsarea,
  subtitle = paste0("Total in and out migration, ", mig_start_year,
                    " to ", mig_end_year, "*"))

# All plots using function plot_all_CA_num ====================================


p_pop_est <- plot_all_CA_num(
  dataset = total_pop_est_all_CA,
  col = pop,
  title = "Council areas of Scotland",
  subtitle = paste0("Total population, ", pop_est_start_year,
                    "-", pop_est_end_year),
  paramsarea) +
  labs(x = "Year to 30 June")


p_pop_proj <- plot_all_CA_num(
  dataset = total_pop_proj_all_CA,
  col = pop,
  title = "Council areas of Scotland",
  subtitle = paste0("Total projected population, ", pop_proj_start_year,
                    "-", pop_proj_end_year),
  paramsarea)


p_marr_num <- plot_all_CA_num(
  dataset = total_marr_all_CA,
  col = mar,
  title = "Council areas of Scotland",
  subtitle = paste0("Number of marriages, ", bir_dea_marr_est_start_year,
                    "-", bir_dea_marr_cp_est_end_year),
  paramsarea)


p_cp_num <- plot_all_CA_num(
  dataset = total_cp_all_CA,
  col = mar,
  title = "Council areas of Scotland",
  subtitle = paste0("Number of civil partnerships, ", cp_start_year,
                    "-", cp_end_yr),
  paramsarea)


p_house_est_num <- plot_all_CA_num(
  dataset = data_house_est_all_CA,
  col = hou,
  title = "Council areas of Scotland",
  subtitle = paste0("Number of households, ", house_est_start_year,
                    "-", house_est_end_year),
  paramsarea) +
  scale_x_continuous(
    breaks = seq(min(data_house_est_all_CA$Year),
                 max(data_house_est_all_CA$Year), 4),
    limits = c(min(data_house_est_all_CA$Year),
               max(data_house_est_all_CA$Year) +
                 0.31 * (max(data_house_est_all_CA$Year) -
                           min(data_house_est_all_CA$Year))))


data_dwell_num_all_CA <- data_dwell_num %>%
  filter(Area != "Scotland")
p_dwell_num <- plot_all_CA_num(
  dataset = data_dwell_num_all_CA,
  col = hou,
  title = "Council areas of Scotland",
  subtitle = paste0("Number of dwellings, ", dwell_est_start_year,
                    "-", dwell_est_end_year),
  paramsarea) +
  scale_x_continuous(breaks = seq(min(data_dwell_num_all_CA$Year) + 1,
                                  max(data_dwell_num_all_CA$Year), 4),
                     limits = c(min(data_dwell_num_all_CA$Year),
                                max(data_dwell_num_all_CA$Year) +
                                  0.31 * (max(data_dwell_num_all_CA$Year) -
                                            min(data_dwell_num_all_CA$Year))))


p_house_proj <- plot_all_CA_num(
  dataset = total_house_proj_all_CA,
  col = hou,
  title = "Council areas of Scotland",
  subtitle = paste0("Projected number of households, ", house_proj_start_year,
                    "-", house_proj_end_year),
  paramsarea)



# All plots using function plot_all_CA_num_mig ================================


# For net migration, min(Year) doesn't work since using string values
# eg. 2001-02.
p_net_mig <- plot_all_CA_num_mig(
  dataset = total_net_mig_all_CA,
  col = mig,
  title = "Council areas of Scotland",
  subtitle = paste0("Total net migration, ", mig_start_year, " to ",
                    mig_end_year, "*"), 
  paramsarea) +
  geom_segment(
    aes(y = 0, yend = 0, x = min(Year), xend = max(Year)),
    linetype = 2)



# All plots using function plot_all_CA_rate ===================================


p_births_fert_rate <- plot_all_CA_rate(
  dataset = fert_rate,
  col = bir,
  title = "Council areas of Scotland",
  subtitle = paste0("Approximate total fertility rates (1), ",
                    bir_dea_marr_est_start_year, "-",
                    bir_dea_marr_cp_est_end_year),
  num_digits = 2, 
  paramsarea) +
  labs(caption = "(1) The approximate total fertility rate is defined to be the average number of children (per woman) that would \n be born to a cohort of women if they experienced, throughout their childbearing years, the age-specific fertility \n rates of the year in question. These rates are approximate because they were calculated using fertility rates \n for 5-year age-groups rather than for individual years of age.")


p_births_rate <- plot_all_CA_rate(
  dataset = births_rate,
  col = bir,
  title = "Council areas of Scotland",
  subtitle = paste0("Standardised birth rates (1), ",
                    bir_dea_marr_est_start_year, "-",
                    bir_dea_marr_cp_est_end_year),
  num_digits = 1, 
  paramsarea) +
  labs(caption = "(1) Births per 1,000 population - 'standardised' using the age/sex-specific rates for Scotland as a whole.")


p_deaths_rate <- plot_all_CA_rate(
  dataset = deaths_rate,
  col = dea,
  title = "Council areas of Scotland",
  subtitle = paste0("Standardised death rates (1), ",
                    bir_dea_marr_est_start_year, "-",
                    bir_dea_marr_cp_est_end_year),
  num_digits = 1, 
  paramsarea) +
  labs(caption = "(1) Deaths per 1,000 population - 'standardised' using the age/sex-specific rates for Scotland as a whole.")


# All plots using function plot_all_CA_rate_mig ===============================


p_net_mig_rates <- plot_all_CA_rate_mig(
  dataset = net_mig_rates,
  col = mig,
  title = "Council areas of Scotland",
  subtitle = paste0("Net migration rates (1), ", mig_rate_start_year,
                    " to ", mig_end_year),
  num_digits = 1, 
  paramsarea) +
  geom_segment(
    aes(y = 0,
        yend = 0,
        x = net_mig_rates$Year[net_mig_rates$Year == min(net_mig_rates$Year)][1],
        xend = net_mig_rates$Year[net_mig_rates$Year == max(net_mig_rates$Year)][1]),
    linetype = 2) +
  scale_x_continuous(
    breaks = unique(net_mig_rates$Year),
    limits = c(min(net_mig_rates$Year),
               max(net_mig_rates$Year) +
                 0.32 * (max(net_mig_rates$Year) - min(net_mig_rates$Year))),
    labels = str_wrap(
      stri_replace_all_fixed(unique(net_mig_rates$Year_2),
                             "-", " -"),
      width = 2)) +
  labs(caption = "(1) People per 1,000 population.")



# All plots using function plot_CA_scot_num_type ==============================


p_life_exp_from_birth <- plot_CA_scot_num_type(
  dataset = life_exp_from_birth_comp,
  col = lif,
  title = paramsarea,
  subtitle = paste0("Life expectancy at birth, ", life_exp_start_year,
                    " to ", life_exp_end_year),
  neg_poss = FALSE, 
  paramsarea)

p_life_exp_at_65 <- plot_CA_scot_num_type(
  dataset = life_exp_at_65_comp,
  col = lif,
  title = paramsarea,
  subtitle = paste0("Life expectancy at age 65-69, ", life_exp_start_year,
                    " to ", life_exp_end_year),
  neg_poss = FALSE, 
  paramsarea)


# All plots using function plot_all_CA_perc ===================================
# Scotland included as well.


p_pop_est_perc <- plot_all_CA_perc(
  dataset = total_pop_est,
  col = pop,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in population, ", pop_est_start_year,
                    "-", pop_est_end_year),
  neg_poss = TRUE, 
  paramsarea) +
  labs(x = "Year to 30 June")


p_pop_proj_perc <- plot_all_CA_perc(
  dataset = total_pop_proj,
  col = pop,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in projected population, ",
                    pop_proj_start_year, "-", pop_proj_end_year),
  neg_poss = TRUE, 
  paramsarea)


p_house_est_perc <- plot_all_CA_perc(
  dataset = total_house_est,
  col = hou,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in the number of households, ",
                    house_est_start_year, "-", house_est_end_year),
  neg_poss = TRUE, 
  paramsarea) +
  scale_x_continuous(
    breaks = seq(min(total_house_est$Year), max(total_house_est$Year), 4),
    limits = c(min(total_house_est$Year),
               max(total_house_est$Year) +
                 0.37 * (max(total_house_est$Year) -
                           min(total_house_est$Year))))


p_dwell_num_perc <- plot_all_CA_perc(
  dataset = total_dwell_num,
  col = hou,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in the number of dwellings, ",
                    dwell_est_start_year, "-", dwell_est_end_year),
  neg_poss = TRUE, 
  paramsarea) +
  scale_x_continuous(
    breaks = seq(min(total_dwell_num$Year) + 1, max(total_dwell_num$Year), 4),
    limits = c(min(total_dwell_num$Year),
               max(total_dwell_num$Year) +
                 0.37 * (max(total_dwell_num$Year) -
                           min(total_dwell_num$Year))))


p_house_proj_perc <- plot_all_CA_perc(
  dataset = total_house_proj,
  col = hou,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in projected households, ",
                    house_proj_start_year, "-", house_proj_end_year),
  neg_poss = TRUE, 
  paramsarea)


p_births_perc <- plot_all_CA_perc(
  dataset = total_births,
  col = bir,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in the number of births, ",
                    bir_dea_marr_est_start_year, "-",
                    bir_dea_marr_cp_est_end_year),
  neg_poss = TRUE, 
  paramsarea)


p_deaths_perc <- plot_all_CA_perc(
  dataset = total_deaths,
  col = dea,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in the number of deaths by sex, ",
                    bir_dea_marr_est_start_year, "-",
                    bir_dea_marr_cp_est_end_year),
  neg_poss = TRUE, 
  paramsarea)



# All plots using function plot_all_CA_perc_mig_life ==========================


p_life_exp_from_birth_f <- plot_all_CA_perc_mig_life(
  dataset = life_exp_from_birth_f,
  col = lif,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in female life expectancy at birth, ",
                    life_exp_start_year, " to ", life_exp_end_year),
  neg_poss = TRUE, 
  paramsarea)


p_life_exp_from_birth_m <- plot_all_CA_perc_mig_life(
  dataset = life_exp_from_birth_m,
  col = lif,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in male life expectancy at birth, ",
                    life_exp_start_year, " to ", life_exp_end_year),
  neg_poss = TRUE, 
  paramsarea)


p_life_exp_at_65_f <- plot_all_CA_perc_mig_life(
  dataset = life_exp_at_65_f,
  col = lif,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in female life expectancy at age 65-69, ",
                    life_exp_start_year, " to ", life_exp_end_year),
  neg_poss = TRUE, 
  paramsarea)


p_life_exp_at_65_m <- plot_all_CA_perc_mig_life(
  dataset = life_exp_at_65_m,
  col = lif,
  title = "Council areas of Scotland",
  subtitle = paste0("Percentage change in male life expectancy at age 65-69, ",
                    life_exp_start_year, " to ", life_exp_end_year),
  neg_poss = TRUE, 
  paramsarea)



# All plots using function plot_pyramid_est_comp ==============================


# Population estimates
ymax_pop_est <- max(total_pop_est_gender$Number) * 1.1
coords_df_pop_est <- data.frame(
  x_coord = rep(c(-min(total_pop_est_gender$Number) / 2,
                  min(total_pop_est_gender$Number) / 2),
                each = 100),
  y_coord = c(-1:98, 98:-1))

p_pop_est_pyramid <- plot_pyramid_est_comp(
  dataset = total_pop_est_gender_comp,
  base_year = pop_est_end_year,
  comp_year = pop_est_start_year,
  ymax = ymax_pop_est,
  coords_df = coords_df_pop_est,
  col = pop,
  title = paramsarea,
  subtitle = paste0("Population profile, ", pop_est_start_year, " and ",
                    pop_est_end_year))



# All plots using function plot_pyramid_proj_comp =============================


# Population projections
ymax_pop_proj <- max(total_pop_proj_sex$Number) * 1.2
coords_df_pop_proj <- data.frame(
  x_coord = rep(c(-min(total_pop_proj_sex$Number) / 2,
                  min(total_pop_proj_sex$Number) / 2),
                each = 100),
  y_coord = c(-1:98, 98:-1))

p_pop_proj_pyramid <- plot_pyramid_proj_comp(
  dataset = total_pop_proj_sex,
  base_year = pop_proj_start_year,
  comp_year = pop_proj_end_year,
  ymax = ymax_pop_proj,
  coords_df = coords_df_pop_proj,
  col = pop,
  title = paramsarea,
  subtitle = paste0("Projected population profile, ", pop_proj_start_year,
                    " and ", pop_proj_end_year))




# All plots using function plot_horizontal_bar ================================


p_deaths_cause_f <- plot_horizontal_bar(
  dataset = deaths_cause_f,
  col = dea,
  title = paramsarea,
  subtitle = paste0("Female leading causes of death, ",
                    bir_dea_marr_cp_est_end_year, "\n\n",
                    abs_comma(deaths_cause_f$Total[1]), " female deaths")) +
  labs(x = NULL, y = NULL)


p_deaths_cause_m <- plot_horizontal_bar(
  dataset = deaths_cause_m,
  col = dea,
  title = paramsarea,
  subtitle = paste0("Male leading causes of death, ",
                    bir_dea_marr_cp_est_end_year, "\n\n",
                    abs_comma(deaths_cause_m$Total[1]), " male deaths")) +
  labs(x = NULL, y = NULL)




# All plots using function plot_horizontal_bar_perc ===========================


p_dwell_tax_grouped <- plot_horizontal_bar_perc(
  dataset = data_dwell_tax_ca,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Percentage of dwellings by Council Tax band, ",
                    dwell_est_end_year))


p_dwell_type <- plot_horizontal_bar_perc(
  dataset = data_dwell_type_ca_plot,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Percentage of dwellings by dwelling type, ",
                    dwell_est_end_year))



# All plots using function plot_bar_age_sex ===================================


p_pop_est_age_sex <- plot_bar_age_sex(
  dataset = total_pop_est_age_sex,
  var1 = total_pop_est_age_sex$Age_group,
  var2 = total_pop_est_age_sex$Sex,
  col1 = pop,
  col2 = grey,
  title = paramsarea,
  subtitle = paste0("Population by age group by sex, ", pop_est_end_year)) +
  xlab("Age group")


p_pop_proj_age_year <- plot_bar_age_sex(
  dataset = total_pop_proj_age_year,
  var1 = total_pop_proj_age_year$Age_group,
  var2 = total_pop_proj_age_year$Year,
  col1 = pop,
  col2 = grey,
  title = paramsarea,
  subtitle = paste0("Population by age group by year, ", pop_proj_start_year,
                    " and ", pop_proj_end_year)) +
  xlab("Age group")


p_births_mothers_age <- plot_bar_age_sex(
  dataset = total_births_mothers_age_plot,
  var1 = total_births_mothers_age_plot$Age_group,
  var2 = total_births_mothers_age_plot$Year,
  col1 = bir,
  col2 = grey,
  title = paramsarea,
  subtitle = paste0("Births by age group of mother, ",
                    bir_dea_marr_est_start_year, " and ",
                    bir_dea_marr_cp_est_end_year)) +
  xlab("Age group of mother")


p_house_proj_type <- plot_bar_age_sex(
  dataset = subset(house_proj_type_plot, Area == paramsarea),
  var1 = subset(house_proj_type_plot, Area == paramsarea)$Type,
  var2 = subset(house_proj_type_plot, Area == paramsarea)$Year,
  col1 = hou,
  col2 = grey,
  title = paramsarea,
  subtitle = paste0("Projected number of households by household type, ",
                    house_proj_start_year, " and ", house_proj_end_year)) +
  xlab("Household type")


# All plots using function plot_bar_age_sex_legend ============================


p_net_mig_age_sex <- plot_bar_age_sex_legend(
  dataset = net_mig_age_sex_ca,
  col = mig,
  title = paramsarea,
  subtitle = paste0("Net migration by age group by sex, ", mig_end_year)) +
  xlab("Age group") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))


p_deaths_age_sex <- plot_bar_age_sex_legend(
  dataset = total_deaths_age_sex,
  col = dea,
  title = paramsarea,
  subtitle = paste0("Deaths by age group by sex, ",
                    bir_dea_marr_cp_est_end_year)) +
  xlab("Age group") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))



# All plots using function plot_bar_sex =======================================


p_deaths_sex_comp <- plot_bar_sex(
  dataset = total_deaths_sex_comp,
  var = total_deaths_sex_comp$Year,
  col1 = dea,
  col2 = grey,
  title = paramsarea,
  subtitle = paste0("Deaths by sex by year, ", bir_dea_marr_est_start_year,
                    " and ", bir_dea_marr_cp_est_end_year))



# All plots using function plot_bar_age_perc ==================================


p_pop_est_age_perc <- plot_bar_age_perc(
  dataset = total_pop_est_age_perc,
  var = total_pop_est_age_perc$Age_group,
  col = pop,
  title = paramsarea,
  subtitle = paste0("Percentage change in population by age group, ",
                    pop_est_start_year, " and ", pop_est_end_year)) +
  labs(x = "Age group")


p_pop_proj_age_perc <- plot_bar_age_perc(
  dataset = total_pop_proj_age_perc,
  var = total_pop_proj_age_perc$Age_group,
  col = pop,
  title = paramsarea,
  subtitle = paste0("Percentage change in projected population by age group, ",
                    pop_proj_start_year, " and ", pop_proj_end_year)) +
  labs(x = "Age group")


p_births_mothers_age_perc <- plot_bar_age_perc(
  dataset = total_births_mothers_age_perc_plot,
  var = total_births_mothers_age_perc_plot$Age_group,
  col = bir,
  title = paramsarea,
  subtitle = paste0("Percentage change in the number of births by age group of mother, ",
                    bir_dea_marr_est_start_year, " and ",
                    bir_dea_marr_cp_est_end_year)) +
  labs(x = "Age group of mother")


p_deaths_sex_perc <- plot_bar_age_perc(
  dataset = total_deaths_sex_perc,
  var = total_deaths_sex_perc$Sex,
  col = dea,
  title = paramsarea,
  subtitle = paste0("Percentage change in the number of deaths by sex, ",
                    bir_dea_marr_est_start_year, " and ",
                    bir_dea_marr_cp_est_end_year)) +
  labs(x = "Sex")


p_house_proj_type_perc <- plot_bar_age_perc(
  dataset = house_proj_type_perc,
  var = house_proj_type_perc$Type,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Percentage change in projected number of households by household type, ",
                    house_proj_start_year, " and ", house_proj_end_year)) +
  labs(x = "Household type")


p_house_proj_age_perc <- plot_bar_age_perc(
  dataset = house_proj_age_perc,
  var = house_proj_age_perc$Age_group,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Percentage change in projected number of households by age group of Household Reference \nPerson (HRP), ",
                    house_proj_start_year, " and ", house_proj_end_year)) +
  labs(x = "Age group") +
  scale_x_discrete(expand = c(0.08, 0.08),
                   labels = function(x) str_wrap(x, width = 6))


p_pop_proj_nature <- plot_bar_age_perc(
  dataset = subset(pop_proj_nature_perc_plot, Area == paramsarea),
  var = subset(pop_proj_nature_perc_plot, Area == paramsarea)$Type,
  col = pop,
  title = paramsarea,
  subtitle = paste0("Components of projected population change (1), ",
                    pop_proj_start_year, "-", pop_proj_end_year)) +
  xlab("Component") +
  scale_x_discrete(labels = c("Natural change", "Net migration",
                              "Total change")) +
  labs(caption = "(1) Projected natural change and net migration are not the only components of change. Other changes that \n are not included in the above chart include changes in armed forces and prisoner populations, and \n changes due to constraining to the National Population Projections for Scotland.")



# All plots using function plot_one_CA_year ===================================


p_house_proj_age <- plot_one_CA_year(
  dataset = house_proj_age_plot,
  col = hou,
  title = paramsarea,
  subtitle = paste0("Projected number of households by age group of Household Reference Person (HRP), \n",
                    house_proj_start_year, " and ", house_proj_end_year),
  house_proj_start_year,
  house_proj_end_year) +
  scale_x_discrete(expand = c(0.08, 0.08),
                   labels = function(x) str_wrap(x, width = 7))



# All plots using function plot_one_CA_age_sex ================================


p_net_mig_agegroup_sex <- plot_one_CA_age_sex(
  dataset = net_mig_age_sex_ca,
  col = mig,
  title = paramsarea,
  subtitle = paste0("Net migration by age group by sex, ", mig_end_year)) +
  xlab("Age group") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
  geom_segment(
    aes(y = 0, yend = 0, x = min(Year), xend = max(Year)),
    linetype = 2)
