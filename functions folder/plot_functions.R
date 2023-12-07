source("content scripts/colour and formatting.R")

# All functions for plots ======================================================
# Plots have been modularised so that different vars can be plotted on the same 
# type of graph 
# Line chart showing paramsarea numbers only, from start year to end year
plot_one_CA_num <- function(dataset, col, title, subtitle){
  ggplot(data = dataset, mapping = aes(x = Year, y = Number, colour = Area)) +
    geom_line(size = 1.5) +
    geom_point(
      data = subset(dataset, Year %in% c(min(Year), max(Year))),
      aes(x = Year, y = Number),
      color = col,
      size = 3.5) +
    geom_text(
      data = subset(dataset, Year %in% c(min(Year), max(Year))),
      aes(x = Year + ifelse(Year > min(Year), 0.25, -0.25), y = Number,
          label = comma(round(Number, 1))),
      size = 4.5,
      hjust = "outward") +
    scale_y_continuous(
      name = "",
      expand = c(0.2, 0.2),
      labels = scales::comma,
      limits = c(0, max(dataset$Number) * 1.1)) +
    scale_x_continuous(
      breaks = seq(min(dataset$Year), max(dataset$Year), 5),
      expand = c(0.2, 0.2)) +
    scale_color_manual(
      breaks = NULL,
      values = col) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}

# Line chart showing paramsarea numbers only, by sex (including "All people"),
# from start year to end year
plot_one_CA_sex <- function(dataset, col, title, subtitle){
  ggplot(data = dataset, mapping = aes(x = Year, y = Number, group = Sex)) +
    geom_line(
      data = subset(dataset, Sex == "Female"),
      col = "black", size = 1.5) +
    geom_line(
      data = subset(dataset, Sex == "Male"),
      col = "black", size = 0.75) +
    geom_point(
      data = subset(dataset, Year %in% c(min(Year), max(Year))),
      aes(x = Year, y = Number),
      colour = ifelse(subset(dataset, Year %in% c(min(Year), max(Year)))$Sex ==
                        "All people",
                      col,
                      "black"),
      shape = ifelse(subset(dataset, Year %in% c(min(Year), max(Year)))$Sex ==
                       "Male",
                     21,
                     16),
      fill = "white",
      stroke = 1.5,
      size = 3.5) +
    geom_text_repel(
      data = subset(dataset, Year == max(Year)),
      aes(x = Year + 1,
          y = Number,
          label = paste0(comma(Number), "    ", Sex)),
      colour = ifelse(subset(dataset, Year == max(Year))$Sex == "All people",
                      col,
                      "black"),
      size = 4.5,
      direction = "y",
      segment.color = NA,
      xlim = c(max(dataset$Year) + 0.05,
               max(dataset$Year) + 0.25 *
                 (max(dataset$Year) - min(dataset$Year)))) +
    geom_text_repel(
      data = subset(dataset, Year == min(Year)),
      aes(x = Year - 0.5,
          y = Number,
          label = comma(Number)),
      colour = ifelse(subset(dataset, Year == min(Year))$Sex == "All people",
                      col,
                      "black"),
      size = 4.5,
      direction = "y",
      segment.color = NA) +
    geom_line(
      data = subset(dataset, Sex == "All people"),
      mapping = aes(x = Year, y = Number),
      size = 1.5,
      colour = col) +
    scale_y_continuous(
      name = "",
      labels = scales::comma,
      expand = c(0.2, 0.2),
      limits = c(0, max(dataset$Number) * 1.1)) +
    scale_x_continuous(
      expand = c(0.05, 0.05),
      breaks = seq(min(dataset$Year), max(dataset$Year), 5),
      limits = c(min(dataset$Year) - 0.05 *
                   (max(dataset$Year) - min(dataset$Year)),
                 max(dataset$Year) + 0.25 *
                   (max(dataset$Year) - min(dataset$Year)))) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.36),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Line chart showing paramsarea numbers only, by type, from start year to end
# year
plot_one_CA_type <- function(dataset, col, title, subtitle){
  ggplot(data = dataset, mapping = aes(x = Year, y = Number, group = Type)) +
    geom_line(
      data = subset(dataset, Type == "In"),
      col = col,
      size = 1.5) +
    geom_line(
      data = subset(dataset, Type == "Out"),
      col = col,
      size = 0.7) +
    geom_point(
      data = subset(dataset, Year %in% c(min(Year), max(Year))),
      aes(x = Year, y = Number),
      colour = col,
      shape = ifelse(subset(dataset, Year %in% c(min(Year), max(Year)))$Type ==
                       "In",
                     16,
                     21),
      fill = "white",
      stroke = 1.5,
      size = 3.5) +
    geom_text_repel(
      data = subset(dataset, Year == min(Year)),
      aes(x = Year, y = Number,
          label = comma(Number)),
      colour = col,
      size = 4.5,
      direction = "y",
      nudge_x = -1,
      segment.color = NA) +
    geom_text_repel(
      data = subset(dataset, Year == max(Year)),
      aes(label = paste0(comma(Number), "   ", Type)),
      colour = col,
      size = 4.5,
      segment.color = NA,
      direction = "y",
      xlim = c(max(dataset$Year) + 0.1, max(dataset$Year) + 20)) +
    scale_y_continuous(
      name = "",
      labels = scales::comma,
      expand = c(0.2, 0.2),
      limits = c(0, max(dataset$Number) * 1.1)) +
    scale_x_continuous(
      expand = c(0.08, 0.08),
      breaks = seq(min(unique(dataset$Year)), max(unique(dataset$Year)), 2),
      limits = c(min(dataset$Year),
                 max(dataset$Year) +
                   0.1 * (max(dataset$Year) - min(dataset$Year))),
      labels = str_wrap(stri_replace_all_fixed(
        unique(
          dataset$Year_2
        )[seq(1, length(unique(dataset$Year_2)), 2)], "-", " -"),
        width = 2)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.45),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Line chart showing paramsarea numbers only, by year (start year and end year
# only), with age groups on the x axis
plot_one_CA_year <- function(dataset, col, title, subtitle, house_proj_start_year, house_proj_end_year){
  ggplot(data = dataset,
         mapping = aes(x = Age_group, y = Number, group = Year)) +
    geom_line(data = subset(dataset, Year == house_proj_end_year),
              col = col,
              size = 1.5) +
    geom_line(
      data = subset(dataset, Year == house_proj_start_year),
      col = col,
      size = 0.7) +
    geom_point(
      data = subset(dataset,
                    Age_group == subset(dataset, Year == min(Year))$Age_group[1]
      ),
      aes(x = Age_group, y = Number),
      col = col,
      shape = ifelse(
        subset(
          dataset,
          Age_group == subset(dataset, Year == min(Year))$Age_group[1])$Year == min(dataset$Year),
        21,
        16),
      fill = "white",
      stroke = 1.5,
      size = 3.5) +
    geom_point(
      data = subset(
        dataset,
        Age_group == subset(dataset, Year == min(Year))$Age_group[length(subset(dataset, Year == min(Year))$Age_group)]),
      aes(x = Age_group, y = Number),
      col = col,
      shape = ifelse(
        subset(
          dataset,
          Age_group == subset(dataset, Year == min(Year))$Age_group[length(subset(dataset, Year == min(Year))$Age_group)])$Year == min(dataset$Year),
        21,
        16),
      fill = "white",
      stroke = 1.5,
      size = 3.5) +
    geom_text_repel(
      data = subset(dataset, Age_group == subset(dataset, Year == min(Year))$Age_group[length(subset(dataset, Year == min(Year))$Age_group)]),
      aes(x = Age_group, y = Number,
          label = Year),
      col = col,
      size = 4.5,
      segment.color = NA,
      xlim = c(length(subset(dataset, Year == min(Year))$Age_group) + 0.1, length(subset(dataset, Year == min(Year))$Age_group) + 7)) +
    scale_y_continuous(
      name = "",
      labels = scales::comma,
      limits = c(0, max(dataset$Number) * 1.1)) +
    xlab("Age group") +
    scale_x_discrete(
      expand = c(0.02, 0.02),
      labels = function(x) str_wrap(x, width = 8)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.45),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}

# Line chart showing paramsarea numbers only, by sex, with age groups on the x-axis 
plot_one_CA_age_sex <- function(dataset, col, title, subtitle){
  ggplot(data = dataset,
         mapping = aes(x = Age_group, y = Number, group = Sex)) +
    geom_line(data = subset(dataset, Sex == "Female"),
              col = col,
              size = 1.5) +
    geom_line(data = subset(dataset, Sex == "Male"),
              col = col,
              size = 0.75) +
    geom_point(
      data = subset(dataset,
                    Age_group == subset(dataset, Sex == "Male")$Age_group[1]),
      aes(x = Age_group, y = Number),
      col = col,
      shape = ifelse(
        subset(
          dataset,
          Age_group == subset(dataset, Sex == "Male")$Age_group[1])$Sex == "Male",
        21,
        16),
      fill = "white",
      stroke = 1.5,
      size = 3.5) +
    geom_point(
      data = subset(dataset, Age_group == subset(dataset, Sex == "Male")$Age_group[length(subset(dataset, Sex == "Male")$Age_group)]),
      aes(x = Age_group, y = Number),
      col = col,
      shape = ifelse(subset(dataset, Age_group == subset(dataset, Sex == "Male")$Age_group[length(subset(dataset, Sex == "Male")$Age_group)])$Sex == "Male", 21, 16),
      fill = "white",
      stroke = 1.5,
      size = 3.5) +
    geom_text_repel(
      data = subset(dataset, Age_group == subset(dataset, Sex == "Male")$Age_group[length(subset(dataset, Sex == "Male")$Age_group)]),
      aes(x = Age_group, y = Number,
          label = Sex),
      col = col,
      size = 4.5,
      segment.color = NA,
      xlim = c(length(subset(dataset, Sex == "Male")$Age_group), length(subset(dataset, Sex == "Male")$Age_group) + 5)) +
    geom_segment(
      aes(y = 0, yend = 0, x = dataset$Age_group, xend = dataset$Age_group),
      linetype = 2) +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    xlab("Age group") +
    scale_x_discrete(
      expand = c(0.05, 0.05),
      labels = function(x) str_wrap(x, width = 8)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.45),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Line chart showing numbers for each council area (but not Scotland) from start
# year to end year 
plot_all_CA_num <- function(dataset, col, title, subtitle, paramsarea){
  ggplot(data = dataset, mapping = aes(x = Year, y = Number, group = Area)) +
    geom_line(data = subset(dataset, !(Area %in% c(paramsarea, "Scotland")) & Area != subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area & Area != subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area),
              size = 0.7,
              col = 8) +
    geom_line(
      data = subset(dataset, Area %in% c(paramsarea, "Scotland")),
      mapping = aes(x = Year, y = Number),
      size = 1.5,
      colour = ifelse(
        subset(
          dataset,
          Area %in% c(paramsarea, "Scotland"))$Area == paramsarea,
        col,
        black)) +
    geom_line(data = subset(dataset, Area != paramsarea & (Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] | Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1])),
              size = 0.7,
              col = "black") +
    geom_text_repel(
      data = subset(
        dataset,
        (Area %in% c(paramsarea, "Scotland") |
           Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year)),
      aes(label = Area),
      colour = ifelse(
        subset(
          dataset,
          (Area %in% c(paramsarea, "Scotland") |
             Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
             Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year))$Area == paramsarea,
        col,
        "black"),
      size = 4.5,
      segment.color = NA,
      direction = "y",
      xlim = c(max(dataset$Year), max(dataset$Year) +
                 0.35 * (max(dataset$Year) - min(dataset$Year)))) +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(min(dataset$Year), max(dataset$Year), by = 5),
      limits = c(min(dataset$Year), max(dataset$Year) +
                   0.31 * (max(dataset$Year) - min(dataset$Year)))) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.39),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Line chart showing numbers for each council area (but not Scotland) from start
# year to end year, used for migration
plot_all_CA_num_mig <- function(dataset, col, title, subtitle, paramsarea){
  ggplot(data = dataset, mapping = aes(x = Year, y = Number, group = Area)) +
    geom_line(
      data = subset(
        dataset,
        !(Area %in% c(paramsarea, "Scotland")) &
          Area != subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area &
          Area != subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area),
      size = 0.7,
      col = 8) +
    geom_line(
      data = subset(
        dataset,
        Area != paramsarea &
          (Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
             Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1])),
      size = 0.7,
      col = "black") +
    geom_line(
      data = subset(dataset, Area %in% c(paramsarea, "Scotland")),
      mapping = aes(x = Year, y = Number),
      size = 1.5,
      colour = ifelse(
        subset(dataset,
               Area %in% c(paramsarea, "Scotland"))$Area == paramsarea,
        col,
        black)) +
    geom_text_repel(
      data = subset(
        dataset,
        (Area %in% c(paramsarea, "Scotland") |
           Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year)),
      aes(label = Area),
      colour = ifelse(
        subset(
          dataset,
          (Area %in% c(paramsarea, "Scotland") |
             Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
             Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year))$Area == paramsarea,
        col,
        "black"),
      size = 4.5,
      segment.color = NA,
      direction = "y",
      xlim = c(max(dataset$Year), max(dataset$Year) +
                 0.35 * (max(dataset$Year) - min(dataset$Year)))) +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(min(unique(dataset$Year)), max(unique(dataset$Year)), 2),
      limits = c(min(dataset$Year), max(dataset$Year) +
                   0.25 * (max(dataset$Year) - min(dataset$Year))),
      labels = str_wrap(stri_replace_all_fixed(unique(dataset$Year_2)[seq(1, length(unique(dataset$Year_2)), 2)], "-", " -"),
                        width = 2)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.4),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Line chart showing rates for each council area and Scotland from start year to
# end year 
plot_all_CA_rate <- function(dataset, col, title, subtitle, num_digits, paramsarea){
  ggplot(data = dataset, mapping = aes(x = Year, y = Number, group = Area)) +
    geom_line(
      data = subset(
        dataset,
        !(Area %in% c(paramsarea, "Scotland")) &
          Area != subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area &
          Area != subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area),
      size = 0.7,
      col = 8) +
    geom_line(
      data = subset(
        dataset,
        (Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1])),
      size = 0.7,
      col = "black") +
    geom_line(
      data = subset(dataset, Area %in% c(paramsarea, "Scotland")),
      mapping = aes(x = Year, y = Number),
      size = 1.5,
      colour = ifelse(
        subset(
          dataset,
          Area %in% c(paramsarea, "Scotland"))$Area == paramsarea,
        col,
        black)) +
    geom_text_repel(
      data = subset(
        dataset,
        (Area %in% c(paramsarea, "Scotland") |
           Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year)),
      aes(label = paste0(
        formatC(
          round(Number, num_digits), format = "f", digits = num_digits),
        "  ",
        Area)),
      colour = ifelse(
        subset(
          dataset,
          (Area %in% c(paramsarea, "Scotland") |
             Area == subset(dataset, Number == min(Number[Year == max(Year)]) & Year == max(Year))$Area[1] |
             Area == subset(dataset, Number == max(Number[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year))$Area == paramsarea,
        col,
        "black"),
      size = 4.5,
      segment.color = NA,
      direction = "y",
      xlim = c(max(dataset$Year), max(dataset$Year) +
                 0.43 * (max(dataset$Year) - min(dataset$Year)))) +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(min(dataset$Year), max(dataset$Year), 5),
      limits = c(min(dataset$Year), max(dataset$Year) +
                   0.35 * (max(dataset$Year) - min(dataset$Year)))) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.4),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(hjust = 0)) +
    ggtitle(title, subtitle)
}


# Line chart showing rates for each council area and Scotland from start year to
# end year, used for migration
plot_all_CA_rate_mig <- function(dataset, col, title, subtitle, num_digits, paramsarea){
  ggplot(data = dataset, mapping = aes(x = Year, y = Number, group = Area)) +
    geom_line(
      data = subset(dataset, !(Area %in% c(paramsarea, "Scotland"))),
      size = 0.7,
      col = 8) +
    geom_line(
      data = subset(dataset, Area %in% c(paramsarea, "Scotland")),
      mapping = aes(x = Year, y = Number),
      size = 1.5,
      colour = ifelse(
        subset(
          dataset,
          Area %in% c(paramsarea, "Scotland"))$Area == paramsarea,
        col,
        black)) +
    geom_text_repel(
      data = subset(dataset,
                    Area %in% c(paramsarea, "Scotland") & Year == max(Year)),
      aes(label = paste0(
        formatC(
          round(Number, num_digits), format = "f", digits = num_digits),
        "  ",
        Area)),
      colour = ifelse(
        subset(
          dataset,
          Area %in% c(paramsarea, "Scotland") &
            Year == max(Year))$Area == paramsarea, col, "black"),
      size = 4.5,
      segment.color = NA,
      direction = "y",
      xlim = c(max(dataset$Year), max(dataset$Year) +
                 0.43 * (max(dataset$Year) - min(dataset$Year)))) +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(min(dataset$Year), max(dataset$Year), 5),
      limits = c(min(dataset$Year), max(dataset$Year) +
                   0.35 * (max(dataset$Year) - min(dataset$Year)))) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.4),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(hjust = 0)) +
    ggtitle(title, subtitle)
}


# Line chart showing estimates for paramsarea and Scotland only, from start year
# to end year, used for life expectancy
plot_CA_scot_num_type <- function(dataset, col, title, subtitle, neg_poss, paramsarea){
  ggplot(data = dataset, mapping = aes(x = Year, y = Estimate, group = Area)) +
    geom_line(
      data = subset(dataset, Sex == "Female"),
      mapping = aes(x = Year, y = Estimate, group = Area),
      size = ifelse(subset(dataset, Sex == "Male")$Area == paramsarea, 1.5, 1),
      colour = ifelse(subset(dataset, Sex == "Female")$Area == paramsarea,
                      col,
                      black)) +
    geom_line(
      data = subset(dataset, Sex == "Male"),
      mapping = aes(x = Year, y = Estimate, group = Area),
      size = ifelse(subset(dataset, Sex == "Male")$Area == paramsarea, 1.5, 1),
      colour = ifelse(subset(dataset, Sex == "Male")$Area == paramsarea,
                      col,
                      black)) +
    geom_ribbon(
      data = subset(dataset, Sex == "Female" & Area == paramsarea),
      aes(ymin = Lower_CI, ymax = Upper_CI),
      fill = col,
      alpha = 0.15) +
    geom_ribbon(
      data = subset(dataset, Sex == "Female" & Area == "Scotland"),
      aes(ymin = Lower_CI, ymax = Upper_CI),
      fill = "black",
      alpha = 0.15) +
    geom_ribbon(
      data = subset(dataset, Sex == "Male" & Area == paramsarea),
      aes(ymin = Lower_CI, ymax = Upper_CI),
      fill = col,
      alpha = 0.15) +
    geom_ribbon(
      data = subset(dataset, Sex == "Male" & Area == "Scotland"),
      aes(ymin = Lower_CI, ymax = Upper_CI),
      fill = "black",
      alpha = 0.15) +
    geom_point(
      data = subset(dataset, Year %in% c(min(Year), max(Year))),
      aes(x = Year, y = Estimate),
      colour = ifelse(
        subset(
          dataset,
          Year %in% c(min(Year), max(Year)))$Area == paramsarea,
        col,
        black),
      size = 3.5) +
    geom_text_repel(
      data = subset(dataset, Year == max(Year)),
      aes(x = Year, y = Estimate,
          label = paste0(
            formatC(
              round(Estimate, 1),
              format = "f",
              digits = 1),
            " ", Sex, ", ", Area)),
      colour = ifelse(subset(dataset, Year == max(Year))$Area == paramsarea,
                      col,
                      black),
      size = 4.5,
      point.padding = 0.3,
      direction = "y",
      segment.color = NA,
      xlim = c(max(dataset$Year) + 0.1,
               max(dataset$Year) +
                 0.85 * (max(dataset$Year) - min(dataset$Year)))) +
    geom_text_repel(
      data = subset(dataset, Year == min(Year)),
      aes(x = Year, y = Estimate,
          label = paste0(formatC(round(Estimate, 1), format = "f", digits = 1))),
      colour = ifelse(subset(dataset, Year == min(Year))$Area == paramsarea,
                      col,
                      black),
      size = 4.5,
      point.padding = 0.4,
      direction = "y",
      segment.color = NA) +
    scale_y_continuous(
      expand = c(0.22, 0.22),
      name = "",
      labels = scales::comma) +
    scale_x_continuous(
      expand = c(0.05, 0.05),
      breaks = seq(min(unique(dataset$Year)), max(unique(dataset$Year)), 2),
      limits = c(min(dataset$Year), max(dataset$Year) +
                   0.5 * (max(dataset$Year) - min(dataset$Year))),
      labels = str_wrap(
        stri_replace_all_fixed(
          unique(dataset$Year_2)[seq(1, length(unique(dataset$Year_2)), 2)],
          "-",
          " -"),
        width = 2)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.33),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Line chart showing percentage change from start year for all council areas and
# Scotland, from start year to end year
plot_all_CA_perc <- function(dataset, col, title, subtitle, neg_poss, paramsarea){
  ggplot(data = dataset, mapping = aes(x = Year, y = Perc, group = Area)) +
    geom_line(
      data = subset(
        dataset,
        !(Area %in% c(paramsarea, "Scotland")) &
          Area != subset(dataset, Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area &
          Area != subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area),
      size = 0.7,
      col = 8) +
    geom_line(
      data = subset(
        dataset,
        (Area == subset(dataset, Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area[1])),
      size = 0.7,
      col = "black") +
    geom_segment(
      aes(y = 0, yend = 0, x = min(Year), xend = max(Year)),
      linetype = ifelse(neg_poss, 2, 0)) +
    geom_line(
      data = subset(dataset, Area %in% c(paramsarea, "Scotland")),
      mapping = aes(x = Year, y = Perc),
      colour = ifelse(
        subset(dataset,
               Area %in% c(paramsarea, "Scotland"))$Area == paramsarea,
        col,
        black),
      size = 1.5) +
    geom_text_repel(
      data = subset(
        dataset,
        (Area %in% c(paramsarea, "Scotland") |
           Area == subset(dataset, Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year)),
      aes(label = paste0(formatC(round(Perc, 1), format = "f", digits = 1),
                         "% ", Area)),
      colour = ifelse(
        subset(dataset,
               (Area %in% c(paramsarea, "Scotland") |
                  Area == subset(dataset, Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area[1] |
                  Area == subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year))$Area == paramsarea,
        col,
        "black"),
      size = 4.5,
      segment.color = NA,
      direction = "y",
      xlim = c(max(dataset$Year), max(dataset$Year) +
                 0.45 * (max(dataset$Year) - min(dataset$Year)))) +
    scale_y_continuous(
      name = "",
      labels = function(x){
        paste0(x, "%")
      }) +
    scale_x_continuous(
      breaks = seq(min(dataset$Year), max(dataset$Year), 5),
      limits = c(min(dataset$Year), max(dataset$Year) +
                   0.37 * (max(dataset$Year) - min(dataset$Year)))) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.37),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Line chart showing percentage change from start year for all council areas and
# Scotland, from start year to end year, used for life expectancy
plot_all_CA_perc_mig_life <- function(dataset, col, title, subtitle, neg_poss, paramsarea){
  ggplot(data = dataset, mapping = aes(x = Year, y = Perc, group = Area)) +
    geom_line(
      data = subset(
        dataset,
        !(Area %in% c(paramsarea, "Scotland")) &
          Area != subset(dataset, Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area &
          Area != subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area),
      size = 0.7,
      col = 8) +
    geom_line(
      data = subset(
        dataset,
        (Area == subset(
          dataset,
          Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area[1])),
      size = 0.7,
      col = "black") +
    geom_segment(
      aes(y = 0, yend = 0, x = min(Year), xend = max(Year)),
      linetype = ifelse(neg_poss, 2, 0)) +
    geom_line(
      data = subset(dataset, Area %in% c(paramsarea, "Scotland")),
      mapping = aes(x = Year, y = Perc),
      colour = ifelse(
        subset(dataset,
               Area %in% c(paramsarea, "Scotland"))$Area == paramsarea,
        col,
        black),
      size = 1.5) +
    geom_text_repel(
      data = subset(
        dataset,
        (Area %in% c(paramsarea, "Scotland") |
           Area == subset(dataset, Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area[1] |
           Area == subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year)),
      aes(label = paste0(formatC(round(Perc, 1), format = "f", digits = 1), "% ", Area)),
      colour = ifelse(
        subset(
          dataset,
          (Area %in% c(paramsarea, "Scotland") |
             Area == subset(dataset, Perc == min(Perc[Year == max(Year)]) & Year == max(Year))$Area[1] |
             Area == subset(dataset, Perc == max(Perc[Year == max(Year)]) & Year == max(Year))$Area[1]) & Year == max(Year))$Area == paramsarea, col, "black"),
      size = 4.5,
      segment.color = NA,
      direction = "y",
      xlim = c(max(dataset$Year), max(dataset$Year) +
                 0.47 * (max(dataset$Year) - min(dataset$Year)))) +
    scale_y_continuous(
      name = "",
      labels = function(x){
        paste0(x, "%")
      }) +
    scale_x_continuous(
      breaks = seq(min(unique(dataset$Year)), max(unique(dataset$Year)), 2),
      limits = c(min(dataset$Year),
                 max(dataset$Year) +
                   0.39 * (max(dataset$Year) - min(dataset$Year))),
      labels = str_wrap(
        stri_replace_all_fixed(
          unique(dataset$Year_2)[seq(1, length(unique(dataset$Year_2)), 2)],
          "-", " -"),
        width = 2)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(hjust = 0.37),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    ggtitle(title, subtitle)
}


# Population pyramid showing numbers for paramsarea only, for start year as line
# chart and end year as bar chart
# Used for population estimates.
plot_pyramid_est_comp <- function(dataset, base_year, comp_year, ymax,
                                  coords_df, col, title, subtitle){
  ggplot(data = dataset,
         mapping = aes(x = Age, y = Number, fill = factor(Sex))) +
    geom_bar(data = dataset %>% filter(Sex == "Female", Year == base_year),
             aes(x = Age, y = Number),
             fill = grey,
             col = grey,
             stat = "identity") +
    geom_bar(data = dataset %>% filter(Sex == "Male", Year == base_year),
             aes(x = Age, y = -Number),
             fill = grey,
             col = grey,
             stat = "identity") +
    geom_step(data = dataset %>% filter(Sex == "Female", Year == comp_year),
              aes(x = Age, y = Number),
              col = col,
              size = 1) +
    geom_step(data =  dataset %>% filter(Sex == "Male", Year == comp_year),
              aes(x = Age, y = -Number),
              col = col,
              size = 1) +
    geom_polygon(data = coords_df,
                 aes(x = y_coord, y = x_coord), fill = "white") +
    coord_flip() +
    annotate("text", x = seq(0, 80, 10), y = 0, label = seq(0, 80, 10),
             size = 4) +
    annotate("text", x = 90, y = 0, label = "90+", size = 4) +
    annotate("text", x = 100, y = 0, label = "Age", size = 4.5) +
    annotate("text", x = 10, y = -ymax / 1.1, label = comp_year, size = 5,
             col = pop, fontface = 2) +
    annotate("text", x = 10, y = -ymax / 5, label = base_year, size = 5,
             col = black, fontface = 2) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    scale_y_continuous(labels = abs_comma,
                       breaks = axisTicks(c(-ymax, ymax), log = FALSE,
                                          nint = 6),
                       limits = c(-ymax - ymax / 4, ymax + ymax / 4)) +
    geom_segment(aes(x = 0, xend = 0, y = ymax, yend = ymax + ymax / 4),
                 size = 0.7) +
    geom_segment(aes(x = 16, xend = 16, y = ymax, yend = ymax + ymax / 4),
                 size = 0.7) +
    geom_segment(aes(x = 64, xend = 64, y = ymax, yend = ymax + ymax / 4),
                 size = 0.7) +
    geom_segment(aes(x = 90, xend = 90, y = ymax, yend = ymax + ymax / 4),
                 size = 0.7) +
    annotate("text", x = -2.5, y = -ymax / 1.25, label = "Male population",
             size = 4.5, hjust = 0) +
    annotate("text", x = -2.5, y = ymax / 1.25, label = "Female population",
             size = 4.5, hjust = 1) +
    geom_text(data = dataset %>%
                filter(Year == base_year) %>%
                mutate(perc1 = sum(Number[Age <= 15]) / sum(Number) * 100) %>%
                mutate(perc1 = formatC(round(perc1, 1), format = "f", digits = 1)),
              aes(x = Age, y = Number,
                  label = paste0(perc1, "% were\n 15 and under\n in ", base_year)),
              x = 8, y = ymax + ymax / 4, size = 3.7, hjust = 1) +
    geom_text(data = dataset %>%
                filter(Year == base_year) %>%
                mutate(perc2 = sum(Number[Age >= 16 & Age <= 64]) / sum(Number) * 100) %>%
                mutate(perc2 = formatC(round(perc2, 1), format = "f", digits = 1)),
              aes(x = Age, y = Number,
                  label = paste0(perc2, "% were\n between\n 16 and 64\n in ", base_year)),
              x = 53, y = ymax + ymax / 4, size = 3.7, hjust = 1) +
    geom_text(data = dataset %>%
                filter(Year == base_year) %>%
                mutate(perc3 = sum(Number[Age >= 65]) / sum(Number) * 100) %>%
                mutate(perc3 = formatC(round(perc3, 1), format = "f", digits = 1)),
              aes(x = Age, y = Number,
                  label = paste0(perc3, "% were\n 65 and over\n in ", base_year)),
              x = 82, y = ymax + ymax / 4, size = 3.7, hjust = 1) +
    ggtitle(title, subtitle)
}


# Population pyramid showing numbers for paramsarea only, for start year as bar
# chart and end year as line chart. Used for population estimates.
plot_pyramid_proj_comp <- function(dataset, base_year, comp_year, ymax,
                                   coords_df, col, title, subtitle){
  ggplot(data = dataset,
         mapping = aes(x = Age, y = Number, fill = factor(Sex))) +
    geom_bar(data = dataset %>% filter(Sex == "Female", Year == base_year),
             aes(x = Age, y = Number),
             fill = grey,
             col = grey,
             stat = "identity") +
    geom_bar(data = dataset %>% filter(Sex == "Male", Year == base_year),
             aes(x = Age, y = -Number),
             fill = grey,
             col = grey,
             stat = "identity") +
    geom_step(data = dataset %>% filter(Sex == "Female", Year == comp_year),
              aes(x = Age, y = Number),
              col = col,
              size = 1) +
    geom_step(data =  dataset %>% filter(Sex == "Male", Year == comp_year),
              aes(x = Age, y = -Number),
              col = col,
              size = 1) +
    geom_polygon(data = coords_df, aes(x = y_coord, y = x_coord),
                 fill = "white") +
    coord_flip() +
    annotate("text", x = seq(0, 80, 10), y = 0, label = seq(0, 80, 10),
             size = 4) +
    annotate("text", x = 90, y = 0, label = "90+", size = 4) +
    annotate("text", x = 100, y = 0, label = "Age", size = 4.5) +
    annotate("text", x = 10, y = -ymax / 1.2, label = comp_year, size = 5,
             col = pop, fontface = 2) +
    annotate("text", x = 10, y = -ymax / 5, label = base_year, size = 5,
             col = black, fontface = 2) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    scale_y_continuous(labels = abs_comma,
                       breaks = axisTicks(c(-ymax, ymax), log = FALSE,
                                          nint = 6),
                       limits = c(-ymax - ymax / 4, ymax + ymax / 4)) +
    annotate("text", x = -2.5, y = -ymax / 1.25, label = "Male population",
             size = 4.5, hjust = 0) +
    annotate("text", x = -2.5, y = ymax / 1.25, label = "Female population",
             size = 4.5, hjust = 1) +
    ggtitle(title, subtitle)
}


# Horizontal bar chart showing numbers of each type for paramsarea only, for end
# year only. Proportions of each type are also given as labels. 
plot_horizontal_bar <- function(dataset, col, title, subtitle){
  ggplot(data = subset(dataset),
         mapping = aes(x = Type, y = Number, fill = Area, width = 0.4)) +
    geom_bar(
      stat = "identity",
      position = "dodge") +
    coord_flip() +
    geom_text(aes(label = paste0(
      formatC(round(Perc, 1), format = "f", digits = 1), "%")),
      hjust = -0.1, colour = col, size = 4.5) +
    scale_y_continuous(
      name = "",
      labels = scales::comma,
      expand = c(0, 0),
      limits = c(0, max(dataset$Number) * 1.3)) +
    scale_x_discrete(limits = rev(levels(dataset$Type)),
                     labels = function(x) str_wrap(x, width = 60)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    scale_fill_manual(values = col) +
    ggtitle(title, subtitle)
}


# Horizontal bar chart showing proportions of each type, for paramsarea only,
# for end year only.
plot_horizontal_bar_perc <- function(dataset, col, title, subtitle){
  ggplot(data = subset(dataset),
         mapping = aes(x = Type, y = Perc, fill = Area, width = 0.4)) +
    geom_bar(
      stat = "identity",
      position = "dodge") +
    coord_flip() +
    geom_text(aes(label = paste0(
      formatC(round(Perc, 1), format = "f", digits = 1), "%")),
      hjust = -0.1, colour = col, size = 4.5) +
    scale_y_continuous(
      name = "",
      labels = function(x){
        paste0(x, "%")
      },
      expand = c(0, 0),
      limits = c(0, max(dataset$Perc) * 1.1)) +
    scale_x_discrete(limits = rev(levels(dataset$Type))) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    scale_fill_manual(values = col) +
    ggtitle(title, subtitle)
}


# Bar chart showing numbers by sex, with age group on x axis, for paramsarea
# only, for end year only. 
plot_bar_age_sex <- function(dataset, var1, var2, col1, col2, title, subtitle){
  ggplot(data = dataset,
         mapping = aes(x = var1,
                       y = Number,
                       fill = factor(var2),
                       width = 0.4)) +
    geom_bar(
      stat = "identity",
      position = "dodge") +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    geom_text(
      aes(x = var1[1],
          y = rep(0 + max(dataset$Number) / 100, 1),
          label = factor(var2)),
      position = position_dodge(0.4),
      angle = 90,
      hjust = "bottom") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    scale_fill_manual(values = c(col1, col2)) +
    ggtitle(title, subtitle) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
}


# Bar chart showing numbers by sex, with age group on x axis, for paramsarea
# only, for end year only. With a legend as well, since bars are too thin to
# label.
plot_bar_age_sex_legend <- function(dataset, col, title, subtitle){
  ggplot(data = dataset,
         mapping = aes(x = Age_group, y = Number, fill = Sex, width = 0.4)) +
    geom_bar(
      stat = "identity",
      position = "dodge") +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "top",
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    scale_fill_manual(values = c(col, grey)) +
    ggtitle(title, subtitle) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
}


# Bar chart showing numbers with sex on x axis, for paramsarea only, for start
# year and end year only. 
plot_bar_sex <- function(dataset, var, col1, col2, title, subtitle){
  ggplot(data = dataset,
         mapping = aes(x = Sex, y = Number, fill = factor(var), width = 0.4)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_y_continuous(
      name = "",
      labels = scales::comma) +
    geom_text(
      aes(x = Sex[1], y = rep(0 + max(dataset$Number) / 100, 1), label = var),
      position = position_dodge(0.4),
      angle = 90,
      hjust = "bottom") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14)) +
    scale_fill_manual(values = c(col1, col2)) +
    ggtitle(title, subtitle)
}


# Bar chart showing percentage change between start and end year, with age
# groups on x axis, for paramsarea only
plot_bar_age_perc <- function(dataset, var, col, title, subtitle){
  ggplot(data = dataset,
         mapping = aes(x = var, y = Perc, alpha = Sign, width = 0.5)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      fill = col,
      col = col,
      alpha = ifelse(dataset$Sign == "positive", 1, 0.4)) +
    scale_y_continuous(
      name = "",
      labels = function(x){
        paste0(x, "%")
      },
      expand = c(0.1, 0)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme_classic() +
    geom_text(aes(label = ifelse(
      dataset$Perc > 0,
      paste0("+", formatC(round(Perc, 1), format = "f", digits = 1), "%"),
      paste0(formatC(round(Perc, 1), format = "f", digits = 1), "%"))),
      vjust = ifelse(dataset$Perc > 0, -0.5, 1.2),
      size = 4.5,
      col = col,
      alpha = 1) +
    theme(axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(hjust = 0)) +
    ggtitle(title, subtitle)
}