produce_CA_plots <- function(CA_env) {
  # we take our CA dataset input and throw it 
  # into this function's local environment
  # this allows us to keep the code exactly as it was 
  # when it ran as a script at global level
  list2env(CA_env, envir = environment())
  # we don't the input object anymore
  # and we don't want to return it lest we start accumulating things
  rm(CA_env)
  
  # ========================
  # This is where the original script begins
  # ========================
  
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
  plot_one_CA_year <- function(dataset, col, title, subtitle){
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
  plot_all_CA_num <- function(dataset, col, title, subtitle){
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
  plot_all_CA_num_mig <- function(dataset, col, title, subtitle){
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
  plot_all_CA_rate <- function(dataset, col, title, subtitle, num_digits){
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
  plot_all_CA_rate_mig <- function(dataset, col, title, subtitle, num_digits){
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
  plot_CA_scot_num_type <- function(dataset, col, title, subtitle, neg_poss){
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
  plot_all_CA_perc <- function(dataset, col, title, subtitle, neg_poss){
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
  plot_all_CA_perc_mig_life <- function(dataset, col, title, subtitle, neg_poss){
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
                      "-", pop_est_end_year)) +
    labs(x = "Year to 30 June")
  
  
  p_pop_proj <- plot_all_CA_num(
    dataset = total_pop_proj_all_CA,
    col = pop,
    title = "Council areas of Scotland",
    subtitle = paste0("Total projected population, ", pop_proj_start_year,
                      "-", pop_proj_end_year))
  
  
  p_marr_num <- plot_all_CA_num(
    dataset = total_marr_all_CA,
    col = mar,
    title = "Council areas of Scotland",
    subtitle = paste0("Number of marriages, ", bir_dea_marr_est_start_year,
                      "-", bir_dea_marr_cp_est_end_year))
  
  
  p_cp_num <- plot_all_CA_num(
    dataset = total_cp_all_CA,
    col = mar,
    title = "Council areas of Scotland",
    subtitle = paste0("Number of civil partnerships, ", cp_start_year,
                      "-", cp_end_yr))
  
  
  p_house_est_num <- plot_all_CA_num(
    dataset = data_house_est_all_CA,
    col = hou,
    title = "Council areas of Scotland",
    subtitle = paste0("Number of households, ", house_est_start_year,
                      "-", house_est_end_year)) +
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
                      "-", dwell_est_end_year)) +
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
                      "-", house_proj_end_year))
  
  
  
  # All plots using function plot_all_CA_num_mig ================================
  
  
  # For net migration, min(Year) doesn't work since using string values
  # eg. 2001-02.
  p_net_mig <- plot_all_CA_num_mig(
    dataset = total_net_mig_all_CA,
    col = mig,
    title = "Council areas of Scotland",
    subtitle = paste0("Total net migration, ", mig_start_year, " to ",
                      mig_end_year, "*")) +
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
    num_digits = 2) +
    labs(caption = "(1) The approximate total fertility rate is defined to be the average number of children (per woman) that would \n be born to a cohort of women if they experienced, throughout their childbearing years, the age-specific fertility \n rates of the year in question. These rates are approximate because they were calculated using fertility rates \n for 5-year age-groups rather than for individual years of age.")
  
  
  p_births_rate <- plot_all_CA_rate(
    dataset = births_rate,
    col = bir,
    title = "Council areas of Scotland",
    subtitle = paste0("Standardised birth rates (1), ",
                      bir_dea_marr_est_start_year, "-",
                      bir_dea_marr_cp_est_end_year),
    num_digits = 1) +
    labs(caption = "(1) Births per 1,000 population - 'standardised' using the age/sex-specific rates for Scotland as a whole.")
  
  
  p_deaths_rate <- plot_all_CA_rate(
    dataset = deaths_rate,
    col = dea,
    title = "Council areas of Scotland",
    subtitle = paste0("Standardised death rates (1), ",
                      bir_dea_marr_est_start_year, "-",
                      bir_dea_marr_cp_est_end_year),
    num_digits = 1) +
    labs(caption = "(1) Deaths per 1,000 population - 'standardised' using the age/sex-specific rates for Scotland as a whole.")
  
  
  # All plots using function plot_all_CA_rate_mig ===============================
  
  
  p_net_mig_rates <- plot_all_CA_rate_mig(
    dataset = net_mig_rates,
    col = mig,
    title = "Council areas of Scotland",
    subtitle = paste0("Net migration rates (1), ", mig_rate_start_year,
                      " to ", mig_end_year),
    num_digits = 1) +
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
    neg_poss = FALSE)
  
  p_life_exp_at_65 <- plot_CA_scot_num_type(
    dataset = life_exp_at_65_comp,
    col = lif,
    title = paramsarea,
    subtitle = paste0("Life expectancy at age 65-69, ", life_exp_start_year,
                      " to ", life_exp_end_year),
    neg_poss = FALSE)
  
  
  # All plots using function plot_all_CA_perc ===================================
  # Scotland included as well.
  
  
  p_pop_est_perc <- plot_all_CA_perc(
    dataset = total_pop_est,
    col = pop,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in population, ", pop_est_start_year,
                      "-", pop_est_end_year),
    neg_poss = TRUE) +
    labs(x = "Year to 30 June")
  
  
  p_pop_proj_perc <- plot_all_CA_perc(
    dataset = total_pop_proj,
    col = pop,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in projected population, ",
                      pop_proj_start_year, "-", pop_proj_end_year),
    neg_poss = TRUE)
  
  
  p_house_est_perc <- plot_all_CA_perc(
    dataset = total_house_est,
    col = hou,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in the number of households, ",
                      house_est_start_year, "-", house_est_end_year),
    neg_poss = TRUE) +
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
    neg_poss = TRUE) +
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
    neg_poss = TRUE)
  
  
  p_births_perc <- plot_all_CA_perc(
    dataset = total_births,
    col = bir,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in the number of births, ",
                      bir_dea_marr_est_start_year, "-",
                      bir_dea_marr_cp_est_end_year),
    neg_poss = TRUE)
  
  
  p_deaths_perc <- plot_all_CA_perc(
    dataset = total_deaths,
    col = dea,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in the number of deaths by sex, ",
                      bir_dea_marr_est_start_year, "-",
                      bir_dea_marr_cp_est_end_year),
    neg_poss = TRUE)
  
  
  
  # All plots using function plot_all_CA_perc_mig_life ==========================
  
  
  p_life_exp_from_birth_f <- plot_all_CA_perc_mig_life(
    dataset = life_exp_from_birth_f,
    col = lif,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in female life expectancy at birth, ",
                      life_exp_start_year, " to ", life_exp_end_year),
    neg_poss = TRUE)
  
  
  p_life_exp_from_birth_m <- plot_all_CA_perc_mig_life(
    dataset = life_exp_from_birth_m,
    col = lif,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in male life expectancy at birth, ",
                      life_exp_start_year, " to ", life_exp_end_year),
    neg_poss = TRUE)
  
  
  p_life_exp_at_65_f <- plot_all_CA_perc_mig_life(
    dataset = life_exp_at_65_f,
    col = lif,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in female life expectancy at age 65-69, ",
                      life_exp_start_year, " to ", life_exp_end_year),
    neg_poss = TRUE)
  
  
  p_life_exp_at_65_m <- plot_all_CA_perc_mig_life(
    dataset = life_exp_at_65_m,
    col = lif,
    title = "Council areas of Scotland",
    subtitle = paste0("Percentage change in male life expectancy at age 65-69, ",
                      life_exp_start_year, " to ", life_exp_end_year),
    neg_poss = TRUE)
  
  
  
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
                      house_proj_start_year, " and ", house_proj_end_year)) +
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
  
  # ========================
  # This is where the original script ends
  # we now repack the function's environment 
  # into a list to return upwards
  # ========================
  
  # here we gather all our environment vars set above
  # into a single list for passing back out of the function
  CA_env = mget(ls(environment()))
  
  # If we wanted to add anything to the list we pass back
  # we should do so here
  
  # CA_env$new_bit = table blah blah blah
  
  return(CA_env)
  
}
