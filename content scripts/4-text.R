# Population Estimates ----------------------------------------------------
current_pop_est <- total_pop_est %>%
  filter(Area == paramsarea,
         Year == pop_est_end_year) %>%
  pull(Number)

previous_pop_est <- total_pop_est %>%
  filter(Area == paramsarea,
         Year == pop_est_end_year - 1) %>% 
  pull(Number)

current_pop_est_scot <- total_pop_est %>%
  filter(Area == "Scotland",
         Year == pop_est_end_year) %>%
  pull(Number)

previous_pop_est_scot <- total_pop_est %>%
  filter(Area == "Scotland",
         Year == pop_est_end_year - 1) %>%
  pull(Number)

pop_est_perc <- (current_pop_est - previous_pop_est) / previous_pop_est * 100

pop_est_perc_scot <-
  (current_pop_est_scot - previous_pop_est_scot) / previous_pop_est_scot * 100

## Population Estimates 01 ------------------------------------------------
pop_est_text_1_1 <-
  case_when(
    previous_pop_est > current_pop_est ~ paste0(
      'a decrease of ',
      scales::percent(abs(pop_est_perc), accuracy = 0.1, scale = 1),
      ' from ',
      format(previous_pop_est, big.mark = ','),
      ' in'
    ),
    previous_pop_est < current_pop_est ~ paste0(
      'an increase of ',
      scales::percent(abs(pop_est_perc), accuracy = 0.1, scale = 1),
      ' from ',
      format(previous_pop_est, big.mark = ','),
      ' in'
    ),
    TRUE ~ 'unchanged from'
  )

pop_est_text_1_2 <- case_when(
  previous_pop_est_scot > current_pop_est_scot ~
    paste0('decreased by ', scales::percent(
      abs(pop_est_perc_scot),
      accuracy = 0.1,
      scale = 1
    )),
  previous_pop_est_scot < current_pop_est_scot ~
    paste0('increased by ', scales::percent(
      abs(pop_est_perc_scot),
      accuracy = 0.1,
      scale = 1
    )),
  TRUE ~ paste0(
    'did not change between ',
    pop_est_end_year - 1,
    ' and ',
    pop_est_end_year
  )
)

pop_est_text_1 <-
  glue(
    "On 30 June {pop_est_end_year}, the population of
      {paramsarea} was {format(current_pop_est, big.mark=',')}.
      This is {pop_est_text_1_1} {pop_est_end_year - 1}.
      Over the same period, the population of Scotland {pop_est_text_1_2}."
  )

## Population Estimates 02 ------------------------------------------------
pop_est_perc <- total_pop_est_end_year %>%
  filter(Area == paramsarea) %>%
  pull(Perc)

pop_est_rank_ca <- total_pop_est_end_year %>%
  filter(Area == paramsarea) %>%
  pull(Rank)

pop_est_rank_diff_ca <- total_pop_est_end_year %>%
  filter(Area == paramsarea) %>%
  pull(Rank_change)

pop_est_rank_text <- rank_text(rank = pop_est_rank_ca,
                               end_year = total_pop_est_end_year$Rank)

pop_est_rank_difference_text <- rank_text(rank = pop_est_rank_diff_ca,
                                          end_year = total_pop_est_end_year$Rank_change)

num_pop_est_increase <- total_pop_est_end_year %>%
  filter(Change_type == "increase") %>%
  nrow()

num_pop_est_decrease <- total_pop_est_end_year %>%
  filter(Change_type == "decrease") %>%
  nrow()

num_pop_est_equal <- total_pop_est_end_year %>%
  filter(Change_type == "equal") %>%
  nrow()

pop_est_scot_perc <- total_pop_est %>%
  filter(Year == pop_est_end_year,
         Area == "Scotland") %>%
  pull(Perc)

pop_est_text_2_1 <- ifelse(
  total_pop_est_end_year %>%
    filter(Rank == pop_est_rank_ca) %>%
    nrow() > 1,
  paste0('joint ', pop_est_rank_text),
  paste0(pop_est_rank_text)
)

pop_est_text_2_2 <-
  case_when(
    num_pop_est_decrease == 32 ~ "all councils saw a population decrease",
    num_pop_est_increase == 32 ~ "all councils saw a population increase",
    num_pop_est_decrease == 1 &
      num_pop_est_increase == 1 ~ paste0(
        num_pop_est_decrease,
        ' council saw a population decrease and ',
        num_pop_est_increase,
        ' council saw a population increase'
      ),
    num_pop_est_increase == 1 &
      num_pop_est_decrease != 1 ~ paste0(
        num_pop_est_decrease,
        ' councils saw a population decrease and ',
        num_pop_est_increase,
        ' council saw a population increase'
      ),
    num_pop_est_increase != 1 &
      num_pop_est_decrease == 1 ~ paste0(
        num_pop_est_decrease,
        ' council saw a population decrease and ',
        num_pop_est_increase,
        ' councils saw a population increase'
      ),
    TRUE ~ paste0(
      num_pop_est_decrease,
      ' councils saw a population decrease and ',
      num_pop_est_increase,
      ' councils saw a population increase'
    )
  )

pop_est_text_2_3 <-
  case_when(num_pop_est_equal == 1 ~ paste0('There was ', num_pop_est_equal, ' council that remained the same.'),
            num_pop_est_equal > 1 ~ paste0(
              'There were ',
              num_pop_est_equal,
              ' councils that remained the same.'
            ),
            TRUE ~ "")

pop_est_text_2 <- glue(
  "{paramsarea} had the ",
  "{pop_est_text_2_1} population in ",
  "{pop_est_end_year}, out of all 32 council areas in Scotland. Between ",
  "{pop_est_end_year - 1} and ",
  "{pop_est_end_year}, ",
  "{pop_est_text_2_2}. ",
  "{pop_est_text_2_3}"
)

## Population Estimates 03 ------------------------------------------------
pop_est_text_3_1 <-
  case_when(
    pop_est_perc < 0 ~ paste0('decreased by ', formatC(
      round(abs(pop_est_perc), 1), format = 'f', digits = 1
    ), '%'),
    pop_est_perc > 0 ~ paste0('increased by ', formatC(
      round(abs(pop_est_perc), 1), format = 'f', digits = 1
    ), '%'),
    TRUE ~ "remained unchanged"
  )

pop_est_text_3_2 <-
  ifelse(
    length(total_pop_est_end_year$Rank_change[total_pop_est_end_year$Rank_change ==
                                                pop_est_rank_diff_ca]) > 1,
    paste0('joint ', pop_est_rank_difference_text),
    paste0(pop_est_rank_difference_text)
  )

pop_est_text_3_3 <-
  case_when(
    pop_est_scot_perc > 0 ~ paste0('rose by ', formatC(
      round(pop_est_scot_perc, 1),
      format = 'f',
      digits = 1
    ), '%'),
    pop_est_scot_perc < 0 ~ paste0('fell by ', formatC(
      round(pop_est_scot_perc, 1),
      format = 'f',
      digits = 1
    ), '%'),
    TRUE ~ "did not change"
  )

pop_est_text_3 <- glue(
  "Between ",
  "{pop_est_start_year} and ",
  "{pop_est_end_year}, the population of ",
  "{paramsarea} has ",
  "{pop_est_text_3_1}. ",
  "This is the ",
  "{pop_est_text_3_2}",
  " percentage change out of the 32 council areas in Scotland. ",
  "Over the same period, Scotland's population ",
  "{pop_est_text_3_3}",
  "."
)

## Population Estimates 04 ------------------------------------------------
pop_est_perc_m <-
  sum(total_pop_est_gender$Number[total_pop_est_gender$Sex == "Male"]) /
  sum(total_pop_est_gender$Number) * 100

pop_est_perc_f <-
  sum(total_pop_est_gender$Number[total_pop_est_gender$Sex == "Female"]) /
  sum(total_pop_est_gender$Number) * 100

pop_est_perc_m_scotland <-
  sum(total_pop_est_gender_scotland$Number[total_pop_est_gender_scotland$Sex ==
                                             "Male"]) /
  sum(total_pop_est_gender_scotland$Number) * 100

pop_est_perc_f_scotland <-
  sum(total_pop_est_gender_scotland$Number[total_pop_est_gender_scotland$Sex ==
                                             "Female"]) /
  sum(total_pop_est_gender_scotland$Number) * 100

pop_est_gender_rank <- case_when(
  pop_est_perc_f > pop_est_perc_m ~ "more females",
  pop_est_perc_f < pop_est_perc_m ~ "fewer females",
  TRUE ~ "equal"
)

pop_est_gender_rank_scotland <- case_when(
  pop_est_perc_f_scotland > pop_est_perc_m_scotland ~ "more females",
  pop_est_perc_f_scotland < pop_est_perc_m_scotland ~ "fewer females",
  TRUE ~ "equal"
)

pop_est_text_4_1 <-
  case_when(
    pop_est_perc_f > pop_est_perc_m ~ paste0(
      'more females (',
      formatC(
        round(pop_est_perc_f, 1),
        format = 'f',
        digits = 1
      ),
      '%) than males (',
      formatC(
        round(pop_est_perc_m, 1),
        format = 'f',
        digits = 1
      ),
      '%)'
    ),
    pop_est_perc_f < pop_est_perc_m ~ paste0(
      'fewer females (',
      formatC(
        round(pop_est_perc_f, 1),
        format = 'f',
        digits = 1
      ),
      '%) than males (',
      formatC(
        round(pop_est_perc_m, 1),
        format = 'f',
        digits = 1
      ),
      '%)'
    ),
    TRUE ~ "an equal number of females and males"
  )

pop_est_text_4_2 <-
  case_when(
    pop_est_perc_f_scotland > pop_est_perc_m_scotland ~ paste0(
      'more females (',
      formatC(
        round(pop_est_perc_f_scotland, 1),
        format = 'f',
        digits = 1
      ),
      '%) than males (',
      formatC(
        round(pop_est_perc_m_scotland, 1),
        format = 'f',
        digits = 1
      ),
      '%)'
    ),
    pop_est_perc_f_scotland < pop_est_perc_m_scotland ~ paste0(
      'fewer females (',
      formatC(
        round(pop_est_perc_f_scotland, 1),
        format = 'f',
        digits = 1
      ),
      '%) than males (',
      formatC(
        round(pop_est_perc_m_scotland, 1),
        format = 'f',
        digits = 1
      ),
      '%)'
    ),
    TRUE ~ "an equal number of females and males"
  )

pop_est_text_4 <- glue(
  "In ",
  "{pop_est_end_year}, there were ",
  "{pop_est_text_4_1} ",
  "living in ",
  "{paramsarea}. There were ",
  "{paste0(ifelse(pop_est_gender_rank == pop_est_gender_rank_scotland, 'also', 'contrastingly'))} ",
  "{pop_est_text_4_2} ",
  "living in Scotland overall."
)

## Population Estimates 05 ------------------------------------------------
# pop_est_age_sex_comp_current_min <-
#   total_pop_est_age_sex_comp$Age_group[total_pop_est_age_sex_comp$Total == min(total_pop_est_age_sex_comp$Total)]
# 
# pop_est_age_sex_comp_current_max <-
#   total_pop_est_age_sex_comp$Age_group[total_pop_est_age_sex_comp$Total == max(total_pop_est_age_sex_comp$Total)]

pop_est_age_sex_comp_current_min <- total_pop_est_age_sex_comp %>%
  top_n(n = -1, Total) %>%
  pull(Age_group)

pop_est_age_sex_comp_current_max <- total_pop_est_age_sex_comp %>%
  top_n(n = 1, Total) %>%
  pull(Age_group)

pop_est_text_5 <- glue(
  "In terms of overall size, the ",
  "{pop_est_age_sex_comp_current_max} ", 
  "{ifelse(length(pop_est_age_sex_comp_current_max) > 1, 'age groups were', 'age group was')} ",
  "the largest in ",
  "{pop_est_end_year}, with a population of ",
  "{format(max(total_pop_est_age_sex_comp[['Total']]), big.mark=',')}",
  ". In contrast, the ",
  "{pop_est_age_sex_comp_current_min} ",
  "{ifelse(length(pop_est_age_sex_comp_current_min) > 1, 'age groups were', 'age group was')} ",
  "the smallest, with a population of ",
  "{format(min(total_pop_est_age_sex_comp[['Total']]), big.mark=',')}",
  ". In {pop_est_end_year}, more females than males lived in ",
  "{paramsarea} in {sum(total_pop_est_age_sex_comp$Comparison)} ",
  "out of {nrow(total_pop_est_age_sex_comp)} age groups."
)

## Population Estimates 06 ------------------------------------------------
pop_est_age_group_largest_positive_change <- total_pop_est_age_perc %>%
  top_n(n = 1, Perc) %>%
  pull(Age_group)

pop_est_age_group_largest_negative_change <- total_pop_est_age_perc %>%
  top_n(n = -1, Perc) %>%
  pull(Age_group)

pop_est_text_6_1 <- case_when(
  sum(total_pop_est_age_perc[["Sign"]] == 'negative') <= 0.05 ~ paste0(
    'each age group increased in size. The ',
    pop_est_age_group_largest_positive_change,
    ' age group saw the largest percentage increase (+',
    formatC(round(max(
      total_pop_est_age_perc[["Perc"]]
    ), 1),
    format = 'f',
    digits = 1),
    '%).'
  ),
  sum(total_pop_est_age_perc[["Sign"]] == 'positive') <= 0.05 ~ paste0(
    'each age group decreased in size. The ',
    pop_est_age_group_largest_negative_change,
    ' age group saw the largest percentage decrease (',
    formatC(round(min(
      total_pop_est_age_perc[["Perc"]]
    ), 1),
    format = 'f',
    digits = 1),
    '%).'
  ),
  TRUE ~ paste0(
    'the ',
    pop_est_age_group_largest_negative_change,
    ' age group saw the largest percentage decrease (',
    formatC(round(min(
      total_pop_est_age_perc[["Perc"]]
    ), 1),
    format = 'f',
    digits = 1),
    '%). The ',
    pop_est_age_group_largest_positive_change,
    ' age group saw the largest percentage increase (+',
    formatC(round(max(
      total_pop_est_age_perc[["Perc"]]
    ), 1),
    format = 'f',
    digits = 1),
    '%).'
  )
)

pop_est_text_6 <- glue(
  "Between {pop_est_start_year} and {pop_est_end_year}, ", 
  "{pop_est_text_6_1}"
)

