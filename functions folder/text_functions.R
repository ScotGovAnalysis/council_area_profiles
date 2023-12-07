# Utility functions -------------------------------------------------------
rank_text <- function(rank, end_year) {
  case_when(
    rank == min(end_year) ~ "highest",
    rank == max(end_year) ~ "lowest",
    rank == max(end_year) - 1 ~ "2nd lowest",
    TRUE ~ paste0(scales::ordinal(rank), " highest")
  )
}