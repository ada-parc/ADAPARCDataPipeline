downloadAndFormatAcs <- function(tables, geography = "state", year) {
  df <- map(
    tables, 
    ~ tidycensus::get_acs(
      year = year,
      geography = geography,
      table = .x,
      geometry = F,
      # cache_table = T,
      show_call = T
    ) %>%
      pivot_wider(
        names_from = variable,
        values_from = c(estimate, moe),
        names_glue = "{variable}_{.value}"
      )
  ) %>%
    reduce(left_join) %>%
    left_join(
      tibble(state.abb, state.name) %>% 
        add_row(
          state.abb = c("PR", "DC"), 
          state.name = c("Puerto Rico", "District of Columbia")
        ) %>%
        select(NAME = state.name, ABBR = state.abb)
    ) %>%
    select(GEOID, NAME, ABBR, everything())
  
  if(geography == "us") {
    df <- df %>%
      mutate(ABBR = "USA")
  }
  
  return(df)
}


