require(tidyverse)
require(tidycensus)

options(scipen = 9999)

config_values <- yaml::read_yaml("config.yaml")
year <- config_values[[1]]$year
survey <- config_values[[2]]$survey
rm(config_values)


# TODO: Work this into two calls--state and national, so they are combined into a single dataset

# utility function ----

# downloadAndFormatAcs <- function(tables, geography = "state", year, survey = "acs5") {
#   df <- map(
#     tables,
#     ~ tidycensus::get_acs(
#       year = year,
#       geography = geography,
#       table = .x,
#       survey = survey,
#       geometry = F
#       # cache_table = T,
#       # show_call = T
#     ) %>%
#       pivot_wider(
#         names_from = variable,
#         values_from = c(estimate, moe),
#         names_glue = "{variable}_{.value}"
#       )
#   ) %>%
#     reduce(left_join) %>%
#     left_join(
#       tibble(state.abb, state.name) %>%
#         add_row(
#           state.abb = c("PR", "DC"),
#           state.name = c("Puerto Rico", "District of Columbia")
#         ) %>%
#         select(NAME = state.name, ABBR = state.abb)
#     ) %>%
#     select(GEOID, NAME, ABBR, everything())
#
#   if(geography == "us") {
#     df <- df %>%
#       mutate(ABBR = "USA",
#              GEOID = "000")
#   }
#
#   return(df)
# }

# download ----
tables <- c("S1810")
state_demographics <- downloadAndFormatAcs(tables, "state", year, survey)
national_demographics <- downloadAndFormatAcs(tables, "us", year, survey)
stacked_demographics <- bind_rows(national_demographics, state_demographics)
rm(state_demographics, national_demographics)

if(year > 2017) {
  tables <- c("S1810", "S2601A", "S2602", "B26108")
  state_living <- downloadAndFormatAcs(tables, "state", year, survey)
  national_living <- downloadAndFormatAcs(tables, "us", year, survey)
  stacked_living <- bind_rows(national_living, state_living)
  rm(state_living, national_living)
} else {
  message("ACS group quarters tables were not available until 2017 and cannot be included in current request.")
}

tables <- c("S1810", "S1811", "B18135")
state_participation <- downloadAndFormatAcs(tables, "state", year, survey)
national_participation <- downloadAndFormatAcs(tables, "us", year, survey)
stacked_participation <- bind_rows(state_participation, national_participation)
rm(state_participation, national_participation)

tables <- c("S1810", "S1811",
            "B18135", "B18140", "B25091", "B25070",
            "C18120", "C18121", "C18130")
state_economic <- downloadAndFormatAcs(tables, "state", year, survey)
national_economic <- downloadAndFormatAcs(tables, "us", year, survey)
stacked_economic <- bind_rows(state_economic, national_economic)
rm(state_economic, national_economic, survey, tables, downloadAndFormatAcs)

# export ----
save.image(here::here("analysis", "data", "national_raw.Rda"))
# save.image(here::here("national", "import", "output", "national_import.Rda"))
rm(list = ls())
