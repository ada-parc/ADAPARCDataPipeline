require(tidyverse)
require(tidycensus)

load(here::here("city", "places_counties_crosswalk", "output", "places_counties_crosswalk.Rda"))
load(here::here("city", "places_tracts_crosswalk", "output", "places_tracts_crosswalk.Rda"))
places_tracts <- read_csv(here::here("city", "places_tracts_crosswalk", "output", "places_tracts.csv"))

year <- config::get("acs")$years[1]

places_acs_s1810 <- get_acs(geography = "place",
                      table = "S1810",
                      year = year,
                      # show_call = T,
                      geometry = FALSE,
                      output = "wide")  %>%
  rename_with(.cols = matches("[0-9]{3}(E|M)$"),
              ~ifelse(str_detect(.x, "E$"), str_replace(.x, "E$", "_estimate"), str_replace(.x, "M$", "_moe")))

places_acs_s1811 <- get_acs(geography = "place",
                            table = "S1811",
                            year = year,
                            # show_call = T,
                            geometry = FALSE,
                            output = "wide")  %>%
  rename_with(.cols = matches("[0-9]{3}(E|M)$"),
              ~ifelse(str_detect(.x, "E$"), str_replace(.x, "E$", "_estimate"), str_replace(.x, "M$", "_moe")))

places_acs <- left_join(places_acs_s1810, places_acs_s1811)

city_place_full <- places_acs %>%
  filter(GEOID %in% places_sf$place_GEOID)

tracts_data <- pmap_df(places_counties %>%
                         select(STATEFP, COUNTYFP) %>%
                         distinct(),
                       ~get_acs(geography = "tract",
                               state = ..1,
                               county = ..2,
                               table = "S1810",
                               year = year,
                               # show_call = T,
                               geometry = FALSE,
                               output = "wide")  %>%
                         rename_with(.cols = matches("[0-9]{3}(E|M)$"),
                                     ~ifelse(str_detect(.x, "E$"), str_replace(.x, "E$", "_estimate"), str_replace(.x, "M$", "_moe"))))

save.image(here::here("city", "import_acs", "output", "city_import_acs.Rda"))

rm(list = ls())
