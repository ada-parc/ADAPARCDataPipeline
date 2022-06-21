library(tidyverse)
library(tidycensus)
library(readxl)
library(tigris)
library(sf)
config_values <- yaml::read_yaml(here::here("national", "import", "hand", "config.yaml"))
year <- config_values[[1]]$year
survey <- config_values[[2]]$survey
rm(config_values)


# FIPS codes --------------------------------------------------------------


# All states and counties
fips_codes_tidy <- force(fips_codes)


# Places/Cities lookup ----------------------------------------------------


# Get spatial footprint of cities/places
# Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))
places_sf <- pmap_df(.l = fips_codes_tidy %>% 
                       filter(as.numeric(state_code) %in% c(1:56, 72)) %>% 
                       select(state_code) %>% 
                       distinct(),
                     .f = ~(tigris::places(state = ..1, 
                                           cb = TRUE, 
                                           year = year,
                                           class = "sf") %>% 
                              rename("place_GEOID" = GEOID, 
                                     "place_NAME" = NAME))) %>%
  select(STATEFP, PLACEFP, place_GEOID, place_NAME) %>% 
  mutate("place_area" = st_area(.),
         "place_area_num" = as.numeric(place_area)) %>% 
  relocate(geometry, .after = last_col())

# Get population estimates for lookup
places_pop_est <- get_acs(geography = "place",
                          variables = c(POP = "B01001_001E"),
                          year = year,
                          show_call = T,
                          geometry = FALSE,
                          output = "wide") %>%
  select(GEOID, NAME, POP)

# Clean for lookup
dict_places <- places_sf %>% 
  st_drop_geometry() %>%
  select(STATEFP, "GEOID" = place_GEOID) %>% 
  # Population to order list
  left_join(places_pop_est,
            by = "GEOID") %>% 
  # Metro/State names
  left_join(fips_codes_tidy %>% 
              select(state_code, state) %>% 
              distinct(),
            by = c("STATEFP" = "state_code")) %>% 
  # Basic scrubbing
  mutate("metro_state" = paste0(str_remove(NAME, ",.*$"),
                                ", ", state) %>% 
           str_replace_all(.,
                           pattern = " (city|village|municipality|town|city and borough|borough|(city|((unified|consolidated|metro|metropolitan) government)) \\(balance\\)|\\(balance\\)), ",
                           replacement = ", ")) %>% 
  # Individual cases
  mutate("metro_state" = case_when(GEOID == "3651000" ~
                                     "New York City, NY",
                                   GEOID == "4752006" ~
                                     "Nashville, TN",
                                   GEOID == "1571550" ~
                                     "Honolulu, HI",
                                   GEOID == "2146027" ~
                                     "Lexington, KY",
                                   GEOID == "2148006" ~
                                     "Louisville, KY",
                                   TRUE ~
                                     metro_state)) %>% 
  select(GEOID, NAME, metro_state, POP)



# Places, counties, tracts crosswalk --------------------------------------


## ----- Get spatial footprint of counties, tracts -----
# Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))
counties_sf <- pmap_df(.l = places_sf %>%
                         st_drop_geometry() %>% 
                         select(STATEFP) %>% 
                         distinct(),
                       .f = ~(tigris::counties(state = ..1, 
                                               cb = TRUE, 
                                               year = year,
                                               class = "sf") %>% 
                                rename("county_GEOID" = GEOID, 
                                       "county_NAME" = NAME))) %>% 
  select(COUNTYFP, county_GEOID, county_NAME)  

# export ----
save.image(here::here("city", "import_sf", "output", "city_import_sf.Rda"))
rm(list = ls())
