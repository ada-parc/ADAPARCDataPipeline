require(tidyverse)
require(tidycensus)
require(readxl)
require(tigris)
require(sf)

config_values <- config::get()
year <- config_values$acs$years[1]
survey <- config_values$acs$survey
rm(config_values)


# FIPS codes --------------------------------------------------------------


# All states and counties
fips_codes_tidy <- tigris::fips_codes


# Places/Cities lookup ----------------------------------------------------

places_sf <- downloadSpatialFootprintOfCitiesPlaces(year)


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
start_time <- Sys.time()
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
end_time <- Sys.time()
time <- end_time - start_time
print(time)

# export ----
save.image(here::here("analysis", "data", "city_import_sf.Rda"))
rm(list = ls())
