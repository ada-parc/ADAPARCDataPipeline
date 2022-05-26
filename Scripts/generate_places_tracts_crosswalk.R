

# Setup -------------------------------------------------------------------


# Libraries
library(tidyverse)
library(tidycensus)
library(readxl)
library(tigris)
library(sf)



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


## ----- Get spatial footprint of counties -----
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


## ----- Unique city/state/county codes -----
# Filter counties by joining to cities
# Set minimum overlap threshold, default units [m^2]
message("Finding intersection of places and counties...")
places_sf <- places_sf %>%
  mutate(STATEFP = as.factor(STATEFP))
places_split <- split(places_sf, places_sf$STATEFP)

counties_sf <- counties_sf %>%
  mutate(STATEFP = str_extract(county_GEOID, "^[0-9]{2}") %>%
           as.factor())
counties_split <- split(counties_sf, counties_sf$STATEFP)

places_counties <- map(1:52, function(x) {
  message(paste("Running Place-County intersection", x))
  st_intersection(places_split[[x]], counties_split[[x]])
})

places_counties <- places_counties %>%
  reduce(bind_rows) %>% 
  mutate("overlap_area" = st_area(.),
         "overlap_area_num" = as.numeric(overlap_area),
         "overlap_pct" = overlap_area_num / place_area_num) %>%
  filter(overlap_pct > 0) %>%
  st_drop_geometry()


## ----- Tracts -----
message("Finding intersection of places and tracts...")

# Tracts sf dataframe
tracts_sf <- pmap_df(.l = places_counties %>% 
                       select(STATEFP, COUNTYFP) %>% 
                       distinct(),
                     .f = ~(tigris::tracts(state = ..1,  
                                           county = ..2,
                                           cb = TRUE, 
                                           year = year,
                                           class = "sf"))) %>% 
  select("tract_GEOID" = GEOID, 
         "tract_NAME" = NAME, 
         STATEFP, COUNTYFP) %>% 
  mutate("tract_area" = st_area(.),
         "tract_area_num" = as.numeric(tract_area)) %>% 
  relocate(geometry, .after = last_col())

tracts_split <- split(tracts_sf, tracts_sf$STATEFP)

places_tracts <- map(1:52, function(x) {
  message(paste("Running Place-Tract intersection", x))
  st_intersection(places_split[[x]], tracts_split[[x]])
})

# Filter counties by joining to cities
# Set minimum overlap threshold, default units [m^2]
places_tracts <- places_tracts %>% 
  reduce(bind_rows) %>%
  mutate("overlap_area" = st_area(.),
         "overlap_area_num" = as.numeric(overlap_area),
         "overlap_pct" = overlap_area_num / place_area_num) %>%
  filter(overlap_pct > 0) %>%
  st_drop_geometry()


## ----- Join place/county/tract ----- 


# Crosswalk, left join above
dict_location_crosswalk <- places_sf %>%
  st_drop_geometry() %>%
  select(place_GEOID) %>%
  # Clean place name
  left_join(dict_places %>% 
              rename("place_GEOID" = GEOID),
            by = "place_GEOID") %>% 
  # Tracts
  left_join(places_tracts %>% 
              select(place_GEOID, STATEFP, COUNTYFP, 
                     tract_GEOID, tract_NAME,
                     "tract_place_overlap_pct" = overlap_pct),
            by = "place_GEOID") %>% 
  # Counties joined to correct tracts
  left_join(places_counties %>% 
              select(place_GEOID, STATEFP, COUNTYFP,
                     county_GEOID, county_NAME,
                     "county_place_overlap_pct" = overlap_pct),
            by = c("place_GEOID", "STATEFP", "COUNTYFP")) %>% 
  select(place_GEOID, metro_state, 
         starts_with("county_"), starts_with("tract_"))

# Spatial join check (don't want counties that only border included)
# mapview::mapshot(
# mapview::mapview(places,
#                  col.regions = "orange") +
#   mapview::mapview(counties %>%
#                      filter(county_GEOID %in%
#                               places_counties_sf %>% 
#                               st_drop_geometry() %>% 
#                               pull(county_GEOID)),
#                    color = "white",
#                    col.regions = "blue",
#                    alpha.regions = 0.4) # ,
# url = paste0(getwd(), "/places_counties.html"))


# Export ------------------------------------------------------------------


# Places
write_csv(dict_places, here::here("Data", "dict_places.txt"))

# Tracts
st_write(tracts_sf, here::here("Data", "geo_tract", "geo_tract.shp"), delete_dsn = TRUE)

# Places, counties, tracts crosswalk
write_csv(dict_location_crosswalk, here::here("Data", "dict_location_crosswalk.txt"))
