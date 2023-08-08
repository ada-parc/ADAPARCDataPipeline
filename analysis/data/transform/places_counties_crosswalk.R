require(tidyverse)
require(tidycensus)
require(readxl)
require(tigris)
require(sf)

load(here::here("city", "import_sf", "output", "city_import_sf.Rda"))

## ----- Unique city/state/county codes -----
# Filter counties by joining to cities
# Set minimum overlap threshold, default units [m^2]
message("Finding intersection of places and counties...")

places_sf <- places_sf %>%
  mutate(STATEFP = as.factor(STATEFP)) %>%
  left_join(places_pop_est, by = c("place_GEOID" = "GEOID")) %>%
  filter(POP > 3500)
places_split <- split(places_sf, places_sf$STATEFP)

counties_sf <- counties_sf %>%
  mutate(STATEFP = str_extract(county_GEOID, "^[0-9]{2}") %>%
           as.factor())
counties_split <- split(counties_sf, counties_sf$STATEFP)

gc()

# Takes about 6 hours - Ethan
places_counties <-
  map(1:52, function(x) {
  message(paste("Running Place-County intersection", x, "of 52"))
  df <- st_intersection(places_split[[x]], counties_split[[x]])
  gc()
  return(df)
})

places_counties <- places_counties %>%
  reduce(bind_rows) %>%
  mutate("overlap_area" = st_area(.),
         "overlap_area_num" = as.numeric(overlap_area),
         "overlap_pct" = overlap_area_num / place_area_num) %>%
  filter(overlap_pct > 0) %>%
  st_drop_geometry()




# export ----
rm(counties_sf, counties_split)
save.image(here::here("city", "places_counties_crosswalk", "output", "places_counties_crosswalk.Rda"))
rm(list = ls())
