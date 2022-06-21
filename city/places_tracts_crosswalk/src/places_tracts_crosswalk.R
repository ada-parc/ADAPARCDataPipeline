library(tidyverse)
library(tidycensus)
library(readxl)
library(tigris)
library(sf)

load(here::here("city", "places_counties_crosswalk", "output", "places_counties_crosswalk.Rda"))

message("Finding intersection of places and tracts...")

tictoc::tic()
gc()

places_tracts <- map(1:52, function(x) {
  message(paste("Running Place-Tract intersection", x, "of 52"))
  tictoc::tic()
  df <- st_intersection(places_split[[x]], tracts_split[[x]])
  tictoc::toc()
  gc()
  return(df)
})
tictoc::toc()

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

save.image(here::here("city", "places_tracts_crosswalk", "output", "places_tracts_crosswalk.Rda"))

write_csv(places_tracts, here::here("city", "places_tracts_crosswalk", "output", "places_tracts.csv"))

write_csv(dict_places, here::here("city", "places_tracts_crosswalk", "output", "dict_places.txt"))

st_write(tracts_sf, here::here("city", "places_tracts_crosswalk", "output", "geo_tract.shp"), delete_dsn = TRUE)

write_csv(dict_location_crosswalk, here::here("city", "places_tracts_crosswalk", "output", "dict_location_crosswalk.txt"))

rm(list = ls())
