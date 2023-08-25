# Steps (replaces places_counties_crosswalk):
## 1. Split Places
## 2. Split counties
## 3. Place-County Intersection
## 4. Split tracts
## 5. Place-Tract Intersection


######################################################################

# Filter counties by joining to cities
# Set minimum overlap threshold, default units [m^2]
#
#
# TODO: Where is places_pop_est???
#
# Place split by state
message("Finding intersection of places and counties...")
places_sf <- places_sf %>%
  mutate(STATEFP = as.factor(STATEFP)) %>%
  left_join(places_pop_est, by = c("place_GEOID" = "GEOID")) %>%
  filter(POP > 3500)
places_split <- split(places_sf, places_sf$STATEFP)

# Counties split by state
counties_sf <- counties_sf %>%
  mutate(STATEFP = str_extract(county_GEOID, "^[0-9]{2}") %>%
           as.factor())
counties_split <- split(counties_sf, counties_sf$STATEFP)

# Tracts split by state
# Tracts sf dataframe
tracts_split <- split(tracts_sf, tracts_sf$STATEFP)



