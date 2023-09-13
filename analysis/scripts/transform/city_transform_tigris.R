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
# Place split by state

message("Finding intersection of places and counties...")


places <- readRawExtractedDataFile("places_sf")
places_pop_est <- readRawExtractedDataFile("places_pop_est")

places_split <- splitPlacesSFByState(places, places_pop_est)


# Counties split by state
counties_sf <- counties_sf %>%
  mutate(STATEFP = str_extract(county_GEOID, "^[0-9]{2}") %>%
           as.factor())
counties_split <- split(counties_sf, counties_sf$STATEFP)

# Tracts split by state
# Tracts sf dataframe
tracts_split <- split(tracts_sf, tracts_sf$STATEFP)



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


places_tracts <- map(1:52, function(x) {
  message(paste("Running Place-Tract intersection", x, "of 52"))
  df <- st_intersection(places_split[[x]], tracts_split[[x]])
  gc()
  return(df)
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


# Clean Places_SF for lookup
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

# Export (places_tracts_crosswalk) ------------------------------------------------------------------

save.image(here::here("city", "places_tracts_crosswalk", "output", "places_tracts_crosswalk.Rda"))

write_csv(places_tracts, here::here("city", "places_tracts_crosswalk", "output", "places_tracts.csv"))

write_csv(dict_places, here::here("city", "places_tracts_crosswalk", "output", "dict_places.txt"))

st_write(tracts_sf, here::here("city", "places_tracts_crosswalk", "output", "geo_tract.shp"), delete_dsn = TRUE)

write_csv(dict_location_crosswalk, here::here("city", "places_tracts_crosswalk", "output", "dict_location_crosswalk.txt"))


# export (places_counties_crosswalk) ----
rm(counties_sf, counties_split)
save.image(here::here("city", "places_counties_crosswalk", "output", "places_counties_crosswalk.Rda"))

