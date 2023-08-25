# Any final transformations or filters that need to take place to be ready for the website go here.

city_place_full <- places_acs %>%
  filter(GEOID %in% places_sf$place_GEOID)
