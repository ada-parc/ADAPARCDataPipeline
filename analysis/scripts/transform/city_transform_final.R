# Script to combine city data from different sources or final transformations that don't make sense in a source-specific transformation script

## NOTE: Until the website code is updated, do not change the final file names--ultimately, we should change them to follow our expected pattern, but it will be easier to transition the website code if we know things work with the old names first.

place_tract_county_map <- readTransformedExtractedDataFile("place_tract_county_map")
saveFinalDataFile(place_tract_county_map, "dict_location_crosswalk")

city_place_data_processed <- readTransformedExtractedDataFile("city_place_data_processed")

# Filter the city_place_data_processed so that it's relevant to cities we will show on the website
city_place_data_final <-
  city_place_data_processed %>%
  filter(GEOID %in% place_tract_county_map$place_GEOID)


saveFinalDataFile(city_place_data_processed, "city_place_full")


city_tracts_data_final <- readTransformedExtractedDataFile("city_tracts_data_processed")
saveFinalDataFile(city_tracts_data_final, "tracts_data")


saveFinalDataFile(readRawExtractedDataFile("tracts_sf"), "tracts_sf")
