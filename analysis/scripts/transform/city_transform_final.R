# Script to combine city data from different sources or final transformations that don't make sense in a source-specific transformation script

## NOTE: Until the website code is updated, do not change the final file names--ultimately, we should change them to follow our expected pattern, but it will be easier to transition the website code if we know things work with the old names first.

city_place_data_final <- readTransformedExtractedDataFile("city_place_data_processed")
saveFinalDataFile(city_place_data_processed, "city_place_full")


city_tracts_data_final <- readTransformedExtractedDataFile("city_tracts_data_processed")
saveFinalDataFile(city_place_data_processed, "tract_data")


# TODO: Fill this in
# dict_location_crosswalk
