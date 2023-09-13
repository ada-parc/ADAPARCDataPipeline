# Script to combine city data from different sources or final transformations

## NOTE: Until the website code is updated, do not change the final file names--ultimately, we should change them to follow our expected pattern, but it will be easier to transition the website code if we know things work with the old names first.

city_place_data_final <- readTransformedExtractedDataFile("city_place_data_processed")
saveFinalDataFile(city_place_data_processed, "city_place_full")


# TODO: Fill this in
# dict_location_crosswalk
#
# tracts_data
