message("Finding intersection of places and tracts")

# Bring raw extracted data files into the environment
places_sf_raw <- readRawExtractedDataFile("places_sf")
tracts_sf_raw <- readRawExtractedDataFile("tracts_sf")

# Note: While this is somewhat breaking our pattern of transformations that are specific to the data source (pulling in place ACS data), it will filter out a significant number of places (as of 2022, we filter places to have populations of 3500+, we drop 23549 places). This offers notable performance improvements.
places_pop_est <- readRawExtractedDataFile("places_pop_est")
state_county_fips <- getFIPSCodesForStateAndCounty()

# Prepare splits of the dataframes with SF
places_split <- splitPlacesSFByState(places_sf_raw, places_pop_est)
# counties_split <- split(counties_sf_raw, counties_sf_raw$STATEFP)
tracts_split <- split(tracts_sf_raw, tracts_sf_raw$STATEFP)


# Combine Places with both Counties and Tracts
start_time <- Sys.time()

places_tracts <- intersectGeographiesSplitByState(places_split, tracts_split)

end_time <- Sys.time()

total_time <- end_time - start_time

message(paste0("Intersecting Places and Tracts Timing: ", total_time))

####################

# THIS PLACE_TRACT_COUNTY_MAP is a replacement concept for the dict_location_crosswalk; the only notable difference between what was before and what we now have is that it doesn't have the population field. Population was not used by the website, and is available both in the place_data object and the tract_data object we will provide to the website

place_tract_county_map <- transformPlacesTractsCountiesWithFIPSAndTidyNames(places_tracts, state_county_fips)



# Export ------------------------------------------------------------------


saveTransformedDataFile(place_tract_county_map, "place_tract_county_map")



