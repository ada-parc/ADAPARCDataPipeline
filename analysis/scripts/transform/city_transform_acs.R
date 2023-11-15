
# Import Raw data files

years <- config::get("acs")$years

tracts_data_acs_raw <- readRawExtractedDataFile("tracts_data_acs_raw")
places_data_acs_raw <- readRawExtractedDataFile("places_data_acs_raw")
places_sf <- readRawExtractedDataFile("places_sf")
ada_parc_base_variable_map <- getADAPARCBaseToSourceVariableMap(years)


# Create "city_place_full" for the website:

place_geoids_in_scope <- unique(places_sf$place_GEOID)
city_place_data_filtered <- filterPlaceAcsDataToScope(places_data_acs_raw, place_geoids_in_scope)

city_place_data_base <-  transformRawVariablesToADAPARCBaseVariables(ada_parc_base_variable_map, city_place_data_filtered, "acs")

city_place_data_processed <- addCalculatedVariablesForBaseDemographics(city_place_data_base)

# Checking for Albany (place GEOID: 36001000202)


saveTransformedDataFile(city_place_data_processed, "city_place_data_processed")

# Create "tracts_data" for the website:
# No filtering needs to happen; the data used to be requested according to the state and county names present in the "places_counties" df, but the same data is retrieved without doing any filtering.

city_tracts_data_base <- transformRawVariablesToADAPARCBaseVariables(ada_parc_base_variable_map, tracts_data_acs_raw, "acs")

city_tracts_data_processed <- addCalculatedVariablesForBaseDemographics(city_tracts_data_base)

saveTransformedDataFile(city_tracts_data_processed, "city_tracts_data_processed")
