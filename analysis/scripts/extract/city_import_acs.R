# Get year config
years <- config::get("acs")$years
survey <- config::get("acs")$survey

fips_for_state <- getFIPSCodesForStates()

# Places data download
#TODO: This function is too specific... It's likely that the direction extraction will go in is to only pull variables that we specifically want, in which case this might fit in with that flow.
places_pop_est <- downloadACSPlacePopulationEstimate(year)

tables <- c("S1810", "S1811")
places_data_acs_raw <- getCountryWideAcsTablesForMultipleYears(tables, years, survey, "place")


# Tracts data download
tables <- c("S1810")
tracts_data_acs_raw <- getCountryWideAcsTablesForMultipleYears(tables, years, survey, "tract", fips_for_state)



saveRawExtractedDataFile(places_pop_est, "places_pop_est")
saveRawExtractedDataFile(places_data_acs_raw, "places_data_acs_raw")
saveRawExtractedDataFile(tracts_data_acs_raw, "tracts_data_acs_raw")
