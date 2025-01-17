# Get year config
years <- config::get("acs")$years
survey <- config::get("acs")$survey

fips_for_state <- getFIPSCodesForStates()

# Places data download
#TODO: This function is too specific... It's likely that the direction extraction will go in is to only pull variables that we specifically want, in which case this might fit in with that flow.
# We use this later in order to filter out places with small populations; it is acceptable to pull this data for only the most recent year to filter places, since that's likely what we're most interested in. Was uncertain about this decision or not, so if someone else feels strongly, please change.
places_pop_est <- downloadACSPlacePopulationEstimate(max(years))

tables <- c("S1810", "S1811", "C18130", "B18140")
places_data_acs_raw <- getCountryWideAcsTablesForMultipleYears(tables, years, survey, "place")


# Tracts data download
tables <- c("S1810", "S1811", "C18130","B18140")
tracts_data_acs_raw <- getCountryWideAcsTablesForMultipleYears(tables, years, survey, "tract", fips_for_state)



saveRawExtractedDataFile(places_pop_est, "places_pop_est")
saveRawExtractedDataFile(places_data_acs_raw, "places_data_acs_raw")
saveRawExtractedDataFile(tracts_data_acs_raw, "tracts_data_acs_raw")
