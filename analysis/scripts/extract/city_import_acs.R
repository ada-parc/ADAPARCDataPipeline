# Get year config
year <- config::get("acs")$years[1]
survey <- config::get("acs")$survey

# Places data download
#TODO: This function is too specific... It's likely that the direction extraction will go in is to only pull variables that we specifically want, in which case this might fit in with that flow.
places_pop_est <- downloadACSPlacePopulationEstimate(year)

tables <- c("S1810", "S1811")
places_acs <- downloadCountryWideAcs(tables, "place", year, survey)


# Tracts data download
tables <- c("S1810")
fips_for_state <- getFIPSCodesForStates()$state_code

tracts_data <- downloadACSTracts(tables, "tract", year, survey, fips_for_state)

saveRawExtractedDataFile(places_pop_est, "places_pop_est")
saveRawExtractedDataFile(places_acs, "places_acs")
saveRawExtractedDataFile(tracts_data, "tracts_data")
