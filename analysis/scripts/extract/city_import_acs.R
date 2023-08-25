# Get year config
year <- config::get("acs")$years[1]
survey <- config::get("acs")$survey

# Places data download

places_pop_est <- downloadACSPlacePopulationEstimate(year)

tables <- c("S1810", "S1811")
places_acs <- downloadCountryWideAcs(tables, "place", year, survey)


# Tracts data download
tables <- c("S1810")
fips_for_state_and_county <- getFIPSCodesForStateAndCounty()
tracts_test <- downloadAndFormatAcs(tables, geography = "tracts", year, survey)
tracts_data <- downloadACSTracts(tables, "tracts", year, survey, fips_for_state_and_county)

saveRawExtractedDataFile(places_pop_est, "places_pop_est")
saveRawExtractedDataFile(places_acs, "places_acs")
saveRawExtractedDataFile(tracts_data, "tracts_data")

rm(list = ls())
