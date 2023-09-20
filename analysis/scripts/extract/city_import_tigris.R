# Get config values

config_values <- config::get()
year <- config_values$acs$years[1]

# Get FIPS

fips_codes_for_states <- getFIPSCodesForStates()

# Places/Cities lookup ----------------------------------------------------

places_sf <- downloadSFOfCitiesPlaces(year, fips_codes_for_states)

counties_sf <- downloadSFOfCounties(year, fips_codes_for_states)

tracts_sf <- downloadSFOfTracts(year, fips_codes_for_states)


# export ----
saveRawExtractedDataFile(places_sf, "places_sf")
saveRawExtractedDataFile(counties_sf, "counties_sf")
saveRawExtractedDataFile(tracts_sf, "tracts_sf")
