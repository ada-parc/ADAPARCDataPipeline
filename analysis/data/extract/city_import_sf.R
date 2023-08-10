# Get config values

config_values <- config::get()
year <- config_values$acs$years[1]
rm(config_values)

# Get FIPS

fips_codes_for_states <- getFIPSCodesForStates()

# Places/Cities lookup ----------------------------------------------------

places_sf <- downloadSFOfCitiesPlaces(year, fips_codes_for_states)

counties_sf <- downloadSFOfCounties(year, fips_codes_for_states)

#TODO: Unclear why the state_code title of this list causes problems for tracts but not other requests.
tracts_sf <- downloadSFOfTracts(year, fips_codes_for_states$state_code)

rm("fips_codes_for_state_and_county", "fips_codes_for_states")

# export ----
save.image(here::here("analysis", "data", "city_import_sf.Rda"))
rm(list = ls())
