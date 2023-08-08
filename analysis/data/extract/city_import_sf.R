# Get config values

config_values <- config::get()
year <- config_values$acs$years[1]
rm(config_values)

# Get FIPS

fips_codes_for_lookup <- getFIPSForSFs()

# Places/Cities lookup ----------------------------------------------------

places_sf <- downloadSFOfCitiesPlaces(year, fips_codes_for_lookup)

counties_sf <- downloadSFOfCounties(year, fips_codes_for_lookup)

tracts_sf <- downloadSFOfTracts(year, fips_codes_for_lookup)

rm(fips_codes_for_lookup)

# export ----
save.image(here::here("analysis", "data", "city_import_sf.Rda"))
rm(list = ls())
