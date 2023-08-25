config_values <- config::get("acs")
# TODO: Update this script so that it will handle multiple years.
year <- config_values$years[1]
survey <- config_values$survey
rm(config_values)


# TODO: Work this into two calls--state and national, so they are combined into a single dataset
# TODO: Create a function that will facilitate and bind downloading the different state and us datasets


# download ----
tables <- c("S1810")
state_demographics <- downloadCountryWideAcs(tables, "state", year, survey)
national_demographics <- downloadCountryWideAcs(tables, "us", year, survey)
stacked_demographics <- bind_rows(national_demographics, state_demographics)
rm(state_demographics, national_demographics)

if(year > 2017) {
  tables <- c("S1810", "S2601A", "S2602", "B26108")
  state_living <- downloadCountryWideAcs(tables, "state", year, survey)
  national_living <- downloadCountryWideAcs(tables, "us", year, survey)
  stacked_living <- bind_rows(national_living, state_living)
  rm(state_living, national_living)
} else {
  message("ACS group quarters tables were not available until 2017 and cannot be included in current request.")
}

tables <- c("S1810", "S1811", "B18135")
state_participation <- downloadCountryWideAcs(tables, "state", year, survey)
national_participation <- downloadCountryWideAcs(tables, "us", year, survey)
stacked_participation <- bind_rows(state_participation, national_participation)
rm(state_participation, national_participation)

tables <- c("S1810", "S1811",
            "B18135", "B18140", "B25091", "B25070",
            "C18120", "C18121", "C18130")
state_economic <- downloadCountryWideAcs(tables, "state", year, survey)
national_economic <- downloadCountryWideAcs(tables, "us", year, survey)
stacked_economic <- bind_rows(state_economic, national_economic)
rm(state_economic, national_economic, survey, tables)

# export ----

saveRawExtractedDataFile(stacked_demographics, "national_demographics")
saveRawExtractedDataFile(stacked_living, "national_living")
saveRawExtractedDataFile(stacked_participation, "national_participation")
saveRawExtractedDataFile(stacked_economic, "national_economic")


# save.image(here::here("analysis", "data", "national_raw.Rda"))
rm(list = ls())
