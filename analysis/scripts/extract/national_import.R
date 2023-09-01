config_values <- config::get("acs")
years <- config_values$years
survey <- config_values$survey
rm(config_values)


# download ----
demographics_tables <- c("S1810")
raw_national_acs_demographics <- getCountryWideAcsTablesForMultipleYears(demographics_tables, years)


living_tables <- c("S1810", "S2601A", "S2602", "B26108")
raw_national_acs_living <- getCountryWideAcsTablesForMultipleYears(living_tables, years)



participation_tables <- c("S1810", "S1811", "B18135")
raw_national_acs_participation <- getCountryWideAcsTablesForMultipleYears(participation_tables, years)



economic_tables <- c("S1810",
                     "S1811",
                     "B18135",
                     "B18140",
                     "B25091",
                     "B25070",
                     "C18120",
                     "C18121",
                     "C18130")

raw_national_acs_economic <- getCountryWideAcsTablesForMultipleYears(economic_tables, years)


# export ----
# TODO: UPDATE THE NAMES FOR THESE RAW FILES TO REFLECT THAT THEY ARE RAW AND FROM ACS.
saveRawExtractedDataFile(raw_national_acs_demographics, "national_demographics")
saveRawExtractedDataFile(raw_national_acs_living, "national_living")
saveRawExtractedDataFile(raw_national_acs_participation, "national_participation")
saveRawExtractedDataFile(raw_national_acs_economic, "national_economic")

# TODO: UPDATE THE

# save.image(here::here("analysis", "data", "national_raw.Rda"))
rm(list = ls())
