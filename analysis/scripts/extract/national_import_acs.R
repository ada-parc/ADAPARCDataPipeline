config_values <- config::get("acs")
years <- config_values$years
rm(config_values)


# download ----
raw_national_acs <- getCountryWideAcsForMultipleYears(years = years, scope = "national")


# export ----
saveRawExtractedDataFile(raw_national_acs, "raw_national_acs")
