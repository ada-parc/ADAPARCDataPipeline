# Get year config
years <- config::get("hud")$years

national_hud <- getNationalHudForMultipleYears(years)

saveRawExtractedDataFile(data, "national_hud")
