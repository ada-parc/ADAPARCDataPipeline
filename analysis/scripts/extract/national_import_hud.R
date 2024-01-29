# Get year config
years <- config::get("hud")$years

national_hud <- getNationalHudForMultipleYears(years)

saveRawExtractedDataFile(national_hud, "national_hud")

rm(list = ls())
