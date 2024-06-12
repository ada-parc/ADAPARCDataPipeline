# Read transformed data files

acs_national <- readTransformedExtractedDataFile("transformed_acs_national")
hud_national <- readTransformedExtractedDataFile("transformed_hud_data")

national_data <- dplyr::left_join(acs_national, hud_national)


# Save final data files

saveFinalDataFile(national_data, "national_data")
