# Read transformed data files
demographics <- readTransformedExtractedDataFile("transformed_acs_demographics")

acs_community_living <- readTransformedExtractedDataFile("transformed_acs_community_living")

hud_community_living <- readTransformedExtractedDataFile("transformed_hud_data")

community_living <- dplyr::left_join(acs_community_living, hud_community_living)

work_economic <- readTransformedExtractedDataFile("transformed_acs_work_economic")

community_participation <- readTransformedExtractedDataFile("transformed_acs_community_participation")


# Save final data files
saveFinalDataFile(demographics, "demographics")

saveFinalDataFile(community_living, "community_living")

saveFinalDataFile(work_economic, "work_economic")

saveFinalDataFile(community_participation, "community_participation")
