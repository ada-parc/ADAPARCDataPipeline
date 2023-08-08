year <- config::get("acs")$years[1]

tables <- c("S1810", "S1811")

places_acs <- downloadACSPlace(tables, "place", year, "acs5")


# MOVE CITY_PLACE_FULL to one of the transform sections; not referenced until loading flows (where we're exporting Rda's)
# city_place_full <- places_acs %>%
#   filter(GEOID %in% places_sf$place_GEOID)

tables <- c("S1810")
tracts_data <- downloadACSTracts(tables, "tracts", year, getFIPSCodes())


save.image(here::here("city", "import_acs", "output", "city_import_acs.Rda"))

rm(list = ls())
