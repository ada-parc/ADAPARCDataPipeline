# Get year config
year <- config::get("acs")$years[1]

# Places data download
tables <- c("S1810", "S1811")
places_acs <- downloadACSPlace(tables, "place", year, "acs5")

# Tracts data download
tables <- c("S1810")
tracts_data <- downloadACSTracts(tables, "tracts", year, getFIPSCodesForStateAndCounty())


rm("tables")
save.image(here::here("city", "import_acs", "output", "city_import_acs.Rda"))

rm(list = ls())
