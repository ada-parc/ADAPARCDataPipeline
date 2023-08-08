year <- config::get("acs")$years[1]

tables <- c("S1810", "S1811")

places_acs <- downloadACSPlace(tables, "place", year, "acs5")


# MOVE CITY_PLACE_FULL to one of the transform sections; not referenced until loading flows (where we're exporting Rda's)
# city_place_full <- places_acs %>%
#   filter(GEOID %in% places_sf$place_GEOID)


# TODO: Jane - can't refactor the tracts data process until we have refactored the
# "places_counties_crosswalk.R" file (that's where we get the places_counties df)

# tracts_data <- pmap_df(places_counties %>%
#                          select(STATEFP, COUNTYFP) %>%
#                          distinct(),
#                        ~get_acs(geography = "tract",
#                                state = ..1,
#                                county = ..2,
#                                table = "S1810",
#                                year = year,
#                                # show_call = T,
#                                geometry = FALSE,
#                                output = "wide")  %>%
#                          rename_with(.cols = matches("[0-9]{3}(E|M)$"),
#                                      ~ifelse(str_detect(.x, "E$"), str_replace(.x, "E$", "_estimate"), str_replace(.x, "M$", "_moe"))))

save.image(here::here("city", "import_acs", "output", "city_import_acs.Rda"))

rm(list = ls())
