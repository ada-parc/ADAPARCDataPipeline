#' transformPlacesTractsCountiesWithFIPSAndTidyNames
#'
#' Combines a places_tracts map with FIPS codes and names, cleaning up the names of places for display. Currently, we don't use the county field; we include the county information in case we decide that we want county-wide analysis. Will save us effort down the line, if need be.
#'
#' @param places_tracts a map that has place, tract, and county GEOIDs plus place and tract names
#' @param county_state_fips dataframe containing places and their population
#'
#' @returns a tidied place_tract_county_map
#'
#' @import dplyr
#' @import stringr
#'

transformPlacesTractsCountiesWithFIPSAndTidyNames <- function(places_tracts, county_state_fips) {

  clean_up_place_name <- function(place_GEOID, original_name) {

    name <- dplyr::case_when(
      place_GEOID == "3651000" ~
        "New York City",
      place_GEOID == "4752006" ~
        "Nashville",
      place_GEOID == "1571550" ~
        "Honolulu",
      place_GEOID == "2146027" ~
        "Lexington",
      place_GEOID == "2148006" ~
        "Louisville",
      TRUE ~
        stringr::str_remove_all(original_name,
                       pattern = " (city|village|municipality|town|city and borough|borough|(city|((unified|consolidated|metro|metropolitan) government)) \\(balance\\)|\\(balance\\)), ")
    )

    return(name)

  }


  place_tract_county_map <-
    places_tracts %>%
    dplyr::left_join(county_state_fips, by = c("COUNTYFP" = "county_code", "STATEFP" = "state_code")) %>%
    dplyr::mutate(place_NAME = clean_up_place_name(place_GEOID, place_NAME),
           metro_state = paste0(place_NAME, ", ", state))

  return(place_tract_county_map)
}
