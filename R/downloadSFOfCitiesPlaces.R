#' downloadSFOfCitiesPlaces
#'
#' Downloads Spatial Footprint (SF) of Cities and Places in US, based on FIPS codes
#'
#' @param year year of data to download
#' @param fips_codes_for_states list of fips codes for states that we want to look up
#'
#' @returns A dataframe of downloaded data for counties and places within the US
#'
#' @import dplyr
#' @import sf
#' @import tigris
#' @import tibble
#' @import purrr
#'

downloadSFOfCitiesPlaces <- function(year, fips_codes_for_states) {

  places_sf <-
    purrr::map(fips_codes_for_states,
               ~ tigris::places(
                   state = .x,
                   cb = TRUE,
                   year = year,
                   class = "sf"
                 )) %>%
    dplyr::bind_rows() %>%
    dplyr::select(STATEFP, PLACEFP, place_GEOID = GEOID, place_NAME = NAME) %>%
    dplyr::mutate("place_area" = sf::st_area(.),
                  "place_area_num" = as.numeric(place_area)) %>%
    dplyr::relocate(geometry, .after = dplyr::last_col())

  return(places_sf)

}
