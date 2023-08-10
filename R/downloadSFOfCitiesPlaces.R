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

  # start_p <- Sys.time()
  places_sf <-
    purrr::map(fips_codes_for_states,
               ~ tigris::places(
                   state = .x,
                   cb = TRUE,
                   year = year,
                   class = "sf"
                 )) %>%
    purrr::reduce(left_join) %>%
    dplyr::select(STATEFP, PLACEFP, place_GEOID = GEOID, place_NAME = NAME) %>%
    dplyr::mutate("place_area" = sf::st_area(.),
                  "place_area_num" = as.numeric(place_area)) %>%
    dplyr::relocate(geometry, .after = last_col())

  # start <- Sys.time()
  # jane <-
  # lapply(fips_codes_for_states, function(x) { tigris::places(
  #   state = x,
  #   cb = TRUE,
  #   year = year,
  #   class = "sf"
  # )}) %>% rbind_tigris()
  # end <- Sys.time()


  return(places_sf)

}
