#' downloadSpatialFootprintOfCitiesPlaces
#'
#' Downloads SF of Cities and Places in US, based on FIPS codes
#'
#' @param year year of data to download
#'
#' @returns A dataframe of downloaded data for counties and places within the US
#'
#' @import dplyr
#' @import sf
#' @import tigris
#' @import tibble
#' @import purrr
#'

downloadSpatialFootprintOfCitiesPlaces <- function(year) {

  # Here, we use all US fips_codes
  fips_codes_tidy <- tigris::fips_codes

  places_sf <-
    purrr::pmap_df(
    .l = fips_codes_tidy %>%
      dplyr::filter(as.numeric(state_code) %in% c(1:56, 72)) %>% # Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))
      dplyr::select(state_code) %>%
      dplyr::distinct(),
    .f = ~ (
      tigris::places(
        state = ..1,
        cb = TRUE,
        year = year,
        class = "sf"
      ) %>%
        dplyr::rename("place_GEOID" = GEOID,
               "place_NAME" = NAME)
    )
  ) %>%
    dplyr::select(STATEFP, PLACEFP, place_GEOID, place_NAME) %>%
    dplyr::mutate("place_area" = sf::st_area(.),
           "place_area_num" = as.numeric(place_area)) %>%
    dplyr::relocate(geometry, .after = last_col())

  return(places_sf)

}
