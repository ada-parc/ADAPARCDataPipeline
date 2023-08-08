#' downloadSFOfCitiesPlaces
#'
#' Downloads Spatial Footprint (SF) of Cities and Places in US, based on FIPS codes
#'
#' @param year year of data to download
#' @param fips_codes_for_lookup list of fips codes that we want to look up
#'
#' @returns A dataframe of downloaded data for counties and places within the US
#'
#' @import dplyr
#' @import sf
#' @import tigris
#' @import tibble
#' @import purrr
#'

downloadSFOfCitiesPlaces <- function(year, fips_codes_for_lookup) {

  places_sf <-
    purrr::pmap_df(
    .l = fips_codes_for_lookup,
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
