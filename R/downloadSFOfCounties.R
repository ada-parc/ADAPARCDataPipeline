#' downloadSFOfCounties
#'
#' Downloads Spatial Footprint (SF) of Cities and Places in US, based on FIPS codes
#'
#' @param year year of data to download
#' @param fips_codes_for_lookup list of fips codes that we want to look up
#'
#' @returns A dataframe of downloaded data for counties and places within the US
#'
#' @import dplyr
#' @import tigris
#' @import purrr
#'

downloadSFOfCounties <- function(year, fips_codes_for_lookup) {

  sf <-
    purrr::pmap_df(.l = fips_codes_for_lookup,
        .f = ~(tigris::counties(state = ..1,
                                cb = TRUE,
                                year = year,
                                class = "sf") %>%
                 dplyr::rename("county_GEOID" = GEOID,
                        "county_NAME" = NAME))) %>%
    dplyr::select(COUNTYFP, county_GEOID, county_NAME)

return(sf)
}
