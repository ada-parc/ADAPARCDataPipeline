#' downloadSFOfTracts
#'
#' Downloads Spatial Footprint (SF) of Tracts in US, based on FIPS codes
#'
#' @param year year of data to download
#' @param fips_codes_for_lookup list of fips codes that we want to look up
#'
#' @returns A dataframe of downloaded data for Census tracts within the US
#'
#' @import dplyr
#' @import tigris
#' @import purrr
#'

downloadSFOfTracts <- function(year, fips_codes_for_lookup) {

  sf <-
    purrr::pmap_df(.l = fips_codes_for_lookup_formatted,
                   .f = ~ (
                     tigris::tracts(
                       state = ..1,
                       county = ..2,
                       cb = TRUE,
                       year = year,
                       class = "sf"
                     )
                   )) %>%
    dplyr::select("t_GEOID" = GEOID,
                  "tract_NAME" = NAME,
                  STATEFP,
                  COUNTYFP) %>%
    dplyr::mutate("tract_area" = st_area(.),
                  "tract_area_num" = as.numeric(tract_area)) %>%
    dplyr::relocate(geometry, .after = last_col())

  return(sf)
}
