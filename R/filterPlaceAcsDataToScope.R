#' filterPlaceAcsDataToScope
#'
#'
#' @param place_acs_data place acs data to be filtered
#' @param geoids_in_scope list of GEOIDs to filter the data
#'
#' @returns A dataframe of filtered ACS data, in wide format.
#'
#' @import dplyr
#'

filterPlaceAcsDataToScope <- function(place_acs_data, geoids_in_scope) {

  df <- place_acs_data %>%
    dplyr::filter(GEOID %in% geoids_in_scope)

  return(df)
}
