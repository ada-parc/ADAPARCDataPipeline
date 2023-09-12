#' splitPlacesSFByState
#'
#' Split a Places SF dataframe into a split dataframe according to states. Note: "Places" is a geography defined by the US Census.
#'
#' @param places original dataframe of places SF
#' @param places_pop_est dataframe containing places and their population
#'
#' @returns a dataframe of places, split by state code
#'
#' @import dplyr

splitPlacesSFByState <- function(places, places_pop_est) {

  filtered_places <-
    places_sf %>%
    dplyr::mutate(STATEFP = as.factor(STATEFP)) %>%
    dplyr::left_join(places_pop_est, by = c("place_GEOID" = "GEOID")) %>%
    dplyr::filter(POP > 3500)

  places_split <- split(filtered_places, filtered_places$STATEFP)


}
