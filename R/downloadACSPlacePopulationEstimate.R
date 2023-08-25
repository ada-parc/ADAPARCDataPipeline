#' downloadACSPlacePopulationEstimate
#'
#' Download the ACS Population Estimates for place
#'
#' Downloads and filters ACS population estimates at the "place" level across the US
#'
#' @param year the year for the lookup
#'
#' @returns a dataframe with GEOID, Name, and Population of places in the US
#'
#' @import tidycensus
#' @import dplyr
#'

downloadACSPlacePopulationEstimate <- function(year) {

  api_key <- loadCensusAPIKey()

  df <- tidycensus::get_acs(
    geography = "place",
    variables = c(POP = "B01001_001E"),
    year = year,
    show_call = FALSE,
    geometry = FALSE,
    output = "wide",
    key = api_key
  ) %>%
    dplyr::select(GEOID, NAME, POP)

  return(df)
}
