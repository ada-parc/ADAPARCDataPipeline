#' downloadACSPlacePopulationEstimate
#'
#' Download the ACS Population Estimates for place
#'
#' Downloads and filters ACS population estimates at the "place" level across the US
#'
#' @param year the year for the lookup
#'
#' @returns Census API Key
#'

downloadACSPlacePopulationEstimate <- function(year) {

  api_key <- loadCensusAPIKey()

  df <- get_acs(
    geography = "place",
    variables = c(POP = "B01001_001E"),
    year = year,
    show_call = FALSE,
    geometry = FALSE,
    output = "wide",
    key = api_key
  ) %>%
    select(GEOID, NAME, POP)

  return(df)
}
