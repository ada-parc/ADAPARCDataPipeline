#' downloadACSTracts
#'
#' Downloads and Formats ACS Tracts for the place geography
#'
#' @param tables array of strings, corresponding to the US Census tables to be downloaded
#' @param geography the geography of the request; can be either "state" or "us".
#' @param year year of data to download
#' @param survey specification of ACS survey type (ex. "acs5" or "acs1")
#'
#' @returns A dataframe of downloaded ACS data, in wide format.
#'
#' @import dplyr
#' @import tidycensus
#' @import stringr
#' @import purrr
#'

downloadACSTracts <- function(tables, geography = "tracts", year, survey = "acs5", fips_codes_for_lookup) {
  # TODO: Understand why this function is meaningfully different to the data download flow from what was written for the national data -- can it be combined? with all ACS flows?
  # TODO: Throw error if there is no Census key loaded
  # TODO: Throw error if any of the input parameters are not correct.
  # TODO: Throw error if any of the tables are not present on our official list, according to what we get from the Census.
  # TODO: Figure out how to create an example for this function's documentation; unable to get \dontrun{} to compile.

  api_key <- loadCensusAPIKey()

  fips_codes_for_lookup_formatted <- fips_codes_for_lookup %>%
    dplyr::select(state_code, county_code)

  df <-
    purrr::pmap_df(.l = fips_codes_for_lookup_formatted,
                   .f = ~ (
                     tidycensus::get_acs(
                       geography = geography,
                       table = tables,
                       year = year,
                       # show_call = T,
                       geometry = FALSE,
                       output = "wide",
                       key = api_key,
                       survey = survey
                     )  %>%
                       dplyr::rename_with(
                         .cols = matches("[0-9]{3}(E|M)$"),
                         ~ ifelse(
                           stringr::str_detect(.x, "E$"),
                           stringr::str_replace(.x, "E$", "_estimate"),
                           stringr::str_replace(.x, "M$", "_moe")
                         )
                       )
                   ))

  return(df)
}
