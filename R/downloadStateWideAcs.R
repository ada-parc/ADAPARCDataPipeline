#' downloadStateWideAcs
#'
#' Downloads and Formats ACS Tracts for the place geography
#'
#' @param tables array of strings, corresponding to the US Census tables to be downloaded
#' @param geography the geography of the request; can be either "state" or "us".
#' @param year year of data to download
#' @param survey specification of ACS survey type (ex. "acs5" or "acs1")
#' @param state_codes_for_lookup list of fips codes of states for lookup
#'
#' @returns A dataframe of downloaded ACS data, in wide format.
#'
#' @import dplyr
#' @import tidycensus
#' @import stringr
#' @import purrr
#'

downloadStateWideAcs <- function(tables, geography = "tract", year, survey = "acs5", state_codes_for_lookup) {
  # TODO: Understand why this function is meaningfully different to the data download flow from what was written for the national data -- can it be combined? with all ACS flows?
  # TODO: Throw error if there is no Census key loaded
  # TODO: Throw error if any of the input parameters are not correct.
  # TODO: Throw error if any of the tables are not present on our official list, according to what we get from the Census.
  # TODO: Figure out how to create an example for this function's documentation; unable to get \dontrun{} to compile.

  api_key <- loadCensusAPIKey()

  get_data_by_state_and_table <- function(state, table) {

    message(paste0("Retrieving data for state: ", state))

    tidycensus::get_acs(
      geography = geography,
      table = table,
      year = year,
      state = state,
      geometry = FALSE,
      output = "wide",
      key = api_key,
      survey = survey
    )
  }

  df <-
    purrr::map(tables, function(table) {

      message(paste0("Retrieving data for table: ", table))

      purrr::map(state_codes_for_lookup, function(state) {
        get_data_by_state_and_table(state, table)
      },
      .progress = TRUE) %>%
        purrr::reduce(left_join)
    }) %>%
    purrr::reduce(left_join)

  return(df)
}
