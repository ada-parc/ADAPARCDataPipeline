#' downloadCountryWideAcs
#'
#' Downloads ACS Data for the full country, according to the specified geography. Please consult the list here to ensure that the geography you are requesting does not require a sub-geography, such as state, in order to request: https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus
#'
#' @param tables array of strings, corresponding to the US Census tables to be downloaded
#' @param geography the geography of the request; can be geographies such as "state" / "us" / "place".
#' @param year year of data to download
#' @param survey specification of ACS survey type (ex. "acs5" or "acs1")
#' @param fips_codes_for_states an optional list of state codes for ACS lookup if the geography must be requested state-by-state
#'
#' @returns A dataframe of downloaded ACS data, in wide format.
#'
#' @import magrittr
#' @import dplyr
#' @import tidycensus
#' @import purrr
#' @import tibble
#' @import tidyr
#'

downloadCountryWideAcs <- function(tables, geography, year, survey = "acs5", fips_codes_for_states = c()) {
  # TODO: Throw error if there is no Census key loaded
  # TODO: Throw error if any of the input parameters are not correct.
  # TODO: Throw error if any of the tables are not present on our official list, according to what we get from the Census.
  # TODO: Figure out how to create an example for this function's documentation; unable to get \dontrun{} to compile.

  api_key <- loadCensusAPIKey()

  # The ACS Request will be slightly different if it should be done state-by-state (and someone passes in FIPS Codes for lookup) or if they can make a national request
  acs_request <- function(table) {
    if(length(fips_codes_for_states) > 0) {

      data <-
        purrr::map(
          fips_codes_for_states,
          ~ tidycensus::get_acs(
            geography = geography,
            table = table,
            year = year,
            state = .x,
            geometry = FALSE,
            key = api_key,
            survey = survey,
            .progress = TRUE
          )
        ) %>%
        dplyr::bind_rows()

    } else {

      data <-
        tidycensus::get_acs(
        year = year,
        geography = geography,
        table = table,
        survey = survey,
        geometry = F,
        key = api_key,
        cache_table = F,
        .progress = TRUE
      )

    }

    return(data)
  }



  df <- purrr::map(
    tables,
    ~ acs_request(.x)	%>%
      dplyr::select(GEOID, NAME, variable, estimate) %>%
      tidyr::pivot_wider(
      names_from = variable,
      values_from = c(estimate))
  ) %>%
    purrr::reduce(left_join) %>%
    dplyr::mutate(year = year)

  if (geography == "state") {
    df <- df %>%
      dplyr::left_join(
        tibble::tibble(state.abb, state.name) %>%
          tibble::add_row(
            state.abb = c("PR", "DC"),
            state.name = c("Puerto Rico", "District of Columbia")
          ) %>%
          dplyr::select(NAME = state.name, ABBR = state.abb)
      ) %>%
      dplyr::select(GEOID, NAME, ABBR, everything())
  }

  if (geography == "us") {
    df <- df %>%
      dplyr::mutate(ABBR = "USA",
             GEOID = "000")
  }

  return(df)
}
