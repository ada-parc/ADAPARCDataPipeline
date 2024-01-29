#' getCountryWideAcsTablesForMultipleYears
#'
#' Description
#'
#' @param tables raw data extracted from an external source (not transformed). Must be an R object.
#' @param years the years of data to retrieve
#' @param survey ACS survey requested (ex. "acs1" or "acs5")
#' @param scope The scope of the geography to request--for example, can be "tracts" to get census tract data, or can be "national," which will request both "state" and "us" geographies from the cesus
#' @param fips_codes_for_states A list of FIPS codes to be used for lookup if the request will be made state-by-state
#'
#' @returns A dataframe of country-wide ACS data, in wide format, for both US and state-levels.
#'
#' @import here
#' @import dplyr
#' @import purrr

getCountryWideAcsTablesForMultipleYears <- function(tables, years, survey, scope, fips_codes_for_states = c()) {

  downloadSingleYearOfAcsTableData <- function(tables, year) {

    tables_to_request <- tables

    if (year < 2017) {
      # Filter out tables that are not available before 2017
      tables_unavailable_before_2017 <- c("S2602", "B26108")
      tables_to_request <- setdiff(tables_to_request, tables_unavailable_before_2017)
    }

    if(scope == "national") {
      acs_state_data <- downloadCountryWideAcs(tables_to_request, "state", year, survey)
      acs_national_data <- downloadCountryWideAcs(tables_to_request, "us", year, survey)

      # Put national data first so that US is first in the DF
      full_country_data <- dplyr::bind_rows(acs_national_data, acs_state_data)
    }

    if (scope == "tract" | scope == "place") {
      full_country_data <- downloadCountryWideAcs(tables_to_request, scope, year, survey, fips_codes_for_states)
    }


    return(full_country_data)
  }


  full_country_data_all_years <-
    purrr::map(years,
      ~ downloadSingleYearOfAcsTableData(tables, .x)) %>%
    dplyr::bind_rows()

    return(full_country_data_all_years)
}
