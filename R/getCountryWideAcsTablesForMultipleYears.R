#' getCountryWideAcsTablesForMultipleYears
#'
#' Description
#'
#' @param tables raw data extracted from an external source (not transformed). Must be an R object.
#' @param years name for the file
#' @param survey ACS survey requested (ex. "acs1" or "acs5")
#'
#' @returns A dataframe of country-wide ACS data, in wide format, for both US and state-levels.
#'
#' @import here
#' @import dplyr
#' @import purrr

getCountryWideAcsTablesForMultipleYears <- function(tables, years, survey) {

  downloadSingleYearOfAcsTableData <- function(tables, year) {

    tables_to_request <- tables

    if (year < 2017) {
      # Filter out tables that are not available before 2017
      tables_unavailable_before_2017 <- c("S2602", "B26108")
      tables_to_request <- setdiff(tables_to_request, tables_unavailable_before_2017)
    }


    acs_state_data <- downloadCountryWideAcs(tables_to_request, "state", year, survey)
    acs_national_data <- downloadCountryWideAcs(tables_to_request, "us", year, survey)

    # Put national data first so that US is first in the DF
    full_country_data <- dplyr::bind_rows(acs_national_data, acs_state_data)

    return(full_country_data)
  }


  full_country_data_all_years <-
    purrr::map(years,
      ~ downloadSingleYearOfAcsTableData(tables, .x)) %>%
    dplyr::bind_rows()

    return(full_country_data_all_years)
}
