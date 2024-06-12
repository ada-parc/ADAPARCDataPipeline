#' getCountryWideAcsForMultipleYears
#'
#' This function retrieves country-wide ACS data, using the file 'ada_parc_base_variables_mapped_to_source_variables.xlsx" to pull the base ACS variables that are specified there. This allows multiple years of data to be retrieved, if relevant.
#'
#' @param years the years of data to retrieve
#' @param scope The scope of the geography to request--for example, can be "tracts" to get census tract data, or can be "national," which will request both "state" and "us" geographies from the cesus
#'
#' @returns A dataframe of country-wide ACS data, in wide format, for both US and state-levels.
#'
#' @import here
#' @import dplyr
#' @import purrr

getCountryWideAcsForMultipleYears <- function(years, scope) {

  # TODO: We use fips codes for states for places and tracts (not yet at national level).
  # Update this once we've finished integrating stuff for the current.

  acs_variables <- purrr::map(years, ~ {
    readxl::read_xlsx(
      path =  here::here(
        "analysis",
        "data",
        "ada_parc_base_variables_mapped_to_source_variables.xlsx"
      ),
      sheet = as.character(.x)
    ) %>%
      dplyr::filter(source == "ACS") %>%
      dplyr::select(source_var_code, year)
  }) %>%
    dplyr::bind_rows()

  downloadACS <- function(variables_for_year, year_to_download, geography) {
    api_key <- loadCensusAPIKey()

    data <-
      tidycensus::get_acs(
        year = year_to_download,
        geography = geography,
        variable = variables_for_year,
        survey = "acs5",
        geometry = F,
        key = api_key,
        cache_table = F,
        .progress = TRUE
      ) %>%
        dplyr::select(GEOID, NAME, variable, estimate) %>%
        tidyr::pivot_wider(
          names_from = variable,
          values_from = c(estimate)) %>%
      dplyr::mutate(year = as.numeric(year_to_download))

    if (geography == "state") {
      df <- data %>%
        dplyr::left_join(
          tibble::tibble(state.abb, state.name) %>%
            tibble::add_row(
              state.abb = c("PR", "DC"),
              state.name = c("Puerto Rico", "District of Columbia")
            ) %>%
            dplyr::select(NAME = state.name, ABBR = state.abb)
        ) %>%
        dplyr::select(GEOID, NAME, ABBR, year, everything())
    }

    if (geography == "us") {
      df <- data %>%
        dplyr::mutate(ABBR = "USA",
                      GEOID = "000")
    }

    return(df)
  }

  downloadSingleYearOfAcsData <- function(year_to_download) {

    print(year_to_download)

    variables_for_year <- acs_variables %>%
      dplyr::filter(year == year_to_download) %>%
      dplyr::pull(source_var_code)

    if(scope == "national") {
      acs_state_data <- downloadACS(variables_for_year, year_to_download,  "state")
      acs_national_data <- downloadACS(variables_for_year, year_to_download, "us")

      # Put national data first so that US is first in the DF
      full_country_data <- dplyr::bind_rows(acs_national_data, acs_state_data)
    }

    # if (scope == "tract" | scope == "place") {
      # full_country_data <- downloadCountryWideAcs(variables_for_year, scope, year_to_download, survey, fips_codes_for_states)
    # }


    return(full_country_data)
  }


  full_country_data_all_years <-
    purrr::map(years,
               ~ downloadSingleYearOfAcsData(.x)) %>%
    dplyr::bind_rows()

  return(full_country_data_all_years)
}
