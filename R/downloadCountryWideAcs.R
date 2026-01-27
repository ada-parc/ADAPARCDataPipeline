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
downloadCountryWideAcs <- function(tables,
                                   geography,
                                   year,
                                   survey = "acs5",
                                   fips_codes_for_states = c()) {

  # Ensure we have a reasonably long timeout while this runs
  old_timeout <- getOption("timeout")
  if (is.null(old_timeout) || old_timeout < 1200) {
    options(timeout = 1200)
  }
  on.exit({
    if (!is.null(old_timeout)) options(timeout = old_timeout)
  }, add = TRUE)

  api_key <- loadCensusAPIKey()

  # Small wrapper around get_acs to:
  # - add cache_table = TRUE
  # - throttle calls with Sys.sleep()
  safe_get_acs <- function(...) {
    Sys.sleep(0.25)  # throttle a bit between calls
    purrr::safely(tidycensus::get_acs, otherwise = NULL)(...)
  }

  # The ACS Request will be slightly different if it should be done state-by-state
  acs_request <- function(table) {

    if (length(fips_codes_for_states) > 0) {
      # ---- State-by-state requests ----
      state_results <- purrr::map(
        fips_codes_for_states,
        function(st) {
          res <- safe_get_acs(
            geography = geography,
            table     = table,
            year      = year,
            state     = st,
            geometry  = FALSE,
            key       = api_key,
            survey    = survey,
            cache_table = TRUE,
            .progress = FALSE
          )

          if (!is.null(res$error)) {
            message(
              "Failed get_acs() for table ", table,
              " year ", year,
              " geography ", geography,
              " state ", st, ": ",
              res$error$message
            )
            return(NULL)
          }

          res$result
        }
      )

      data <- state_results %>%
        purrr::compact() %>%      # drop NULLs
        dplyr::bind_rows()

    } else {
      # ---- Single national-level request (e.g. state / us / place) ----
      res <- safe_get_acs(
        geography = geography,
        table     = table,
        year      = year,
        survey    = survey,
        geometry  = FALSE,
        key       = api_key,
        cache_table = TRUE,
        .progress = FALSE
      )

      if (!is.null(res$error)) {
        message(
          "Failed get_acs() for table ", table,
          " year ", year,
          " geography ", geography, ": ",
          res$error$message
        )
        data <- NULL
      } else {
        data <- res$result
      }
    }

    data
  }

  # Download all requested tables and pivot wide
  df_list <- purrr::map(
    tables,
    function(tbl) {
      dat <- acs_request(tbl)

      if (is.null(dat) || nrow(dat) == 0) {
        message("No data returned for table ", tbl,
                " year ", year,
                " geography ", geography, " – skipping.")
        return(NULL)
      }

      dat %>%
        dplyr::select(GEOID, NAME, variable, estimate) %>%
        tidyr::pivot_wider(
          names_from  = variable,
          values_from = c(estimate)
        )
    }
  ) %>%
    purrr::compact()  # drop NULLs from failed tables

  # If everything failed, return empty tibble
  if (length(df_list) == 0) {
    warning("No ACS data could be downloaded for any table for year ",
            year, " and geography ", geography, ". Returning empty tibble.")
    df <- tibble::tibble()
  } else {
    df <- df_list %>%
      purrr::reduce(dplyr::left_join, by = c("GEOID", "NAME")) %>%
      dplyr::mutate(year = as.numeric(year))
  }

  if (geography == "state" && nrow(df) > 0) {
    df <- df %>%
      dplyr::left_join(
        tibble::tibble(state.abb, state.name) %>%
          tibble::add_row(
            state.abb  = c("PR", "DC"),
            state.name = c("Puerto Rico", "District of Columbia")
          ) %>%
          dplyr::select(NAME = state.name, ABBR = state.abb),
        by = "NAME"
      ) %>%
      dplyr::select(GEOID, NAME, ABBR, year, dplyr::everything())
  }

  if (geography == "us" && nrow(df) > 0) {
    df <- df %>%
      dplyr::mutate(
        ABBR  = "USA",
        GEOID = "000"
      )
  }

  return(df)
}
