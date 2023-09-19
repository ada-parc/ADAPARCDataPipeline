#' getFIPSCodesForStateAndCounty
#'
#' Downloads FIPS codes for states and counties to use for analysis
#'
#' @returns A df of US FIPS codes for states and counties, filtered for use in analysis. Note that state codes are unique and county codes are unique WITHIN states.
#'
#' @import dplyr
#' @import tigris
#'

getFIPSCodesForStateAndCounty <- function() {

  fips_codes_for_lookup <-
      tigris::fips_codes %>%
        dplyr::filter(as.numeric(state_code) %in% c(1:56, 72))# Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))

    return(fips_codes_for_lookup)
  }
