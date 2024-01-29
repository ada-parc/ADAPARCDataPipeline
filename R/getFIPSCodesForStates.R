#' getFIPSCodesForStates
#'
#' Downloads FIPS codes for states and counties to use for analysis
#'
#' @returns A list of FIPS state codes
#'
#' @import dplyr
#' @import tigris
#'

getFIPSCodesForStates <- function() {

  fips_codes_for_lookup <- getFIPSCodesForStateAndCounty() %>%
    dplyr::pull(state_code) %>%
    unique()

  return(fips_codes_for_lookup)
}
