#' getFIPSCodesForStates
#'
#' Downloads FIPS codes for states and counties to use for analysis
#'
#' @returns A df including information on US FIPS codes, filtered for use in analysis
#'
#' @import dplyr
#' @import tigris
#'

getFIPSCodesForStates <- function() {

  fips_codes_for_lookup <- c(getFIPSCodesForStateAndCounty() %>%
                               dplyr::select(state_code))

  return(fips_codes_for_lookup)
}
