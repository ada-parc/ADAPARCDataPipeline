#' getFIPSCodes
#'
#' Downloads FIPS codes to use for analysis
#'
#' @returns A df including information on US FIPS codes, filtered for use in analysis
#'
#' @import dplyr
#' @import tigris
#'

getFIPSCodes <- function() {

  fips_codes_for_lookup <- c(
      tigris::fips_codes %>%
        dplyr::filter(as.numeric(state_code) %in% c(1:56, 72)) %>% # Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))
        dplyr::select(state_code) %>%
        dplyr::distinct()
    )

    return(fips_codes_for_lookup)
  }
