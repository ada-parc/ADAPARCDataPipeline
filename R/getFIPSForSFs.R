#' getFIPSForSFs
#'
#' Downloads FIPS codes to use for downloading SFs
#'
#' @returns A list of FIPS codes to be used to download SFs
#'
#' @import dplyr
#' @import tigris
#'

getFIPSForSFs <- function() {

  fips_codes_for_lookup <- c(
      tigris::fips_codes %>%
        dplyr::filter(as.numeric(state_code) %in% c(1:56, 72)) %>% # Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))
        dplyr::select(state_code) %>%
        dplyr::distinct()
    )

    return(fips_codes_for_lookup)
  }
