#' getADAPARCBaseToSourceVariableMap
#'
#' Read in a dataframe that maps ADA-PARC variables to their source
#'
#' @returns a dataframe mapping ADA-PARC base variables to their source variables, including the year and the source of the data (ex. ACS, HUD)
#'
#' @import readxl
#' @import here

getADAPARCBaseToSourceVariableMap <- function() {

  return(readxl::read_xlsx(here::here("analysis", "data", "ada_parc_base_variables_mapped_to_source_variables.xlsx")))
}
