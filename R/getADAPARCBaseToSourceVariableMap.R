#' getADAPARCBaseToSourceVariableMap
#'
#' Read in a dataframe that maps ADA-PARC variables to their source
#'
#' @returns a dataframe mapping ADA-PARC base variables to their source variables, including the year and the source of the data (ex. ACS, HUD)
#'
#' @import readxl
#' @import here

getADAPARCBaseToSourceVariableMap <- function() {

  # TODO: Check for errors--for example, need unique ada_parc_var_names and unique source_var_names for each year (i.e. no repeats within a specified year)

  return(readxl::read_xlsx(here::here("analysis", "data", "ada_parc_base_variables_mapped_to_source_variables.xlsx")))
}
