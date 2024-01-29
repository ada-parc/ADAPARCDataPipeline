#' getADAPARCBaseToSourceVariableMap
#'
#' Read in a dataframe that maps ADA-PARC variables to their source.
#'
#' @param years list of years being retrieved
#'
#' @returns a dataframe mapping ADA-PARC base variables to their source variables, including the year and the source of the data (ex. ACS, HUD)
#'
#' @import readxl
#' @import here

getADAPARCBaseToSourceVariableMap <- function(years) {

  # TODO: Check for errors--for example, need unique ada_parc_var_names and unique source_var_names for each year (i.e. no repeats within a specified year)

  read_base_to_source_map_for_year <- function(year) {

    sheet_name <- as.character(year)

    df <- readxl::read_xlsx(here::here("analysis", "data", "ada_parc_base_variables_mapped_to_source_variables.xlsx"),
                      sheet = sheet_name) %>%
      dplyr::mutate(year = as.numeric(year))
  }


  ada_parc_base_to_source_variable_map <-
    map(years,
        ~ read_base_to_source_map_for_year(.x)) %>%
    dplyr::bind_rows()

  return(ada_parc_base_to_source_variable_map)
}
