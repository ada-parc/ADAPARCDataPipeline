#' transformRawVariablesToADAPARCBaseVariables
#'
#' Transform Raw Variables to ADA-PARC Base Variables. This function assumes `GEOID`, `NAME`, `ABBR`, `year`as column names.
#'
#' @param ada_parc_base_to_source_var_map dataframe with ADA-PARC base variables mapped to source variables
#' @param raw_data dataframe with raw data from source
#' @param source source of data (ACS, HUD, etc.)
#'
#' @returns a dataframe with ADA-PARC base data across all years
#'
#' @import dplyr
#' @import tidyselect

transformRawVariablesToADAPARCBaseVariables <- function(ada_parc_base_to_source_var_map, raw_data, source) {

  # For accuracy of filtering base data, we want to pull the accurate mapping for the year and source in question. This has an additional benefit of making further calculations more efficient, as we will drop columns from our dataset that are not in scope.


  years_in_data <- unique(raw_data$year)

  getBaseVariablesForYear <- function(year_for_filter) {
    in_scope_base_to_source_variables_map <-
      ada_parc_base_to_source_var_map %>%
      dplyr::filter(source == source,
                    year == year_for_filter,
                    source_var_code %in% names(raw_data))


    ada_parc_base_data <-

      raw_data %>%
      filter(year == year_for_filter) %>%
      dplyr::select(
        GEOID,
        NAME,
        dplyr::any_of(c("ABBR")),
        year,
        tidyselect::all_of(in_scope_base_to_source_variables_map$source_var_code)
      ) %>%
      dplyr::rename_with(
        ~ ifelse(
          .x %in% in_scope_base_to_source_variables_map$source_var_code,
          in_scope_base_to_source_variables_map$ada_parc_var_code[which(in_scope_base_to_source_variables_map$source_var_code == .x)],
          .x
        ),
        .cols = c(in_scope_base_to_source_variables_map$source_var_code)
      )
  }

  ada_parc_base_data_all_years <-
    map(years_in_data, ~ getBaseVariablesForYear(.x)) %>%
    dplyr::bind_rows()

  return(ada_parc_base_data_all_years)
}
