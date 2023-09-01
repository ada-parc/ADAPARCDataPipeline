#' addCalculatedVariablesToBaseData
#'
#' Add Calculated Variables To Base Data
#'
#' @param ada_parc_base_and_calc_vars_map df mapping ADA_PARC base and calculated variables
#' @param base_data dataframe with ADA-PARC base variables
#'
#' @returns a tibble with ADA-PARC base and calculated variables
#'
#' @import purrr

addCalculatedVariablesToBaseData <- function(ada_parc_base_and_calc_vars_map, base_data) {

  mutated_df <- raw_data
  count <- 0

  mutate_new_df <- function(ada_parc_base_variable) {

    print(ada_parc_base_variable)
    count <<- count + 1

    mutated_df <<- mutated_df %>% add_column({{x}} := count)




    return(mutated_df)
  }


  df_with_ada_parc_base_variables <-
    purrr::map(ada_parc_base_variables, ~ mutate_new_df(.x), .progress = TRUE) %>%
    purrr::reduce(left_join)

  return(df_with_ada_parc_base_variables)

}
