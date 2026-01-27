#' transformRawVariablesToADAPARCBaseVariables
#'
#' Transform Raw Variables to ADA-PARC Base Variables. This function assumes
#' `GEOID`, `NAME`, `ABBR`, `year` as column names.
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

  norm_source <- function(x) tolower(stringr::str_squish(stringr::str_replace_all(x, "\u00A0", " ")))

  # Normalize ACS variable codes:
  # - remove NBSP
  # - trim/squish whitespace
  # - drop trailing E/M (because your raw names are like B18135_013 not B18135_013E)
  norm_code <- function(x) {
    x %>%
      stringr::str_replace_all("\u00A0", " ") %>%
      stringr::str_squish() %>%
      sub("(E|M)$", "", .)
  }

  years_in_data <- unique(raw_data$year)

  # Build a lookup of raw column names -> normalized code
  raw_name_map <- tibble::tibble(
    raw_name = names(raw_data),
    raw_code_norm = norm_code(names(raw_data))
  )

  # Normalize map once
  map_norm <- ada_parc_base_to_source_var_map %>%
    dplyr::mutate(
      source_norm = norm_source(.data$source),
      source_var_code_norm = norm_code(.data$source_var_code),
      ada_parc_var_code_norm = stringr::str_squish(stringr::str_replace_all(.data$ada_parc_var_code, "\u00A0", " "))
    )

  getBaseVariablesForYear <- function(year_for_filter) {

    in_scope_map <- map_norm %>%
      dplyr::filter(
        .data$source_norm == norm_source(source),
        .data$year == year_for_filter,
        .data$source_var_code_norm %in% raw_name_map$raw_code_norm
      )

    if (nrow(in_scope_map) == 0) {
      # Helpful debug: show a few map codes and raw codes
      example_map <- head(map_norm %>% dplyr::filter(.data$source_norm == norm_source(source), .data$year == year_for_filter) %>% dplyr::pull(.data$source_var_code_norm), 25)
      example_raw <- head(raw_name_map$raw_code_norm, 25)

      rlang::abort(paste0(
        "No variables matched for source='", source, "', year=", year_for_filter, ".\n",
        "Example normalized map codes: ", paste(example_map, collapse = ", "), "\n",
        "Example normalized raw codes: ", paste(example_raw, collapse = ", ")
      ))
    }

    # Map from normalized code -> actual raw column name
    raw_cols <- raw_name_map$raw_name[match(in_scope_map$source_var_code_norm, raw_name_map$raw_code_norm)]

    # Named lookup: raw column name -> ADA-PARC base name
    rename_lookup <- stats::setNames(in_scope_map$ada_parc_var_code_norm, raw_cols)

    raw_data %>%
      dplyr::filter(.data$year == year_for_filter) %>%
      dplyr::select(
        GEOID,
        NAME,
        dplyr::any_of(c("ABBR")),
        year,
        tidyselect::all_of(raw_cols)
      ) %>%
      dplyr::rename_with(
        .fn = function(nms) unname(rename_lookup[nms]),
        .cols = tidyselect::all_of(raw_cols)
      )
  }

  purrr::map(years_in_data, getBaseVariablesForYear) %>%
    dplyr::bind_rows()
}
