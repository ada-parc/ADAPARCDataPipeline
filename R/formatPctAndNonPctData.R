#' formatPctAndNonPctData
#'
#' Prepare data for analysis:
#' - Keep ID columns as character
#' - Ensure all other columns are numeric
#' - Percent columns are identified by containing "pct" (anywhere, case-insensitive)
#' - Percent columns are already in percent units (NO scaling)
#'
#' @param df dataframe that is not yet formatted
#'
#' @return formatted df
#'
#' @import dplyr
formatPctAndNonPctData <- function(df) {

  stopifnot(is.data.frame(df))

  # ID columns to preserve as character if present
  id_cols <- intersect(c("GEOID", "ABBR", "NAME"), names(df))

  # Identify percent columns by name (contain "pct" anywhere)
  pct_cols <- grep("(?i)pct", names(df), value = TRUE, perl = TRUE)

  # All non-ID columns should be numeric
  numeric_candidate_cols <- setdiff(names(df), id_cols)

  # Helper to safely coerce to numeric
  coerce_numeric <- function(x) {
    if (is.numeric(x)) return(x)

    x_chr <- as.character(x)
    x_chr[x_chr == ""] <- NA_character_

    # Strip common formatting artifacts if they exist
    x_chr <- gsub(",", "", x_chr, fixed = TRUE)
    x_chr <- gsub("%", "", x_chr, fixed = TRUE)
    x_chr <- trimws(x_chr)

    suppressWarnings(as.numeric(x_chr))
  }

  formatted_df <- df %>%
    # Coerce all non-ID columns to numeric
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_candidate_cols),
        ~ coerce_numeric(.x)
      )
    ) %>%
    # Enforce ID columns as character
    dplyr::mutate(
      dplyr::across(dplyr::all_of(id_cols), as.character)
    ) %>%
    # Explicitly enforce pct columns as numeric (already percent units)
    dplyr::mutate(
      dplyr::across(dplyr::all_of(intersect(pct_cols, names(.))), as.numeric)
    )

  return(formatted_df)
}
