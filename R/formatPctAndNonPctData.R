#' formatPctAndNonPctData
#'
#' Format percent and non-percent data consistently across all data
#'
#' @param df dataframe that is not yet formatted
#'
#' @return formatted df
#'
#' @import dplyr
formatPctAndNonPctData <- function(df) {

  formatted_df <- df %>%
    dplyr::mutate(dplyr::across(where(is.numeric) & !year & !dplyr::ends_with("_pct"),
                                ~ scales::comma(.))) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_pct"),
                                ~ as.numeric(.))) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_pct"),
                                ~ round(.x * 100, 2)))

  return(formatted_df)
}
