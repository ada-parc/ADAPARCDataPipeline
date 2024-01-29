#' getNationalHudForMultipleYears
#'
#' Get HUD data for multiple years specified.
#'
#' @param years the years of data to retrieve
#'
#' @returns A dataframe of country-wide ACS data, in wide format, for both US and state-levels.
#'
#' @import here
#' @import dplyr
#' @import purrr
#' @import httr
#' @import readxl

getNationalHudForMultipleYears <- function(years) {

  downloadSingleYearOfData <- function(year) {

    # Specify the URL to the Excel file. NOTE: This has been tested for 2021 and 2022. Not sure exactly how many years for which this URL will be stable.
    file_url <-
      paste0("https://www.huduser.gov/portal/datasets/pictures/files/STATE_",
             year,
             ".xlsx")

    # Use GET from httr to handle the request and save the file temporarily
    httr::GET(file_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")), progress())

    # Define the column names to keep
    columns_to_keep <- c(
      "States",
      "program_label",
      "pct_disabled_lt62",
      "pct_disabled_ge62",
      "pct_lt24_head",
      "pct_age25_50",
      "pct_age51_61",
      "pct_age62plus"
    )

    # Read the first sheet of the Excel file directly from the temporary file
    data <- readxl::read_excel(tf, sheet = 1) %>%
      dplyr::select(all_of(columns_to_keep)) %>%
      dplyr::mutate(year = as.numeric(year),
        across(starts_with("pct_"), ~ ifelse(. < 0, NA, . / 100)))# Function to replace negative values with NA and convert to proportion


    # Clean up by removing the temporary file
    unlink(tf)

    return(data)
  }


  full_country_data_all_years <-
    purrr::map(years,
               ~ downloadSingleYearOfData(.x)) %>%
    dplyr::bind_rows()

  return(full_country_data_all_years)
}
