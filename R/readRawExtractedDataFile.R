#' readRawExtractedDataFile
#'
#' Read raw extracted data file.
#'
#'
#' @param name name for the file
#'
#' @returns the R object that was saved
#'
#'
#' @import here

readRawExtractedDataFile <- function(name) {

  folder_path <-
    here::here("analysis", "data", "raw")

  full_file_name <- paste0(folder_path, "/", name, ".Rds")

  return(readRDS(full_file_name))

}
