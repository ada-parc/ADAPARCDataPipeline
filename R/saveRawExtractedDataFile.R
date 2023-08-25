#' saveRawExtractedDataFile
#'
#' Save raw Extracted Data Files as Rds. Why Rds? W"You can save an R object like a data frame as either an RData file or an RDS file. RData files can store multiple R objects at once, but RDS files are the better choice because they foster reproducible code...For example, if you have three R objects, `a`, `b`, and `c`, you could save them all in the same RData file and then reload them in another R session...However, if you forget the names of your objects or give your file to someone else to use, it will be difficult to determine what was in the file—even after you (or they) load it. The user interface for RDS files is much more clear. You can save only one object per file, and whoever loads it can decide what they want to call their new data. As a bonus, you don’t have to worry about `load` overwriting any R objects that happened to have the same name as the objects you are loading:" For more information, read "D.4.1 Saving R Files" in  https://rstudio-education.github.io/hopr/dataio.html
#'
#'
#' @param raw_extracted_data raw data extracted from an external source (not transformed). Must be an R object.
#' @param name name for the file
#'
#'
#' @import here

saveRawExtractedDataFile <- function(raw_extracted_data, name) {

  folder_path <-
    here::here("analysis", "data", "raw")
  full_file_name <- paste0(folder_path, "/", name, ".Rds")

  # Check if the folder exists
  if (!file.exists(folder_path)) {
    # If it doesn't exist, create the folder
    #
    dir.create(folder_path)
  }

  saveRDS(raw_extracted_data, full_file_name)

}
