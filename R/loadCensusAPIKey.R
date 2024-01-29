#' loadCensusAPIKey
#'
#' Load Census API Key
#'
#' This allows for US Census Data to be downloaded. Here, we assume that someone already has a key present and we simply load it.
#'
#'
#' @returns Census API Key
#'

loadCensusAPIKey <- function() {

  readRenviron("~/.Renviron")
  census_key <- Sys.getenv("CENSUS_API_KEY")

  census_key
}
