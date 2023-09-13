#' intersectGeographiesSplitByState
#'
#' Intersect two SF dataframes that are split by states
#'
#'
#' @param geography_split_by_state_1 a dataframe (type SF) that is split by state
#' @param geography_split_by_state_2 a dataframe (type SF) that is split by state
#'
#' @returns a dataframe that combines
#'
#'
#' @import dplyr

intersectGeographiesSplitByState <- function(geography_split_by_state_1,
                                       geography_split_by_state_2) {

  combined_dfs <-
    dplyr::map(1:52, function(x) {

      message(paste("Running Intersection", x, "of 52"))
      df <-
        sf::st_intersection(geography_split_by_state_1[[x]], geography_split_by_state_2[[x]])
      gc()
      return(df)
    })

  combined_geographies <- combined_dfs %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate("overlap_area" = st_area(.),
           "overlap_area_num" = as.numeric(overlap_area),
           "overlap_pct" = overlap_area_num / place_area_num) %>%
    dplyr::filter(overlap_pct > 0) %>%
    sf::st_drop_geometry()


  return(combined_geographies)
}
