#' intersectGeographiesSplitByState
#'
#' Intersect two SF dataframes that are split by states. They will be intersected, left-right. The two dataframes must be of the same length.
#'
#'
#' @param places_sf a dataframe (type SF) that is split by state
#' @param tracts_sf a dataframe (type SF) that is split by state
#'
#' @returns a dataframe that combines
#'
#'
#' @import dplyr
#' @import purrr
#' @import sf

intersectGeographiesSplitByState <- function(places_sf,
                                       tracts_sf) {

  map_min <- 1
  map_max <- length(places_sf)

  intersect_geographies_by_state <- function(index) {

    message(paste("Running Intersection ", index, " of ", map_max))

    places_sf_state <- places_sf[[index]]
    tract_sf_state <- tracts_sf[[index]]

    # sf::st_intersects is important because it is a relatively efficient operation to determine TRUE/FALSE if geographies overlap. Note that some of these intersections can be two borders that are touching and so we call them "potential"
    potential_intersections <- sf::st_intersects(places_sf_state, tract_sf_state)


    result <-
      data.frame(
        place_GEOID = character(0),
        place_NAME = character(0),
        tract_GEOID = character(0),
        tract_NAME = character(0),
        STATEFP = character(0),
        COUNTYFP = character(0)
      )


    # Iterate through potential intersections, using the places as the starting point. For each place, we look at each census tract that was marked "TRUE" by st_intersects.
    for (i in seq_along(potential_intersections)) {


      place_observation <- places_sf_state[i, ]

      # These are the census tract indices that are potentially in-scope for each place:
      intersection_indices <- potential_intersections[[i]]


      for (j in intersection_indices) {
        tract_observation <- tract_sf_state[j, ]

        # Because the tract may border the place instead of actually overlapping it, we
        overlap_area <-
          as.numeric(sf::st_area(
            sf::st_intersection(place_observation, tract_observation)
          ))

        if (length(overlap_area) > 0 &&
            !is.na(overlap_area) && overlap_area > 0) {


          # We can save overlap area data here, but we don't use it. We save the other columns that we know we will want later and already have access to.
          result <-
            rbind(
              result,
              data.frame(
                place_GEOID = place_observation$place_GEOID,
                place_NAME = place_observation$place_NAME,
                tract_GEOID = tract_observation$tract_GEOID,
                tract_NAME = tract_observation$tract_NAME,
                STATEFP = tract_observation$STATEFP,
                COUNTYFP = tract_observation$COUNTYFP
              )
            )
        }
      }
    }

    return(result)

  }



  combined_geographies <-
    purrr::map(map_min:map_max,
               ~ intersect_geographies_by_state(.x),
               .progress = TRUE) %>%
    purrr::reduce(dplyr::bind_rows)


  return(combined_geographies)
}
