#' addCalculatedVariablesForBaseLiving
#'
#' Add Calculated Variables To Base Data for Living
#'
#' @param base_data dataframe with ADA-PARC base variables
#'
#' @returns a DF with ADA-PARC base and calculated variables
#'
#' @import purrr
#' @import dplyr

addCalculatedVariablesForBaseLiving <- function(base_data) {

  years_in_data <- unique(base_data$year)

  generate_calculated_variables_per_year <- function(year_for_filter) {

    # While we aren't taking advantage of this structure right now, if/when there are different calculations for variables across different years, we will be able to create additional sub-functions and use for the mutation in that specific year. For example, the ACS changed how it calculates certain variables around disability before 2012; if we are able to determine which base variables from 2012 we can use to calculate an equivalent to a >2012 variable, we will be able to compare across those time periods.

    df <-
      base_data %>%
      dplyr::filter(year == year_for_filter) %>%
      dplyr::mutate(
        ### ID
        GEOID = GEOID,
        NAME = NAME,
        ABBR = ABBR,
        year = year,
        ### ----- CL. Pop, PWD, PWOD -----
        # Must use table S2601A since it is total population
        # Rather than civilian noninstitutionalized as is ACS default
        pop_total_grpquarters = pop_total_grpquarters,
        pwd_pct = pwd_pct / 100,
        pwd_total_grpquarters = round(pop_total_grpquarters * pwd_pct, 0),
        pwod_total = pop_total_grpquarters - pwd_total_grpquarters,

        ### ----- CL. Group Quarters -----
        # ***NOTE: Group quarters sometimes uses a different universe for calculating percentages.
        # E.g. pop_grpquarters_institution_pwd_pct (S2601A_C03_047_estimate)
        # Universe is number of people living in institution, NOT PWD
        pop_grpquarters_institution_pwd_pct = pop_grpquarters_institution_pwd_pct / 100,
        pop_grpquarters_institution_pwod_pct = (1 - pop_grpquarters_institution_pwd_pct),
        pop_grpquarters_noninstitution_pwd_pct = pop_grpquarters_noninstitution_pwd_pct / 100,
        pop_grpquarters_noninstitution_pwod_pct = (1 - pop_grpquarters_noninstitution_pwd_pct),

        # Front end group quarters variables
        pop_grpquarters = pop_grpquarters,
        pwd_grpquarters_pct = pwd_grpquarters_pct / 100,
        # Percentages supplied by ACS are whole numbers
        grpquarters_pct = pop_grpquarters / pop_total_grpquarters,

        # ----- CL. Institution -----
        pop_grpquarters_institution = pop_grpquarters_institution,
        ### PWD
        pwd_grpquarters_institution = round(
          pop_grpquarters_institution * pop_grpquarters_institution_pwd_pct,
          0
        ),
        pwd_grpquarters_institution_pct = pwd_grpquarters_institution / pop_total_grpquarters,
        ### PWOD
        pwod_grpquarters_institution = pop_grpquarters_institution - pwd_grpquarters_institution,
        pwod_grpquarters_institution_pct = pwod_grpquarters_institution / pop_total_grpquarters,

        # ----- CL. Non-Institution -----
        pop_grpquarters_noninstitution = pop_grpquarters_noninstitution,
        ### PWD
        pwd_grpquarters_noninstitution = round(
          pop_grpquarters_noninstitution * pop_grpquarters_noninstitution_pwd_pct,
          0
        ),
        pwd_grpquarters_noninstitution_pct = pwd_grpquarters_noninstitution / pop_total_grpquarters,
        ### PWOD
        pwod_grpquarters_noninstitution = pop_grpquarters_noninstitution - pwd_grpquarters_noninstitution,
        pwod_grpquarters_noninstitution_pct = pwod_grpquarters_noninstitution / pop_total_grpquarters,

        # ----- CL. Home -----
        ### PWD
        pwd_home_pct = (
          pwd_total - pwd_grpquarters_institution - pwd_grpquarters_noninstitution
        ) / pwd_total,
        pwd_home = round((pwd_total * pwd_home_pct), 2),
        ### PWOD
        pwod_home_pct = (
          pwod_total - pwod_grpquarters_institution - pwod_grpquarters_noninstitution
        ) / pop_total_grpquarters,
        pwod_home = round((pwod_total * pwod_home_pct), 2),

        ### ----- CL. Nursing Homes -----
        ### Pop 18-64, PWD, PWOD
        ### NOTE: JANE UPDATED THESE VAR NAMES IN THE DOCUMENT TO BE UNIQUE, WITH pop_grpqrters_18_64 and pwd_grpqrters_18_64
        pwd_grpqrters_18_64_pct = pwd_grpqrters_18_64 / 100,
        pwd_grpqrters_18_64 = round(pop_grpqrters_18_64 * pwd_18_64_pct, 0),
        pwod_grpqrters_18_64_pct = pwod_nursing_18_64_pct / 100,
        pwod_grpqrters_18_64 = round(pop_grpqrters_18_64 * pwod_grpqrters_18_64_pct, 0),
        ### PWD
        pwd_nursing_18_64 = round(pop_nursing_18_64 *
                                    (pwd_nursing_18_64_pct / 100), 0),
        pwd_nursing_18_64_pct = pwd_nursing_18_64 / pwd_grpqrters_18_64,
        ### PWOD
        pwod_nursing_18_64 = round(pop_nursing_18_64 *
                                     (pwod_nursing_18_64 / 100), 0),
        pwod_nursing_18_64_pct = pwod_nursing_18_64 / pwod_grpqrters_18_64,

        ### ----- CL. Incarcerated Persons -----
        ### PWD
        pwd_corrections = pwd_corrections,
        pwd_corrections_pct = pwd_corrections / pwd_grpquarters,
        ### PWOD
        pwod_corrections = pwod_corrections,
        pwod_corrections_pct = pwod_corrections / pwod_grpquarters,
        .keep = "none"
      ) %>%
      formatPctAndNonPctData(.)

    return(df)
  }


  calculated_and_base_data <-
    map(years, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
}
