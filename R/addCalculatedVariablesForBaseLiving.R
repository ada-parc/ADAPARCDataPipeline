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

    df <- base_data %>%
      dplyr::filter(year == year_for_filter) %>%
      dplyr::mutate(
        base_data %>%
          mutate(
            ### ID
            GEOID = GEOID,
            NAME = NAME,
            ABBR = ABBR,
            year = year,
            ### ----- CL. Pop, PWD, PWOD -----
            # Must use table S2601A since it is total population
            # Rather than civilian noninstitutionalized as is ACS default
            pop_total = S2601A_C01_001_estimate,
            pwd_pct = S2601A_C01_047_estimate / 100,
            pwd_total = round(pop_total * pwd_pct, 0),
            pwod_total = pop_total - pwd_total,

            ### ----- CL. Group Quarters -----
            # ***NOTE: Group quarters sometimes uses a different universe for calculating percentages.
            # E.g. pop_grpquarters_institution_pwd_pct (S2601A_C03_047_estimate)
            # Universe is number of people living in institution, NOT PWD
            pop_grpquarters_institution_pwd_pct = S2601A_C03_047_estimate / 100,
            pop_grpquarters_institution_pwod_pct = (1 - pop_grpquarters_institution_pwd_pct),
            pop_grpquarters_noninstitution_pwd_pct = S2601A_C04_047_estimate / 100,
            pop_grpquarters_noninstitution_pwod_pct = (1 - pop_grpquarters_noninstitution_pwd_pct),

            # Front end group quarters variables
            pop_grpquarters = S2601A_C02_001_estimate,
            pwd_grpquarters_pct = S2601A_C02_047_estimate / 100,  # Percentages supplied by ACS are whole numbers
            grpquarters_pct = pop_grpquarters / pop_total,

            # ----- CL. Institution -----
            pop_grpquarters_institution = S2601A_C03_001_estimate,
            ### PWD
            pwd_grpquarters_institution = round(pop_grpquarters_institution * pop_grpquarters_institution_pwd_pct,
                                                0),
            pwd_grpquarters_institution_pct = pwd_grpquarters_institution / pop_total,
            ### PWOD
            pwod_grpquarters_institution = pop_grpquarters_institution - pwd_grpquarters_institution,
            pwod_grpquarters_institution_pct = pwod_grpquarters_institution / pop_total,

            # ----- CL. Non-Institution -----
            pop_grpquarters_noninstitution = S2601A_C04_001_estimate,
            ### PWD
            pwd_grpquarters_noninstitution = round(pop_grpquarters_noninstitution * pop_grpquarters_noninstitution_pwd_pct, 0),
            pwd_grpquarters_noninstitution_pct = pwd_grpquarters_noninstitution / pop_total,
            ### PWOD
            pwod_grpquarters_noninstitution = pop_grpquarters_noninstitution - pwd_grpquarters_noninstitution,
            pwod_grpquarters_noninstitution_pct = pwod_grpquarters_noninstitution / pop_total,

            # ----- CL. Home -----
            ### PWD
            pwd_home_pct = (pwd_total - pwd_grpquarters_institution - pwd_grpquarters_noninstitution) / pop_total,
            pwd_home = round((pwd_total * pwd_home_pct), 2),
            ### PWOD
            pwod_home_pct = (pwod_total - pwod_grpquarters_institution - pwod_grpquarters_noninstitution) / pop_total,
            pwod_home = round((pwod_total * pwod_home_pct), 2),

            ### ----- CL. Nursing Homes -----
            ### Pop 18-64, PWD, PWOD
            ### NOTE: JANE UPDATED THESE VAR NAMES IN THE DOCUMENT TO BE UNIQUE, WITH pop_grpqrters_18_64 and pwd_grpqrters_18_64
            pop_18_64 = S2602_C01_047_estimate,
            pwd_18_64_pct = S2602_C01_048_estimate / 100,
            pwd_18_64 = round(pop_18_64 * pwd_18_64_pct, 0),
            pwod_18_64_pct = S2602_C01_049_estimate / 100,
            pwod_18_64 = round(pop_18_64 * pwod_18_64_pct, 0),
            ### PWD
            pwd_nursing_18_64 = round(S2602_C04_047_estimate *
                                        (S2602_C04_048_estimate / 100), 0),
            pwd_nursing_18_64_pct = pwd_nursing_18_64 / pwd_18_64,
            ### PWOD
            pwod_nursing_18_64 = round(S2602_C04_047_estimate *
                                         (S2602_C04_049_estimate / 100), 0),
            pwod_nursing_18_64_pct = pwod_nursing_18_64 / pwod_18_64,

            ### ----- CL. Incarcerated Persons -----
            ### PWD
            ### NOTE: JANE UPDATED THE DENOMINATORS FOR THE PCT'S IN THE MAP FILE TO "pwd_corrections_base_number" AND "pwod_corrections_base_number"
            pwd_corrections = B26108_038_estimate,
            pwd_corrections_pct = pwd_corrections / B26108_002_estimate,
            ### PWOD
            pwod_corrections = B26108_039_estimate,
            pwod_corrections_pct = pwod_corrections / B26108_003_estimate,
            .keep = "none"
          ) %>%
          mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))
      )

    return(df)
  }


  calculated_and_base_data <-
    map(years, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
}
