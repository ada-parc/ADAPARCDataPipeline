#' addCalculatedVariablesForBaseParticipation
#'
#' Add Calculated Variables To Base Data for Participation
#'
#' @param base_data dataframe with ADA-PARC base variables
#'
#' @returns a DF with ADA-PARC base and calculated variables
#'
#' @import purrr
#' @import dplyr

addCalculatedVariablesForBaseParticipation <- function(base_data) {

  years_in_data <- unique(base_data$year)

  generate_calculated_variables_per_year <- function(year_for_filter) {

    # While we aren't taking advantage of this structure right now, if/when there are different calculations for variables across different years, we will be able to create additional sub-functions and use for the mutation in that specific year. For example, the ACS changed how it calculates certain variables around disability before 2012; if we are able to determine which base variables from 2012 we can use to calculate an equivalent to a >2012 variable, we will be able to compare across those time periods.

    df <- base_data %>%
      dplyr::filter(year == year_for_filter) %>%
      dplyr::mutate(
        ### ID
        GEOID = GEOID,
        NAME = NAME,
        ABBR = ABBR,

        ### ----- CP. Pop, PWD, PWOD -----
        # Pop
        pop_total = pop_total,
        # PWD
        pwd_total = pwd_total,
        pwd_pct = pwd_total / pop_total,
        # PWOD
        pwod_total = pop_total - pwd_total,
        pwod_pct = pwod_total / pop_total,

        ### ----- CP. Health Insurance -----

        ### ----- CP. Insured/Uninsured, 19-64 -----
        pop_19_64 = pop_19_64,
        pwd_19_64 = pwd_19_64,
        pwd_19_64_insured = pwd_19_64_insured,
        pwd_19_64_insured_private = pwd_19_64_insured_private,
        pwd_19_64_insured_public = pwd_19_64_insured_public,
        pwd_19_64_uninsured = pwd_19_64_uninsured,

        pwod_19_64 = pwod_19_64,
        pwod_19_64_insured = pwod_19_64_insured,
        pwod_19_64_insured_private = pwod_19_64_insured_private,
        pwod_19_64_insured_public = pwod_19_64_insured_public,
        pwod_19_64_uninsured = pwod_19_64_uninsured,

        ### ----- CP. Insured/Uninsured, 65+ -----
        pop_grtoeq_65 = pop_grtoeq_65,

        pwd_grtoeq_65 = pwd_grtoeq_65,
        pwd_grtoeq_65_insured = pwd_grtoeq_65_insured,
        pwd_grtoeq_65_insured_private = pwd_grtoeq_65_insured_private,
        pwd_grtoeq_65_insured_public = pwd_grtoeq_65_insured_public,
        pwd_grtoeq_65_uninsured = pwd_grtoeq_65_uninsured,

        pwod_grtoeq_65 = pwod_grtoeq_65,
        pwod_grtoeq_65_insured = pwod_grtoeq_65_insured,
        pwod_grtoeq_65_insured_private = pwod_grtoeq_65_insured_private,
        pwod_grtoeq_65_insured_public = pwod_grtoeq_65_insured_public,
        pwod_grtoeq_65_uninsured = pwod_grtoeq_65_uninsured,

        ### ----- CP. Insurance, Percents -----
        pwd_19_64_insured_pct = pwd_19_64_insured / pwd_19_64,
        pwd_19_64_uninsured_pct = pwd_19_64_uninsured / pwd_19_64,
        pwod_19_64_insured_pct = pwod_19_64_insured / pwod_19_64 ,
        pwod_19_64_uninsured_pct = pwod_19_64_uninsured / pwod_19_64 ,
        pwd_grtoeq_65_insured_pct = pwd_grtoeq_65_insured / pwd_grtoeq_65,
        pwd_grtoeq_65_uninsured_pct = pwd_grtoeq_65_uninsured / pwd_grtoeq_65,
        pwod_grtoeq_65_insured_pct = pwod_grtoeq_65_insured / pwod_grtoeq_65,
        pwod_grtoeq_65_uninsured_pct = pwod_grtoeq_65_uninsured / pwod_grtoeq_65,

        ### ----- CP. Insurance, Public -----
        pwd_19_64_insured_public_pct = pwd_19_64_insured_public / pwd_19_64,
        pwod_19_64_insured_public_pct = pwod_19_64_insured_public / pwod_19_64,
        pwd_grtoeq_65_insured_public_pct = pwd_grtoeq_65_insured_public / pwd_grtoeq_65,
        pwod_grtoeq_65_insured_public_pct = pwod_grtoeq_65_insured_public / pwod_grtoeq_65,

        ### ----- CP. Insurance, Private -----
        pwd_19_64_insured_private_pct = pwd_19_64_insured_private / pwd_19_64,
        pwod_19_64_insured_private_pct = pwod_19_64_insured_private / pwod_19_64,
        pwd_grtoeq_65_insured_private_pct = pwd_grtoeq_65_insured_private / pwd_grtoeq_65,
        pwod_grtoeq_65_insured_private_pct = pwod_grtoeq_65_insured_private / pwod_grtoeq_65,

        ### ----- CP. Commute to Work -----
        ## Transit
        # Population
        pop_commute_public_pct = pop_commute_public_pct / 100,
        pop_commute_public = pop_commute_public_pct * pop_total_commute,
        # PWD
        pwd_commute_public_pct = pwd_commute_public_pct / 100,
        pwd_commute_public = pwd_commute_public_pct * pwd_total_commute,
        # PWOD
        pwod_commute_public_pct = pwod_commute_public_pct / 100,
        pwod_commute_public = pwod_commute_public_pct * pwod_total_commute,
        ## Private Car
        # Population
        pop_commute_car_alone_pct = pop_commute_car_alone_pct / 100,
        pop_commute_car_alone = pop_commute_car_alone_pct * pop_total_commute,
        # PWD
        pwd_commute_car_alone_pct = pwd_commute_car_alone_pct / 100,
        pwd_commute_car_alone = pwd_commute_car_alone_pct * pwd_total_commute,
        # PWOD
        pwod_commute_car_alone_pct = pwod_commute_car_alone_pct / 100,
        pwod_commute_car_alone = pwod_commute_car_alone_pct * pwod_total_commute,

        ### ----- CP. Educational Attainment -----
        ### Percents
        pwd_lessthan_highschool_pct = pwd_lessthan_highschool_pct / 100,
        pwod_lessthan_highschool_pct = pwod_lessthan_highschool_pct / 100,
        pwd_highschoolequiv_pct = pwd_highschoolequiv_pct / 100,
        pwod_highschoolequiv_pct = pwod_highschoolequiv_pct / 100,
        pwd_degree_aa_pct = pwd_degree_aa_pct / 100,
        pwod_degree_aa_pct = pwod_degree_aa_pct / 100,
        pwd_degree_grtoeq_ba_pct = pwd_degree_grtoeq_ba_pct / 100,
        pwod_degree_grtoeq_ba_pct = pwod_degree_grtoeq_ba_pct / 100,
        ### Values (base below named "pwd_pop_educ" in map)
        pwd_lessthan_highschool = pwd_lessthan_highschool_pct * pwd_pop_educ,
        pwod_lessthan_highschool = pwod_lessthan_highschool_pct * pwod_pop_educ,
        pwd_highschoolequiv = pwd_highschoolequiv_pct * pwd_pop_educ,
        pwod_highschoolequiv = pwod_highschoolequiv_pct * pwod_pop_educ,
        pwd_degree_aa = pwd_degree_aa_pct * pwd_pop_educ,
        pwod_degree_aa = pwod_degree_aa_pct * pwod_pop_educ,
        pwd_degree_grtoeq_ba = pwd_degree_grtoeq_ba_pct * pwd_pop_educ,
        pwod_degree_grtoeq_ba = pwod_degree_grtoeq_ba_pct * pwod_pop_educ
      ) %>%
      formatPctAndNonPctData(.)

return(df)
}


  calculated_and_base_data <-
    map(years, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
  }
