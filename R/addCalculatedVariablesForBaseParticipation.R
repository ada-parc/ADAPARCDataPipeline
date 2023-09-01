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
        pop_total = S1810_C01_001_estimate,
        # PWD
        pwd_total = S1810_C02_001_estimate,
        pwd_pct = pwd_total / pop_total,
        # PWOD
        pwod_total = pop_total - pwd_total,
        pwod_pct = pwod_total / pop_total,

        ### ----- CP. Health Insurance -----

        ### ----- CP. Insured/Uninsured, 19-64 -----
        pop_19_64 = B18135_013_estimate,
        pwd_19_64 = B18135_014_estimate,
        pwd_19_64_insured = B18135_015_estimate,
        pwd_19_64_insured_private = B18135_016_estimate,
        pwd_19_64_insured_public = B18135_017_estimate,
        pwd_19_64_uninsured = B18135_018_estimate,

        pwod_19_64 = B18135_019_estimate,
        pwod_19_64_insured = B18135_020_estimate,
        pwod_19_64_insured_private = B18135_021_estimate,
        pwod_19_64_insured_public = B18135_022_estimate,
        pwod_19_64_uninsured = B18135_023_estimate,

        ### ----- CP. Insured/Uninsured, 65+ -----
        pop_grtoeq_65 = B18135_024_estimate,

        pwd_grtoeq_65 = B18135_025_estimate,
        pwd_grtoeq_65_insured = B18135_026_estimate,
        pwd_grtoeq_65_insured_private = B18135_027_estimate,
        pwd_grtoeq_65_insured_public = B18135_028_estimate,
        pwd_grtoeq_65_uninsured = B18135_029_estimate,

        pwod_grtoeq_65 = B18135_030_estimate,
        pwod_grtoeq_65_insured = B18135_031_estimate,
        pwod_grtoeq_65_insured_private = B18135_032_estimate,
        pwod_grtoeq_65_insured_public = B18135_033_estimate,
        pwod_grtoeq_65_uninsured = B18135_034_estimate,

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
        pop_commute_public_pct = S1811_C01_035_estimate / 100,
        pop_commute_public = pop_commute_public_pct * S1811_C01_032_estimate,
        # PWD
        pwd_commute_public_pct = S1811_C02_035_estimate / 100,
        pwd_commute_public = pwd_commute_public_pct * S1811_C02_032_estimate,
        # PWOD
        pwod_commute_public_pct = S1811_C03_035_estimate / 100,
        pwod_commute_public = pwod_commute_public_pct * S1811_C03_032_estimate,
        ## Private Car
        # Population
        pop_commute_car_alone_pct = S1811_C01_033_estimate / 100,
        pop_commute_car_alone = pop_commute_car_alone_pct * S1811_C01_032_estimate,
        # PWD
        pwd_commute_car_alone_pct = S1811_C02_033_estimate / 100,
        pwd_commute_car_alone = pwd_commute_car_alone_pct * S1811_C02_032_estimate,
        # PWOD
        pwod_commute_car_alone_pct = S1811_C03_033_estimate / 100,
        pwod_commute_car_alone = pwod_commute_car_alone_pct * S1811_C03_032_estimate,

        ### ----- CP. Educational Attainment -----
        ### Percents
        pwd_lessthan_highschool_pct = S1811_C02_040_estimate / 100,
        pwod_lessthan_highschool_pct = S1811_C03_040_estimate / 100,
        pwd_highschoolequiv_pct = S1811_C02_041_estimate / 100,
        pwod_highschoolequiv_pct = S1811_C03_041_estimate / 100,
        pwd_degree_aa_pct = S1811_C02_042_estimate / 100,
        pwod_degree_aa_pct = S1811_C03_042_estimate / 100,
        pwd_degree_grtoeq_ba_pct = S1811_C02_043_estimate / 100,
        pwod_degree_grtoeq_ba_pct = S1811_C03_043_estimate / 100,
        ### Values (base below named "pwd_pop_educ" in map)
        pwd_lessthan_highschool = pwd_lessthan_highschool_pct * S1811_C02_039_estimate,
        pwod_lessthan_highschool = pwod_lessthan_highschool_pct * S1811_C03_039_estimate,
        pwd_highschoolequiv = pwd_highschoolequiv_pct * S1811_C02_039_estimate,
        pwod_highschoolequiv = pwod_highschoolequiv_pct * S1811_C03_039_estimate,
        pwd_degree_aa = pwd_degree_aa_pct * S1811_C02_039_estimate,
        pwod_degree_aa = pwod_degree_aa_pct * S1811_C03_039_estimate,
        pwd_degree_grtoeq_ba = pwd_degree_grtoeq_ba_pct * S1811_C02_039_estimate,
        pwod_degree_grtoeq_ba = pwod_degree_grtoeq_ba_pct * S1811_C03_039_estimate
      ) %>%
      mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))

return(df)
}


  calculated_and_base_data <-
    map(years, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
  }
