#' addCalculatedVariablesForBaseDemographics
#'
#' Add Calculated Variables To Base Data for Demographics
#'
#' @param base_data dataframe with ADA-PARC base variables
#'
#'
#' @returns a DF with ADA-PARC base and calculated variables
#'
#' @import purrr
#' @import dplyr

addCalculatedVariablesForBaseDemographics <- function(base_data) {

  years_in_data <- unique(base_data$year)

  generate_calculated_variables_per_year <- function(year_for_filter) {

    # While we aren't taking advantage of this structure right now, if/when there are different calculations for variables across different years, we will be able to create additional sub-functions and use for the mutation in that specific year. For example, the ACS changed how it calculates certain variables around disability before 2012; if we are able to determine which base variables from 2012 we can use to calculate an equivalent to a >2012 variable, we will be able to compare across those time periods.

    df <- base_data %>%
      filter(year == year_for_filter) %>%
      dplyr::mutate(
        ### ----- D. Pop, PWD, PWOD -----
        # PWD
        pwd_pct = pwd_total / pop_total,
        # PWOD
        pwod_total = pop_total - pwd_total,
        pwod_pct = pwod_total / pop_total,

        ### ----- D. Age -----
        pop_18_64 = pop_18_64_ages18to34 + pop_18_64_ages35to64,
        pwd_18_64 = pwd_18_64_noninstitutionalized18to34yrs + pwd_18_64_noninstitutionalized35to64,
        pwd_18_64_pct = pwd_18_64 / pop_18_64,
        pop_grtoeq_65 = pop_grtoeq_65_65to74 + pop_grtoeq_65_75yrsplus,
        pwd_grtoeq_65 = pwd_grtoeq_65_65to74 + pwd_grtoeq_65_75yrsplus,
        pwd_grtoeq_65_pct = pwd_grtoeq_65 / pop_grtoeq_65,

        ### ----- D. Race/Ethnicity -----

        pwd_other = (pwd_other_americanindian_alaskanative + # American Indian and Alaska Native alone
          pwd_other_nativehawaiian_otherpacificislander + # Native Hawaiian and Other Pacific Islander alone
          pwd_other_other_race + # Some other race alone
          pwd_multiple # Multiple
          ),
        ### Percents of total population
        pwd_white_pct = pwd_white / pop_total,
        pwd_black_pct = pwd_black / pop_total,
        pwd_hisp_pct = pwd_hisp / pop_total,
        pwd_asian_pct = pwd_asian / pop_total,
        pwd_white_nonhisp_pct = pwd_white_nonhisp / pop_total,
        pwd_other_pct = pwd_other / pop_total,
        pwd_multiple_pct = pwd_multiple / pop_total,

        # Percents of total disabled population
        pwd_white_dis_pct = pwd_white / pwd_total,
        pwd_black_dis_pct = pwd_black / pwd_total,
        pwd_hisp_dis_pct = pwd_hisp / pwd_total,
        pwd_asian_dis_pct = pwd_asian / pwd_total,
        pwd_white_nonhisp_dis_pct = pwd_white_nonhisp / pwd_total,
        pwd_other_dis_pct = pwd_other / pwd_total,
        pwd_multiple_dis_pct = pwd_multiple / pwd_total,

        ### ----- D. Gender -----
        female_pwd_pct = pwd_female / pop_female,
        pwd_female_pct = pwd_female / pwd_total,
        male_pwd_pct = pwd_male / pop_male,
        pwd_male_pct = pwd_male / pwd_total,

        ### ----- D. Type of Disability -----
        pwd_hearing_pct = pwd_hearing / pop_total,
        pwd_vision_pct = pwd_vision / pop_total,
        pwd_cognitive_pct = pwd_cognitive / pop_total,
        pwd_ambulatory_pct = pwd_ambulatory / pop_total,
        pwd_selfcare_pct = pwd_selfcare / pop_total,
        pwd_indliving_pct = pwd_indliving / pop_total,


        # Other labor data -- this is confusing, but we currently don't pull the correct tables for pwd_not_labor and need this data available; constructing using what we have from the acs5/subject table (this is what has been done historically; we can revisit).
        pwd_not_in_labor_force = pwd_16_plus_subj - pop_total_employed_16_plus

        # .keep = "none"
      ) %>%


      ### ---- Commute ------
    { if ("pwd_total_commute" %in% colnames(.)) # This var isn't present in all geographies
      mutate(.,
             pwd_car_commute = pwd_total_commute * ((pwd_commute_car_alone_pct + pwd_commute_carpool) / 100),
             pwd_car_commute_pct = pwd_car_commute / pwd_total_commute,
             pwd_pub_transit = pwd_total_commute * (pwd_commute_public_pct / 100),
             pwd_pub_transit_pct = pwd_commute_public_pct,
             pwd_pub_transit_dis_pct = pwd_pub_transit / pwd_total_commute,
             pwd_walk_bike = pwd_total_commute * ((pwd_commute_walk + pwd_commute_taxi_car_bike_etc) / 100),
             pwd_walk_bike_pct = pwd_walk_bike / pwd_total_commute,
             pwd_wfh = pwd_total_commute * (pwd_grtoeq_16_wfh_pct / 100),
             pwd_wfh_pct = pwd_grtoeq_16_wfh_pct,
             pwd_wfh_dis_pct = pwd_wfh / pwd_total_commute,
        )
      else .} %>%
      formatPctAndNonPctData(.)

    return(df)
  }


  calculated_and_base_data <-
    map(years, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
}
