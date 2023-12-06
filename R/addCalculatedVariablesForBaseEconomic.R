#' addCalculatedVariablesForBaseEconomic
#'
#' Add Calculated Variables To Base Data for Economic
#'
#' @param base_data dataframe with ADA-PARC base variables
#'
#' @returns a DF with ADA-PARC base and calculated variables
#'
#' @import purrr
#' @import dplyr

addCalculatedVariablesForBaseEconomic <- function(base_data) {

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
        year = year,
        ### ----- WE. Pop, PWD, PWOD -----
        # Pop
        pop_total = pop_total,
        # PWD
        pwd_total = pwd_total,
        pwd_pct = pwd_total / pop_total,
        # PWOD
        pwod_total = pop_total - pwd_total,
        pwod_pct = pwod_total / pop_total,

        ### ----- WE. Employment Status -----
        pop_19_64 = pop_19_64,
        # Not the same as the instructions spreadsheet; used this instead to keep calculations in same universe
        pwd_19_64 = pwd_19_64,
        pwd_19_64_pct = pwd_19_64 / pop_19_64,
        pwod_19_64 = pop_19_64 - pwd_19_64,
        pwd_employed_subj = pwd_employed_subj,
        pwod_employed = pwod_employed,
        pwd_not_employed_subj = pwd_not_employed_subj,
        pwod_unemployed = pwod_unemployed,
        pwd_notlabor = pwd_notlabor,
        pwod_notlabor = pwod_notlabor,
        pwd_employed_pct = pwd_employed_subj / pwd_19_64,
        pwod_employed_pct = pwod_employed / pwod_19_64,
        pwd_unemployed_pct = pwd_not_employed_subj / pwd_19_64,
        pwod_unemployed_pct = pwod_unemployed / pwod_19_64,
        pwd_notlabor_pct = pwd_notlabor / pwd_19_64,
        pwod_notlabor_pct = pwod_notlabor / pwod_19_64,

        ### ----- WE. Poverty Status -----
        ### the first two vars here have been renamed "pop_total_class_18_64" and "pwd_class_18_64"
        ### and "pwod_class_18_64"
        #TODO: If combining all datasets, need pop_18_64 and pwd_18_64 to get unique names, since they have some confusing similar variables elsewhere
        pop_18_64 = pop_total_class_18_64,
        pwd_18_64 = pwd_class_18_64,
        pwod_18_64 = pwod_class_18_64,
        pwd_below_poverty = pwd_below_poverty,
        pwod_below_poverty = pwod_below_poverty,
        pwd_atorabove_poverty = pwd_atorabove_poverty,
        pwod_atorabove_poverty = pwod_atorabove_poverty,
        pwd_below_poverty_pct = pwd_below_poverty / pwd_18_64,
        pwod_below_poverty_pct = pwod_below_poverty / pwod_18_64,
        pwd_atorabove_poverty_pct = pwd_atorabove_poverty / pwd_18_64,
        pwod_atorabove_poverty_pct = pwod_atorabove_poverty / pwod_18_64,

        ### ----- WE. Housing Affordability -----
        ### Mortgage
        mortgage_burdened_30_35 = mortgage_burdened_30_35,
        mortgage_burdened_35_40 =  mortgage_burdened_35_40,
        mortgage_burdened_40_50 = mortgage_burdened_40_50,
        mortgage_burdened_grtoeq_50 = mortgage_burdened_grtoeq_50,
        mortgage_burdened = mortgage_burdened_30_35 + mortgage_burdened_35_40 + mortgage_burdened_40_50 + mortgage_burdened_grtoeq_50,
        mortgage_burdened_pct = mortgage_burdened / total_housing_units_mortgage,
        # total_housing_units_mortgage
        ### Rent
        rent_burdened_30_35 = rent_burdened_30_35,
        rent_burdened_35_40 = rent_burdened_35_40,
        rent_burdened_40_50 = rent_burdened_40_50,
        rent_burdened_grtoeq_50 = rent_burdened_grtoeq_50,
        rent_burdened = rent_burdened_30_35 + rent_burdened_35_40 + rent_burdened_40_50  + rent_burdened_grtoeq_50,
        rent_burdened_pct = rent_burdened / rent_total,
        # rent_total

        ### ----- WE. Full/Part Time Workers -----
        ### Values
        pop_fulltime = pop_fulltime,
        pwd_fulltime = pwd_fulltime,
        pwod_fulltime = pwod_fulltime,
        pop_not_fulltime = pop_not_fulltime,
        pwd_not_fulltime = pwd_not_fulltime,
        pwod_not_fulltime = pwod_not_fulltime,
        pop_didnotwork = pwod_not_fulltime,
        pwd_didnotwork = pwd_didnotwork,
        pwod_didnotwork = pwod_didnotwork,
        ### Percents
        pwd_fulltime_pct = pwd_fulltime / pwd_19_64,
        pwod_fulltime_pct = pwod_fulltime / pwod_19_64,
        pwd_not_fulltime_pct = pwd_not_fulltime / pwd_19_64,
        pwod_not_fulltime_pct = pwod_not_fulltime / pwod_19_64,

        ### ----- WE. Median Income -----
        pwd_grtoeq_16_med_individual_income = pwd_grtoeq_16_med_individual_income,
        pwod_grtoeq_16_med_individual_income = pwod_grtoeq_16_med_individual_income,

        ### ----- WE. Working from Home -----
        # Percentages supplied by ACS are whole numbers, numbers derived
        # Pop
        pop_grtoeq_16_wfh = pop_grtoeq_16_wfh_pct * pop_total_commute,
        pop_grtoeq_16_wfh_pct = pop_grtoeq_16_wfh_pct / 100,
        # PWD
        pwd_grtoeq_16_wfh = pwd_grtoeq_16_wfh_pct * pwd_total_commute,
        pwd_grtoeq_16_wfh_pct = pwd_grtoeq_16_wfh_pct / 100,
        # PWOD
        pwod_grtoeq_16_wfh = pwod_grtoeq_16_wfh_pct * pwod_total_commute,
        pwod_grtoeq_16_wfh_pct = pwod_grtoeq_16_wfh_pct / 100,
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
