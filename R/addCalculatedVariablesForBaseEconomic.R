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

        ### ----- WE. Pop, PWD, PWOD -----
        # Pop
        pop_total = S1810_C01_001_estimate,
        # PWD
        pwd_total = S1810_C02_001_estimate,
        pwd_pct = pwd_total / pop_total,
        # PWOD
        pwod_total = pop_total - pwd_total,
        pwod_pct = pwod_total / pop_total,

        ### ----- WE. Employment Status -----
        pop_19_64 = B18135_013_estimate,
        # Not the same as the instructions spreadsheet; used this instead to keep calculations in same universe
        pwd_19_64 = B18135_014_estimate,
        pwod_19_64 = pop_19_64 - pwd_19_64,
        pwd_employed = C18120_004_estimate,
        pwod_employed = C18120_005_estimate,
        pwd_unemployed = C18120_007_estimate,
        pwod_unemployed = C18120_008_estimate,
        pwd_notlabor = C18120_010_estimate,
        pwod_notlabor = C18120_011_estimate,
        pwd_employed_pct = pwd_employed / pwd_19_64,
        pwod_employed_pct = pwod_employed / pwod_19_64,
        pwd_unemployed_pct = pwd_unemployed / pwd_19_64,
        pwod_unemployed_pct = pwod_unemployed / pwod_19_64,
        pwd_notlabor_pct = pwd_notlabor / pwd_19_64,
        pwod_notlabor_pct = pwod_notlabor / pwod_19_64,

        ### ----- WE. Poverty Status -----
        ### the first two vars here have been renamed "pop_total_class_18_64" and "pwd_class_18_64"
        ### and "pwod_class_18_64"
        pop_18_64 = C18130_009_estimate,
        pwd_18_64 = C18130_010_estimate,
        pwod_18_64 = C18130_013_estimate,
        pwd_below_poverty = C18130_011_estimate,
        pwod_below_poverty = C18130_014_estimate,
        pwd_atorabove_poverty = C18130_012_estimate,
        pwod_atorabove_poverty = C18130_015_estimate,
        pwd_below_poverty_pct = pwd_below_poverty / pwd_18_64,
        pwod_below_poverty_pct = pwod_below_poverty / pwod_18_64,
        pwd_atorabove_poverty_pct = pwd_atorabove_poverty / pwd_18_64,
        pwod_atorabove_poverty_pct = pwod_atorabove_poverty / pwod_18_64,

        ### ----- WE. Housing Affordability -----
        ### Mortgage
        mortgage_burdened_30_35 = B25091_008_estimate,
        mortgage_burdened_35_40 =  B25091_009_estimate,
        mortgage_burdened_40_50 = B25091_010_estimate,
        mortgage_burdened_grtoeq_50 = B25091_011_estimate,
        mortgage_burdened = mortgage_burdened_30_35 + mortgage_burdened_35_40 + mortgage_burdened_40_50 + mortgage_burdened_grtoeq_50,
        mortgage_burdened_pct = mortgage_burdened / B25091_002_estimate,
        # total_housing_units_mortgage
        ### Rent
        rent_burdened_30_35 = B25070_007_estimate,
        rent_burdened_35_40 = B25070_008_estimate,
        rent_burdened_40_50 = B25070_009_estimate,
        rent_burdened_grtoeq_50 = B25070_010_estimate,
        rent_burdened = rent_burdened_30_35 + rent_burdened_35_40 + rent_burdened_40_50  + rent_burdened_grtoeq_50,
        rent_burdened_pct = rent_burdened / B25070_001_estimate,
        # rent_total

        ### ----- WE. Full/Part Time Workers -----
        ### Values
        pop_fulltime = C18121_002_estimate,
        pwd_fulltime = C18121_003_estimate,
        pwod_fulltime = C18121_004_estimate,
        pop_not_fulltime = C18121_005_estimate,
        pwd_not_fulltime = C18121_006_estimate,
        pwod_not_fulltime = C18121_007_estimate,
        pop_didnotwork = C18121_008_estimate,
        pwd_didnotwork = C18121_009_estimate,
        pwod_didnotwork = C18121_010_estimate,
        ### Percents
        pwd_fulltime_pct = pwd_fulltime / pwd_19_64,
        pwod_fulltime_pct = pwod_fulltime / pwod_19_64,
        pwd_not_fulltime_pct = pwd_not_fulltime / pwd_19_64,
        pwod_not_fulltime_pct = pwod_not_fulltime / pwod_19_64,

        ### ----- WE. Median Income -----
        pwd_grtoeq_16_med_individual_income = B18140_002_estimate,
        pwod_grtoeq_16_med_individual_income = B18140_005_estimate,

        ### ----- WE. Working from Home -----
        # Percentages supplied by ACS are whole numbers, numbers derived
        # Pop
        pop_grtoeq_16_wfh = S1811_C01_038_estimate * S1811_C01_032_estimate,
        pop_grtoeq_16_wfh_pct = S1811_C01_038_estimate / 100,
        # PWD
        pwd_grtoeq_16_wfh = S1811_C02_038_estimate * S1811_C02_032_estimate,
        pwd_grtoeq_16_wfh_pct = S1811_C02_038_estimate / 100,
        # PWOD
        pwod_grtoeq_16_wfh = S1811_C03_038_estimate * S1811_C03_032_estimate,
        pwod_grtoeq_16_wfh_pct = S1811_C03_038_estimate / 100,
        .keep = "none"
      ) %>%
      mutate(across(.cols = ends_with("pct"), .fns = ~ round(.x * 100, 2)))


    return(df)
  }


  calculated_and_base_data <-
    map(years, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
}
