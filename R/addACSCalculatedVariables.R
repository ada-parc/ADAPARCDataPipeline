#' addACSCalculatedVariables
#'
#' Calculate variables and add to dataset, based on ACS variables
#'
#' @param base_data dataframe with ADA-PARC base variables
#'
#' @returns a DF with ADA-PARC base and calculated variables
#'
#' @import purrr
#' @import dplyr

addACSCalculatedVariables <- function(base_data) {

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
        ### ----- D. Pop, PWD, PWOD -----
        # PWD
        pwd_pct = (pwd_total / pop_total),
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
                       pwd_other_other_race # Some other race alone
        ),
        pwd_other_multiple = (pwd_other_americanindian_alaskanative + # American Indian and Alaska Native alone
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
        pwd_other_multiple_dis_pct = pwd_other_multiple / pwd_total,

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

        ### ----- WE. Employment Status -----
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
        pop_total_class_18_64 = pop_total_class_18_64,
        pwd_class_18_64 = pwd_class_18_64,
        pwod_class_18_64 = pwod_class_18_64,
        pwd_below_poverty = pwd_below_poverty,
        pwod_below_poverty = pwod_below_poverty,
        pwd_atorabove_poverty = pwd_atorabove_poverty,
        pwod_atorabove_poverty = pwod_atorabove_poverty,
        pwd_below_poverty_pct = pwd_below_poverty / pwd_class_18_64,
        pwod_below_poverty_pct = pwod_below_poverty / pwod_class_18_64,
        pwd_atorabove_poverty_pct = pwd_atorabove_poverty / pwd_class_18_64,
        pwod_atorabove_poverty_pct = pwod_atorabove_poverty / pwod_class_18_64,

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

        ### ----- CL. Pop, PWD, PWOD -----
        # Must use table S2601A since it is total population
        # Rather than civilian noninstitutionalized as is ACS default
        pop_total_grpquarters = pop_total_grpquarters,
        pwd_pct_grpquarters = pwd_pct / 100,
        pwd_total_grpquarters = round(pop_total_grpquarters * pwd_grpquarters_pct, 0),
        pwod_total_grpquarters = pop_total_grpquarters - pwd_total_grpquarters,

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
          pwd_total_grpquarters - pwd_grpquarters_institution - pwd_grpquarters_noninstitution
        ) / pop_total_grpquarters,
        pwd_home = round((pwd_total_grpquarters * pwd_home_pct), 2),
        ### PWOD
        pwod_home_pct = (
          pwod_total - pwod_grpquarters_institution - pwod_grpquarters_noninstitution
        ) / pop_total_grpquarters,
        pwod_home = round((pwod_total * pwod_home_pct), 2),

        ### ----- CL. Nursing Homes -----
        ### Pop 18-64, PWD, PWOD
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
        ### ----- CP. Health Insurance -----

        ### ----- CP. Insured/Uninsured, 19-64 -----
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

      { if ("pwd_16_plus_subj" %in% colnames(.) &
            "pop_total_employed_16_plus" %in% colnames(.))
        # Other labor data -- this is confusing, but we currently don't pull the correct tables for pwd_not_labor and need this data available; constructing using what we have from the acs5/subject table (this is what has been done historically; we can revisit).
        mutate(.,
               pwd_not_in_labor_force = pwd_16_plus_subj - pop_total_employed_16_plus)
        else .} %>%

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
