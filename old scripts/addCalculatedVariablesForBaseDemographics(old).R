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

    df <- base_data %>%
      filter(year == year_for_filter)

    # Check if "ABBR" exists in the data frame before proceeding with mutation
    if ("ABBR" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(ABBR = ABBR) # This line ensures ABBR is retained
    }

    df <- df %>%
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
        pwd_18_64_pct = pwd_18_64 / pwd_total,
        pop_grtoeq_65 = pop_grtoeq_65_65to74 + pop_grtoeq_65_75yrsplus,
        pwd_grtoeq_65 = pwd_grtoeq_65_65to74 + pwd_grtoeq_65_75yrsplus,
        pwd_grtoeq_65_pct = pwd_grtoeq_65 / pwd_total,

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
        ### Percents of total population /  % OF THE DISABLED POPULATION
        pwd_white_pct = pwd_white  /pwd_total,

        pwd_black_pct = pwd_black / pwd_total,

        pwd_hisp_pct = pwd_hisp / pwd_total,

        pwd_asian_pct = pwd_asian / pwd_total,

        pwd_white_nonhisp_pct = pwd_white_nonhisp / pwd_total,

        pwd_other_pct = pwd_other / pwd_total,

        pwd_multiple_pct = pwd_multiple / pwd_total,

        pwd_other_americanindian_alaskanative_pct = pwd_other_americanindian_alaskanative / pwd_total,

        pwd_other_nativehawaiian_otherpacificislander_pct =  pwd_other_nativehawaiian_otherpacificislander / pwd_total,


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
        pwod_degree_grtoeq_ba = pwod_degree_grtoeq_ba_pct * pwod_pop_educ,

        # NEW!

        # Work / Economic
        ### ----- WE. Employment Status -----
        pwd_19_64_pct = pwd_19_64 / pop_19_64,
        pwod_19_64 = pop_19_64 - pwd_19_64,
        pwd_employed_subj = pwd_employed_subj,
        pwd_not_employed_subj = pwd_not_employed_subj,
       # pwd_employed_pct = pwd_employed / pwd_19_64,
       # pwd_unemployed_pct = pwd_unemployed / pwd_19_64,

        ### ----- WE. Poverty Status -----
        pwd_below_poverty = pwd_below_poverty,
        pwd_atorabove_poverty = pwd_atorabove_poverty,
        pwd_below_poverty_pct = pwd_below_poverty / pwd_class_18_64,
        pwd_atorabove_poverty_pct = pwd_atorabove_poverty / pwd_class_18_64,

       ### ----- CL. Pop, PWD, PWOD -----
       # Must use table S2601A since it is total population
       # Rather than civilian noninstitutionalized as is ACS default
       pop_total_grpquarters = pop_total_grpquarters,

       pop_grpquarters = pop_grpquarters,

       pwd_grpquarters_pct = pwd_grpquarters_pct / 100,

       pwd_grpquarters = pwd_grpquarters,

       pwd_total_grpquarters = pwd_grpquarters * pwd_grpquarters_pct,

       pwod_total_grpquarters = pop_total_grpquarters - pwd_total_grpquarters,

       # Percentages supplied by ACS are whole numbers
       grpquarters_pct = pop_grpquarters / pop_total_grpquarters,

       ### ----- CL. Group Quarters -----
       # ***NOTE: Group quarters sometimes uses a different universe for calculating percentages.
       # E.g. pop_grpquarters_institution_pwd_pct (S2601A_C03_047_estimate)
       # Universe is number of people living in institution, NOT PWD
       pop_grpquarters_institution_pwd_pct = pop_grpquarters_institution_pwd_pct / 100,
       pop_grpquarters_institution_pwod_pct = (1 - pop_grpquarters_institution_pwd_pct),
       pop_grpquarters_noninstitution_pwd_pct = pop_grpquarters_noninstitution_pwd_pct / 100,
       pop_grpquarters_noninstitution_pwod_pct = (1 - pop_grpquarters_noninstitution_pwd_pct),

       # ----- CL. Institution -----
       pop_grpquarters_institution = pop_grpquarters_institution,
       ### PWD
       pwd_grpquarters_institution = round(
         pop_grpquarters_institution * pop_grpquarters_institution_pwd_pct,
         0
       ),
       pwd_grpquarters_institution_pct = pwd_grpquarters_institution / pwd_total, # changed from pop_total_grpquarters,
       ### PWOD
       pwod_grpquarters_institution = pop_grpquarters_institution - pwd_grpquarters_institution,
       pwod_grpquarters_institution_pct = pwod_grpquarters_institution / pwod_total, # changed from pop_total_grpquarters,

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

       ### PWOD
       pwod_home = (pwod_grpquarters - (pwod_grpquarters_institution + pwod_grpquarters_noninstitution)),
       pwod_home_pct = (pwod_home / pwod_grpquarters),

       ### PWD
       pwd_home = (pwd_grpquarters - (pwd_grpquarters_institution + pwd_grpquarters_noninstitution)),
       pwd_home_pct = (pwd_home / pwd_grpquarters),

       ### ----- CL. Nursing Homes -----
       ### Pop 18-64, PWD, PWOD
       pwd_grpqrters_18_64pct = pwd_grpqrters_18_64 / 100,
       pwd_grpqrters_18_64 = round(pop_grpqrters_18_64 * pwd_18_64, 0),
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


        ### ----- WE. Median Income -----
       # pwd_grtoeq_16_med_individual_income = pwd_grtoeq_16_med_individual_income,
       # pwod_grtoeq_16_med_individual_income = pwod_grtoeq_16_med_individual_income,

        # .keep = "none"
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
