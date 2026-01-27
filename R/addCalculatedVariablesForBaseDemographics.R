#' addCalculatedVariablesForBaseDemographics
#'
#' Add Calculated Variables To Base Data for Demographics
#'
#' @param base_data dataframe with ADA-PARC base variables
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

    # Character columns to preserve (do NOT convert to numeric)
    char_cols_to_keep <- c("NAME", "ABBR", "year", "GEOID")

    # Identify character columns eligible for numeric conversion
    char_cols_to_convert <- names(df)[
      sapply(df, is.character) & !(names(df) %in% char_cols_to_keep)
    ]

    # Clean: Convert comma-formatted character numbers to numeric
    if (length(char_cols_to_convert) > 0) {
      df <- df %>%
        mutate(across(
          all_of(char_cols_to_convert),
          ~ as.numeric(gsub(",", "", .x))
        ))
    }

    # Ensure ABBR retained if present
    if ("ABBR" %in% names(df)) {
      df <- df %>%
        mutate(ABBR = ABBR)
    }

    df <- df %>%
      mutate(
        ### ----- D. Pop, PWD, PWOD -----

        # PWD
        # % of People with Disabilities ~ Civilian Non-Institutional Population
        pwd_pct = (pwd_total / pop_total),

        # PWOD
        # total number of people without disabilities ~ Civilian Non-Institutional Population
        pwod_total = pop_total - pwd_total,

        # % of people without disabilities ~ Civilian Non-Institutional Population
        pwod_pct = pwod_total / pop_total,

        ### ----- D. Age -----
        # Of People Age 18-64~ Civilian Non-Institutional Population
        pop_18_64 = pop_18_64_ages18to34 + pop_18_64_ages35to64,

        # Of People With Disabilities Age 18-64 ~ Civilian Non-Instituional Population
        pwd_18_64 = pwd_18_64_noninstitutionalized18to34yrs + pwd_18_64_noninstitutionalized35to64,

        # % of Disabled Population That is Between 18-64 ~ Civilian Non-Institutional Population
        eighteen_64_pwd_pct = pwd_18_64 / pwd_total,

        # % of People Who Are Between Age 18-64 & Disabled ~ Civilian Non-Institutional Population
        pct_18to64_and_dis = pwd_18_64 / pop_total,

        # % of Age 18-64 Population Which is Disabled
        pwd_18_64_pct = pwd_18_64 / pop_18_64,

        # Of People Greater Than Age 65 ~ Civilian Non-Institutional Population
        pop_grtoeq_65 = pop_grtoeq_65_65to74 + pop_grtoeq_65_75yrsplus,

        # Of People With Disability Aged Greater Than 65 ~ Civilian Non-Institutional Population
        pwd_grtoeq_65 = pwd_grtoeq_65_65to74 + pwd_grtoeq_65_75yrsplus,

        # % of Disabled Population 65 years or Older ~ Civilian Non-Institutional Population
        grtoeq_65_pwd_pct = pwd_grtoeq_65 / pwd_total,

        # % of People Who Are 65 or Older & Disabled ~ Civilian Non-Institutional Population
        pct_grtoeq_65_and_dis =  pwd_grtoeq_65 / pop_total,

        # %. of Age65+ Population That is Disabled
        pwd_grtoeq_65_pct = pwd_grtoeq_65 / pop_grtoeq_65,


        ### ----- D. Race/Ethnicity -----

        # Of People Identifying as Race 'Other' ~ Civilian Non-Institutional Population
        pwd_other = pwd_other_americanindian_alaskanative +
          pwd_other_nativehawaiian_otherpacificislander +
          pwd_other_other_race,

        # Of People Identifying as Race 'Multiple' ~ Civilian Non-Instituional Population
        pwd_other_multiple = pwd_other_americanindian_alaskanative +
          pwd_other_nativehawaiian_otherpacificislander +
          pwd_other_other_race + pwd_multiple,

        # % of Disabled Population That Is White ~ Civilian Non-Institutional Population
        white_pwd_pct = pwd_white / pwd_total,

        # % of People that are White & Disabled ~ Civilian Non-Institutional Population
        pwd_white_pct = pwd_white / pop_total,

        # % of White Population That is Disabled
        white_dis_pct = pwd_white / pop_white,

        # % of Disabled Population That Is Black ~ Civilian Non-Institutional Population
        black_pwd_pct = pwd_black / pwd_total,

        # % of People That Are Black & Disabled ~ Civilian Non-Institutional Population
        pwd_black_pct = pwd_black / pop_total,

        # % of Black Population That is Disabled
        black_dis_pct = pwd_black / pop_black ,

        # % of Disabled Population That is Hispanic ~ Civilian Non-Institutional Population
        hisp_pwd_pct = pwd_hisp / pwd_total,

        # % of People That Are Hispanic & Disabled
        pwd_hisp_pct = pwd_hisp / pop_total,

        # % of Hispanic Population That is Disabled
        hispanic_dis_pct =  pwd_hisp / pop_hisp,

        # % of People With Disabilty That Are Asian
        asian_pwd_pct = pwd_asian / pwd_total,

        # % of People That Are Disabled And Asian
        pwd_asian_pct = pwd_asian / pop_total,

        # % of Asian Population That is Disabled
        asian_dis_pct = pwd_asian / pop_asian ,

        # % of Disabled Population That Is White Non-Hispanic
        white_nonhisp_pwd_pct = pwd_white_nonhisp / pwd_total,

        # % of People That Are White Non-Hispanic And Disabled
        pwd_white_nonhisp_pct = pwd_white_nonhisp / pop_total,

        # % of White-Non-Hispanic Population That is Disabled
        nonhisp_white_dis_pct =  pwd_white_nonhisp / pop_white_nonhisp,

        # % of Disabled Population Race Other
        other_pwd_pct = pwd_other / pwd_total,

        # % of People That Are Disabled And Race Other
        pwd_other_pct = pwd_other / pop_total,

        # % of Race 'other' Population That is Disabled
        other_dis_pct = pwd_other / pop_other_other_race,

        # % of Disabled Population Multiple Race
        multiple_pwd_pct = pwd_multiple / pwd_total,

        # % of People That Are Disabled And Multiple Race
        pwd_multiple_pct = pwd_multiple / pop_total,

        # % of Multiple Race Population That is Disabled
        multiple_dis_pct = pwd_multiple / pop_multiple,

        # % of Disabled Population That Are AI or AN
        other_americanindian_alaskanative_pwd_pct = pwd_other_americanindian_alaskanative / pwd_total,

        # % of People That are Disabled And AI or AN
        pwd_other_americanindian_alaskanative_pct = pwd_other_americanindian_alaskanative / pop_total,

        # % of AI / AN Population That is Disabled
        ai_an_dis_pct = pwd_other_americanindian_alaskanative / pop_other_americanindian_alaskanative ,

        # % of Disabled Population That Are NHOPI
        other_nativehawaiian_otherpacificislander_pwd_pct = pwd_other_nativehawaiian_otherpacificislander / pwd_total,

        # % of People That Are Disabled And NHOPI
        pwd_other_nativehawaiian_otherpacificislander_pct = pwd_other_nativehawaiian_otherpacificislander / pop_total,

        # % of Black Population That is Disabled
        nhopi_dis_pct = pwd_other_nativehawaiian_otherpacificislander / pop_other_nativehawaiian_otherpacificislander ,

        ### ----- D. Gender -----
        # % of Females That Are Disabled
        female_pwd_pct = pwd_female / pop_female,

        # % of Disabled Population That Is Female
        pwd_female_pct = pwd_female / pwd_total,

        # % of Total Population That Is Female And Disabled
        female_total_pwd_pct = pwd_female / pop_total,

        # % of Males That Are Disabled
        male_pwd_pct = pwd_male / pop_male,

        # % of Disabled Population That Is Male
        pwd_male_pct = pwd_male / pwd_total,

        # % of Population That Is Male And Disabled
        male_total_pwd_pct = pwd_male / pop_total,



        ### ----- D. Type of Disability -----

        # % of People With Hearing Disability
        pwd_hearing_pct = pwd_hearing / pop_total,

        # % of People With Vision Disability
        pwd_vision_pct = pwd_vision / pop_total,

        # % of People With Cognitive Disability
        pwd_cognitive_pct = pwd_cognitive / pop_total,

        # % of People With Ambulatory Disability
        pwd_ambulatory_pct = pwd_ambulatory / pop_total,

        # % of People With Self-Care Disability
        pwd_selfcare_pct = pwd_selfcare / pop_total,

        # % of People With Individual Living Disability
        pwd_indliving_pct = pwd_indliving / pop_total,

        ### ----- CP. Health Insurance -----


        ### ----- CP. Insured/Uninsured, 19-64 -----
        # Number of People With Disabilities, Has Health Insurance, Age 19-64
        pwd_19_64_insured = pwd_19_64_insured,

        # Number of People With Disabilities, Has Private Health Insurance, Age 19-64
        pwd_19_64_insured_private = pwd_19_64_insured_private,

        # Number of People With Disabilities, Has Public Health Insurance, Age 19-64
        pwd_19_64_insured_public = pwd_19_64_insured_public,

        # Number of People With Disabilties, Uninsured Age 19-64
        pwd_19_64_uninsured = pwd_19_64_uninsured,

        # Number of People Without Disabilities, Age 19-64
        pwod_19_64 = pwod_19_64,

        # Number of People WIthout Disabilities, Insured, Age 19-64
        pwod_19_64_insured = pwod_19_64_insured,

        # Number of People Without Disabilities, Has Private Insurance, Age 19-64
        pwod_19_64_insured_private = pwod_19_64_insured_private,

        # Number of People Without Disabiltites, Has Public Insurance, Age 19-64
        pwod_19_64_insured_public = pwod_19_64_insured_public,

        # Number of People Without Disabiltiies, Uninsured, Age 19-64
        pwod_19_64_uninsured = pwod_19_64_uninsured,

        ### ----- CP. Insured/Uninsured, 65+ -----
        # Population Age Greater Or Equal 65
        pop_grtoeq_65 = pop_grtoeq_65,

        # Disabled Population Age Greater or Equal 65
        pwd_grtoeq_65 = pwd_grtoeq_65,

        # Number of People With Disabilities Age Greater or Equal 65, Insured
        pwd_grtoeq_65_insured = pwd_grtoeq_65_insured,

        # Number of People With Disabilities, Age Greater or Equal 65, Has Private Insurance
        pwd_grtoeq_65_insured_private = pwd_grtoeq_65_insured_private,

        # Number of People With Disabilities, Age Greater or Equal 65, Has Public Insurance
        pwd_grtoeq_65_insured_public = pwd_grtoeq_65_insured_public,

        # Number of People With Disabiltiies, Age 65 or Greater, Uninsured
        pwd_grtoeq_65_uninsured = pwd_grtoeq_65_uninsured,

        # Non-Disabled Population, Age Greater or Equal 65
        pwod_grtoeq_65 = pwod_grtoeq_65,

        # Number of People Without Disabilities, Age Greater or Equal 65, Insured
        pwod_grtoeq_65_insured = pwod_grtoeq_65_insured,

        # Number of People Without Disabiltieis, Age Greater or Equal 65, Has Private Insurance
        pwod_grtoeq_65_insured_private = pwod_grtoeq_65_insured_private,

        # Number of People Without Disabiltieis, Age Greater or Equal 65, Has Public Insurance
        pwod_grtoeq_65_insured_public = pwod_grtoeq_65_insured_public,

        # Number of People Without Disabiltieis, Age Greater or Equal 65, Uninsured
        pwod_grtoeq_65_uninsured = pwod_grtoeq_65_uninsured,

        ### ----- CP. Insurance, Percents -----
        # Percentage of People With Disabilities Age 19-64, Insured
        pwd_19_64_insured_pct = pwd_19_64_insured / pwd_19_64,

        # Percentage of People With Disabilities Age 19-64, Uninsured
        pwd_19_64_uninsured_pct = pwd_19_64_uninsured / pwd_19_64,

        # Percentage of People Without Disabilities Age 19-64, Insured
        pwod_19_64_insured_pct = pwod_19_64_insured / pwod_19_64 ,

        # Percentage of People Without Disabilities, Age 19-64, Uninsured
        pwod_19_64_uninsured_pct = pwod_19_64_uninsured / pwod_19_64 ,

        # Percentage of People With Disabilites Age 65+. Insured
        pwd_grtoeq_65_insured_pct = pwd_grtoeq_65_insured / pwd_grtoeq_65,

        # Percentage of People With Disabilities Age 65+, Uninsured
        pwd_grtoeq_65_uninsured_pct = pwd_grtoeq_65_uninsured / pwd_grtoeq_65,

        # Percentage of People Without Disabilities, Age 65+, Insured
        pwod_grtoeq_65_insured_pct = pwod_grtoeq_65_insured / pwod_grtoeq_65,

        # Percentage of People Without Disabilities, Age 65+, Uninsured
        pwod_grtoeq_65_uninsured_pct = pwod_grtoeq_65_uninsured / pwod_grtoeq_65,

        ### ----- CP. Insurance, Public -----
        # Percentage of People With Disabilities 19-64 With Public Insurance
        pwd_19_64_insured_public_pct = pwd_19_64_insured_public / pwd_19_64,

        # Percentage of People Without Disabilities 19-64 With Public Insurance
        pwod_19_64_insured_public_pct = pwod_19_64_insured_public / pwod_19_64,

        # Percentage of People With Disabilties 65+ With Public Insurance
        pwd_grtoeq_65_insured_public_pct = pwd_grtoeq_65_insured_public / pwd_grtoeq_65,

        # Percentage of People Without Disabilities Aeg 65+ With Public Insurance
        pwod_grtoeq_65_insured_public_pct = pwod_grtoeq_65_insured_public / pwod_grtoeq_65,

        ### ----- CP. Insurance, Private -----

        # Percentage of People With Disabilities 19-64 With Private Insurance
        pwd_19_64_insured_private_pct = pwd_19_64_insured_private / pwd_19_64,

        # Percentage of People Without Disabilities Age 19-64 With Private Insurance
        pwod_19_64_insured_private_pct = pwod_19_64_insured_private / pwod_19_64,

        # Percentage of People With Disabilities Age 65+ With private Insurance
        pwd_grtoeq_65_insured_private_pct = pwd_grtoeq_65_insured_private / pwd_grtoeq_65,

        # Percentage of People Without Disabilities Age 65+ With Private Insurance
        pwod_grtoeq_65_insured_private_pct = pwod_grtoeq_65_insured_private / pwod_grtoeq_65,


        ### ----- CP. Educational Attainment -----
        pwd_lessthan_highschool_pct = pwd_lessthan_highschool_pct / 100,
        pwod_lessthan_highschool_pct = pwod_lessthan_highschool_pct / 100,
        pwd_highschoolequiv_pct = pwd_highschoolequiv_pct / 100,
        pwod_highschoolequiv_pct = pwod_highschoolequiv_pct / 100,
        pwd_degree_aa_pct = pwd_degree_aa_pct / 100,
        pwod_degree_aa_pct = pwod_degree_aa_pct / 100,
        pwd_degree_grtoeq_ba_pct = pwd_degree_grtoeq_ba_pct / 100,
        pwod_degree_grtoeq_ba_pct = pwod_degree_grtoeq_ba_pct / 100,

        pwd_lessthan_highschool = pwd_lessthan_highschool_pct * pwd_pop_educ,
        pwod_lessthan_highschool = pwod_lessthan_highschool_pct * pwod_pop_educ,
        pwd_highschoolequiv = pwd_highschoolequiv_pct * pwd_pop_educ,
        pwod_highschoolequiv = pwod_highschoolequiv_pct * pwod_pop_educ,
        pwd_degree_aa = pwd_degree_aa_pct * pwd_pop_educ,
        pwod_degree_aa = pwod_degree_aa_pct * pwod_pop_educ,
        pwd_degree_grtoeq_ba = pwd_degree_grtoeq_ba_pct * pwd_pop_educ,
        pwod_degree_grtoeq_ba = pwod_degree_grtoeq_ba_pct * pwod_pop_educ,

        ### ----- WE. Employment Status -----

        # % of People With Disabilities In 19-64 Age Range
        pwd_19_64_pct = pwd_19_64 / pop_19_64,

        # % of People Without Disabilities in 19-64 Age Range
        pwod_19_64 = pop_19_64 - pwd_19_64,

        # of People with Disabilities, Employed ( )
        pwd_employed_subj = pwd_employed_subj,

        # of People with Disabilities, Employed
        pwd_employed = pwd_employed,

        # of people without disabilities, employed in age range
        pwod_employed = pwod_employed,

        # of People With Disabilities Not Employed ( )
        pwd_not_employed_subj = pwd_not_employed_subj,

        # of People With Disabilities, Unemployed
        pwd_unemployed = pwd_unemployed,

        # of people without disabilities, unemployed, in age range
        pwod_unemployed = pwod_unemployed,

        # of people with disabilities, not in labor force, in age range
        pwd_notlabor = pwd_notlabor,

        # of people without disabilities, not in labor force, in age range
        pwod_notlabor = pwod_notlabor,

        # Percent of People With Disabilities Employed, in age range
        pwd_employed_pct = pwd_employed / pwd_19_64,

        # Percent of People Without Disabilities, Employed, in age range
        pwod_employed_pct = pwod_employed / pwod_19_64,

        # Percent of People With Disabilities, Unemployed, in age range
        pwd_unemployed_pct = pwd_unemployed / pwd_19_64,

        # Percent of People Without Disabilities, Unemployed, in age range
        pwod_unemployed_pct = pwod_unemployed / pwod_19_64,

        # Percent of People With Disabilities Not in Labor Force, in age range
        pwd_notlabor_pct = pwd_notlabor / pwd_19_64,

        # Percent of People Without Disabilities, Not in Labor Force, in age range
        pwod_notlabor_pct = pwod_notlabor / pwod_19_64,

        ### ----- WE. Poverty Status -----

        #
        pop_total_class_18_64 = pop_total_class_18_64,

        #
        pwd_class_18_64 = pwd_class_18_64,

        #
        pwod_class_18_64 = pwod_class_18_64,

        # Number of People With Disabilities Below Poverty Line
        pwd_below_poverty = pwd_below_poverty,

        # Number of People Without Disabilities Below Poverty Line
        pwod_below_poverty = pwod_below_poverty,

        # Number of People With Disabilities At Or Above Poverty Line
        pwd_atorabove_poverty = pwd_atorabove_poverty,

        # Number of People Without Disabilities At Or Above Poverty Line
        pwod_atorabove_poverty = pwod_atorabove_poverty,

        # Percent of People With Disabilities At Or Above Poverty Line
        pwd_below_poverty_pct = pwd_below_poverty / pwd_class_18_64,

        # Percent of People Without Disabilities Below Poverty Line
        pwod_below_poverty_pct = pwod_below_poverty / pwod_class_18_64,

        # Percent of People At Or Above Poverty Line With Disabilities
        pwd_atorabove_poverty_pct = pwd_atorabove_poverty / pwd_class_18_64,

        # Percent of People Without Disabilities At Or Above Poverty Line
        pwod_atorabove_poverty_pct = pwod_atorabove_poverty / pwod_class_18_64,

        ### ----- WE. Median Income -----
        # Median Income for PWD Above Age 16
        pwd_grtoeq_16_med_individual_income = pwd_grtoeq_16_med_individual_income,

        # Median income for PWOD Above Age 16
        pwod_grtoeq_16_med_individual_income = pwod_grtoeq_16_med_individual_income,


        ### ----- CL. Pop, PWD, PWOD -----
        pop_total_grpquarters = pop_total_grpquarters,
        pop_grpquarters = pop_grpquarters,
        pwd_grpquarters_pct = pwd_grpquarters_pct / 100,
        pwd_grpquarters = pwd_grpquarters,
        pwd_total_grpquarters = pwd_grpquarters * pwd_grpquarters_pct,
        pwod_total_grpquarters = pop_total_grpquarters - pwd_total_grpquarters,
        grpquarters_pct = pop_grpquarters / pop_total_grpquarters,

        ### ----- CL. Group Quarters -----
        pop_grpquarters_institution_pwd_pct = pop_grpquarters_institution_pwd_pct / 100,
        pop_grpquarters_institution_pwod_pct = (1 - pop_grpquarters_institution_pwd_pct),
        pop_grpquarters_noninstitution_pwd_pct = pop_grpquarters_noninstitution_pwd_pct / 100,
        pop_grpquarters_noninstitution_pwod_pct = (1 - pop_grpquarters_noninstitution_pwd_pct),

        pop_grpquarters_institution = pop_grpquarters_institution,
        pwd_grpquarters_institution = round(pop_grpquarters_institution * pop_grpquarters_institution_pwd_pct, 0),
        pwd_grpquarters_institution_pct = pwd_grpquarters_institution / pwd_total,
        pwod_grpquarters_institution = pop_grpquarters_institution - pwd_grpquarters_institution,
        pwod_grpquarters_institution_pct = pwod_grpquarters_institution / pwod_total,

        pop_grpquarters_noninstitution = pop_grpquarters_noninstitution,
        pwd_grpquarters_noninstitution = round(pop_grpquarters_noninstitution * pop_grpquarters_noninstitution_pwd_pct, 0),
        pwd_grpquarters_noninstitution_pct = pwd_grpquarters_noninstitution / pop_total_grpquarters,
        pwod_grpquarters_noninstitution = pop_grpquarters_noninstitution - pwd_grpquarters_noninstitution,
        pwod_grpquarters_noninstitution_pct = pwod_grpquarters_noninstitution / pop_total_grpquarters,

        ### ----- CL. Home -----
        pwod_home = pwod_grpquarters - (pwod_grpquarters_institution + pwod_grpquarters_noninstitution),
        pwod_home_pct = pwod_home / pwod_grpquarters,
        pwd_home = pwd_grpquarters - (pwd_grpquarters_institution + pwd_grpquarters_noninstitution),
        pwd_home_pct = pwd_home / pwd_grpquarters,

        ### ----- CL. Incarcerated Persons -----
        pwd_corrections = pwd_corrections,
        pwd_corrections_pct = pwd_corrections / pwd_grpquarters,
        pwod_corrections = pwod_corrections,
        pwod_corrections_pct = pwod_corrections / pwd_grpquarters
      ) %>%
      { if ("pwd_16_plus_subj" %in% colnames(.) & "pop_total_employed_16_plus" %in% colnames(.))
        mutate(., pwd_not_in_labor_force = pwd_16_plus_subj - pop_total_employed_16_plus)
        else . } %>%
      { if ("pwd_total_commute" %in% colnames(.))
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
               pwd_wfh_dis_pct = pwd_wfh / pwd_total_commute
        )
        else . } %>%
      formatPctAndNonPctData()

    return(df)
  }

  calculated_and_base_data <-
    purrr::map(years_in_data, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
}
