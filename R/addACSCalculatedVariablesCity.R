#' addACSCalculatedVariablesCity
#'
#' Calculate variables and add to dataset, based on ACS variables
#'
#' @param base_data dataframe with ADA-PARC base variables
#'
#' @returns a DF with ADA-PARC base and calculated variables
#'
#' @import purrr
#' @import dplyr

addACSCalculatedVariablesCity <- function(base_data) {

  years_in_data <- unique(base_data$year)

  generate_calculated_variables_per_year <- function(year_for_filter) {

    # NOTE ON VARIABLE CHANGES

    # While we aren't taking advantage of this structure right now,
    # if/when there are different calculations for variables across different years,
    # we will be able to create additional sub-functions and use for the mutation in
    # that specific year. For example, the ACS changed how it calculates certain
    # variables around disability before 2012; if we are able to determine which
    # base variables from 2012  we can use to calculate an equivalent to a >2012
    # variable, we will be able to compare across those time periods.

    # NOTE ON UNIVERSES

    # The U.S. Census Bureau universe for reporting most disability data is the
    # civilian non-institutional population, which is defined as persons who are not inmates of
    # institutions (e.g., penal and mental facilities, homes for the aged),
    # and who are not on active duty in the Armed Forces.

    df <- base_data %>%
      dplyr::filter(year == year_for_filter) %>%
      dplyr::mutate(


        # PLACE ADDITIONS

        # Demographics. CNI. Disability status from B18101 (place/tract-friendly).
        # NOTE:
        # - All calculated variables end with _place (for consistency and to distinguish from subject-table-derived vars).
        # - For each subgroup we compute:
        #   (1) subgroup disability prevalence:      dis / subgroup_all * 100
        #   (2) subgroup share of total disabled:    dis / total_dis * 100
        # - Percent variables remain 0–100 doubles (NOT proportions).

        # ----------------------------
        # Demographics. CNI. General.
        # ----------------------------

        # Number of CNI individuals (total; all disability statuses)
        pop_cni_total_all_place = pop_cni_total_all_place,

        # Number of CNI individuals with a disability (sum of male + female disability cells)
        pop_cni_total_dis_place =
          pop_cni_male_under5_dis_place +
          pop_cni_male_5to17_dis_place +
          pop_cni_male_18to34_dis_place +
          pop_cni_male_35to64_dis_place +
          pop_cni_male_65to74_dis_place +
          pop_cni_male75plus_dis_place +
          pop_cni_female_under5_dis_place +
          pop_cni_female_5to17_dis_place +
          pop_cni_female_18to34_dis_place +
          pop_cni_female_35to64_dis_place +
          pop_cni_female_65to74_dis_place +
          pop_cni_female75plus_dis_place,

        # Percent of total CNI individuals with a disability
        pct_cni_total_dis_place = (pop_cni_total_dis_place / pop_cni_total_all_place) * 100,

        # Number of CNI individuals without a disability
        pop_cni_total_nodis_place = pop_cni_total_all_place - pop_cni_total_dis_place,

        # Percent of total CNI individuals without a disability
        pct_cni_total_nodis_place = (pop_cni_total_nodis_place / pop_cni_total_all_place) * 100,


        # ----------------------------
        # Demographics. CNI. Sex.
        # ----------------------------

        # Male totals
        pop_cni_male_all_place = pop_cni_male_all_place,

        pop_cni_male_dis_place =
          pop_cni_male_under5_dis_place +
          pop_cni_male_5to17_dis_place +
          pop_cni_male_18to34_dis_place +
          pop_cni_male_35to64_dis_place +
          pop_cni_male_65to74_dis_place +
          pop_cni_male75plus_dis_place,

        # (1) Disability prevalence among males (denominator = male_all)
        pct_cni_male_dis_of_male_place = (pop_cni_male_dis_place / pop_cni_male_all_place) * 100,

        # (2) Male disabled as share of total disabled (denominator = total_dis)
        pct_cni_male_dis_of_totaldis_place = (pop_cni_male_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_male_nodis_place = pop_cni_male_all_place - pop_cni_male_dis_place,

        pct_cni_male_nodis_of_male_place = (pop_cni_male_nodis_place / pop_cni_male_all_place) * 100,

        # Female totals
        pop_cni_female_all_place = pop_cni_female_all_place,

        pop_cni_female_dis_place =
          pop_cni_female_under5_dis_place +
          pop_cni_female_5to17_dis_place +
          pop_cni_female_18to34_dis_place +
          pop_cni_female_35to64_dis_place +
          pop_cni_female_65to74_dis_place +
          pop_cni_female75plus_dis_place,

        # (1) Disability prevalence among females (denominator = female_all)
        pct_cni_female_dis_of_female_place = (pop_cni_female_dis_place / pop_cni_female_all_place) * 100,

        # (2) Female disabled as share of total disabled (denominator = total_dis)
        pct_cni_female_dis_of_totaldis_place = (pop_cni_female_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_female_nodis_place = pop_cni_female_all_place - pop_cni_female_dis_place,

        pct_cni_female_nodis_of_female_place = (pop_cni_female_nodis_place / pop_cni_female_all_place) * 100,


        # ----------------------------
        # Demographics. CNI. Age groups (both sexes combined).
        # ----------------------------

        # Under 5
        pop_cni_under5_all_place =
          pop_cni_male_under5_all_place +
          pop_cni_female_under5_all_place,

        pop_cni_under5_dis_place =
          pop_cni_male_under5_dis_place +
          pop_cni_female_under5_dis_place,

        # (1) Disability prevalence in under-5 population (denominator = under5_all)
        pct_cni_under5_dis_of_under5_place = (pop_cni_under5_dis_place / pop_cni_under5_all_place) * 100,

        # (2) Under-5 disabled as share of total disabled (denominator = total_dis)
        pct_cni_under5_dis_of_totaldis_place = (pop_cni_under5_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_under5_nodis_place = pop_cni_under5_all_place - pop_cni_under5_dis_place,

        pct_cni_under5_nodis_of_under5_place = (pop_cni_under5_nodis_place / pop_cni_under5_all_place) * 100,


        # Ages 5 to 17
        pop_cni_5to17_all_place =
          pop_cni_male_5to17_all_place +
          pop_cni_female_5to17_all_place,

        pop_cni_5to17_dis_place =
          pop_cni_male_5to17_dis_place +
          pop_cni_female_5to17_dis_place,

        pct_cni_5to17_dis_of_5to17_place = (pop_cni_5to17_dis_place / pop_cni_5to17_all_place) * 100,
        pct_cni_5to17_dis_of_totaldis_place = (pop_cni_5to17_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_5to17_nodis_place = pop_cni_5to17_all_place - pop_cni_5to17_dis_place,

        pct_cni_5to17_nodis_of_5to17_place = (pop_cni_5to17_nodis_place / pop_cni_5to17_all_place) * 100,


        # Ages 18 to 34
        pop_cni_18to34_all_place =
          pop_cni_male_18to34_all_place +
          pop_cni_female_18to34_all_place,

        pop_cni_18to34_dis_place =
          pop_cni_male_18to34_dis_place +
          pop_cni_female_18to34_dis_place,

        pct_cni_18to34_dis_of_18to34_place = (pop_cni_18to34_dis_place / pop_cni_18to34_all_place) * 100,
        pct_cni_18to34_dis_of_totaldis_place = (pop_cni_18to34_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_18to34_nodis_place = pop_cni_18to34_all_place - pop_cni_18to34_dis_place,

        pct_cni_18to34_nodis_of_18to34_place = (pop_cni_18to34_nodis_place / pop_cni_18to34_all_place) * 100,


        # Ages 35 to 64
        pop_cni_35to64_all_place =
          pop_cni_male_35to64_all_place +
          pop_cni_female_35to64_all_place,

        pop_cni_35to64_dis_place =
          pop_cni_male_35to64_dis_place +
          pop_cni_female_35to64_dis_place,

        pct_cni_35to64_dis_of_35to64_place = (pop_cni_35to64_dis_place / pop_cni_35to64_all_place) * 100,
        pct_cni_35to64_dis_of_totaldis_place = (pop_cni_35to64_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_35to64_nodis_place = pop_cni_35to64_all_place - pop_cni_35to64_dis_place,

        pct_cni_35to64_nodis_of_35to64_place = (pop_cni_35to64_nodis_place / pop_cni_35to64_all_place) * 100,


        # Ages 65 to 74
        pop_cni_65to74_all_place =
          pop_cni_male_65to74_all_place +
          pop_cni_female_65to74_all_place,

        pop_cni_65to74_dis_place =
          pop_cni_male_65to74_dis_place +
          pop_cni_female_65to74_dis_place,

        pct_cni_65to74_dis_of_65to74_place = (pop_cni_65to74_dis_place / pop_cni_65to74_all_place) * 100,
        pct_cni_65to74_dis_of_totaldis_place = (pop_cni_65to74_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_65to74_nodis_place = pop_cni_65to74_all_place - pop_cni_65to74_dis_place,

        pct_cni_65to74_nodis_of_65to74_place = (pop_cni_65to74_nodis_place / pop_cni_65to74_all_place) * 100,


        # Ages 75+
        pop_cni_75plus_all_place =
          pop_cni_male_75plus_all_place +
          pop_cni_female_75plus_all_place,

        pop_cni_75plus_dis_place =
          pop_cni_male75plus_dis_place +
          pop_cni_female75plus_dis_place,

        # (1) Disability prevalence in 75+ population (denominator = 75plus_all)
        pct_cni_75plus_dis_of_75plus_place = (pop_cni_75plus_dis_place / pop_cni_75plus_all_place) * 100,

        # (2) 75+ disabled as share of total disabled (denominator = total_dis)
        pct_cni_75plus_dis_of_totaldis_place = (pop_cni_75plus_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_75plus_nodis_place = pop_cni_75plus_all_place - pop_cni_75plus_dis_place,

        pct_cni_75plus_nodis_of_75plus_place = (pop_cni_75plus_nodis_place / pop_cni_75plus_all_place) * 100,


        # -------------------------------------------------
        # Demographics. CNI. Aggregated Age Groups (Place)
        # -------------------------------------------------
        # Adds:
        #   • 18–64
        #   • 65+
        # For each group we compute:
        #   (1) disability prevalence within the age group
        #   (2) share of total disabled population
        #   (3) non-disabled counts and prevalence within the age group
        # All variables end with _place and percentages are 0–100 doubles.

        # ----------------------------
        # Ages 18 to 64
        # ----------------------------

        pop_cni_18to64_all_place =
          pop_cni_18to34_all_place +
          pop_cni_35to64_all_place,

        pop_cni_18to64_dis_place =
          pop_cni_18to34_dis_place +
          pop_cni_35to64_dis_place,

        # (1) Disability prevalence among ages 18–64
        pct_cni_18to64_dis_of_18to64_place =
          (pop_cni_18to64_dis_place / pop_cni_18to64_all_place) * 100,

        # (2) Ages 18–64 disabled as share of total disabled population
        pct_cni_18to64_dis_of_totaldis_place =
          (pop_cni_18to64_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_18to64_nodis_place =
          pop_cni_18to64_all_place - pop_cni_18to64_dis_place,

        pct_cni_18to64_nodis_of_18to64_place =
          (pop_cni_18to64_nodis_place / pop_cni_18to64_all_place) * 100,


        # ----------------------------
        # Ages 65 and Over
        # ----------------------------

        pop_cni_65plus_all_place =
          pop_cni_65to74_all_place +
          pop_cni_75plus_all_place,

        pop_cni_65plus_dis_place =
          pop_cni_65to74_dis_place +
          pop_cni_75plus_dis_place,

        # (1) Disability prevalence among ages 65+
        pct_cni_65plus_dis_of_65plus_place =
          (pop_cni_65plus_dis_place / pop_cni_65plus_all_place) * 100,

        # (2) Ages 65+ disabled as share of total disabled population
        pct_cni_65plus_dis_of_totaldis_place =
          (pop_cni_65plus_dis_place / pop_cni_total_dis_place) * 100,

        pop_cni_65plus_nodis_place =
          pop_cni_65plus_all_place - pop_cni_65plus_dis_place,

        pct_cni_65plus_nodis_of_65plus_place =
          (pop_cni_65plus_nodis_place / pop_cni_65plus_all_place) * 100,


        # GENERAL

        # ID
        GEOID = GEOID,
        NAME = NAME,
        year = year,

        # People (PPL) Universe Variables

        # Number of people
        pop_ppl_total_all = pop_ppl_total_all,

        # Number of people with disabilities
        #pop_ppl_total_dis = pop_cni_total_dis + pop_grp_total_dis,

        # Percent of people with disabilities
        #pct_ppl_total_dis = pop_ppl_total_dis / pop_ppl_total_all,

        # Number of people without disabilities
        #pop_ppl_total_nodis = pop_cni_total_nodis + pop_grp_total_nodis,

        # Percent of people without disabilities
        #pct_ppl_total_nodis = pop_ppl_total_nodis / pop_ppl_total_all,

        # DEMOGRAPHICS

        # Demographics. CNI. General.

        # Number of CNI individuals
        pop_cni_total_all = pop_cni_total_all,

        # Percent of CNI individuals with Disabilities
        pct_cni_total_dis = (pop_cni_total_dis / pop_cni_total_all) * 100,

        # Number of CNI individuals without disabilities ~ Civilian Non-Institutional Population
        pop_cni_total_nodis = pop_cni_total_all - pop_cni_total_dis,

        # Percent of CNI individuals without disabilities ~ Civilian Non-Institutional Population
        pct_cni_total_nodis = (pop_cni_total_nodis / pop_cni_total_all) * 100,

        # Demographics. CNI. Age.

        # 0 - 5

        # Number of CNI individuals Age 0 to 5 all
        pop_cni_under5_all = pop_cni_under5_all,

        # Number of CNI individuals Age 0 to 5 with a disability
        pop_cni_under5_dis = pop_cni_under5_dis,

        # Number of CNI individuals age 0 to 5 without a disability
        pop_cni_under5_nodis = pop_cni_under5_all - pop_cni_under5_dis,

        # Percent of CNI individuals Age 0 to 5 with a disability
        pct_cni_under5_dis = pct_cni_under5_dis,

        # Percent of CNI individuals Age 0 to 5 without a disability
        pct_cni_under5_nodis = (pop_cni_under5_nodis / pop_cni_under5_all) * 100,

        # 5 - 17

        # Number of CNI individuals Age 5 to 17 all
        pop_cni_5to17_all = pop_cni_5to17_all,

        # Number of CNI individuals Age 5 to 17 with a disability
        pop_cni_5to17_dis = pop_cni_5to17_dis,

        # Number of CNI individuals Age 5 to 17 without a disability
        pop_cni_5to17_nodis = pop_cni_5to17_all - pop_cni_5to17_dis,

        # Percent of CNI individuals Age 5 to 17 with a disability
        pct_cni_5to17_dis = pct_cni_5to17_dis,

        # Percent of CNI individuals Age 5 to 17 without a disability
        pct_cni_5to17_nodis = (pop_cni_5to17_nodis / pop_cni_5to17_all) * 100 ,

        # 18 - 34

        # Number of CNI 18 - 34 all ability statuses
        pop_cni_18to34_all = pop_cni_18to34_all,

        # Number of CNI 18 - 34 with a disability
        pop_cni_18to34_dis = pop_cni_18to34_dis,

        # Number of CNI 18 - 34 without disability
        pop_cni_18to34_nodis = pop_cni_18to34_all - pop_cni_18to34_dis,

        # Percentage of CNI 18to34 with a disability
        pct_cni_18to34_dis = pct_cni_18to34_dis,

        # Percentage of CNI 18to34 without a disability
        pct_cni_18to34_nodis = (pop_cni_18to34_nodis / pop_cni_18to34_all ) * 100,

        # 35 - 64

        # Number of CNI 35 - 64 all ability statuses
        pop_cni_35to64_all = pop_cni_35to64_all,

        # Number of CNI 35 - 64 with a disability
        pop_cni_35to64_dis = pop_cni_35to64_dis,

        # Number of CNI 35 - 64 without disability
        pop_cni_35to64_nodis = pop_cni_35to64_all - pop_cni_35to64_dis,

        # Percentage of CNI 35 - 64 with a disability
        pct_cni_35to64_dis = pct_cni_35to64_dis,

        # Percentage of CNI 35 - 64 without a disability
        pct_cni_35to64_nodis = (pop_cni_35to64_nodis / pop_cni_35to64_all ) * 100,

        # 65 - 74

        # Number of CNI 65 - 74 all ability statuses
        pop_cni_65to74_all = pop_cni_65to74_all,

        # Number of CNI 65 - 74 with a disability
        pop_cni_65to74_dis = pop_cni_65to74_dis,

        # Number of CNI 65 - 74 without disability
        pop_cni_65to74_nodis = pop_cni_65to74_all - pop_cni_65to74_dis,

        # Percentage of CNI 65 - 74 with a disability
        pct_cni_65to74_dis = pct_cni_65to74_dis,

        # Percentage of CNI 65 - 74 without a disability
        pct_cni_65to74_nodis = (pop_cni_65to74_nodis / pop_cni_65to74_all ) * 100,

        # 75 plus

        # Number of CNI 75plus all ability statuses
        pop_cni_75plus_all = pop_cni_75plus_all,

        # Number of CNI 75plus with a disability
        pop_cni_75plus_dis = pop_cni_75plus_dis,

        # Number of CNI 75plus without disability
        pop_cni_75plus_nodis = pop_cni_75plus_all - pop_cni_75plus_dis,

        # Percentage of CNI 75plus with a disability
        pct_cni_75plus_dis = pct_cni_75plus_dis,

        # Percentage of CNI 75plus without a disability
        pct_cni_75plus_nodis = (pop_cni_75plus_nodis / pop_cni_75plus_all ) * 100,

        # 18 - 64

        # Number of CNI Individuals Age 18-64
        pop_cni_18to64_all = pop_cni_18to34_all + pop_cni_35to64_all,

        # Number of CNI individuals with Disabilities Age 18-64 ~
        pop_cni_18to64_dis = pop_cni_18to34_dis + pop_cni_35to64_dis,

        # Number of CNI individuals without disabilities age 18 to 64
        pop_cni_18to64_nodis = pop_cni_18to34_nodis + pop_cni_35to64_nodis,

        # Percent of CNI Disabled Population That is Between 18-64 ~ Civilian Non-Institutional Population
        pct_cni_dis_18to64 = (pop_cni_18to64_dis / pop_cni_total_dis) * 100,

        # Percent of Population Who Are Between Age 18-64 & Disabled ~ Civilian Non-Institutional Population
        pct_ppl_18to64_and_dis = (pop_cni_18to64_dis / pop_ppl_total_all) * 100,

        # Percent of CNI Age 18-64 Population Which is Disabled
        pct_cni_18to64_dis = (pop_cni_18to64_dis / pop_cni_18to64_all) * 100,

        # 65plus

        # Number of CNI individuals Greater Than Age 65 ~ Civilian Non-Institutional Population
        pop_cni_65plus_all = pop_cni_65to74_all + pop_cni_75plus_all,

        # Number of CNI individuals With Disability Aged Greater Than 65 ~ Civilian Non-Institutional Population
        pop_cni_65plus_dis = pop_cni_65to74_dis + pop_cni_75plus_dis,

        # Percent of CNI Disabled Population 65 years or Older ~ Civilian Non-Institutional Population
        pct_cni_dis_65plus = (pop_cni_65plus_dis / pop_cni_total_dis) * 100,

        # Percent of people Who Are 65 or Older & Disabled ~ Civilian Non-Institutional Population
        pct_ppl_65plus_and_dis =  (pop_cni_65plus_dis / pop_ppl_total_all) * 100,

        # Percent of CNI Age 65+ That is Disabled
        pct_cni_65plus_dis = (pop_cni_65plus_dis / pop_cni_65plus_all) * 100,

        # Demographics. CNI.  Race/Ethnicity.

        # Other

        # Number of CNI individuals race other
        pop_cni_raceother_all = pop_cni_raceother_all,

        # Number of CNI individuals race other with a disability
        pop_cni_raceother_dis = pop_cni_raceother_dis,

        # Percent of CNI individuals with disabilities that are Race Other
        pct_cni_dis_raceother = (pop_cni_raceother_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are disabled and race other*
        pct_cni_raceother_and_dis = (pop_cni_raceother_dis / pop_cni_total_all) * 100,

        # Percent of Race 'other' individuals that are disabled
        pct_cni_raceother_dis = pct_cni_raceother_dis,

        # Multiple

        # Number of CNI individuals race multiple
        pop_cni_racemulti_all = pop_cni_racemulti_all,

        # Number of CNI individuals with disabilities race multiple
        pop_cni_racemulti_dis = pop_cni_racemulti_dis,

        # Percent of CNI Disabled Population That is Multiple Race
        pct_cni_dis_racemulti = (pop_cni_racemulti_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are Disabled And Multiple Race
        pct_cni_racemulti_and_dis = (pop_cni_racemulti_dis / pop_cni_total_all) * 100,

        # Percent of Multiple Race Population That is Disabled
        pct_cni_racemulti_dis = pct_cni_racemulti_dis,

        # White Non-Hispanic

        # Number of CNI White Non-Hispanics
        pop_cni_whitenonhisp_all = pop_cni_whitenonhisp_all,

        # Number of CNI white non-Hispanic individuals with disabilities
        pop_cni_whitenonhisp_dis = pop_cni_whitenonhisp_dis,

        # Percent of CNI individuals with disabilities that are white non-Hispanic
        pct_cni_dis_whitenonhisp = (pop_cni_whitenonhisp_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals with disabilities that are white non-hispanic
        pct_cni_whitenonhisp_and_dis = (pop_cni_whitenonhisp_dis / pop_cni_total_all) * 100,

        # Percent of CNI white non-Hispanic individuals that are disabled
        pct_cni_whitenonhisp_dis =  pct_cni_whitenonhisp_dis,

        # White All (includes hispanic)

        # Number of CNI white alone individuals
        pop_cni_white_all = pop_cni_white_all,

        # Number of CNI white alone individuals with a disability
        pop_cni_white_dis = pop_cni_white_dis,

        # Percent of CNI white alone individuals with a disability
        pct_cni_white_dis = pct_cni_white_dis,

        # Percent of CNI individuals with a disability that are white non-hispanic
        pct_cni_dis_white = (pop_cni_white_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are white and disabled
        pct_cni_white_and_dis = (pop_cni_white_dis / pop_cni_total_all) * 100,

        # Black

        # Number of CNI black individuals
        pop_cni_black_all = pop_cni_black_all,

        # Number of CNI black individuals with a disability
        pop_cni_black_dis = pop_cni_black_dis,

        # Percent of CNI disabled individuals that are black
        pct_cni_dis_black = (pop_cni_black_dis / pop_cni_total_dis) * 100,

        # Percent of CNI black individuals that are disabled
        pct_cni_black_dis = pct_cni_black_dis,

        # Percent of CNI people that are black and disabled
        pct_cni_black_and_dis = (pop_cni_black_dis / pop_cni_total_all) * 100,

        # Hispanic

        # Number of CNI hispanic individuals
        pop_cni_hisp_all = pop_cni_hisp_all,

        # Number of CNI hispanic individuals with a disability
        pop_cni_hisp_dis = pop_cni_hisp_dis,

        # Percent of CNI disabled individuals that are hispanic
        pct_cni_dis_hisp = (pop_cni_hisp_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are hispanic and disabled
        pct_cni_hisp_and_dis = (pop_cni_hisp_dis / pop_cni_total_all) * 100,

        # Percent pf CNI hispanic indivduals that are disabled
        pct_cni_hisp_dis = pct_cni_hisp_dis,

        # Asian

        # Number of CNI Asian individuals
        pop_cni_asian_all = pop_cni_asian_all,

        # Number of CNI Asian individuals with a disability
        pop_cni_asian_dis = pop_cni_asian_dis,

        # Percent of CNI disabled individuals that are Asian
        pct_cni_dis_asian = (pop_cni_asian_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are Asian and disabled
        pct_cni_asian_and_dis = (pop_cni_asian_dis / pop_cni_total_all) * 100,

        # Percent of CNI Asian individuals that are disabled
        pct_cni_asian_dis = pct_cni_asian_dis,

        # AI / AN

        # Number of CNI AIAN Individuals
        pop_cni_aian_all = pop_cni_aian_all,

        # Number of CNI AIAN Individuals with a disability
        pop_cni_aian_dis = pop_cni_aian_dis,

        # Percent of CNI disabled individuals that are AI or AN
        pct_cni_dis_aian = (pop_cni_aian_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are AIAN and Disabled
        pct_cni_aian_and_dis = (pop_cni_aian_dis / pop_cni_total_all) * 100,

        # Percent of CNI AIAN individuals that are disabled
        pct_cni_aian_dis = pct_cni_aian_dis,

        # NHOPI

        # Number of NHOPI individuals
        pop_cni_nhopi_all = pop_cni_nhopi_all,

        # Number of NHOPI individuals with a disability
        pop_cni_nhopi_dis = pop_cni_nhopi_dis,

        # Percent of CNI disabled individuals that are NHOPI
        pct_cni_dis_nhopi = (pop_cni_nhopi_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are NHOPI and disabled
        pct_cni_nhopi_and_dis = (pop_cni_nhopi_dis / pop_cni_total_all) * 100 ,

        # Percent of CNI NHOPI individuals that are disabled
        pct_cni_nhopi_dis = pct_cni_nhopi_dis,


        # Demographics. CNI. Gender

        # Female

        # Percent of CNI females that are disabled
        pct_cni_female_dis = pct_cni_female_dis,

        # Percent of CNI disabled individuals that are female
        pct_cni_dis_female = (pop_cni_female_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are  Female And Disabled
        pct_cni_female_and_dis = (pop_cni_female_dis / pop_cni_total_all) * 100,

        # Male

        # Percent of CNI males that have a disability
        pct_cni_male_dis = pct_cni_male_dis,

        # Percent of CNI disabled population that is male
        pct_cni_dis_male = (pop_cni_male_dis / pop_cni_total_dis) * 100,

        # Percent of CNI individuals that are male and disabled
        pct_cni_male_and_dis = (pop_cni_male_dis / pop_cni_total_all) * 100,

        # Demographics. CNI. Type of Disability

        # Hearing

        # Percent of CNI Population With Hearing Disability
        pct_cni_total_hearing_dis = pct_cni_total_hearing_dis,

        # Vision

        # Percent of CNI Population With Vision Disability
        pct_cni_total_vision_dis = pct_cni_total_vision_dis,

        # Cognitive

        # Percent of CNI Population With Cognitive Disability
        pct_cni_total_cognitive_dis = pct_cni_total_cognitive_dis,

        # Ambulatory

        # Percent of CNI Population With Ambulatory Disability
        pct_cni_total_mobility_dis  = pct_cni_total_mobility_dis,

        # Self-Care

        # Percent of CNI Population With Self-Care Disability
        pct_cni_selfcare_dis = pct_cni_selfcare_dis,

        # Independent Living

        # Percent of CNI Population With Independent Living Disability
        pct_cni_indepliving_dis = pct_cni_indepliving_dis,

        # WORK ECONOMIC -
        # UNIVERSE: Civilian Non-Institutionalized Population (Age 16+)

        # Work Economic. CNI. Age 16+.  Employment Status

        # Employed (PERCENT OF POPS NOT LABOR FORCE)

        # Number of CNI 16+ individuals employed, all ability statuses
        pop_cni_16plus_employed_all = pop_cni_16plus_employed_all,

        # Number of CNI 16+ individuals with disabilities, employed
        pop_cni_16plus_employed_dis = pop_cni_16plus_employed_dis,

        # Number of CNI 16+ individuals without disabilities, employed
        pop_cni_16plus_employed_nodis = pop_cni_16plus_employed_nodis,

        # Percent of CNI 16+ individuals with disabilities, employed
        pct_cni_16plus_employed_dis = (pop_cni_16plus_employed_dis / pop_cni_16plus_dis) * 100,

        # Percent of CNI 16+ individuals without disabilities, employed
        pct_cni_16plus_employed_nodis = (pop_cni_16plus_employed_nodis / pop_cni_16plus_nodis) * 100,

        # Unemployed (PERCENT OF POPS NOT LABOR FORCE)

        # Number of CNI 16+ individuals unemployed, all ability statuses
        pop_cni_16plus_unemployed_all = pop_cni_16plus_unemployed_all,

        # Number CNI 16+ individuals with disabilities, unemployed
        pop_cni_16plus_unemployed_dis = pop_cni_16plus_unemployed_dis,

        # Number of CNI 16+ individuals without disabilities, unemployed
        pop_cni_16plus_unemployed_nodis = pop_cni_16plus_unemployed_nodis,

        # Percent of CNI 16+ individuals with disabilities, unemployed
        pct_cni_16plus_unemployed_dis = (pop_cni_16plus_unemployed_dis / pop_cni_16plus_dis) * 100,

        # Percent of CNI 16+ individuals without disabilities, unemployed
        pct_cni_16plus_nodis_unemployed = (pop_cni_16plus_unemployed_nodis / pop_cni_16plus_nodis) * 100,

        # Not in Labor Force

        # Number of of CNI 16+ individuals with disabilities, not in labor force
        pop_cni_16plus_notlabor_dis = pop_cni_16plus_notlabor_dis,

        # Number of CNI 16+ individuals without disabilities, not in labor force
        pop_cni_16plus_notlabor_nodis = pop_cni_16plus_notlabor_nodis,

        # Percent of CNI 16+ individuals with disabilities, not in labor force
        pct_cni_16plus_notlabor_dis = (pop_cni_16plus_notlabor_dis / pop_cni_16plus_dis) * 100,

        # Percent of CNI 16+ individuals without disabilities, not in labor force
        pct_cni_16plus_notlabor_nodis = (pop_cni_16plus_notlabor_nodis / pop_cni_16plus_nodis) * 100,

        # WE.CNI. Poverty Status

        # Below Poverty Under 18

        # Percent of CNI 65plus individuals with disabilities below poverty line
        pct_cni_under18_poverty_dis = (pop_cni_under18_poverty_dis / pop_cni_under18_dis) * 100,

        # Percent of CNI 65 plus individuals without disabilities below poverty line
        pct_cni_under18_poverty_nodis = (pop_cni_under18_poverty_nodis / pop_cni_under18_nodis) * 100,

        # Below Poverty 18-64

        # Number of CNI 18-64 individuals with disabilities below poverty line
        pop_cni_18to64_poverty_dis = pop_cni_18to64_poverty_dis,

        # Number of CNI 18-64 individuals without disabilities below poverty line
        pop_cni_18to64_poverty_nodis = pop_cni_18to64_poverty_nodis,

        # Percent of CNI 18-64 individuals with disabilities, below poverty line
        pct_cni_18to64_poverty_dis = (pop_cni_18to64_poverty_dis / pop_cni_18to64_dis) * 100,

        # Percent of CNI 18-64 individuals without disabilities, below poverty line
        pct_cni_18to64_poverty_nodis = (pop_cni_18to64_poverty_nodis / pop_cni_18to64_nodis) * 100,


        # Below Poverty 65 plus

        # Percent of CNI 65plus individuals with disabilities below poverty line
        pct_cni_65plus_poverty_dis = (pop_cni_65plus_poverty_dis / pop_cni_65plus_dis) * 100,

        # Percent of CNI 65 plus individuals without disabilities below poverty line
        pct_cni_65plus_poverty_nodis = (pop_cni_65plus_poverty_nodis / pop_cni_65plus_nodis) * 100,


        # WE.CNI.16+. Median Income

        # Median Income for PWD Above Age 16
        pop_cni_16plus_medincome_dis = pop_cni_16plus_medincome_dis,

        # Median income for PWOD Above Age 16
        pop_cni_16plus_medincome_nodis = pop_cni_16plus_medincome_nodis,

        # WE.CNI.16+. Housing Affordability

        # Mortgage

        # Total number of mortgages
        total_mortgages = total_mortgages,

        # Number of  Population Mortgage Burdened In 30-35 percent of cost range
        mortgage_burden_30to35 = mortgage_burden_30to35,

        # Number of  Population Mortgage Burdened In 35-40 percent of cost range
        mortgage_burden_35to40 =  mortgage_burden_35to40,

        # Number of  Population Mortgage Burdened In 40-50 percent of cost range
        mortgage_burden_40to50 = mortgage_burden_40to50,

        # Number of  Population Mortgage Burdened above 50 percent cost range
        mortgage_burden_50plus = mortgage_burden_50plus,

        # Total Number of Households Mortgage Burdened
        mortgage_burdened_households = mortgage_burden_30to35 + mortgage_burden_35to40 + mortgage_burden_40to50 +  mortgage_burden_50plus,

        # Percentage of Households Mortgage Burdened
        pct_households_mortgage_burdened = (mortgage_burdened_households / total_mortgages) * 100,

        # Rent

        # Total renters
        total_renters = total_renters,

        # Number of People Rent Burdened Age Range 30-35
        rent_burden_30to35 = rent_burden_30to35,

        # Number of People Rent Burdened Age Range 35-40
        rent_burden_35to40 = rent_burden_35to40,

        # Number of People Rent Burdened Age Range 40-50
        rent_burden_40to49 = rent_burden_40to49,

        # Number of People Rent Burdened Age Greater Than 50
        rent_burden_50plus = rent_burden_50plus,

        # Total Number of People Rent Burdened
        total_rent_burdened = rent_burden_30to35 + rent_burden_35to40 + rent_burden_40to49  + rent_burden_50plus,

        # Percentage of People Rent Burdened
        pct_rent_burdened = (total_rent_burdened / total_renters) * 100,

        # COMMUNITY LIVING

        # CL General. Pop, PWD, PWOD

        # GRP Universe Population

        # Number of GRP universe individuals total population all ability statuses
        pop_grp_total_all = pop_grp_total_all,

        # Percent of GRP universe individuals total population with a disability
        pct_grp_total_dis = pct_grp_total_dis,

        # Number of GRP universe individuals total with a disability
        pop_grp_total_dis = pop_grp_total_all * (pct_grp_total_dis / 100),

        # Number of GRP universe individuals total without a disability
        pop_grp_total_nodis = pop_grp_total_all - pop_grp_total_dis,

        # Percent of GRP universe individuals total population without a disability
        pct_grp_total_nodis = (pop_grp_total_nodis / pop_grp_total_all) * 100,

        # GRP Universe Population 18 - 64

        # Number of GRP universe individuals age 18 to 64 all
        pop_grp_18to64_all = pop_grp_18to64_all,

        # Percentage of GRP universe individuals age 18 to 64 with disabilities
        pct_grp_18to64_dis = pct_grp_18to64_dis,

        # Number of GRP universe individuals age 18 to 64 with disabilities
        pop_grp_18to64_dis = pop_grp_18to64_all * (pct_grp_18to64_dis / 100),

        # Percentage of GRP universe individuals age 18 to 64 without disabilities
        pct_grp_18to64_nodis = pct_grp_18to64_nodis,

        # Number of GRP universe individuals age 18 to 64 without disabilities
        pop_grp_18to64_nodis = pop_grp_18to64_all * (pct_grp_18to64_nodis /100),


        # GRP Universe Population 65 plus

        # Number of GRP universe individuals, 65plus, all ability statuses
        pop_grp_65plus_all = pop_grp_65plus_all,

        # Percent of GRP universe individuals 65 plus, with a disability
        pct_grp_65plus_dis = pct_grp_65plus_dis,

        # Number of GRP universe individuals 65 plus, with a disability
        pop_grp_65plus_dis = pop_grp_65plus_all * (pct_grp_65plus_dis / 100),

        # Percent of GRP universe individuals 65 plus, without a disability
        pct_grp_65plus_nodis = 100 - pct_grp_65plus_dis,

        # Number of GRP universe individuals age 65 plus, without a disability
        pop_grp_65plus_nodis = pop_grp_65plus_all * (pct_grp_65plus_nodis / 100),

        # GRP Universe Population Actively Living in Group Quarters

        # Number of GRP universe individuals actively living in group quarters
        pop_grp_groupquarters_total_all = pop_grp_groupquarters_total_all,

        # Number of GRP universe individuals actively living in group quarters with a disability
        pop_grp_groupquarters_total_dis = pop_grp_groupquarters_total_dis,

        # Number of GRP universe individuals living in group quarters, without disability
        pop_grp_groupquarters_total_nodis = pop_grp_groupquarters_total_nodis,

        # CL Living in Community: Non-Institutionalized Group Quarters

        # Number of GRP universe individuals living in non-institutionalized group quarters, all
        pop_grp_noninstgroupquarters_total_all = pop_grp_noninstgroupquarters_total_all,

        # Number of GRP universe individuals living in non-institutionalized group quarters, with a disability
        pop_grp_noninstgroupquarters_total_dis = pop_grp_noninstgroupquarters_total_dis,

        # Number of GRP universe individuals living in non-institutionalized group quarters, without a disability
        pop_grp_noninstgroupquarters_total_nodis = pop_grp_noninstgroupquarters_total_nodis,

        # Percent of GRP universe individuals living in non-institutionalized group quarters, with a disability
        pct_grp_noninstgroupquarters_total_dis = pct_grp_noninstgroupquarters_total_dis,

        # Percent of GRP universe individuals living in non-institutionalized groupquarters, without a disability
        pct_grp_noninstgroupquarters_total_nodis = 100 - pct_grp_noninstgroupquarters_total_dis,

        # Percent of GRP universe individuals with disabilities that are living in non-institutionalized groupquarters
        pct_grp_dis_noninstgroupquarters = (pop_grp_noninstgroupquarters_total_dis / pop_grp_total_dis) * 100,

        # Percent of GRP universe individuals without disabilities that are living in non-institutionalized groupquarters
        pct_grp_nodis_noninstgroupquarters = (pop_grp_noninstgroupquarters_total_nodis / pop_grp_total_nodis) * 100,

        # CL. Living in Community. Living at Home.

        # Number of GRP universe individuals total living at home, all ability statuses
        pop_grp_home_all = pop_grp_total_all - pop_grp_groupquarters_total_all,

        # Number of GRP universe individuals total living at home, without a disability
        pop_grp_home_nodis = pop_grp_total_nodis - pop_grp_groupquarters_total_nodis,

        # Number of GRP unviverse individuals total living at home, with a disability
        pop_grp_home_dis = pop_grp_total_dis - pop_grp_groupquarters_total_dis,

        # Percentage of GRP universe individuals without disabilities living at home
        pct_grp_nodis_home = (pop_grp_home_nodis / pop_grp_total_nodis) * 100,

        # Percentage of GRP universe individuals with disabilities living at home
        pct_grp_dis_home = (pop_grp_home_dis / pop_grp_total_dis) * 100,

        # Percentage of those in GRP universe living at home that are disabled
        pct_grp_home_dis = (pop_grp_home_dis / pop_grp_home_all) * 100,

        # Percentage of those in GRP universe living at home that are not disabled
        pct_grp_home_nodis = (pop_grp_home_nodis / pop_grp_home_all) * 100,

        # CL. Living in Institution. Nursing Homes. Age 18 to 64

        # Number of GRP universe individuals living in nursing home between age of 18 and 64, all ability statuses
        pop_grp_nursing_18to64_all = pop_grp_nursing_18to64_all,

        # Number of GRP universe individuals living in nursing home between age of 18 and 64, with a disability
        pop_grp_nursing_18to64_dis = pop_grp_nursing_18to64_dis,

        # Number of GRP universe individuals living in nursing home between age of 18 and 64, without a disability
        pop_grp_nursing_18to64_nodis = pop_grp_nursing_18to64_nodis,

        # Percentage of GRP universe individuals with a disability aged 18 to 64, living in nursing homes
        pct_grp_dis_18to64_nursing = (pop_grp_nursing_18to64_dis / pop_grp_18to64_dis) * 100,

        # Percentage of GRP universe individuals without a disability aged 18 to 64, living in nursing homes
        pct_grp_nodis_18to64_nursing = (pop_grp_nursing_18to64_nodis / pop_grp_18to64_nodis) * 100,

        # Percentage of GRP universe individuals aged 18 to 64 living in a nursing home that are disabled
        pct_grp_nursing_18to64_dis = pct_grp_nursing_18to64_dis,

        # Percentage of GRP universe individuals aged 18 to 64 living in a nursing home that are not disabled
        pct_grp_nursing_18to64_nodis = 100 - pct_grp_nursing_18to64_dis,

        # CL. Living in Institution. Nursing Homes. Age 65 plus

        # Number of GRP universe individuals living in nursing home between age 65 plus all ability statuses
        pop_grp_nursing_65plus_all = pop_grp_nursing_65plus_all,

        # Number of GRP universe individuals living in nursing home between age of 65plus, with a disability
        pop_grp_nursing_65plus_dis = pop_grp_nursing_65plus_dis,

        # Number of GRP universe individuals living in nursing home between age of 65plus, without a disability
        pop_grp_nursing_65plus_nodis = pop_grp_nursing_65plus_nodis,

        # Percentage of GRP universe individuals with a disability aged 65plus, living in nursing homes
        pct_grp_dis_65plus_nursing = (pop_grp_nursing_65plus_dis / pop_grp_65plus_dis) * 100,

        # Percentage of GRP universe individuals without a disability aged 65plus, living in nursing homes
        pct_grp_nodis_65plus_nursing = (pop_grp_nursing_65plus_nodis / pop_grp_65plus_nodis) * 100,

        # Percentage of GRP universe individuals aged 65plus living in a nursing home that are disabled
        pct_grp_nursing_65plus_dis = pct_grp_nursing_65plus_dis,

        # Percentage of GRP universe individuals aged 65plus living in a nursing home that are not disabled
        pct_grp_nursing_65plus_nodis = 100 - pct_grp_nursing_65plus_dis,


        # CL. Living in Institution. Correctional Facility

        # Number of GRP universe individuals living in correctional facility, all ability statuses
        pop_grp_corrections_total_all = pop_grp_corrections_total_all,

        # Number of GRP universe individuals living in correctional facilities, with a disabiltiy
        pop_grp_corrections_total_dis = pop_grp_corrections_total_dis,

        # Number of GRP universe individuals living in correctional facilities, without a disabiltiy
        pop_grp_corrections_total_nodis = pop_grp_corrections_total_nodis,

        # Percentage of those in GRP universe living in correctional facilities, that are disabled
        pct_grp_corrections_total_dis = pct_grp_corrections_total_dis,

        # Percentage of those in GRP universe living in correctional facilities, that are not disabled
        pct_grp_corrections_total_nodis = 100 - pct_grp_corrections_total_dis,

        # Percentage of GRP universe individuals with disabilities that living in correctional facilities
        pct_grp_dis_corrections = (pop_grp_corrections_total_dis / pop_grp_total_dis) * 100,

        # Percent of GRP universe individuals without disabilities that are living in correctional facilities
        pct_grp_nodis_corrections = (pop_grp_corrections_total_nodis / pop_grp_total_nodis) * 100,

        # COMMUNITY PARTICIPATION

        # General

        # Number of CNI individuals total age 19-64, all ability statuses
        pop_cni_19to64_total_all = pop_cni_19to64_total_all,

        # Number of CNI individuals age 19-64, with disabilities
        pop_cni_19to64_total_dis = pop_cni_19to64_total_dis,

        # Number of CNI individuals age 19-64, without disabilities
        pop_cni_19to64_nodis = pop_cni_19to64_total_all  - pop_cni_19to64_total_dis,

        # Number of CNI inidivudls age 65 plus, all ability statuses
        pop_cni_65plus_total_all = pop_cni_65plus_total_all,

        # Disabled Population Age Greater or Equal 65
        pop_cni_65plus_total_dis = pop_cni_65plus_total_dis,

        # Non-Disabled Population, Age Greater or Equal 65
        pop_cni_65plus_nodis = pop_cni_65plus_total_all - pop_cni_65plus_total_dis,

        # CP. Health Insurance

        # Health Insurance Status: None - Age 19 to 64

        # Number of People With Disabilities, Uninsured Age 19-64
        pop_cni_19to64_dis_nohealthins =  pop_cni_19to64_dis_nohealthins,

        # Number of People Without Disabilities, Uninsured, Age 19-64
        pop_cni_19to64_nodis_nohealthins = pop_cni_19to64_nodis_nohealthins,

        # Percentage of CNI disabled population age 19-64, without health insurance
        pct_cni_19to64_dis_nohealthins = (pop_cni_19to64_dis_nohealthins / pop_cni_19to64_total_dis) * 100,

        # Percentage of CNI non-disabled population age 19-64, without health insurance
        pct_cni_19to64_nodis_nohealthins = (pop_cni_19to64_nodis_nohealthins / pop_cni_19to64_nodis) * 100,

        # Health Insurance Status: None - Age 65+

        # Number of People With Disabilities, Age 65 or Greater, Uninsured
        pop_cni_65plus_dis_nohealthins = pop_cni_65plus_dis_nohealthins,

        # Number of People Without Disabilities, Age 65 or Greater, Uninsured
        pop_cni_65plus_nodis_nohealthins = pop_cni_65plus_nodis_nohealthins,

        # Percentage of CNI disabled population age 65+, without health insurance
        pct_cni_65plus_dis_nohealthins = (pop_cni_65plus_dis_nohealthins / pop_cni_65plus_dis) * 100,

        # Percentage of CNI non-disabled population age 65+, without health insurance,
        pct_cni_65plus_nodis_nohealthins = (pop_cni_65plus_nodis_nohealthins / pop_cni_65plus_nodis) * 100,

        # Health Insurance Status: Covered - Age 19-64

        # Number of People Without Disabilities, Insured, Age 19-64
        pop_cni_19to64_nodis_healthins = pop_cni_19to64_nodis_healthins,

        # Number of People With Disabilities, Has Health Insurance, Age 19-64
        pop_cni_19to64_dis_healthins = pop_cni_19to64_dis_healthins,

        # Percentage of CNI disabled population age 19-64. with health insurance
        pct_cni_19to64_dis_healthins = (pop_cni_19to64_dis_healthins / pop_cni_19to64_total_dis) * 100 ,

        # Percentage CNI non-disabled population age 19-64, with health insurance
        pct_cni_19to64_nodis_healthins = (pop_cni_19to64_nodis_healthins / pop_cni_19to64_nodis) * 100 ,

        # Health Insurance Status: Covered - Age 65+

        # Number of People With Disabilities Age Greater or Equal 65, Insured
        pop_cni_65plus_dis_healthins = pop_cni_65plus_dis_healthins,

        # Number of People Without Disabilities, Age Greater or Equal 65, Insured
        pop_cni_65plus_nodis_healthins = pop_cni_65plus_nodis_healthins,

        # Percentage of CNI disabled population age 65+, with health insurance
        pct_cni_65plus_dis_healthins = (pop_cni_65plus_dis_healthins / pop_cni_65plus_total_dis) * 100,

        # Percentage of CNI non-disabled population age 65+, with health insurance
        pct_cni_65plus_nodis_healthins = (pop_cni_65plus_nodis_healthins / pop_cni_65plus_nodis) * 100,

        # Health Insurance Type: Public - Age 19-64

        # Number of People With Disabilities, Has Public Health Insurance, Age 19-64
        pop_cni_19to64_dis_publichealth = pop_cni_19to64_dis_publichealth,

        # Number of People Without Disabiltites, Has Public Insurance, Age 19-64
        pop_cni_19to64_nodis_publichealth = pop_cni_19to64_nodis_publichealth,

        # Percentage of  CNI disabled population age 19-64, with public health insurance
        pct_cni_19to64_dis_publichealth = (pop_cni_19to64_dis_publichealth / pop_cni_19to64_total_dis) * 100 ,

        # Percentage of CNI non-disabled population age 19-64, with public health insurance
        pct_cni_19to64_nodis_publichealth = (pop_cni_19to64_nodis_publichealth / pop_cni_19to64_nodis) * 100,

        # Health Insurance Type: Public - Age 65+

        # Number of People With Disabilities, Age Greater or Equal 65, Has Public Insurance
        pop_cni_65plus_dis_publichealth = pop_cni_65plus_dis_publichealth,

        # Number of People Without Disabilities, Age Greater or Equal 65, Has Public Insurance
        pop_cni_65plus_nodis_publichealth = pop_cni_65plus_nodis_publichealth,

        # Percentage of CNI disabled population age 65+ with public health insurance
        pct_cni_65plus_dis_publichealth = (pop_cni_65plus_dis_publichealth / pop_cni_65plus_dis) * 100,

        # Percentage of CNI non-disabled population age 65+, with public health insurance
        pct_cni_65plus_nodis_publichealth = (pop_cni_65plus_nodis_publichealth / pop_cni_65plus_nodis) * 100,

        # Health Insurance Type: Private - Age 19-64

        # Number of People Without Disabilities, Has Private Insurance, Age 19-64
        pop_cni_19to64_nodis_privatehealth = pop_cni_19to64_nodis_privatehealth,

        # Number of People With Disabilities, Has Private Health Insurance, Age 19-64
        pop_cni_19to64_dis_privatehealth = pop_cni_19to64_dis_privatehealth,

        # Percentage of CNI disabled population age 19-64, with private insurance
        pct_cni_19to64_dis_privatehealth = (pop_cni_19to64_dis_privatehealth / pop_cni_19to64_total_dis) * 100,

        # Percentage of CNI non-disabled population age 19-64, with private insurance
        pct_cni_19to64_nodis_privatehealth = (pop_cni_19to64_nodis_privatehealth / pop_cni_19to64_nodis) * 100,

        # Health Insurance Type: Private - Age 65+

        # Number of People With Disabilities, Age Greater or Equal 65, Has Private Insurance
        pop_cni_65plus_dis_privatehealth = pop_cni_65plus_dis_privatehealth,

        # Number of People Without Disabilities, Age Greater or Equal 65, Has Private Insurance
        pop_cni_65plus_nodis_privatehealth = pop_cni_65plus_nodis_privatehealth,

        # Percentage of CNI disabled population age 65+, with private insurance
        pct_cni_65plus_dis_privatehealth = (pop_cni_65plus_dis_privatehealth / pop_cni_65plus_dis) * 100,

        # Percentage of CNI non-disabled population age 65+, with private insurance
        pct_cni_65plus_nodis_privatehealth = (pop_cni_65plus_nodis_privatehealth / pop_cni_65plus_nodis) * 100,

        # CP. Commute to Work.

        # Number of CNI 16 plus individuals commute to work all
        pop_cni_16plus_commute_all = pop_cni_16plus_commute_all,

        # Number of CNI 16plus individuals commute to work, with a disability
        pop_cni_16plus_commute_dis = pop_cni_16plus_commute_dis,

        # Number of CNI 16 plus individuals commute to work, without a disability
        pop_cni_16plus_commute_nodis = pop_cni_16plus_commute_nodis,

        # Transit

        # Number of CNI 16 plus individuals commute to work by public transit, all
        pop_cni_16plus_publictrans_all = pop_cni_16plus_publictrans_all,

        # Percentage of CNI 16plus individuals with a disability who commute to work by public transit
        pct_cni_16plus_transit_dis = pct_cni_16plus_transit_dis,

        # Percentage of CNI 16 plus individuals without a disability who commute to work by public transit
        pct_cni_16plus_transit_nodis = pct_cni_16plus_transit_nodis,

        # Number of CNI 16 plus individuals who commute to work by public transit, without a disability
        pop_cni_16plus_transit_nodis = pop_cni_16plus_commute_nodis * (pct_cni_16plus_transit_nodis / 100),

        # Number of CNI 16 plus invdividuals who commute to work by public transit, with a disability
        pop_cni_16plus_transit_dis = pop_cni_16plus_commute_dis * (pct_cni_16plus_transit_dis / 100),

        # Private Car

        # Number of CNI 16 plus individuals commute to work by driving alone, all
        pop_cni_16plus_drive_all = pop_cni_16plus_drive_all,

        # Percentage of CNI 16 plus individuals commute to work with a disability by driving alone
        pct_cni_16plus_drivealone_dis = pct_cni_16plus_drivealone_dis,

        # Percentage of CNI 16 plus individuals commute to work without a disability by driving alone
        pct_cni_16plus_drivealone_nodis = pct_cni_16plus_drivealone_nodis,

        # Number of CNI 16 plus individuals commute to work with a disability by driving alone
        pop_cni_16plus_drivealone_dis = pop_cni_16plus_commute_dis * (pct_cni_16plus_drivealone_dis / 100),

        # Number of CNI 16 plus individuals commute to work without a disability by driving alone
        pop_cni_16plus_drivealone_nodis = pop_cni_16plus_commute_nodis * (pct_cni_16plus_drivealone_nodis / 100),

        # CP.Education. CNI Age 25plus

        # CNI 25plus all
        pop_cni_25plus_edu_all = pop_cni_25plus_edu_all,

        # CNI 25 plus dis
        pop_cni_25plus_edu_dis = pop_cni_25plus_edu_dis,

        # CNI 25 plus no dis
        pop_cni_25plus_edu_nodis = pop_cni_25plus_edu_nodis,

        # Less Than High School

        # Percent of CNI age 25 plus individuals less than high school / GED
        pct_cni_25plus_noGED_all = pct_cni_25plus_noGED_all,

        # Percent of CNI age 25 plus individuals with a disability less than high school / GED
        pct_cni_25plus_noGED_dis = pct_cni_25plus_noGED_dis,

        # Percent of CNI age 25 plus individuals witout a disability, less than high school / GED
        pct_cni_25plus_noGED_nodis = pct_cni_25plus_noGED_nodis,

        # Number of CNI age 25 plus individuals with disabilities, less than high school / GED
        pop_cni_25plus_noGED_dis = pop_cni_25plus_edu_dis * (pct_cni_25plus_noGED_dis / 100),

        # Number of CNI age 25 plus individuals without disabilities, less than high school / GED
        pop_cni_25plus_noGED_nodis = pop_cni_25plus_edu_nodis * (pct_cni_25plus_noGED_nodis / 100),


        # High School / GED

        # Percent of CNI age 25 plus individuals high school / GED
        pct_cni_25plus_GED_all = pct_cni_25plus_GED_all,

        # Percent of CNI age 25 plus individuals with a disability high school / GED
        pct_cni_25plus_GED_dis = pct_cni_25plus_GED_dis,

        # Percent of CNI age 25 plus individuals witout a disability, high school / GED
        pct_cni_25plus_GED_nodis = pct_cni_25plus_GED_nodis,

        # Number of CNI age 25 plus individuals with disabilities, high school / GED
        pop_cni_25plus_GED_dis = pop_cni_25plus_edu_dis * (pct_cni_25plus_GED_dis / 100),

        # Number of CNI age 25 plus individuals without disabilities, high school / GED
        pop_cni_25plus_GED_nodis = pop_cni_25plus_edu_nodis * (pct_cni_25plus_GED_nodis / 100),

        # Bachelors Degree or Higher

        # Percent of CNI age 25 plus individuals bachelors or higher
        pct_cni_25plus_bachelors_all = pct_cni_25plus_bachelors_all,

        # Percent of CNI age 25 plus individuals with disabilities bachelors or higher
        pct_cni_25plus_bachelors_dis = pct_cni_25plus_bachelors_dis,

        # Percent of CNI age 25 plus individuals without disabilities bachelors or higher
        pct_cni_25plus_bachelors_nodis = pct_cni_25plus_bachelors_nodis,

        # Number of CNI age 25 plus individuals with disabilities bachelors or higher
        pop_cni_25plus_bachelors_dis = pop_cni_25plus_edu_dis * (pct_cni_25plus_bachelors_dis / 100),

        # Number of CNI age 25 plus individuals without disabilities bachelors or higher
        pop_cni_25plus_bachelors_nodis = pop_cni_25plus_edu_nodis * (pct_cni_25plus_bachelors_nodis / 100),

      ) %>%

      formatPctAndNonPctData(.)

    return(df)
  }

  calculated_and_base_data <-
    map(years, ~ generate_calculated_variables_per_year(.x)) %>%
    dplyr::bind_rows()

  return(calculated_and_base_data)
}
