

### ---- Load libraries -----


# require(tidyverse)


### ---- Input -----


# Read in national import data (import step already run)
# load(here::here("national", "import",
#                 "output", "national_import.Rda"))


load(here::here("analysis", "data",
                "national_raw.Rda"))

### ---- ACS variable lookup -----


# Load ACS metadata (ACS 5-Year and Subject, bind together
# ACS 5-Year
# ref_vars <- tidycensus::load_variables(2020, "acs5", cache = TRUE)
# ref_vars_sub <- tidycensus::load_variables(2020, "acs5/subject", cache = TRUE)
# ref_vars <- bind_rows(ref_vars, ref_vars_sub)
# rm(ref_vars_sub)

# Workspace for checking calculations
# Filter ACS ref metadata
# temp <- ref_vars %>%
#   filter(str_detect(name, "^S2602_")) %>%
#   select(-geography) %>%
#   # Join required data table, check USA values for calcs
#   left_join(stacked_living %>%
#               filter(ABBR == "USA") %>%
#               mutate(across(everything(), as.character)) %>%
#               pivot_longer(cols = everything(),
#                            names_to = "name",
#                            values_to = "usa_value") %>%
#               filter(!str_detect(name, "_moe$")) %>%
#               mutate("name" = str_remove(name, "_estimate$")),
#             by = "name") %>%
#   relocate(usa_value, .after = label)
#
# clipr::write_clip(temp)


### ---- Clean -----


# ---- (D) Demographics -----
demographics <- stacked_demographics %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    ABBR = ABBR,

    ### ----- D. Pop, PWD, PWOD -----
    # Pop
    pop_total = S1810_C01_001_estimate,
    # PWD
    pwd_total = S1810_C02_001_estimate,
    pwd_pct = pwd_total / pop_total,
    # PWOD
    pwod_total = pop_total - pwd_total,
    pwod_pct = pwod_total / pop_total,

    ### ----- D. Age -----
    pop_18_64 = S1810_C01_015_estimate + S1810_C01_016_estimate,
    pwd_18_64 = S1810_C02_015_estimate + S1810_C02_016_estimate,
    pwd_18_64_pct = pwd_18_64 / pop_18_64,
    pop_grtoeq_65 = S1810_C01_017_estimate + S1810_C01_018_estimate,
    pwd_grtoeq_65 = S1810_C02_017_estimate + S1810_C02_018_estimate,
    pwd_grtoeq_65_pct = pwd_grtoeq_65 / pop_grtoeq_65,

    ### ----- D. Race/Ethnicity -----
    pwd_white = S1810_C02_004_estimate,
    pwd_black = S1810_C02_005_estimate,
    pwd_hisp = S1810_C02_012_estimate,
    pwd_asian = S1810_C02_007_estimate,
    pwd_white_nonhisp = S1810_C02_011_estimate,

    pwd_other = S1810_C02_006_estimate + # American Indian and Alaska Native alone
      S1810_C02_008_estimate + # Native Hawaiian and Other Pacific Islander alone
      S1810_C02_009_estimate, # Some other race alone
    pwd_multiple = S1810_C02_010_estimate,
    ### Percents
    pwd_white_pct = pwd_white / pop_total,
    pwd_black_pct = pwd_black / pop_total,
    pwd_hisp_pct = pwd_hisp / pop_total,
    pwd_asian_pct = pwd_asian / pop_total,
    pwd_white_nonhisp_pct = pwd_white_nonhisp / pop_total,
    pwd_other_pct = pwd_other / pop_total,
    pwd_multiple_pct = pwd_multiple / pop_total,

    ### ----- D. Gender -----
    pop_female = S1810_C01_003_estimate,
    pwd_female = S1810_C02_003_estimate,
    female_pwd_pct = pwd_female / pop_female,
    pwd_female_pct = pwd_female / pwd_total,
    pop_male = S1810_C01_002_estimate,
    pwd_male = S1810_C02_002_estimate,
    male_pwd_pct = pwd_male / pop_male,
    pwd_male_pct = pwd_male / pwd_total,

    ### ----- D. Type of Disability -----
    pwd_hearing = S1810_C02_019_estimate,
    pwd_hearing_pct = pwd_hearing / pop_total,
    pwd_vision = S1810_C02_029_estimate,
    pwd_vision_pct = pwd_vision / pop_total,
    pwd_cognitive = S1810_C02_039_estimate,
    pwd_cognitive_pct = pwd_cognitive / pop_total,
    pwd_ambulatory = S1810_C02_047_estimate,
    pwd_ambulatory_pct = pwd_ambulatory / pop_total,
    pwd_selfcare = S1810_C02_055_estimate,
    pwd_selfcare_pct = pwd_selfcare / pop_total,
    pwd_indliving = S1810_C02_063_estimate,
    pwd_indliving_pct = pwd_indliving / pop_total
  ) %>%
  mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))


# ---- (CL) Community Living -----
community_living <- stacked_living %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    ABBR = ABBR,

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
    pwd_corrections = B26108_038_estimate,
    pwd_corrections_pct = pwd_corrections / B26108_002_estimate,
    ### PWOD
    pwod_corrections = B26108_039_estimate,
    pwod_corrections_pct = pwod_corrections / B26108_003_estimate
  ) %>%
  mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))


# ---- (CP) Community Participation -----
community_participation <- stacked_participation %>%
  transmute(
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
    ### Values
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


# ---- (WE) Work/Economic -----
work_economic <- stacked_economic %>%
  transmute(
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
    pop_19_64 = B18135_013_estimate, # Not the same as the instructions spreadsheet; used this instead to keep calculations in same universe
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
    ### Rent
    rent_burdened_30_35 = B25070_007_estimate,
    rent_burdened_35_40 = B25070_008_estimate,
    rent_burdened_40_50 = B25070_009_estimate,
    rent_burdened_grtoeq_50 = B25070_010_estimate,
    rent_burdened = rent_burdened_30_35 + rent_burdened_35_40 + rent_burdened_40_50  + rent_burdened_grtoeq_50,
    rent_burdened_pct = rent_burdened / B25070_001_estimate,

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
    pwod_grtoeq_16_wfh_pct = S1811_C03_038_estimate / 100
  ) %>%
  mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))


# ----- Save Output -----


rm(stacked_demographics, stacked_living, stacked_participation, stacked_economic)
save.image(here::here("national", "clean", "output", "national_clean.Rda"))
rm(list = ls())
