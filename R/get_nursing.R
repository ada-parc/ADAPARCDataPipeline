#' Nursing home indicators by age (18–64, 65+) and disability status
#'
#' Convenience helper to pull ACS 5-year estimates for:
#'   - Nursing facilities/skilled nursing facilities by age & disability (B26108)
#'   - Total 65+ population (B01001)
#'   - PWD / PWOD population by age band (B18101)
#'
#' and return a tibble with:
#'
#'   GEOID, NAME,
#'
#'   # 65+ nursing home indicators (numerators + denominators + %)
#'   pwd_65plus_nursing_total,      # 65+ PWD in nursing facilities
#'   pwod_65plus_nursing_total,     # 65+ PWOD in nursing facilities
#'   pop_65plus_nursing_total,      # total 65+ in nursing facilities
#'   pop_65plus_total,              # total 65+ population (all settings)
#'   pop_65plus_nursing_pct,        # % of all 65+ living in nursing facilities (0–100)
#'   pwd_65plus_pop,                # 65+ PWD population (civilian noninstitutionalized)
#'   pwod_65plus_pop,               # 65+ PWOD population (civilian noninstitutionalized)
#'   pwd_65plus_nursing_pct,        # % of 65+ PWD living in nursing facilities (0–100)
#'   pwod_65plus_nursing_pct,       # % of 65+ PWOD living in nursing facilities (0–100)
#'   pct_disabled_65plus_nursing,   # % of 65+ nursing pop that is PWD (0–100)
#'   pct_not_disabled_65plus_nursing,# % of 65+ nursing pop that is PWOD (0–100)
#'
#'   # 18–64 nursing home indicators (numerators + denominators + %)
#'   pwd_18to64_nursing_total,      # 18–64 PWD in nursing facilities
#'   pwod_18to64_nursing_total,     # 18–64 PWOD in nursing facilities
#'   pwd_18to64_pop,                # 18–64 PWD population (civilian noninstitutionalized)
#'   pwod_18to64_pop,               # 18–64 PWOD population (civilian noninstitutionalized)
#'   pwd_18to64_nursing_pct,        # % of 18–64 PWD living in nursing facilities (0–100)
#'   pwod_18to64_nursing_pct,       # % of 18–64 PWOD living in nursing facilities (0–100)
#'   pct_disabled_18to64_nursing,   # % of 18–64 nursing pop that is PWD (0–100)
#'   pct_not_disabled_18to64_nursing# % of 18–64 nursing pop that is PWOD (0–100)
#'
#' NOTES:
#' * All estimates are ACS 5-year detailed tables (survey = "acs5").
#' * B26108 counts include group quarters (nursing facilities).
#' * B18101 denominators are CIVILIAN NONINSTITUTIONALIZED only, so
#'   PWD/PWOD percentages using those denominators are relative to that universe,
#'   not truly “all PWD including those in group quarters”. This is an ACS limitation.
#'
#' @param year_to_use Integer ACS year (e.g. 2023; must have ACS 5-year data).
#' @param write_path Optional file path to write CSV. If NULL, nothing is written.
#'
#' @return Invisibly, a tibble with the indicators described above, for US + states.
#'
#' @importFrom dplyr bind_rows select rename_with filter mutate left_join if_else across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom tidycensus get_acs load_variables
#' @importFrom stringr str_detect str_starts
get_s2602_nursing_home <- function(year_to_use, write_path = NULL) {
  api_key <- loadCensusAPIKey()

  # --------------------------------------------------------------------------
  # Helper: safely pick a single ACS variable name by label pattern(s)
  # --------------------------------------------------------------------------
  pick_single_var <- function(vars_tbl, patterns) {
    out <- vars_tbl
    for (pat in patterns) {
      out <- dplyr::filter(out, stringr::str_detect(.data$label, pat))
    }
    n <- nrow(out)
    if (n != 1L) {
      stop(
        "Expected exactly 1 match for label patterns ",
        paste(patterns, collapse = " & "),
        " but found ", n, ". Inspect the filtered table to debug."
      )
    }
    out$name[[1]]
  }

  # --------------------------------------------------------------------------
  # 0) Load ACS variable metadata once
  # --------------------------------------------------------------------------
  acs5_vars <- tidycensus::load_variables(year_to_use, "acs5", cache = TRUE)

  # --------------------------------------------------------------------------
  # 1) B26108 – Nursing facility residents by age (18–64, 65+) & disability
  #     -> numerators for all nursing-home counts
  # --------------------------------------------------------------------------
  b26108_vars <- dplyr::filter(
    acs5_vars,
    stringr::str_starts(.data$name, "B26108_")
  )

  # 65+ in nursing facilities, with & without disability
  b26108_pwd_65plus_nh <- pick_single_var(
    b26108_vars,
    patterns = c(
      "Nursing facilities/skilled nursing facilities",
      "Population 65 years and over",
      "With a disability"
    )
  )

  b26108_pwod_65plus_nh <- pick_single_var(
    b26108_vars,
    patterns = c(
      "Nursing facilities/skilled nursing facilities",
      "Population 65 years and over",
      "No disability"
    )
  )

  # 18–64 in nursing facilities, with & without disability
  b26108_pwd_18to64_nh <- pick_single_var(
    b26108_vars,
    patterns = c(
      "Nursing facilities/skilled nursing facilities",
      "Population 18 to 64 years",
      "With a disability"
    )
  )

  b26108_pwod_18to64_nh <- pick_single_var(
    b26108_vars,
    patterns = c(
      "Nursing facilities/skilled nursing facilities",
      "Population 18 to 64 years",
      "No disability"
    )
  )

  nh_vars <- c(
    pwd_65plus_nursing_total    = b26108_pwd_65plus_nh,
    pwod_65plus_nursing_total   = b26108_pwod_65plus_nh,
    pwd_18to64_nursing_total    = b26108_pwd_18to64_nh,
    pwod_18to64_nursing_total   = b26108_pwod_18to64_nh
  )

  nh_state <- tidycensus::get_acs(
    geography = "state",
    year      = year_to_use,
    survey    = "acs5",
    variables = nh_vars,
    geometry  = FALSE,
    key       = api_key
  )

  nh_us <- tidycensus::get_acs(
    geography = "us",
    year      = year_to_use,
    survey    = "acs5",
    variables = nh_vars,
    geometry  = FALSE,
    key       = api_key
  )

  nh_all <- dplyr::bind_rows(nh_us, nh_state) %>%
    dplyr::select(GEOID, NAME, variable, estimate) %>%
    tidyr::pivot_wider(
      names_from  = variable,
      values_from = estimate
    ) %>%
    dplyr::mutate(
      pop_65plus_nursing_total = pwd_65plus_nursing_total + pwod_65plus_nursing_total
    )

  # --------------------------------------------------------------------------
  # 2) B18101 – PWD/PWOD denominators (civilian noninstitutionalized)
  #     -> 18–64 & 65+ PWD / PWOD population
  # --------------------------------------------------------------------------
  b18101_vars <- dplyr::filter(
    acs5_vars,
    stringr::str_starts(.data$name, "B18101_")
  )

  # 18–64 PWD / PWOD: 18–34 + 35–64
  b18101_18to64 <- b18101_vars %>%
    dplyr::filter(
      stringr::str_detect(.data$label, "18 to 34 years:|35 to 64 years:"),
      stringr::str_detect(.data$label, "With a disability|No disability")
    )

  vars_pwd_18to64 <- b18101_18to64$name[
    stringr::str_detect(b18101_18to64$label, "With a disability")
  ]

  vars_pwod_18to64 <- b18101_18to64$name[
    stringr::str_detect(b18101_18to64$label, "No disability")
  ]

  # 65+ PWD / PWOD: 65–74 + 75+
  b18101_65plus <- b18101_vars %>%
    dplyr::filter(
      stringr::str_detect(.data$label, "65 to 74 years:|75 years and over:"),
      stringr::str_detect(.data$label, "With a disability|No disability")
    )

  vars_pwd_65plus <- b18101_65plus$name[
    stringr::str_detect(b18101_65plus$label, "With a disability")
  ]

  vars_pwod_65plus <- b18101_65plus$name[
    stringr::str_detect(b18101_65plus$label, "No disability")
  ]

  den_acs_vars <- c(
    vars_pwd_18to64,
    vars_pwod_18to64,
    vars_pwd_65plus,
    vars_pwod_65plus
  )

  den_state <- tidycensus::get_acs(
    geography = "state",
    year      = year_to_use,
    survey    = "acs5",
    variables = den_acs_vars,
    geometry  = FALSE,
    key       = api_key
  )

  den_us <- tidycensus::get_acs(
    geography = "us",
    year      = year_to_use,
    survey    = "acs5",
    variables = den_acs_vars,
    geometry  = FALSE,
    key       = api_key
  )

  den_all <- dplyr::bind_rows(den_us, den_state) %>%
    dplyr::select(GEOID, NAME, variable, estimate) %>%
    tidyr::pivot_wider(
      names_from  = variable,
      values_from = estimate
    ) %>%
    dplyr::mutate(
      # 18–64 denominators (PWD & PWOD, civilian noninstitutionalized)
      pwd_18to64_pop = rowSums(
        dplyr::across(dplyr::all_of(vars_pwd_18to64)),
        na.rm = TRUE
      ),
      pwod_18to64_pop = rowSums(
        dplyr::across(dplyr::all_of(vars_pwod_18to64)),
        na.rm = TRUE
      ),
      # 65+ denominators (PWD & PWOD, civilian noninstitutionalized)
      pwd_65plus_pop = rowSums(
        dplyr::across(dplyr::all_of(vars_pwd_65plus)),
        na.rm = TRUE
      ),
      pwod_65plus_pop = rowSums(
        dplyr::across(dplyr::all_of(vars_pwod_65plus)),
        na.rm = TRUE
      )
    ) %>%
    dplyr::select(
      GEOID, NAME,
      pwd_18to64_pop,
      pwod_18to64_pop,
      pwd_65plus_pop,
      pwod_65plus_pop
    )

  # --------------------------------------------------------------------------
  # 3) B01001 – total 65+ population (all settings)
  #     -> denominator for "% of Total Population 65+ in nursing home"
  # --------------------------------------------------------------------------
  b01001_vars <- dplyr::filter(
    acs5_vars,
    stringr::str_starts(.data$name, "B01001_")
  )

  # All 65+ age lines, both sexes
  b01001_65plus <- b01001_vars %>%
    dplyr::filter(
      stringr::str_detect(
        .data$label,
        "65 and 66 years|67 to 69 years|70 to 74 years|75 to 79 years|80 to 84 years|85 years and over"
      )
    )

  vars_65plus_total <- b01001_65plus$name

  pop65_state <- tidycensus::get_acs(
    geography = "state",
    year      = year_to_use,
    survey    = "acs5",
    variables = vars_65plus_total,
    geometry  = FALSE,
    key       = api_key
  )

  pop65_us <- tidycensus::get_acs(
    geography = "us",
    year      = year_to_use,
    survey    = "acs5",
    variables = vars_65plus_total,
    geometry  = FALSE,
    key       = api_key
  )

  pop65_all <- dplyr::bind_rows(pop65_us, pop65_state) %>%
    dplyr::select(GEOID, NAME, variable, estimate) %>%
    tidyr::pivot_wider(
      names_from  = variable,
      values_from = estimate
    ) %>%
    dplyr::mutate(
      pop_65plus_total = rowSums(
        dplyr::across(dplyr::all_of(vars_65plus_total)),
        na.rm = TRUE
      )
    ) %>%
    dplyr::select(GEOID, NAME, pop_65plus_total)

  # --------------------------------------------------------------------------
  # 4) Combine numerators + denominators & compute requested percentages
  #     All % variables are on 0–100 scale (not proportions).
  # --------------------------------------------------------------------------
  den_full <- den_all %>%
    dplyr::left_join(pop65_all, by = c("GEOID", "NAME"))

  out <- nh_all %>%
    dplyr::left_join(den_full, by = c("GEOID", "NAME")) %>%
    dplyr::mutate(
      # 1. Percentage of Total Population Age 65+ Living in Nursing Home
      #    = total 65+ in nursing facilities / total 65+ population
      pop_65plus_nursing_pct = dplyr::if_else(
        !is.na(pop_65plus_total) & pop_65plus_total > 0,
        (pop_65plus_nursing_total / pop_65plus_total) * 100,
        NA_real_
      ),

      # 2. 65+ PWD: % of *all PWD age 65+* living in nursing homes
      #    (denominator: 65+ PWD civilian noninstitutionalized)
      pwd_65plus_nursing_pct = dplyr::if_else(
        !is.na(pwd_65plus_pop) & pwd_65plus_pop > 0,
        (pwd_65plus_nursing_total / pwd_65plus_pop) * 100,
        NA_real_
      ),

      # 3. 65+ PWOD: % of *all PWOD age 65+* living in nursing homes
      #    (denominator: 65+ PWOD civilian noninstitutionalized)
      pwod_65plus_nursing_pct = dplyr::if_else(
        !is.na(pwod_65plus_pop) & pwod_65plus_pop > 0,
        (pwod_65plus_nursing_total / pwod_65plus_pop) * 100,
        NA_real_
      ),

      # 4. 18–64 PWD: % of *all PWD age 18–64* living in nursing homes
      #    (denominator: 18–64 PWD civilian noninstitutionalized)
      pwd_18to64_nursing_pct = dplyr::if_else(
        !is.na(pwd_18to64_pop) & pwd_18to64_pop > 0,
        (pwd_18to64_nursing_total / pwd_18to64_pop) * 100,
        NA_real_
      ),

      # 5. 18–64 PWOD: % of *all PWOD age 18–64* living in nursing homes
      #    (denominator: 18–64 PWOD civilian noninstitutionalized)
      pwod_18to64_nursing_pct = dplyr::if_else(
        !is.na(pwod_18to64_pop) & pwod_18to64_pop > 0,
        (pwod_18to64_nursing_total / pwod_18to64_pop) * 100,
        NA_real_
      ),

      # ------------------------------------------------------------------
      # 6–9. Composition of the nursing-home population by disability
      # ------------------------------------------------------------------

      # 18–64: % of nursing population that is PWD / PWOD
      pct_disabled_18to64_nursing = dplyr::if_else(
        !is.na(pwd_18to64_nursing_total + pwod_18to64_nursing_total) &
          (pwd_18to64_nursing_total + pwod_18to64_nursing_total) > 0,
        (pwd_18to64_nursing_total /
           (pwd_18to64_nursing_total + pwod_18to64_nursing_total)) * 100,
        NA_real_
      ),
      pct_not_disabled_18to64_nursing = dplyr::if_else(
        !is.na(pwd_18to64_nursing_total + pwod_18to64_nursing_total) &
          (pwd_18to64_nursing_total + pwod_18to64_nursing_total) > 0,
        (pwod_18to64_nursing_total /
           (pwd_18to64_nursing_total + pwod_18to64_nursing_total)) * 100,
        NA_real_
      ),

      # 65+: % of nursing population that is PWD / PWOD
      pct_disabled_65plus_nursing = dplyr::if_else(
        !is.na(pwd_65plus_nursing_total + pwod_65plus_nursing_total) &
          (pwd_65plus_nursing_total + pwod_65plus_nursing_total) > 0,
        (pwd_65plus_nursing_total /
           (pwd_65plus_nursing_total + pwod_65plus_nursing_total)) * 100,
        NA_real_
      ),
      pct_not_disabled_65plus_nursing = dplyr::if_else(
        !is.na(pwd_65plus_nursing_total + pwod_65plus_nursing_total) &
          (pwd_65plus_nursing_total + pwod_65plus_nursing_total) > 0,
        (pwod_65plus_nursing_total /
           (pwd_65plus_nursing_total + pwod_65plus_nursing_total)) * 100,
        NA_real_
      )
    )

  if (!is.null(write_path)) {
    utils::write.csv(out, write_path, row.names = FALSE)
  }

  invisible(out)
}
