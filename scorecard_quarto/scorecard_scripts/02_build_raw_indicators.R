# ============================================================
# STEP 2: Build raw indicators table from national_data + dictionary
#
# 2024 Revision Notes:
#   - indicator dictionary now includes a `source_status` column:
#       acs_ready            = variable exists in national_data, ready to build
#       acs_needs_pull       = ACS source confirmed, variable name established,
#                              but NOT yet in national_data (add to ACS pull first)
#       external_placeholder = non-ACS external source (FBI, HUD, SSA/BEA);
#                              PLACEHOLDER_ variable names — skip until data added
#       external_ready       = external data has been pulled and merged into
#                              national_data via 01b; PLACEHOLDER_ column names
#                              remain but data is present — ready to build
#   - This script processes `acs_ready` and `external_ready` indicators.
#   - Indicators with other statuses are logged as informational messages.
#   - Old age-split composites (poverty/health insurance/nursing) are removed
#     from the dictionary; composites are no longer needed in step 3b.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(glue)
})

# -----------------------------
# 0) PATHS & CONFIG
# -----------------------------

NATIONAL_DATA_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/analysis/data/final/national_data.Rds"

DICT_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/dictionary/scorecard_indicator_dictionary.csv"

OUT_DIR <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/processing"
OUT_PATH <- file.path(OUT_DIR, "indicator_raw_long.csv")

ID_COLS <- c("GEOID", "NAME", "ABBR", "year")

# Which source_status values to process.
#   - Add "acs_needs_pull" once those variables are in national_data.
#   - "external_ready" = external data merged into national_data via 01b (PLACEHOLDER_
#     column names remain but values are present — safe to process).
PROCESS_STATUSES <- c("acs_ready", "external_ready")

# -----------------------------
# 1) LOAD DATA
# -----------------------------

national_data <- readRDS(NATIONAL_DATA_PATH)

dict_full <- readr::read_csv(DICT_PATH, show_col_types = FALSE)

# -----------------------------
# 1b) DERIVE ALL-AGES POVERTY VARIABLES
#
#   pct_cni_dis_poverty and pct_cni_nodis_poverty are NOT pulled directly
#   from the ACS — they are derived here from age-split count columns already
#   present in national_data (B18130 age-split poverty counts from main pipeline).
#
#   Formula:
#     numerator   = sum of age-group poverty counts (under18 + 18to64 + 65plus)
#     denominator = sum of age-group total CNI population counts
#
#   These variables are referenced by rel/eq_score_poverty in the indicator
#   dictionary. If the underlying count columns are ever renamed in the main
#   pipeline, update the column names below to match.
# -----------------------------

national_data <- national_data %>%
  mutate(
    pct_cni_dis_poverty = (
        pop_cni_under18_poverty_dis +
        pop_cni_18to64_poverty_dis  +
        pop_cni_65plus_poverty_dis
      ) / (
        pop_cni_under18_dis +
        pop_cni_18to64_dis  +
        pop_cni_65plus_dis
      ),
    pct_cni_nodis_poverty = (
        pop_cni_under18_poverty_nodis +
        pop_cni_18to64_poverty_nodis  +
        pop_cni_65plus_poverty_nodis
      ) / (
        pop_cni_under18_nodis +
        pop_cni_18to64_nodis  +
        pop_cni_65plus_nodis
      )
  )

message(glue(
  "Derived pct_cni_dis_poverty:   {sum(!is.na(national_data$pct_cni_dis_poverty))} non-NA values\n",
  "Derived pct_cni_nodis_poverty: {sum(!is.na(national_data$pct_cni_nodis_poverty))} non-NA values"
))

# -----------------------------
# 2) REPORT ON SKIPPED INDICATORS
# -----------------------------

if (!"source_status" %in% names(dict_full)) {
  stop(glue(
    "indicator dictionary is missing the `source_status` column.\n",
    "Expected values: acs_ready | acs_needs_pull | external_placeholder | external_ready"
  ))
}

dict_full <- dict_full %>%
  mutate(source_status = str_trim(str_to_lower(source_status)))

skipped <- dict_full %>% filter(!source_status %in% PROCESS_STATUSES)

if (nrow(skipped) > 0) {
  needs_pull <- skipped %>% filter(source_status == "acs_needs_pull")
  placeholders <- skipped %>% filter(source_status == "external_placeholder")

  if (nrow(needs_pull) > 0) {
    message(glue("\n--- SKIPPED: {nrow(needs_pull)} indicator(s) with source_status = acs_needs_pull ---"))
    message("These are confirmed ACS variables not yet added to national_data:")
    walk(needs_pull$indicator_id, ~ message("  ", .x))
    message("Add them to the ACS data pull, then set PROCESS_STATUSES to include 'acs_needs_pull'.\n")
  }

  if (nrow(placeholders) > 0) {
    message(glue("--- SKIPPED: {nrow(placeholders)} indicator(s) with source_status = external_placeholder ---"))
    message("These require non-ACS external data sources (FBI, HUD, SSA/BEA, etc.):")
    walk(placeholders$indicator_id, ~ message("  ", .x))
    message("See indicator dictionary `notes` column for source details.\n")
  }
}

# Filter to processable indicators only
dict <- dict_full %>% filter(source_status %in% PROCESS_STATUSES)
message(glue("Processing {nrow(dict)} indicators with source_status in: {paste(PROCESS_STATUSES, collapse=', ')}"))

# -----------------------------
# 3) VALIDATION HELPERS
# -----------------------------

assert_required_cols <- function(df, cols, df_name = "dataframe") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(glue("{df_name} is missing required columns:\n- {paste(missing, collapse = '\n- ')}"))
  }
}

coerce_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(x))
}

validate_dictionary <- function(dict) {

  required <- c(
    "indicator_id","category","dimension","indicator_label",
    "pwd_raw_var","pwod_raw_var","derived","direction",
    "transform","weight","source_status","notes"
  )

  assert_required_cols(dict, required, "dictionary")

  dict <- dict %>%
    mutate(
      indicator_id = str_trim(indicator_id),
      category     = str_trim(category),
      dimension    = str_to_lower(str_trim(dimension)),
      direction    = str_to_lower(str_trim(direction)),
      transform    = str_to_lower(str_trim(transform)),
      derived      = as.integer(derived),
      weight       = as.numeric(weight),
      pwd_raw_var  = na_if(str_trim(pwd_raw_var), ""),
      pwod_raw_var = na_if(str_trim(pwod_raw_var), "")
    )

  # Unique indicator IDs
  if (anyDuplicated(dict$indicator_id)) {
    stop("Duplicate indicator_id values found in dictionary.")
  }

  # Dimension checks
  if (any(!dict$dimension %in% c("relative", "equity"))) {
    stop("dimension must be 'relative' or 'equity'")
  }

  # Relative rules
  bad_rel <- dict %>%
    filter(dimension == "relative" & (derived != 0 | !is.na(pwod_raw_var)))
  if (nrow(bad_rel) > 0) {
    stop("Relative indicators must have derived = 0 and blank pwod_raw_var.")
  }

  # Equity rules
  bad_eq <- dict %>%
    filter(dimension == "equity" & (derived != 1 | is.na(pwd_raw_var) | is.na(pwod_raw_var)))
  if (nrow(bad_eq) > 0) {
    stop("Equity indicators must have derived = 1 and both pwd_raw_var and pwod_raw_var filled.")
  }

  # Direction values
  if (any(!dict$direction %in% c("higher_better","lower_better"))) {
    stop("direction must be 'higher_better' or 'lower_better'")
  }

  # Strong invariant warning
  bad_eq_dir <- dict %>%
    filter(dimension == "equity" & direction != "lower_better")
  if (nrow(bad_eq_dir) > 0) {
    warning(
      "Equity indicators should almost always be 'lower_better'.\n",
      paste(bad_eq_dir$indicator_id, collapse = ", ")
    )
  }

  dict
}

# -----------------------------
# 4) BUILD RAW INDICATORS
# -----------------------------

build_indicator_raw_long <- function(national_data, dict, id_cols) {

  assert_required_cols(national_data, id_cols, "national_data")

  dict <- validate_dictionary(dict)

  # Collect all needed source variables
  needed_vars <- unique(na.omit(c(dict$pwd_raw_var, dict$pwod_raw_var)))

  missing_vars <- setdiff(needed_vars, names(national_data))
  if (length(missing_vars) > 0) {
    stop(glue(
      "national_data missing variables referenced in dictionary:\n- {paste(missing_vars, collapse = '\n- ')}\n\n",
      "Tip: If these are new ACS variables, set source_status = 'acs_needs_pull' in the dictionary\n",
      "and add them to your ACS data pull before running this script."
    ))
  }

  base <- national_data %>%
    select(all_of(id_cols), all_of(needed_vars))

  out <- purrr::pmap_dfr(
    dict,
    function(indicator_id, category, dimension, indicator_label,
             pwd_raw_var, pwod_raw_var, derived, direction,
             transform, weight, source_status, notes) {

      if (dimension == "relative") {

        v_pwd <- coerce_numeric(base[[pwd_raw_var]])

        tibble(
          !!!base[id_cols],
          indicator_id    = indicator_id,
          category        = category,
          dimension       = dimension,
          indicator_label = indicator_label,
          value_pwd       = v_pwd,
          value_pwod      = NA_real_,
          value_raw       = v_pwd,
          direction       = direction,
          transform       = transform,
          weight          = weight,
          source_status   = source_status,
          notes           = notes
        )

      } else {

        v_pwd  <- coerce_numeric(base[[pwd_raw_var]])
        v_pwod <- coerce_numeric(base[[pwod_raw_var]])
        gap    <- v_pwod - v_pwd

        tibble(
          !!!base[id_cols],
          indicator_id    = indicator_id,
          category        = category,
          dimension       = dimension,
          indicator_label = indicator_label,
          value_pwd       = v_pwd,
          value_pwod      = v_pwod,
          value_raw       = gap,
          direction       = direction,
          transform       = transform,
          weight          = weight,
          source_status   = source_status,
          notes           = notes
        )
      }
    }
  )

  # -----------------------------
  # QC SUMMARY
  # -----------------------------

  qc <- out %>%
    summarise(
      rows = n(),
      pct_missing = mean(is.na(value_raw)),
      infinite = sum(is.infinite(value_raw), na.rm = TRUE)
    )

  message(glue("Rows built: {qc$rows}"))
  message(glue("Missing value_raw: {round(100 * qc$pct_missing, 2)}%"))
  message(glue("Infinite values: {qc$infinite}"))

  zero_var <- out %>%
    group_by(indicator_id, year) %>%
    summarise(sd_raw = sd(value_raw, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(sd_raw) & sd_raw == 0)

  if (nrow(zero_var) > 0) {
    warning("Some indicator-year groups have zero variance (z-score undefined).")
    print(zero_var)
  }

  out
}

# -----------------------------
# 5) RUN & SAVE
# -----------------------------

indicator_raw_long <- build_indicator_raw_long(
  national_data = national_data,
  dict          = dict,
  id_cols       = ID_COLS
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(indicator_raw_long, OUT_PATH)

message(glue("Saved indicator_raw_long to:\n{OUT_PATH}"))
message(glue("Indicators in output: {n_distinct(indicator_raw_long$indicator_id)}"))
