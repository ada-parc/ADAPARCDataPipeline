# ============================================================
# STEP 2: Build raw indicators table from national_data + dictionary
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

DICT_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/dictionary/scorecard_indicator_dictionary.csv"

OUT_DIR <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing"
OUT_PATH <- file.path(OUT_DIR, "indicator_raw_long.csv")

ID_COLS <- c("GEOID", "NAME", "ABBR", "year")

# -----------------------------
# 1) LOAD DATA
# -----------------------------

national_data <- readRDS(NATIONAL_DATA_PATH)

dict <- readr::read_csv(DICT_PATH, show_col_types = FALSE)

# -----------------------------
# 2) VALIDATION HELPERS
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
    "transform","weight","notes"
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
# 3) BUILD RAW INDICATORS
# -----------------------------

build_indicator_raw_long <- function(national_data, dict, id_cols) {

  assert_required_cols(national_data, id_cols, "national_data")

  dict <- validate_dictionary(dict)

  # Collect all needed source variables
  needed_vars <- unique(na.omit(c(dict$pwd_raw_var, dict$pwod_raw_var)))

  missing_vars <- setdiff(needed_vars, names(national_data))
  if (length(missing_vars) > 0) {
    stop(glue(
      "national_data missing variables referenced in dictionary:\n- {paste(missing_vars, collapse = '\n- ')}"
    ))
  }

  base <- national_data %>%
    select(all_of(id_cols), all_of(needed_vars))

  out <- purrr::pmap_dfr(
    dict,
    function(indicator_id, category, dimension, indicator_label,
             pwd_raw_var, pwod_raw_var, derived, direction,
             transform, weight, notes) {

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
# 4) RUN & SAVE
# -----------------------------

indicator_raw_long <- build_indicator_raw_long(
  national_data = national_data,
  dict          = dict,
  id_cols       = ID_COLS
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(indicator_raw_long, OUT_PATH)

message(glue("Saved indicator_raw_long to:\n{OUT_PATH}"))
