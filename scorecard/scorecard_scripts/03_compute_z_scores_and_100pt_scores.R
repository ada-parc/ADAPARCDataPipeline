# ============================================================
# STEP 3: Compute z-scores and 0–100 scores from indicator_raw_long
#   - Transform value_raw (optional)
#   - Z-score within year x indicator_id
#   - Convert z to 0–100 via pnorm(z)*100
#   - Apply direction: lower_better -> flip (100 - score)
# Outputs a long table ready for index aggregation (Step 4)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(glue)
})

# -----------------------------
# 0) PATHS & CONFIG
# -----------------------------

IN_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing/indicator_raw_long.csv"

OUT_DIR  <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing"
OUT_PATH <- file.path(OUT_DIR, "indicator_scored_long.csv")

ID_COLS <- c("GEOID", "NAME", "ABBR", "year")

# If you want mild outlier control, set WINSOR_P to e.g. 0.01 (1% tails).
# For now, keep NULL to skip winsorization.
WINSOR_P <- NULL  # e.g., 0.01 or NULL

# -----------------------------
# 1) HELPERS
# -----------------------------

assert_required_cols <- function(df, cols, df_name = "dataframe") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(glue("{df_name} is missing required columns:\n- {paste(missing, collapse = '\n- ')}"))
  }
}

apply_transform <- function(x, transform) {
  # transform is a single string like "none", "log1p"
  # x is numeric vector
  if (is.null(transform) || is.na(transform) || transform == "" || transform == "none") {
    return(x)
  }

  if (transform == "log1p") {
    # safe for zeros; will return NA for x < -1
    return(log1p(x))
  }

  if (transform == "log") {
    # only defined for positive values
    return(log(x))
  }

  stop(glue("Unknown transform: '{transform}'. Allowed: none, log1p, log"))
}

winsorize <- function(x, p = 0.01) {
  # Winsorize numeric vector x at p and 1-p quantiles (ignoring NAs)
  qs <- stats::quantile(x, probs = c(p, 1 - p), na.rm = TRUE, names = FALSE, type = 7)
  x <- pmin(pmax(x, qs[1]), qs[2])
  x
}

safe_z <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sdv <- stats::sd(x, na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sdv
}

z_to_score100 <- function(z) {
  # Convert z to 0-100 using normal CDF
  stats::pnorm(z) * 100
}

apply_direction <- function(score100, direction) {
  # direction is scalar per indicator row group; return adjusted score
  if (direction == "higher_better") return(score100)
  if (direction == "lower_better")  return(100 - score100)
  stop(glue("Unknown direction: '{direction}'. Allowed: higher_better, lower_better"))
}

# -----------------------------
# 2) LOAD & VALIDATE INPUT
# -----------------------------

df <- readr::read_csv(IN_PATH, show_col_types = FALSE)

required <- c(
  ID_COLS,
  "indicator_id","category","dimension","indicator_label",
  "value_raw","direction","transform","weight"
)

assert_required_cols(df, required, "indicator_raw_long")

# Normalize text fields defensively
df <- df %>%
  mutate(
    indicator_id = str_trim(indicator_id),
    category     = str_trim(category),
    dimension    = str_to_lower(str_trim(dimension)),
    direction    = str_to_lower(str_trim(direction)),
    transform    = str_to_lower(str_trim(transform))
  )

# -----------------------------
# 3) TRANSFORM -> Z -> 100 SCORE
# -----------------------------

scored <- df %>%
  group_by(indicator_id, year) %>%
  mutate(
    # 3a) transform (per row, but transform should be constant within indicator_id)
    value_transformed = apply_transform(value_raw, transform[1]),
    # 3b) optional winsorization (within year x indicator)
    value_winsor = if (is.null(WINSOR_P)) value_transformed else winsorize(value_transformed, WINSOR_P),
    # 3c) z-score within year x indicator
    z_score = safe_z(value_winsor),
    # 3d) 0-100 score
    score_100_unadjusted = z_to_score100(z_score),
    # 3e) direction adjustment
    score_100 = apply_direction(score_100_unadjusted, direction[1])
  ) %>%
  ungroup()

# -----------------------------
# 4) QC SUMMARY
# -----------------------------

qc <- scored %>%
  summarise(
    rows = n(),
    pct_na_raw = mean(is.na(value_raw)),
    pct_na_z   = mean(is.na(z_score)),
    pct_na_100 = mean(is.na(score_100)),
    min_100 = min(score_100, na.rm = TRUE),
    max_100 = max(score_100, na.rm = TRUE)
  )

message(glue("Rows: {qc$rows}"))
message(glue("NA value_raw: {round(100*qc$pct_na_raw, 2)}%"))
message(glue("NA z_score:   {round(100*qc$pct_na_z, 2)}%"))
message(glue("NA score_100: {round(100*qc$pct_na_100, 2)}%"))
message(glue("score_100 range (ignoring NA): [{round(qc$min_100,2)}, {round(qc$max_100,2)}]"))

# Identify indicator-years with zero variance (why z becomes NA)
zero_var <- scored %>%
  group_by(indicator_id, year) %>%
  summarise(sd_transformed = sd(value_winsor, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(sd_transformed) & sd_transformed == 0)

if (nrow(zero_var) > 0) {
  warning("Some indicator_id-year groups have sd == 0 after transform/winsorization; z_score will be NA for those groups.")
  print(zero_var)
}

# -----------------------------
# 5) SAVE OUTPUT
# -----------------------------

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Keep both raw and scored columns (useful for audit)
readr::write_csv(scored, OUT_PATH)

message(glue("Saved:\n{OUT_PATH}"))
