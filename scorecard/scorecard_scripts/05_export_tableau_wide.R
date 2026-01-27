# ============================================================
# STEP 5: Export Tableau-friendly wide file from index_scores_long
#   Input:  index_scores_long.csv (long: one row per GEOID-year-index)
#   Output: index_scores_wide_tableau.csv (wide: one row per GEOID-year)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(glue)
})

# -----------------------------
# PATHS (EDIT IF NEEDED)
# -----------------------------

IN_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing/index_scores_long.csv"

OUT_DIR <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/final"
OUT_PATH <- file.path(OUT_DIR, "index_scores_wide_tableau.csv")

ID_COLS <- c("GEOID", "NAME", "ABBR", "year")

# -----------------------------
# LOAD
# -----------------------------
df <- readr::read_csv(IN_PATH, show_col_types = FALSE)

required <- c(ID_COLS, "index_id", "index_score_100")
missing <- setdiff(required, names(df))
if (length(missing) > 0) {
  stop(glue("Missing required columns in input:\n- {paste(missing, collapse = '\n- ')}"))
}

# -----------------------------
# DEDUPE SAFETY CHECK
# -----------------------------
dups <- df %>%
  count(across(all_of(ID_COLS)), index_id) %>%
  filter(n > 1)

if (nrow(dups) > 0) {
  stop("Found duplicate rows per GEOID-year-index_id. Resolve before pivoting.")
}

# -----------------------------
# WIDE EXPORT
# -----------------------------
wide <- df %>%
  select(all_of(ID_COLS), index_id, index_score_100) %>%
  tidyr::pivot_wider(
    names_from  = index_id,
    values_from = index_score_100
  ) %>%
  arrange(year, ABBR, GEOID)

# -----------------------------
# SAVE
# -----------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(wide, OUT_PATH)

message(glue("Saved Tableau wide file:\n{OUT_PATH}"))
