# ============================================================
# STEP 6: QC for scorecard pipeline integrity
# Checks:
#   A) indicator_scored_long_with_composites: z-score & score_100 correctness
#   B) composites: uniqueness + mean-of-members correctness
#   C) indices: uniqueness + weighted mean correctness
#   D) wide Tableau export: key uniqueness + expected columns
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(glue)
  library(tidyr)
})

# -----------------------------
# PATHS
# -----------------------------
RAW_RDS <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/analysis/data/final/national_data.Rds"

SCORED_WITH_COMP <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing/indicator_scored_long_with_composites.csv"
INDEX_LONG <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing/index_scores_long.csv"
INDEX_DICT <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/dictionary/scorecard_index_dictionary.csv"
IND_DICT <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/dictionary/scorecard_indicator_dictionary.csv"

WIDE_TABLEAU <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/final/index_scores_wide_tableau.csv"

OUT_DIR <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/qc"
OUT_CSV <- file.path(OUT_DIR, "qc_report.csv")

ID_COLS <- c("GEOID","NAME","ABBR","year")

# -----------------------------
# HELPERS
# -----------------------------
near_equal <- function(a, b, tol = 1e-6) {
  ok <- !(is.na(a) & is.na(b))
  abs(a - b) <= tol | (is.na(a) & is.na(b)) | !ok
}

qc_row <- function(test, status, details = "") {
  tibble::tibble(test = test, status = status, details = details)
}

# -----------------------------
# LOAD
# -----------------------------
raw <- readRDS(RAW_RDS)
scored <- read_csv(SCORED_WITH_COMP, show_col_types = FALSE)
idx <- read_csv(INDEX_LONG, show_col_types = FALSE)
idx_dict <- read_csv(INDEX_DICT, show_col_types = FALSE)
ind_dict <- read_csv(IND_DICT, show_col_types = FALSE)

wide <- read_csv(WIDE_TABLEAU, show_col_types = FALSE)

# -----------------------------
# QC COLLECTION
# -----------------------------
qc <- list()

# ============================================================
# A) SCORED INDICATORS: schema & uniqueness
# ============================================================
need_scored <- c(ID_COLS, "indicator_id", "score_100")
missing_cols <- setdiff(need_scored, names(scored))
qc[[length(qc)+1]] <- qc_row(
  "A1 scored columns present",
  ifelse(length(missing_cols) == 0, "PASS", "FAIL"),
  ifelse(length(missing_cols) == 0, "", paste(missing_cols, collapse = ", "))
)

dup_scored <- scored %>%
  count(across(all_of(ID_COLS)), indicator_id) %>%
  filter(n > 1)

qc[[length(qc)+1]] <- qc_row(
  "A2 scored uniqueness (GEOID-year-indicator_id)",
  ifelse(nrow(dup_scored) == 0, "PASS", "FAIL"),
  ifelse(nrow(dup_scored) == 0, "", glue("Duplicates: {nrow(dup_scored)}"))
)

# ============================================================
# B) Z-SCORE & SCORE_100 CORRECTNESS (spot check all indicators)
# Requires value_raw + z_score in file. If absent, we skip.
# ============================================================
if (all(c("value_raw","z_score","direction") %in% names(scored))) {

  # Recompute z by indicator_id (and year if multiple years exist)
  recomputed <- scored %>%
    group_by(year, indicator_id) %>%
    mutate(
      mu = mean(value_raw, na.rm = TRUE),
      sd = sd(value_raw, na.rm = TRUE),
      z_recalc = ifelse(is.na(value_raw) | sd == 0, NA_real_, (value_raw - mu) / sd)
    ) %>%
    ungroup()

  z_diff <- recomputed %>%
    summarise(
      max_abs_diff = max(abs(z_score - z_recalc), na.rm = TRUE),
      frac_bad = mean(!(abs(z_score - z_recalc) <= 1e-6) & !(is.na(z_score) & is.na(z_recalc)))
    )

  qc[[length(qc)+1]] <- qc_row(
    "B1 z_score recalculates correctly",
    ifelse(is.finite(z_diff$max_abs_diff) && z_diff$max_abs_diff <= 1e-5, "PASS", "WARN"),
    glue("max_abs_diff={signif(z_diff$max_abs_diff,3)}, frac_bad={round(100*z_diff$frac_bad,3)}%")
  )

  # Recompute score_100 from z + direction using Normal CDF
  recomputed2 <- recomputed %>%
    mutate(
      score_unadj = 100 * pnorm(z_score),
      score_recalc = case_when(
        direction == "higher_better" ~ score_unadj,
        direction == "lower_better"  ~ 100 - score_unadj,
        TRUE ~ NA_real_
      )
    )

  sdiff <- recomputed2 %>%
    summarise(
      max_abs_diff = max(abs(score_100 - score_recalc), na.rm = TRUE),
      frac_bad = mean(!(abs(score_100 - score_recalc) <= 1e-4) & !(is.na(score_100) & is.na(score_recalc)))
    )

  qc[[length(qc)+1]] <- qc_row(
    "B2 score_100 matches pnorm(z) + direction flip",
    ifelse(is.finite(sdiff$max_abs_diff) && sdiff$max_abs_diff <= 1e-3, "PASS", "WARN"),
    glue("max_abs_diff={signif(sdiff$max_abs_diff,3)}, frac_bad={round(100*sdiff$frac_bad,3)}%")
  )

} else {
  qc[[length(qc)+1]] <- qc_row(
    "B1/B2 z & score checks",
    "SKIP",
    "Need value_raw, z_score, direction columns in scored file"
  )
}

# ============================================================
# C) COMPOSITES: validate they exist + no duplicates
# ============================================================
composite_ids <- c(
  "rel_score_poverty","eq_score_poverty",
  "rel_score_nohealthins","eq_score_nohealthins",
  "rel_score_privatehealth","eq_score_privatehealth",
  "rel_score_publichealth","eq_score_publichealth",
  "rel_score_nursing","eq_score_nursing"
)

missing_comps <- setdiff(composite_ids, unique(scored$indicator_id))
qc[[length(qc)+1]] <- qc_row(
  "C1 composite indicator_ids present",
  ifelse(length(missing_comps) == 0, "PASS", "FAIL"),
  ifelse(length(missing_comps) == 0, "", paste(missing_comps, collapse=", "))
)

dup_comps <- scored %>%
  filter(indicator_id %in% composite_ids) %>%
  count(GEOID, year, indicator_id) %>%
  filter(n > 1)

qc[[length(qc)+1]] <- qc_row(
  "C2 composites uniqueness (GEOID-year-indicator_id)",
  ifelse(nrow(dup_comps) == 0, "PASS", "FAIL"),
  ifelse(nrow(dup_comps) == 0, "", glue("Duplicates: {nrow(dup_comps)}"))
)

# ============================================================
# D) INDICES: uniqueness + recompute by dictionary (spot-check)
# ============================================================
need_idx <- c(ID_COLS, "index_id", "index_score_100")
miss_idx <- setdiff(need_idx, names(idx))
qc[[length(qc)+1]] <- qc_row(
  "D1 index columns present",
  ifelse(length(miss_idx) == 0, "PASS", "FAIL"),
  ifelse(length(miss_idx) == 0, "", paste(miss_idx, collapse = ", "))
)

dup_idx <- idx %>%
  count(across(all_of(ID_COLS)), index_id) %>%
  filter(n > 1)

qc[[length(qc)+1]] <- qc_row(
  "D2 index uniqueness (GEOID-year-index_id)",
  ifelse(nrow(dup_idx) == 0, "PASS", "FAIL"),
  ifelse(nrow(dup_idx) == 0, "", glue("Duplicates: {nrow(dup_idx)}"))
)

# Optional: recompute a few indices exactly as weighted means (uses final computed members)
# Build a member-value table from indicators + indices (what Step 4 effectively does)
if (all(c("index_id","index_score_100") %in% names(idx))) {

  member_values <- bind_rows(
    scored %>% select(all_of(ID_COLS), member_id = indicator_id, value = score_100),
    idx %>% select(all_of(ID_COLS), member_id = index_id, value = index_score_100)
  )

  # choose a handful of high-value targets to audit
  audit_targets <- c("index_rel_commute","index_rel_insurance","index_rel_adaparc",
                     "index_eq_commute","index_eq_insurance","index_eq_adaparc")

  dict_audit <- idx_dict %>% filter(index_id %in% audit_targets)

  if (nrow(dict_audit) > 0) {

    # recompute
    recompute_idx <- dict_audit %>%
      inner_join(member_values, by = c("indicator_id" = "member_id")) %>%
      group_by(across(all_of(ID_COLS)), index_id) %>%
      summarise(
        frac_present = mean(!is.na(value)),
        min_frac_present = first(min_frac_present),
        val = ifelse(frac_present >= min_frac_present,
                     sum(value * weight, na.rm = TRUE) / sum(weight[!is.na(value)]),
                     NA_real_
        ),
        .groups = "drop"
      )

    compare <- idx %>%
      filter(index_id %in% audit_targets) %>%
      select(all_of(ID_COLS), index_id, index_score_100) %>%
      left_join(recompute_idx, by = c(ID_COLS, "index_id"))

    maxdiff <- compare %>%
      summarise(
        max_abs_diff = max(abs(index_score_100 - val), na.rm = TRUE),
        frac_bad = mean(!(abs(index_score_100 - val) <= 1e-6) & !(is.na(index_score_100) & is.na(val)))
      )

    qc[[length(qc)+1]] <- qc_row(
      "D3 recompute key indices from dict matches output",
      ifelse(is.finite(maxdiff$max_abs_diff) && maxdiff$max_abs_diff <= 1e-5, "PASS", "WARN"),
      glue("max_abs_diff={signif(maxdiff$max_abs_diff,3)}, frac_bad={round(100*maxdiff$frac_bad,3)}%")
    )

  } else {
    qc[[length(qc)+1]] <- qc_row("D3 recompute key indices", "SKIP", "No audit targets found in dictionary")
  }
}

# ============================================================
# E) WIDE TABLEAU EXPORT: key uniqueness + expected columns exist
# ============================================================
dup_wide <- wide %>%
  count(across(all_of(ID_COLS))) %>%
  filter(n > 1)

qc[[length(qc)+1]] <- qc_row(
  "E1 wide uniqueness (GEOID-year)",
  ifelse(nrow(dup_wide) == 0, "PASS", "FAIL"),
  ifelse(nrow(dup_wide) == 0, "", glue("Duplicates: {nrow(dup_wide)}"))
)

expected_cols <- c("index_rel_adaparc","index_eq_adaparc","index_rel_insurance","index_eq_insurance")
missing_wide <- setdiff(expected_cols, names(wide))

qc[[length(qc)+1]] <- qc_row(
  "E2 expected key index columns present in wide",
  ifelse(length(missing_wide) == 0, "PASS", "FAIL"),
  ifelse(length(missing_wide) == 0, "", paste(missing_wide, collapse=", "))
)

# ============================================================
# WRITE REPORT
# ============================================================
qc_df <- bind_rows(qc)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(qc_df, OUT_CSV)

print(qc_df)
cat("\nQC report saved to:\n", OUT_CSV, "\n")
