# ============================================================
# STEP 6: QC for scorecard pipeline integrity
# Checks:
#   A) indicator_scored_long_with_composites: schema, uniqueness, coverage
#   B) Z-score & score_100 correctness (spot check)
#   C) Composites: (placeholder for future use; no composites in 2024)
#   D) Indices: uniqueness + weighted mean correctness
#   E) Wide export: key uniqueness, expected columns, USA row completeness
#
# 2024 Revision:
#   - Index IDs updated to match 2024 structure:
#       CL: living_arrangements + community_resource
#       CP: tech + insurance + education + commute + safety + housing_support
#       WE: positive_work + negative_work + housing_affordability
#   - All external data now pulled (01b complete); placeholder language removed
#   - Added checks for external_ready indicators and USA row completeness
#   - QC report includes data completeness summary per category
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

SCORED_WITH_COMP <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/processing/indicator_scored_long_with_composites.csv"
INDEX_LONG  <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/processing/index_scores_long.csv"
INDEX_DICT  <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/dictionary/scorecard_index_dictionary.csv"
IND_DICT    <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/dictionary/scorecard_indicator_dictionary.csv"

WIDE_TABLEAU <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/final/index_scores_wide.csv"

OUT_DIR <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/qc"
OUT_CSV <- file.path(OUT_DIR, "qc_report.csv")

ID_COLS <- c("GEOID","NAME","ABBR","year")

# -----------------------------
# HELPERS
# -----------------------------
near_equal <- function(a, b, tol = 1e-6) {
  abs(a - b) <= tol | (is.na(a) & is.na(b))
}

qc_row <- function(test, status, details = "") {
  tibble::tibble(test = test, status = status, details = details)
}

# -----------------------------
# LOAD
# -----------------------------
raw    <- readRDS(RAW_RDS)
scored <- read_csv(SCORED_WITH_COMP, show_col_types = FALSE)
idx    <- read_csv(INDEX_LONG,       show_col_types = FALSE)
idx_dict <- read_csv(INDEX_DICT,     show_col_types = FALSE)
ind_dict <- read_csv(IND_DICT,       show_col_types = FALSE)
wide   <- read_csv(WIDE_TABLEAU,     show_col_types = FALSE)

# -----------------------------
# QC COLLECTION
# -----------------------------
qc <- list()

# ============================================================
# A) SCORED INDICATORS: schema, uniqueness, coverage
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

# Check that all acs_ready indicators from dictionary are present
acs_ready_ids <- ind_dict %>%
  filter(str_trim(str_to_lower(source_status)) == "acs_ready") %>%
  pull(indicator_id) %>% unique()

missing_ready <- setdiff(acs_ready_ids, unique(scored$indicator_id))
qc[[length(qc)+1]] <- qc_row(
  "A3 all acs_ready indicators present in scored output",
  ifelse(length(missing_ready) == 0, "PASS", "FAIL"),
  ifelse(length(missing_ready) == 0,
         glue("{length(acs_ready_ids)} acs_ready indicators all present"),
         paste(missing_ready, collapse = ", "))
)

# Check that all external_ready indicators are also present
ext_ready_ids <- ind_dict %>%
  filter(str_trim(str_to_lower(source_status)) == "external_ready") %>%
  pull(indicator_id) %>% unique()

missing_ext <- setdiff(ext_ready_ids, unique(scored$indicator_id))
qc[[length(qc)+1]] <- qc_row(
  "A4 all external_ready indicators present in scored output",
  ifelse(length(missing_ext) == 0, "PASS", "FAIL"),
  ifelse(length(missing_ext) == 0,
         glue("{length(ext_ready_ids)} external_ready indicators all present"),
         paste(missing_ext, collapse = ", "))
)

# Check that external_ready indicators have non-NA values for states
ext_scored <- scored %>%
  filter(indicator_id %in% ext_ready_ids, ABBR != "USA") %>%
  group_by(indicator_id) %>%
  summarise(
    n_total = n(),
    n_non_na = sum(!is.na(score_100)),
    pct_coverage = round(100 * n_non_na / n_total, 1),
    .groups = "drop"
  )

low_coverage <- ext_scored %>% filter(pct_coverage < 90)
qc[[length(qc)+1]] <- qc_row(
  "A5 external_ready indicator coverage (>90% non-NA for states)",
  ifelse(nrow(low_coverage) == 0, "PASS", "WARN"),
  ifelse(nrow(low_coverage) == 0,
         glue("All {nrow(ext_scored)} external indicators have >90% state coverage"),
         paste(glue("{low_coverage$indicator_id}: {low_coverage$pct_coverage}%"), collapse = "; "))
)

# ============================================================
# B) Z-SCORE & SCORE_100 CORRECTNESS (spot check)
# ============================================================
if (all(c("value_raw","z_score","direction") %in% names(scored))) {

  recomputed <- scored %>%
    group_by(year, indicator_id) %>%
    mutate(
      mu        = mean(value_raw, na.rm = TRUE),
      sd        = sd(value_raw, na.rm = TRUE),
      z_recalc  = ifelse(is.na(value_raw) | sd == 0, NA_real_, (value_raw - mu) / sd)
    ) %>%
    ungroup()

  z_diff <- recomputed %>%
    filter(!is.na(z_score) & !is.na(z_recalc)) %>%
    summarise(
      max_abs_diff = max(abs(z_score - z_recalc)),
      frac_bad = mean(abs(z_score - z_recalc) > 1e-6)
    )

  qc[[length(qc)+1]] <- qc_row(
    "B1 z_score recalculates correctly",
    ifelse(is.finite(z_diff$max_abs_diff) && z_diff$max_abs_diff <= 1e-5, "PASS", "WARN"),
    glue("max_abs_diff={signif(z_diff$max_abs_diff,3)}, frac_bad={round(100*z_diff$frac_bad,3)}%")
  )

  recomputed2 <- recomputed %>%
    mutate(
      score_unadj  = 100 * pnorm(z_score),
      score_recalc = case_when(
        direction == "higher_better" ~ score_unadj,
        direction == "lower_better"  ~ 100 - score_unadj,
        TRUE ~ NA_real_
      )
    )

  sdiff <- recomputed2 %>%
    filter(!is.na(score_100) & !is.na(score_recalc)) %>%
    summarise(
      max_abs_diff = max(abs(score_100 - score_recalc)),
      frac_bad = mean(abs(score_100 - score_recalc) > 1e-4)
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
# C) COMPOSITES: no composites in 2024 revision
# ============================================================
qc[[length(qc)+1]] <- qc_row(
  "C1 composite indicators check",
  "SKIP",
  paste(
    "No composite indicators defined in 2024 revision.",
    "All prior age-split composites replaced by single-variable ACS pulls.",
    "Re-enable this check if new composites are added to 03b."
  )
)

# ============================================================
# D) INDICES: uniqueness + recompute correctness
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

# Check expected 2024-revision indices are present in index_scores_long
expected_indices_2024 <- c(
  # CL
  "index_rel_living_arrangements", "index_eq_living_arrangements",
  "index_rel_community_resource",
  "index_rel_community_living", "index_eq_community_living",
  # CP
  "index_rel_tech",       "index_eq_tech",
  "index_rel_insurance",  "index_eq_insurance",
  "index_rel_education",  "index_eq_education",
  "index_rel_commute",    "index_eq_commute",
  "index_rel_safety",
  "index_rel_housing_support",
  "index_rel_community_participation", "index_eq_community_participation",
  # WE
  "index_rel_positive_work",          "index_eq_positive_work",
  "index_rel_negative_work",          "index_eq_negative_work",
  "index_rel_all_work_indicators",    "index_eq_all_work_indicators",
  "index_rel_housing_affordability",  "index_eq_housing_affordability",
  "index_rel_work_economic",          "index_eq_work_economic",
  # Overall
  "index_rel_adaparc", "index_eq_adaparc", "index_adaparc"
)

missing_2024_indices <- setdiff(expected_indices_2024, unique(idx$index_id))
qc[[length(qc)+1]] <- qc_row(
  "D3 all 2024-revision indices present in index_scores_long",
  ifelse(length(missing_2024_indices) == 0, "PASS", "FAIL"),
  ifelse(length(missing_2024_indices) == 0,
         glue("{length(expected_indices_2024)} expected indices all present"),
         paste(missing_2024_indices, collapse=", "))
)

# Spot-check: recompute key indices from dictionary + scored data
if (all(c("index_id","index_score_100") %in% names(idx))) {

  member_values <- bind_rows(
    scored %>% select(all_of(ID_COLS), member_id = indicator_id, value = score_100),
    idx    %>% select(all_of(ID_COLS), member_id = index_id,     value = index_score_100)
  )

  # Audit a mix of ACS-only and external-containing indices
  audit_targets <- c(
    "index_rel_commute",    "index_eq_commute",
    "index_rel_education",  "index_eq_education",
    "index_rel_tech",       "index_eq_tech",
    "index_rel_community_resource",
    "index_rel_insurance",  "index_eq_insurance",
    "index_rel_safety",
    "index_rel_housing_affordability",
    "index_rel_community_living",
    "index_eq_community_living",
    "index_rel_positive_work",
    "index_rel_negative_work"
  )

  dict_audit <- idx_dict %>% filter(index_id %in% audit_targets)

  if (nrow(dict_audit) > 0) {

    recompute_idx <- dict_audit %>%
      inner_join(member_values, by = c("indicator_id" = "member_id")) %>%
      group_by(across(all_of(ID_COLS)), index_id) %>%
      summarise(
        frac_present     = mean(!is.na(value)),
        min_frac_present = first(min_frac_present),
        val = ifelse(
          frac_present >= min_frac_present,
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
      filter(!is.na(index_score_100) & !is.na(val)) %>%
      summarise(
        n_compared = n(),
        max_abs_diff = max(abs(index_score_100 - val)),
        frac_bad = mean(abs(index_score_100 - val) > 1e-6)
      )

    qc[[length(qc)+1]] <- qc_row(
      "D4 recompute audit indices from dict matches output",
      ifelse(is.finite(maxdiff$max_abs_diff) && maxdiff$max_abs_diff <= 1e-5, "PASS", "WARN"),
      glue("n={maxdiff$n_compared}, max_abs_diff={signif(maxdiff$max_abs_diff,3)}, frac_bad={round(100*maxdiff$frac_bad,3)}%")
    )

  } else {
    qc[[length(qc)+1]] <- qc_row("D4 recompute key indices", "SKIP", "No audit targets found in dictionary")
  }
}

# ============================================================
# E) WIDE EXPORT: key uniqueness, expected columns, USA row
# ============================================================
dup_wide <- wide %>%
  count(across(all_of(ID_COLS))) %>%
  filter(n > 1)

qc[[length(qc)+1]] <- qc_row(
  "E1 wide uniqueness (GEOID-year)",
  ifelse(nrow(dup_wide) == 0, "PASS", "FAIL"),
  ifelse(nrow(dup_wide) == 0, "", glue("Duplicates: {nrow(dup_wide)}"))
)

# Check that all 2024-revision indices appear as columns in wide output
missing_wide <- setdiff(expected_indices_2024, names(wide))

qc[[length(qc)+1]] <- qc_row(
  "E2 all 2024-revision index columns present in wide export",
  ifelse(length(missing_wide) == 0, "PASS", "FAIL"),
  ifelse(length(missing_wide) == 0,
         glue("{length(expected_indices_2024)} expected columns all present"),
         paste(missing_wide, collapse=", "))
)

# Check USA row exists and is reasonably complete
usa_row <- wide %>% filter(ABBR == "USA")

qc[[length(qc)+1]] <- qc_row(
  "E3 USA row exists in wide export",
  ifelse(nrow(usa_row) == 1, "PASS", "FAIL"),
  ifelse(nrow(usa_row) == 1,
         "USA row present",
         glue("Expected 1 USA row, found {nrow(usa_row)}"))
)

if (nrow(usa_row) == 1) {
  index_cols <- setdiff(names(wide), ID_COLS)
  usa_na_count <- sum(is.na(usa_row[index_cols]))
  usa_pct_complete <- round(100 * (1 - usa_na_count / length(index_cols)), 1)

  qc[[length(qc)+1]] <- qc_row(
    "E4 USA row completeness",
    ifelse(usa_pct_complete >= 95, "PASS", ifelse(usa_pct_complete >= 80, "WARN", "FAIL")),
    glue("USA has {usa_pct_complete}% of index scores ({length(index_cols) - usa_na_count}/{length(index_cols)})")
  )

  if (usa_na_count > 0) {
    na_cols <- index_cols[is.na(usa_row[index_cols])]
    qc[[length(qc)+1]] <- qc_row(
      "E5 USA NA indices (informational)",
      "INFO",
      paste(na_cols, collapse = ", ")
    )
  }
}

# Data completeness summary per category
cat_summary <- idx %>%
  filter(ABBR != "USA", index_type == "scorecard_index") %>%
  group_by(category, index_id) %>%
  summarise(
    n_states = n(),
    pct_non_na = round(100 * mean(!is.na(index_score_100)), 1),
    .groups = "drop"
  )

low_cat <- cat_summary %>% filter(pct_non_na < 90)
qc[[length(qc)+1]] <- qc_row(
  "E6 scorecard_index coverage by category (>90% non-NA for states)",
  ifelse(nrow(low_cat) == 0, "PASS", "WARN"),
  ifelse(nrow(low_cat) == 0,
         "All scorecard indices have >90% state coverage",
         paste(glue("{low_cat$index_id}: {low_cat$pct_non_na}%"), collapse = "; "))
)

# ============================================================
# WRITE REPORT
# ============================================================
qc_df <- bind_rows(qc)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(qc_df, OUT_CSV)

print(qc_df)
cat("\nQC report saved to:\n", OUT_CSV, "\n")
