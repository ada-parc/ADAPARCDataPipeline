# ============================================================
# STEP 3b: Build composite indicators from scored indicators
#
# 2024 Revision Notes:
#   All composite indicators from the prior version have been REMOVED.
#   The composites previously handled here were age-split aggregations:
#
#     REMOVED composites (old age-split → single aggregate):
#       rel_score_poverty     <- poverty_under18 + poverty_18to64 + poverty_65plus
#       eq_score_poverty      <- (same)
#       rel_score_nursing     <- nursing_18to64 + nursing_65plus
#       eq_score_nursing      <- (same)
#       rel_score_nohealthins <- 19to64_nohealthins + 65plus_nohealthins
#       eq_score_nohealthins  <- (same)
#       rel_score_privatehealth <- 19to64_privatehealth + 65plus_privatehealth
#       eq_score_privatehealth  <- (same)
#       rel_score_publichealth  <- 19to64_publichealth + 65plus_publichealth
#       eq_score_publichealth   <- (same)
#
#   These have been replaced by:
#     - Single all-ages poverty variable (pct_cni_dis_poverty from B18130)
#     - Single all-ages health insurance variables (from B18135, no age restriction)
#     - 18-64 only nursing home variable (no 65+ component)
#     - Medicare and Medicaid as separate new indicators (also from B18135)
#
#   As a result, this script now passes indicator_scored_long through
#   unchanged (no composites to build). It is retained as a pipeline
#   step for future use if new composite indicators are needed.
#
#   TO ADD A COMPOSITE: add a row to the COMPOSITES tribble below,
#   following the same schema:
#     new_indicator_id     — the composite indicator_id to create
#     new_indicator_label  — human-readable label
#     members              — pipe-separated member indicator_ids
#     min_frac_present     — fraction of members required (0–1)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(glue)
})

IN_PATH  <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/processing/indicator_scored_long.csv"
OUT_DIR  <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/processing"
OUT_PATH <- file.path(OUT_DIR, "indicator_scored_long_with_composites.csv")

ID_COLS <- c("GEOID","NAME","ABBR","year")

# ============================================================
# COMPOSITES TABLE
# Add any future composite indicators here.
# Currently empty — all prior age-split composites removed in 2024 revision.
# ============================================================
COMPOSITES <- tibble::tribble(
  ~new_indicator_id, ~new_indicator_label, ~members, ~min_frac_present
  # Example format (do not uncomment — illustration only):
  # "rel_score_example", "Example Composite: Relative", "rel_score_a|rel_score_b", 1.0
)

# ============================================================
# LOAD & VALIDATE
# ============================================================
df <- readr::read_csv(IN_PATH, show_col_types = FALSE)

need <- c(ID_COLS, "indicator_id", "category", "dimension", "indicator_label", "score_100")
miss <- setdiff(need, names(df))
if (length(miss) > 0) stop("Input missing columns:\n- ", paste(miss, collapse="\n- "))

# ============================================================
# BUILD COMPOSITES (skip if none defined)
# ============================================================
if (nrow(COMPOSITES) == 0) {

  message("No composite indicators defined. Passing indicator_scored_long through unchanged.")
  out <- df

} else {

  # Expand members
  comp_long <- COMPOSITES %>%
    mutate(member_indicator_id = str_split(members, "\\|")) %>%
    tidyr::unnest(member_indicator_id) %>%
    select(new_indicator_id, new_indicator_label, member_indicator_id, min_frac_present)

  # Validate member existence
  missing_members <- setdiff(unique(comp_long$member_indicator_id), unique(df$indicator_id))
  if (length(missing_members) > 0) {
    stop("Composite members not found in scored data:\n- ", paste(missing_members, collapse="\n- "))
  }

  # Infer category/dimension for each composite from its first member
  member_meta <- df %>% select(indicator_id, category, dimension) %>% distinct()

  comp_meta <- comp_long %>%
    left_join(member_meta, by = c("member_indicator_id" = "indicator_id")) %>%
    group_by(new_indicator_id, new_indicator_label, min_frac_present) %>%
    summarise(
      category  = dplyr::first(category),
      dimension = dplyr::first(dimension),
      .groups   = "drop"
    )

  # Build composite rows (ONE ROW PER GEOID-year per composite)
  comp_scores <- df %>%
    select(all_of(ID_COLS), indicator_id, score_100) %>%
    inner_join(comp_long, by = c("indicator_id" = "member_indicator_id")) %>%
    group_by(across(all_of(ID_COLS)), new_indicator_id, new_indicator_label, min_frac_present) %>%
    reframe(
      frac_present = mean(!is.na(score_100)),
      score_100 = {
        fp <- mean(!is.na(score_100))
        if (is.na(fp) || fp < first(min_frac_present)) NA_real_ else mean(score_100, na.rm = TRUE)
      }
    ) %>%
    ungroup() %>%
    left_join(comp_meta, by = c("new_indicator_id","new_indicator_label","min_frac_present")) %>%
    transmute(
      !!!rlang::syms(ID_COLS),
      indicator_id    = new_indicator_id,
      category,
      dimension,
      indicator_label = new_indicator_label,
      score_100,
      transform       = "none",
      weight          = 1
    )

  # Remove any pre-existing composite ids from df (defensive)
  composite_ids <- unique(COMPOSITES$new_indicator_id)
  df_clean <- df %>% filter(!(indicator_id %in% composite_ids))

  out <- bind_rows(df_clean, comp_scores)

  # HARD ASSERT: no duplicates for composite ids
  dup_check <- out %>%
    filter(indicator_id %in% composite_ids) %>%
    count(GEOID, year, indicator_id) %>%
    filter(n > 1)

  if (nrow(dup_check) > 0) {
    print(dup_check)
    stop("Composite build produced duplicates. See printed dup_check.")
  }

  message(glue("Added composite indicator_ids: {length(unique(comp_scores$indicator_id))}"))
}

# ============================================================
# SAVE
# ============================================================
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, OUT_PATH)

message(glue("Total indicator_ids in output: {n_distinct(out$indicator_id)}"))
message(glue("Saved:\n{OUT_PATH}"))
