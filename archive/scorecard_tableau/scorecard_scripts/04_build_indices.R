# ============================================================
# STEP 4 (RECURSIVE): Build scorecard indices + category indices (multi-level)
#   - Builds scorecard_index from indicators
#   - Then repeatedly builds category_index as long as new indices can be formed
#   - This supports ADA-PARC rollups that depend on other category indices
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(glue)
})

# -----------------------------
# PATHS
# -----------------------------
SCORED_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing/indicator_scored_long_with_composites.csv"
INDEX_DICT_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/dictionary/scorecard_index_dictionary.csv"

OUT_DIR <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing"
OUT_PATH <- file.path(OUT_DIR, "index_scores_long.csv")

ID_COLS <- c("GEOID", "NAME", "ABBR", "year")

# -----------------------------
# HELPERS
# -----------------------------
weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Build a set of indices from a dict layer using available members in value_long
# value_long schema: ID_COLS + member_id + value
build_indices_from_dict <- function(value_long, dict_layer) {

  keys <- value_long %>% distinct(across(all_of(ID_COLS)))

  expanded <- keys %>%
    tidyr::crossing(
      dict_layer %>%
        select(index_id, index_label, index_type, category, dimension,
               indicator_id, weight, agg_method, min_frac_present)
    )

  joined <- expanded %>%
    left_join(
      value_long %>% select(all_of(ID_COLS), member_id, value),
      by = c(setNames(ID_COLS, ID_COLS), "indicator_id" = "member_id")
    )

  out <- joined %>%
    group_by(across(all_of(ID_COLS)), index_id, index_label, index_type, category, dimension) %>%
    summarise(
      n_members = dplyr::n(),
      frac_present = mean(!is.na(value)),
      index_score_100 = ifelse(
        frac_present >= first(min_frac_present),
        weighted_mean_safe(value, weight),
        NA_real_
      ),
      .groups = "drop"
    )

  out
}

# Determine which category indices are computable given available member ids
computable_category_indices <- function(dict_category, available_member_ids, already_built_ids) {
  candidates <- dict_category %>%
    filter(!(index_id %in% already_built_ids)) %>%
    group_by(index_id) %>%
    summarise(
      ok = all(indicator_id %in% available_member_ids),
      .groups = "drop"
    ) %>%
    filter(ok) %>%
    pull(index_id)

  candidates
}

# -----------------------------
# LOAD
# -----------------------------
scored <- readr::read_csv(SCORED_PATH, show_col_types = FALSE)
dict <- readr::read_csv(INDEX_DICT_PATH, show_col_types = FALSE)

# Normalize types
dict <- dict %>%
  mutate(
    index_type = str_trim(index_type),
    indicator_id = str_trim(indicator_id),
    index_id = str_trim(index_id),
    weight = as.numeric(weight),
    min_frac_present = as.numeric(min_frac_present)
  )

# -----------------------------
# VALIDATION: avoid silent drops
# -----------------------------
missing_scorecard_members <- setdiff(
  dict$indicator_id[dict$index_type == "scorecard_index"],
  scored$indicator_id
)
if (length(missing_scorecard_members) > 0) {
  stop("scorecard_index dictionary references missing indicator_id(s):\n- ",
       paste(unique(missing_scorecard_members), collapse = "\n- "))
}

# -----------------------------
# START: available member values = indicators
# -----------------------------
value_long <- scored %>%
  select(all_of(ID_COLS), indicator_id, score_100) %>%
  rename(member_id = indicator_id, value = score_100)

# -----------------------------
# STAGE 1: build all scorecard_index
# -----------------------------
dict_scorecard <- dict %>% filter(index_type == "scorecard_index")

scorecard_indices <- build_indices_from_dict(value_long, dict_scorecard) %>%
  mutate(index_level = "scorecard")

# Add scorecard indices to available member values
value_long <- bind_rows(
  value_long,
  scorecard_indices %>%
    select(all_of(ID_COLS), index_id, index_score_100) %>%
    rename(member_id = index_id, value = index_score_100)
)

# -----------------------------
# STAGE 2+: iteratively build category_index (multi-level)
# -----------------------------
dict_category <- dict %>% filter(index_type == "category_index")

built_category <- tibble()
already_built <- character(0)

repeat {
  avail_ids <- unique(value_long$member_id)

  next_ids <- computable_category_indices(dict_category, avail_ids, already_built)

  if (length(next_ids) == 0) break

  dict_next <- dict_category %>% filter(index_id %in% next_ids)

  next_indices <- build_indices_from_dict(value_long, dict_next) %>%
    mutate(index_level = "category")

  # Append
  built_category <- bind_rows(built_category, next_indices)
  already_built <- union(already_built, next_ids)

  # Add to available values for subsequent rollups (e.g., ADA-PARC)
  value_long <- bind_rows(
    value_long,
    next_indices %>%
      select(all_of(ID_COLS), index_id, index_score_100) %>%
      rename(member_id = index_id, value = index_score_100)
  )
}

# -----------------------------
# COMBINE & SAVE
# -----------------------------
out <- bind_rows(scorecard_indices, built_category) %>%
  select(all_of(ID_COLS), index_id, index_label, index_type, category, dimension,
         index_level, n_members, frac_present, index_score_100) %>%
  arrange(dimension, category, index_level, index_id, year, GEOID)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, OUT_PATH)

message(glue("Saved index scores to:\n{OUT_PATH}"))
message(glue("Unique indices created: {n_distinct(out$index_id)}"))
