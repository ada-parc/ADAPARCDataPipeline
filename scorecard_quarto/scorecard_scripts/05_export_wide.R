# ============================================================
# STEP 5: Export wide index scores file from index_scores_long
#   Input:  index_scores_long.csv (long: one row per GEOID-year-index)
#   Output: index_scores_wide.csv (wide: one row per GEOID-year)
#
# 2024 Revision Notes:
#   - Added USA average computation: for any index where the USA row
#     (GEOID=000) has NA, the mean of all non-USA state scores is used.
#     This ensures the USA row provides a valid national baseline for
#     comparison in scorecards, even for indices built entirely from
#     external data sources (FBI, HUD, SSA, BEA, PUMS) that lack a
#     national aggregate in their source files.
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

IN_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/processing/index_scores_long.csv"

OUT_DIR <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/final"
OUT_PATH <- file.path(OUT_DIR, "index_scores_wide.csv")

ID_COLS <- c("GEOID", "NAME", "ABBR", "year")

USA_GEOID <- "000"

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
# USA AVERAGE: fill NA index scores for the USA row
#
# For indices where the USA row has NA (typically because all member
# indicators come from external sources without a national aggregate),
# compute the mean of all non-USA state scores as the US average.
# This keeps the USA row usable as a reference baseline.
# -----------------------------
state_means <- df %>%
  filter(GEOID != USA_GEOID) %>%
  group_by(index_id, year) %>%
  summarise(
    usa_fill = mean(index_score_100, na.rm = TRUE),
    n_states_used = sum(!is.na(index_score_100)),
    .groups = "drop"
  )

usa_rows <- df %>% filter(GEOID == USA_GEOID)

usa_filled <- usa_rows %>%
  left_join(state_means, by = c("index_id", "year")) %>%
  mutate(
    index_score_100 = ifelse(is.na(index_score_100), usa_fill, index_score_100)
  )

n_filled <- sum(is.na(usa_rows$index_score_100) & !is.na(usa_filled$index_score_100))
if (n_filled > 0) {
  filled_ids <- usa_filled %>%
    filter(is.na(usa_rows$index_score_100) & !is.na(index_score_100)) %>%
    pull(index_id)
  message(glue("USA averages filled for {n_filled} indices: {paste(filled_ids, collapse = ', ')}"))
}

usa_filled <- usa_filled %>%
  select(-usa_fill, -n_states_used)

# Replace USA rows in the main dataframe
df <- bind_rows(
  df %>% filter(GEOID != USA_GEOID),
  usa_filled
)

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

message(glue("Saved wide index scores:\n{OUT_PATH}"))
message(glue("Rows: {nrow(wide)} | Index columns: {ncol(wide) - length(ID_COLS)}"))
