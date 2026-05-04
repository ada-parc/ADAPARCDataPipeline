# ============================================================
# STEP 3b (FINAL): Build composite indicators from scored indicators
#   - Guarantees 1 row per GEOID-year per composite indicator_id
#   - Uses score_100 (already direction-adjusted)
#   - Hard-stops if duplicates remain
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(glue)
})

IN_PATH  <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing/indicator_scored_long.csv"
OUT_DIR  <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard/scorecard_data/processing"
OUT_PATH <- file.path(OUT_DIR, "indicator_scored_long_with_composites.csv")

ID_COLS <- c("GEOID","NAME","ABBR","year")

COMPOSITES <- tibble::tribble(
  ~new_indicator_id,        ~new_indicator_label,                                ~members,                                                                  ~min_frac_present,
  "rel_score_poverty",      "Living Below Poverty (All Ages): Relative",          "rel_score_poverty_under18|rel_score_poverty_18to64|rel_score_poverty_65plus", 1.0,
  "eq_score_poverty",       "Living Below Poverty (All Ages): Equity",            "eq_score_poverty_under18|eq_score_poverty_18to64|eq_score_poverty_65plus",     1.0,

  "rel_score_nohealthins",  "No Health Insurance (19–64 & 65+): Relative",        "rel_score_19to64_nohealthins|rel_score_65plus_nohealthins",                    1.0,
  "eq_score_nohealthins",   "No Health Insurance (19–64 & 65+): Equity",          "eq_score_19to64_nohealthins|eq_score_65plus_nohealthins",                      1.0,

  "rel_score_privatehealth","Private Health Insurance (19–64 & 65+): Relative",   "rel_score_19to64_privatehealth|rel_score_65plus_privatehealth",                1.0,
  "eq_score_privatehealth", "Private Health Insurance (19–64 & 65+): Equity",     "eq_score_19to64_privatehealth|eq_score_65plus_privatehealth",                  1.0,

  "rel_score_publichealth", "Public Health Insurance (19–64 & 65+): Relative",    "rel_score_19to64_publichealth|rel_score_65plus_publichealth",                  1.0,
  "eq_score_publichealth",  "Public Health Insurance (19–64 & 65+): Equity",      "eq_score_19to64_publichealth|eq_score_65plus_publichealth",                    1.0,

  "rel_score_nursing",      "Nursing Home (18–64 & 65+): Relative",               "rel_score_nursing_18to64|rel_score_nursing_65plus",                            1.0,
  "eq_score_nursing",       "Nursing Home (18–64 & 65+): Equity",                 "eq_score_nursing_18to64|eq_score_nursing_65plus",                              1.0
)

df <- readr::read_csv(IN_PATH, show_col_types = FALSE)

need <- c(ID_COLS, "indicator_id", "category", "dimension", "indicator_label", "score_100")
miss <- setdiff(need, names(df))
if (length(miss) > 0) stop("Input missing columns:\n- ", paste(miss, collapse="\n- "))

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
    category = dplyr::first(category),
    dimension = dplyr::first(dimension),
    .groups = "drop"
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
    indicator_id = new_indicator_id,
    category,
    dimension,
    indicator_label = new_indicator_label,
    score_100,
    transform = "none",
    weight = 1
  )

# Remove any accidental pre-existing composite ids from df (defensive)
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

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, OUT_PATH)

message(glue("Added composite indicator_ids: {length(unique(comp_scores$indicator_id))}"))
message(glue("Saved:\n{OUT_PATH}"))
