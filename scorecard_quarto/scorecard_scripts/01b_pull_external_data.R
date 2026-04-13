# ============================================================
# STEP 1b: External Data Pull — Non-ACS External Sources
#
# PURPOSE:
#   Pulls all data sources that are NOT in national_data.Rds.
#   After each section runs, the INTEGRATION block at the bottom
#   merges new variables into national_data.Rds.
#
# NOTE ON ACS VARIABLES (April 2026):
#   Section A (ACS needs-pull via tidycensus) has been REMOVED.
#   All ACS-sourced indicators — including public health insurance
#   (B18135_017E), unemployment rate (B18131), and poverty (B18130
#   age-split counts) — are already present in national_data.Rds
#   from the main pipeline ACS run. Specifically:
#     pct_cni_19to64_dis_publichealth    (B18135_017E, already in national_data)
#     pct_cni_19to64_nodis_publichealth  (B18135_022E, already in national_data)
#     pct_cni_16plus_unemployed_dis      (B18131, already in national_data)
#     pct_cni_16plus_nodis_unemployed    (B18131 PWOD, already in national_data)
#     pop_cni_*_poverty_dis/nodis        (B18130 age-split counts, in national_data)
#   All-ages poverty (pct_cni_dis_poverty) is derived from age-split
#   count columns in script 02 — no separate pull needed.
#   If ACS pulls ever need to be re-run or extended, add them to the
#   main pipeline (ADAPARCDataPipeline), not here.
#
# TO RUN A SECTION:
#   Flip its  if (FALSE)  to  if (TRUE),  then source the whole file.
#   Each section saves a CSV to scorecard_data/external/.
#   After all desired sections have run, flip the INTEGRATION block to TRUE.
#
# SECTIONS:
#   B  — FBI Crime Data Explorer
#   C  — HUD POSH (HCV + Public Housing)
#   D  — SSA SSI + HUD Fair Market Rents
#   E  — KFF / CMS HCBS data
#   F  — BEA RPP + ACS PUMS income
#   G  — ACS PUMS housing burden
#   INTEGRATION — merge all completed CSVs into national_data.Rds
#
# CURRENT STATUS (April 2026):
#   Sections B–G: Templates — external downloads/API setup required
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(glue)
})

# ── Paths & config ────────────────────────────────────────────────────────────

NATIONAL_DATA_PATH <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/analysis/data/final/national_data.Rds"
EXT_DIR            <- "/Users/heener/Documents/GitHub/ADAPARCDataPipeline/scorecard_quarto/scorecard_data/external"

dir.create(EXT_DIR, recursive = TRUE, showWarnings = FALSE)

ID_COLS <- c("GEOID", "NAME")


# ============================================================
# SECTION B: FBI NIBRS 2024 — Property Crime + Violent Crime Rates
#
#   Source files (already downloaded to external_data/):
#     NIBRS_Table_27_Crimes_Against_Persons_Offenses_Offense_Category_by_State_2024.xlsx
#     NIBRS_Table_28_Crimes_Against_Property_Offenses_Offense_Category_by_State_2024.xlsx
#   Source: FBI Crime Data Explorer — crime-data-explorer.app.cloud.gov
#   Data year: 2024 (released 2025)
#
#   Indicators produced:
#     property_crime_rate   — property crime offenses per 100k covered population
#     violent_crime_rate    — violent crime offenses per 100k covered population
#
#   Definitions:
#     Property crime = Burglary + Larceny/Theft + Motor Vehicle Theft
#                      (Arson excluded — consistent with legacy UCR definition)
#     Violent crime  = Assault Offenses + Homicide Offenses + Sex Offenses [T27]
#                      + Robbery [T28]
#                      (Note: NIBRS Assault Offenses includes all assault levels;
#                       Sex Offenses is broader than legacy UCR rape definition)
#
#   Rate denominator = Population Covered (agencies reporting to NIBRS), NOT
#   total state population. Rates reflect crime in areas that participate in NIBRS.
#
#   2024 NIBRS coverage notes (low-participation states):
#     FL: ~62% of state population covered (159 agencies)
#     PA: ~51% of state population covered (202 agencies)
#     AK: ~61% of state population covered (32 agencies)
#   These states rely more heavily on FBI estimation procedures. Flag in output
#   if using rates for policy-sensitive comparisons.
# ============================================================

if (TRUE) {

  library(readxl)

  RAW_EXT_DIR <- file.path(
    dirname(dirname(EXT_DIR)),   # up from scorecard_data/external → scorecard_quarto
    "external_data"
  )

  # ── Load Table 27: Crimes Against Persons ──────────────────────────────────
  # Rows 1-6 are title/header rows; row 7 is the national Total; rows 8-58 are states
  t27 <- read_excel(
    file.path(RAW_EXT_DIR,
      "NIBRS_Table_27_Crimes_Against_Persons_Offenses_Offense_Category_by_State_2024.xlsx"),
    sheet = 1,
    skip  = 6,
    col_names = c("state", "n_agencies", "pop_covered", "total_offenses",
                  "assault", "homicide", "human_trafficking",
                  "kidnapping", "sex_offenses")
  ) %>%
    filter(!is.na(state), state != "Total") %>%
    select(state, pop_covered, assault, homicide, sex_offenses) %>%
    mutate(across(c(pop_covered, assault, homicide, sex_offenses), as.numeric))

  # ── Load Table 28: Crimes Against Property ─────────────────────────────────
  t28 <- read_excel(
    file.path(RAW_EXT_DIR,
      "NIBRS_Table_28_Crimes_Against_Property_Offenses_Offense_Category_by_State_2024.xlsx"),
    sheet = 1,
    skip  = 6,
    col_names = c("state", "n_agencies", "pop_covered", "total_offenses",
                  "arson", "bribery", "burglary", "counterfeiting",
                  "vandalism", "embezzlement", "extortion", "fraud",
                  "larceny_theft", "mv_theft", "robbery", "stolen_property")
  ) %>%
    filter(!is.na(state), state != "Total") %>%
    select(state, burglary, larceny_theft, mv_theft, robbery) %>%
    mutate(across(c(burglary, larceny_theft, mv_theft, robbery), as.numeric))

  # ── Join tables and compute rates ──────────────────────────────────────────
  crime_wide <- t27 %>%
    left_join(t28, by = "state") %>%
    mutate(
      violent_offenses  = assault + homicide + sex_offenses + robbery,
      property_offenses = burglary + larceny_theft + mv_theft,
      # Rates per 100,000 covered population
      violent_crime_rate  = violent_offenses  / pop_covered * 1e5,
      property_crime_rate = property_offenses / pop_covered * 1e5,
      # Flag states with notably low 2024 NIBRS participation
      fbi_coverage_note = case_when(
        state == "Florida"       ~ "~62% pop. covered (159 agencies, NIBRS 2024)",
        state == "Pennsylvania"  ~ "~51% pop. covered (202 agencies, NIBRS 2024)",
        state == "Alaska"        ~ "~61% pop. covered (32 agencies, NIBRS 2024)",
        TRUE ~ NA_character_
      )
    )

  # ── Map state full names → ABBR ────────────────────────────────────────────
  # R built-in state.name/state.abb; DC added manually
  # Puerto Rico is not in FBI NIBRS data — will remain NA after join
  state_xwalk <- tibble(
    state = c(state.name, "District of Columbia"),
    ABBR  = c(state.abb,  "DC")
  )

  crime_out <- crime_wide %>%
    left_join(state_xwalk, by = "state") %>%
    select(ABBR, property_crime_rate, violent_crime_rate, fbi_coverage_note)

  # QC: flag any unmatched state names
  missing_abbr <- crime_out %>% filter(is.na(ABBR))
  if (nrow(missing_abbr) > 0) {
    warning("Unmatched state names (no ABBR assigned): ",
            paste(missing_abbr$state, collapse = ", "))
  }

  out_path <- file.path(EXT_DIR, "fbi_crime_2024.csv")
  readr::write_csv(crime_out, out_path)
  message("Saved FBI NIBRS 2024 crime rates to: ", out_path)
  message("States in output: ", sum(!is.na(crime_out$ABBR)))

}  # end B


# ============================================================
# SECTION C: HUD POSH — HCV + Public Housing
#   Source file: external_data/POSH_2025.csv
#     (downloaded from huduser.gov/portal/datasets/assthsg.html
#      → Picture of Subsidized Households → State Summary → 2025)
#
#   Indicators produced:
#     hcv_hoh        — HCV occupied units / disabled pop  (CL)
#     pubhousing_hoh — PH occupied units  / disabled pop  (CL)
#
#   OLD version: no disability filter on HoH (all occupied units used).
#   CL and CP use identical data — one pull, four column aliases.
#
#   Denominator: pop_cni_total_dis (ACS total civilian non-inst. pop
#   with a disability by state — already in national_data).
#
#   Rate interpretation: vouchers/units per person with a disability
#   in the state.  Higher = better access.
# ============================================================

if (TRUE) {

  national_data <- readRDS(NATIONAL_DATA_PATH)

  # ── Load POSH state-summary file ───────────────────────────────────────────
  # File structure: one row per state × program; three programs per state:
  #   "Summary of All HUD Programs", "Public Housing", "Housing Choice Vouchers"
  # Key columns used:
  #   State            — 2-letter state abbreviation (ABBR)
  #   Program label    — program name string
  #   # Occupied Units — actual households currently receiving assistance

  posh_raw <- readr::read_csv(
    file.path(RAW_EXT_DIR, "POSH_2025.csv"),
    show_col_types = FALSE
  ) %>%
    rename(
      ABBR          = State,
      program_label = `Program label`,
      occupied_hhs  = `# Occupied Units`
    ) %>%
    # Drop the summary row; keep only program-level rows
    filter(program_label != "Summary of All HUD Programs") %>%
    select(ABBR, program_label, occupied_hhs) %>%
    mutate(occupied_hhs = as.numeric(occupied_hhs))

  # ── Split into HCV and Public Housing ─────────────────────────────────────
  hcv <- posh_raw %>%
    filter(str_detect(program_label, "Housing Choice")) %>%
    select(ABBR, hcv_hhs = occupied_hhs)

  ph <- posh_raw %>%
    filter(str_detect(program_label, "Public Housing")) %>%
    select(ABBR, ph_hhs = occupied_hhs)

  # ── Pull denominator from national_data ────────────────────────────────────
  # pop_cni_total_dis = ACS total civilian non-institutionalized population
  # with a disability, by state.  Already present in national_data.
  if (!"pop_cni_total_dis" %in% names(national_data)) {
    stop(
      "pop_cni_total_dis not found in national_data.\n",
      "Available pop_dis columns: ",
      paste(names(national_data)[str_detect(names(national_data), "pop.*dis|dis.*pop")],
            collapse = ", ")
    )
  }

  dis_pop <- national_data %>%
    select(ABBR, dis_pop = pop_cni_total_dis)

  # ── Compute rates and build output ─────────────────────────────────────────
  hud_rates <- hcv %>%
    full_join(ph,      by = "ABBR") %>%
    left_join(dis_pop, by = "ABBR") %>%
    mutate(
      # Rate: occupied units per person with a disability
      # CL indicators (community living index)
      hcv_hoh        = hcv_hhs / dis_pop,
      pubhousing_hoh = ph_hhs  / dis_pop
    ) %>%
    select(ABBR,
           hcv_hoh, pubhousing_hoh)

  # ── QC ─────────────────────────────────────────────────────────────────────
  message("HUD POSH 2025 — states with HCV data:          ", sum(!is.na(hud_rates$hcv_hoh)))
  message("HUD POSH 2025 — states with Public Housing data: ", sum(!is.na(hud_rates$pubhousing_hoh)))

  missing_hcv <- hud_rates %>% filter(is.na(hcv_hoh)) %>% pull(ABBR)
  missing_ph  <- hud_rates %>% filter(is.na(pubhousing_hoh)) %>% pull(ABBR)
  if (length(missing_hcv) > 0) message("  Missing HCV:           ", paste(missing_hcv,  collapse = ", "))
  if (length(missing_ph)  > 0) message("  Missing Public Housing: ", paste(missing_ph,   collapse = ", "))

  # ── Save ───────────────────────────────────────────────────────────────────
  out_path <- file.path(EXT_DIR, "hud_posh_2025.csv")
  readr::write_csv(hud_rates, out_path)
  message("Saved HUD POSH rates to: ", out_path)

}  # end C


# ============================================================
# SECTION D: SSA SSI + HUD Fair Market Rents
#
#   Source files (already in external_data/):
#     ssi_asr24.xlsx         — SSA SSI Annual Statistical Report 2024
#     FY25_FMRs_revised.csv  — HUD FY2025 FMRs (county-level, pre-converted
#                              from .xlsx via Python zipfile because the xlsx
#                              docProps/core.xml has a malformed ISO datetime
#                              that trips readxl; CSV is drop-in equivalent)
#
#   SSI source detail:
#     Table 39 — "Average monthly payment, by state or other area and
#     diagnostic group, December 2024 (in dollars)" — under-65 recipients.
#     The "Total" column reflects actual combined (federal + state supplement)
#     average payments. Using the average rather than the federal maximum
#     ($943/mo) because it captures real state supplement differences and
#     represents what recipients actually receive.  States with no supplement
#     average ~$700–$750 (partial benefit recipients pull the mean below max);
#     states with generous supplements (e.g. CA ~$916) show clearly higher.
#
#   FMR source detail:
#     County-level FY2025 FMR (revised). Aggregated to state using a
#     population-weighted mean of county 1BR FMRs (pop2022 weights).
#     Covers 50 states + DC + PR; territories (AS, GU, MP, VI) excluded.
#
#   Indicator produced:
#     ssi_pct_1br_rent — (avg_monthly_SSI / state_1BR_FMR) × 100
#     Dictionary direction: lower_better
#       Higher ratio = SSI covers more of FMR rent cost = more affordable
#       Lower ratio  = SSI purchasing power is weaker relative to local rents
#
#   NOTE on FY25_FMRs_revised.csv:
#     Run this one-time Python pre-conversion if the CSV is missing:
#       python3 -c "
#         import zipfile, xml.etree.ElementTree as ET, csv
#         with zipfile.ZipFile('external_data/FY25_FMRs_revised.xlsx','r') as z:
#             ss = [(''.join(t.text or '' for t in si.findall(
#                    './/{http://schemas.openxmlformats.org/spreadsheetml/2006/main}t')))
#                   for si in ET.parse(z.open('xl/sharedStrings.xml')).getroot()
#                          .findall('.//{http://schemas.openxmlformats.org/spreadsheetml/2006/main}si')]
#             ns = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
#             rows = [[ss[int(c.find(f'{{{ns}}}v').text)] if c.get('t')=='s'
#                      else (c.find(f'{{{ns}}}v').text or '')
#                      for c in r.findall(f'{{{ns}}}c')]
#                     for r in ET.parse(z.open('xl/worksheets/sheet1.xml'))
#                               .getroot().findall(f'.//{{{ns}}}row')]
#         csv.writer(open('external_data/FY25_FMRs_revised.csv','w')).writerows(rows)"
# ============================================================

if (TRUE) {

  library(readxl)

  RAW_EXT_DIR <- file.path(
    dirname(dirname(EXT_DIR)),
    "external_data"
  )

  # ── SSI: Table 39 average monthly payment (December 2024) ─────────────────
  ssi_raw <- read_excel(
    file.path(RAW_EXT_DIR, "ssi_asr24.xlsx"),
    sheet     = "Table 39",
    skip      = 4,          # skip: category header, table title, col header, blank row
    col_names = FALSE
  )

  # Column layout after skip (all col_names FALSE = ...1, ...2, etc.):
  #   ...1  = state name (for state rows) | NA for the "All areas" row
  #   ...2  = blank
  #   ...3  = area label ("All areas") | NA for state rows
  #   ...4  = Total avg monthly SSI payment (combined federal + supplement)
  #   ...5+ = by diagnostic group (not needed)
  #
  # Row 1 = "All areas" national; rows 2–52 = 50 states + DC (alphabetical)
  # Row 53+ = outlying areas, source notes

  state_xwalk <- tibble(
    state_full = c(state.name, "District of Columbia"),
    ABBR       = c(state.abb,  "DC")
  )

  ssi_states <- ssi_raw %>%
    slice(2:52) %>%                        # rows 2–52 = states + DC; skip "All areas"
    select(state_full = ...1, ssi_monthly = ...4) %>%
    mutate(
      state_full  = as.character(state_full),
      ssi_monthly = as.numeric(ssi_monthly)
    ) %>%
    filter(!is.na(state_full), !is.na(ssi_monthly)) %>%
    left_join(state_xwalk, by = "state_full")

  missing_ssi <- setdiff(state_xwalk$ABBR, ssi_states$ABBR)
  if (length(missing_ssi) > 0)
    message("  SSI — missing ABBR after join: ", paste(missing_ssi, collapse = ", "))

  # ── FMR: population-weighted state 1BR FMR from county-level data ──────────
  # FY25_FMRs_revised.csv was pre-converted from xlsx (see header note above).
  # Columns: stusps, state (FIPS), hud_area_code, countyname, county_town_name,
  #           metro, hud_area_name, fips, pop2022, fmr_0..fmr_4

  fmr_raw <- readr::read_csv(
    file.path(RAW_EXT_DIR, "FY25_FMRs_revised.csv"),
    col_types       = cols(.default = col_character()),
    show_col_types  = FALSE
  ) %>%
    mutate(
      pop2022 = as.numeric(pop2022),
      fmr_1   = as.numeric(fmr_1)
    ) %>%
    filter(
      stusps %in% c(state.abb, "DC", "PR"),  # 50 states + DC + PR
      !is.na(pop2022), pop2022 > 0,
      !is.na(fmr_1),   fmr_1   > 0
    )

  fmr_state <- fmr_raw %>%
    group_by(ABBR = stusps) %>%
    summarise(
      fmr_1br    = weighted.mean(fmr_1, pop2022, na.rm = TRUE),
      n_counties = n(),
      .groups    = "drop"
    )

  # ── Compute SSI / FMR ratio ────────────────────────────────────────────────
  ssi_fmr <- ssi_states %>%
    left_join(fmr_state, by = "ABBR") %>%
    mutate(
      ssi_pct_1br_rent = (ssi_monthly / fmr_1br) * 100
    ) %>%
    select(ABBR, ssi_monthly, fmr_1br, n_counties, ssi_pct_1br_rent)

  # ── QC ─────────────────────────────────────────────────────────────────────
  n_complete <- sum(!is.na(ssi_fmr$ssi_pct_1br_rent))
  message("SSI/FMR — states with complete ratio: ", n_complete, " of 51")

  missing_fmr <- ssi_fmr %>% filter(is.na(fmr_1br))    %>% pull(ABBR)
  missing_ssi <- ssi_fmr %>% filter(is.na(ssi_monthly)) %>% pull(ABBR)
  if (length(missing_fmr) > 0) message("  Missing FMR: ", paste(missing_fmr, collapse = ", "))
  if (length(missing_ssi) > 0) message("  Missing SSI: ", paste(missing_ssi, collapse = ", "))

  message("  SSI range:  $", round(min(ssi_fmr$ssi_monthly, na.rm = TRUE)),
          " – $", round(max(ssi_fmr$ssi_monthly, na.rm = TRUE)), "/month")
  message("  FMR range:  $", round(min(ssi_fmr$fmr_1br, na.rm = TRUE)),
          " – $", round(max(ssi_fmr$fmr_1br, na.rm = TRUE)), "/month (pop-wtd state avg)")
  message("  Ratio range: ",
          round(min(ssi_fmr$ssi_pct_1br_rent, na.rm = TRUE), 1), "% – ",
          round(max(ssi_fmr$ssi_pct_1br_rent, na.rm = TRUE), 1), "%")

  # ── Save ───────────────────────────────────────────────────────────────────
  out_path <- file.path(EXT_DIR, "ssi_fmr_ratio.csv")
  readr::write_csv(ssi_fmr, out_path)
  message("Saved SSI/FMR ratio to: ", out_path)

}  # end D


# ============================================================
# SECTION E: KFF — HCBS Spending Ratio + Per Individual
#
#   Source file: external_data/HCBS 2024.csv
#     KFF Custom State Report — Medicaid spending and enrollment data.
#     Spending data: FY2024 (fee-for-service only from CMS-64).
#     Enrollment data: 2023 (most recent available in this file).
#     Downloaded from: kff.org → Medicaid → State Indicator → Custom Report
#
#   Indicators produced:
#     hcbs_ratio
#       = Home Health & Personal Care (FFS) /
#         (Home Health & PC + Nursing Facilities + ICF-ID)  [all FFS]
#       Direction: higher_better (more spending in community vs. institution)
#       NOTE: FFS-only — excludes managed-care HCBS (no CMS-64 HCBS breakout
#       within managed care). This is consistent with KFF's LTSS methodology
#       and the CMS-64 expenditure source used in prior scorecard vintages.
#       States with high managed-care penetration (e.g. TX, NY, FL) will
#       understate their HCBS ratio; flag in QC if flagging logic is added.
#
#     hcbs_per_individual
#       = Home Health & Personal Care FFS spend / People with Disabilities
#         Medicaid enrollees (2023)
#       Direction: higher_better (more dollars flowing to community per person)
#       NOTE: denominator is ALL disabled Medicaid enrollees, not just HCBS
#       waiver recipients. No unduplicated HCBS participant count is available
#       in this file. This makes the metric comparable across states even if
#       waiver-participation reporting is inconsistent, but it will be lower
#       than a "per HCBS participant" figure.
#
#   File structure (wide format):
#     Row 1 = KFF title; Row 2 = blank; Row 3 = column headers
#       (first col blank/metric label, then "United States", "Alabama", ...)
#     Row 4+ = data rows (some are section-header rows with no values)
#
#   Target rows extracted:
#     "Nursing Facilities"             — FFS LTC institutional spend, FY2024
#     "ICF-ID"                         — FFS LTC institutional spend, FY2024
#     "Home Health and Personal Care"  — FFS HCBS proxy, FY2024
#     "People with Disabilities"       — Medicaid enrollees, 2023
# ============================================================

if (TRUE) {

  # ── Read HCBS CSV (wide format) ───────────────────────────────────────────
  # Use col_names = FALSE, set header from first data row after skip
  hcbs_raw <- readr::read_csv(
    file.path(RAW_EXT_DIR, "HCBS 2024.csv"),
    skip           = 2,        # skip KFF title (row 1) + blank (row 2)
    col_names      = FALSE,    # handle manually — first column name is empty
    col_types      = cols(.default = col_character()),
    show_col_types = FALSE
  )

  # Row 1 after skip = original row 3 = column header row
  # ("" | "United States" | "Alabama" | ... | "Wyoming")
  col_names_vec    <- as.character(hcbs_raw[1, ])
  col_names_vec[1] <- "metric"           # name the label column
  hcbs_data        <- hcbs_raw[-1, ]    # drop header row from data
  names(hcbs_data) <- col_names_vec

  # ── Add section-header tracking ───────────────────────────────────────────
  # Section header rows have blank/NA for all state columns (only the metric
  # label column has content).  We fill the section label downward so every
  # data row knows which section it belongs to.
  #
  # The KFF file contains "People with Disabilities" in FOUR different sections:
  #   1. Medicaid Spending per Enrollee (Full-Benefit)   — per-person $
  #   2. Medicaid Spending per Enrollee (Full or Partial) — per-person $
  #   3. Medicaid Spending by Enrollment Group            — total-spend $
  #   4. Medicaid Enrollees by Enrollment Group           — HEAD COUNT ← want
  # Likewise the spending rows (NF, ICF-ID, HCBS) live only in the
  # "Medicaid Spending on Long Term Care, Fee-for-Service" section.
  # Anchoring by section makes the pull unambiguous.

  hcbs_data_sectioned <- hcbs_data %>%
    mutate(
      # A section-header row has no state values — "United States" cell is blank
      is_header = is.na(`United States`) | trimws(`United States`) == "",
      section   = if_else(is_header, metric, NA_character_)
    ) %>%
    fill(section, .direction = "down") %>%
    filter(!is_header)

  # ── Extract target rows (section-aware) ───────────────────────────────────
  spending_rows <- hcbs_data_sectioned %>%
    filter(
      metric %in% c("Nursing Facilities", "ICF-ID",
                    "Home Health and Personal Care"),
      str_detect(section, fixed("Long Term Care", ignore_case = TRUE))
    )

  enrollee_rows <- hcbs_data_sectioned %>%
    filter(
      metric == "People with Disabilities",
      str_detect(section, fixed("Medicaid Enrollees by Enrollment Group",
                                ignore_case = TRUE))
    )

  hcbs_sub <- bind_rows(spending_rows, enrollee_rows)

  if (nrow(hcbs_sub) != 4)
    warning("Expected 4 target rows; found ", nrow(hcbs_sub),
            ". Check if KFF file structure has changed.\n",
            "  Sections found for spending rows: ",
            paste(unique(spending_rows$section), collapse = " | "), "\n",
            "  Sections found for enrollee rows: ",
            paste(unique(enrollee_rows$section), collapse = " | "))

  # ── State name → ABBR crosswalk ───────────────────────────────────────────
  # KFF uses "Dist. of Columbia" (abbreviated), not "District of Columbia"
  state_name_xwalk <- tibble(
    kff_name = c(state.name, "Dist. of Columbia"),
    ABBR     = c(state.abb,  "DC")
  )

  # ── Helper: parse KFF currency / count strings → numeric ─────────────────
  # Handles: "$1,234,567,890" → 1234567890  |  "12,342,700" → 12342700
  #          "N/A" → NA  |  negative values like "-$1,000" → -1000
  parse_kff_num <- function(x) {
    cleaned <- str_remove_all(x, "[$,]")   # strip $ and commas
    as.numeric(cleaned)                    # "N/A" and "" → NA
  }

  # ── Pivot long, join ABBR, parse values ───────────────────────────────────
  hcbs_long <- hcbs_sub %>%
    select(-section, -is_header) %>%   # drop tracking cols before pivot
    pivot_longer(
      cols      = -metric,
      names_to  = "kff_name",
      values_to = "value_chr"
    ) %>%
    filter(kff_name != "United States") %>%           # drop national row
    left_join(state_name_xwalk, by = "kff_name") %>%
    filter(!is.na(ABBR)) %>%                          # drop territories
    mutate(value = parse_kff_num(value_chr)) %>%
    select(ABBR, metric, value)

  hcbs_wide <- hcbs_long %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    rename(
      nf_spend      = `Nursing Facilities`,
      icf_spend     = `ICF-ID`,
      hcbs_spend    = `Home Health and Personal Care`,
      dis_enrollees = `People with Disabilities`
    )

  # ── Compute indicators ────────────────────────────────────────────────────
  hcbs_out <- hcbs_wide %>%
    mutate(
      # FFS LTSS only: HCBS / (HCBS + NF + ICF-ID)
      hcbs_ratio = hcbs_spend / (hcbs_spend + nf_spend + icf_spend),

      # HCBS FFS spend per disabled Medicaid enrollee (FY2024 $ / 2023 count)
      hcbs_per_individual = hcbs_spend / dis_enrollees
    ) %>%
    select(ABBR, hcbs_ratio, hcbs_per_individual)

  # ── QC ─────────────────────────────────────────────────────────────────────
  n_ratio <- sum(!is.na(hcbs_out$hcbs_ratio))
  n_per   <- sum(!is.na(hcbs_out$hcbs_per_individual))
  message("HCBS ratio — states with data: ", n_ratio, " of 51")
  message("HCBS per individual — states:  ", n_per,   " of 51")

  missing_states <- setdiff(c(state.abb, "DC"), hcbs_out$ABBR)
  if (length(missing_states) > 0)
    message("  Missing states: ", paste(missing_states, collapse = ", "))

  # Flag implausible ratio values (outside [0, 1])
  bad_ratio <- hcbs_out %>%
    filter(!is.na(hcbs_ratio),
           hcbs_ratio < 0 | hcbs_ratio > 1)
  if (nrow(bad_ratio) > 0)
    message("  WARN — ratio outside [0, 1] for: ",
            paste(bad_ratio$ABBR, collapse = ", "),
            " — likely negative spending entry in KFF file; review manually.")

  message("  HCBS ratio range: ",
          round(min(hcbs_out$hcbs_ratio, na.rm = TRUE), 3), " – ",
          round(max(hcbs_out$hcbs_ratio, na.rm = TRUE), 3))
  message("  HCBS per-individual range: $",
          round(min(hcbs_out$hcbs_per_individual, na.rm = TRUE)),
          " – $",
          round(max(hcbs_out$hcbs_per_individual, na.rm = TRUE)))

  # ── Save ───────────────────────────────────────────────────────────────────
  out_path <- file.path(EXT_DIR, "hcbs_kff_cms.csv")
  readr::write_csv(hcbs_out, out_path)
  message("Saved HCBS indicators to: ", out_path)

}  # end E


# ============================================================
# SECTION F: BEA RPP + ACS PUMS — COL-Adjusted Income
#
#   Source files:
#     RPP.csv  — BEA SARPP state-level Regional Price Parities, 2022–2024
#       (downloaded from bea.gov → Regional → Regional Price Parities → State)
#     ACS PUMS — via tidycensus (not yet downloaded; requires API + compute)
#
#   Indicators produced:
#     coladj_ind_income        (WE, positive_work, higher_better)
#     coladj_ind_income_nodis  (WE equity pair)
#     coladj_hh_income         (WE, positive_work, higher_better)
#     coladj_hh_income_nodis   (WE equity pair)
#
#   COL adjustment formula: nominal_income / (state_RPP / 100)
#     RPP = 100.0 → prices equal national average; >100 = higher cost of living.
#     Confirmed vintage: 2022 RPP (per project decision, April 2026).
#     RPP.csv also includes 2023 and 2024 — update rpp_year if switching.
#
#   TWO-PART STRUCTURE:
#     F1 — Read + save RPP lookup table (runs immediately; data is in hand)
#     F2 — PUMS income pull + join with RPP (if FALSE; requires tidycensus)
#
#   When F2 is ready to run:
#     1. Install tidycensus and set API key:
#          install.packages("tidycensus")
#          tidycensus::census_api_key("YOUR_KEY", install = TRUE)
#     2. Confirm ACS_YEAR matches the main pipeline vintage (default 2023)
#     3. Flip F2 block from if (FALSE) to if (TRUE)
#     4. Run — expect ~30–60 min for 50-state PUMS pull
#     5. After CSV is written, re-run INTEGRATION block
# ============================================================

# ── F1: BEA RPP — runs immediately ────────────────────────────────────────────
if (TRUE) {

  # RPP.csv structure:
  #   Row 1: "SARPP Regional price parities by state" (title)
  #   Row 2: "SARPP Regional price parities by state" (repeated)
  #   Row 3: "State or DC" (section label)
  #   Row 4: "GeoFIPS,GeoName,2022,2023,2024" (column header)
  #   Row 5: "00000,United States,100.000,..." (US total)
  #   Rows 6+: state rows ("01000,Alabama,88.021,...")

  # State FIPS crosswalk — GeoFIPS = state_fips_2digit × 1000 → 5-digit string
  rpp_fips_xwalk <- tibble(
    geo_fips = sprintf("%05d", c(
      1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50,
      51, 53, 54, 55, 56
    ) * 1000L),
    ABBR = c(
      "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
      "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
      "NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX",
      "UT","VT","VA","WA","WV","WI","WY"
    )
  )

  rpp_raw <- readr::read_csv(
    file.path(RAW_EXT_DIR, "RPP.csv"),
    skip           = 3,      # skip rows 1–3 (2 title rows + "State or DC" label)
    col_names      = TRUE,   # row 4 = header: GeoFIPS, GeoName, 2022, 2023, 2024
    col_types      = cols(GeoFIPS = col_character(), .default = col_double()),
    show_col_types = FALSE
  ) %>%
    rename(geo_name = GeoName, rpp_2022 = `2022`, rpp_2023 = `2023`, rpp_2024 = `2024`) %>%
    mutate(geo_fips = str_pad(GeoFIPS, 5, pad = "0")) %>%
    select(geo_fips, geo_name, rpp_2022, rpp_2023, rpp_2024) %>%
    inner_join(rpp_fips_xwalk, by = "geo_fips") %>%   # inner join drops US row + territories
    select(ABBR, geo_name, rpp_2022, rpp_2023, rpp_2024)

  message("RPP loaded — states: ", nrow(rpp_raw))
  message("  RPP 2022 range: ",
          round(min(rpp_raw$rpp_2022, na.rm = TRUE), 1), " – ",
          round(max(rpp_raw$rpp_2022, na.rm = TRUE), 1))

  missing_rpp <- setdiff(c(state.abb, "DC"), rpp_raw$ABBR)
  if (length(missing_rpp) > 0)
    message("  Missing from RPP: ", paste(missing_rpp, collapse = ", "))

  # Save RPP lookup (intermediate — used when PUMS income data is ready)
  rpp_out_path <- file.path(EXT_DIR, "rpp_by_state.csv")
  readr::write_csv(rpp_raw, rpp_out_path)
  message("Saved RPP by state to: ", rpp_out_path)
  message("NEXT: run F2 block (requires tidycensus + PUMS pull) to produce",
          " coladj_income_bea_rpp.csv")

}  # end F1


# ── F2: ACS PUMS income pull + COL adjustment — requires tidycensus ───────────
#   ACS_YEAR updated to 2024 to match main pipeline vintage (April 2026).
#   pums_hh is saved to a temp RDS (_pums_hh_temp.rds) so Section G can
#   reuse the same household pull without a redundant 30-60 min download.
if (TRUE) {

  library(tidycensus)
  census_api_key("0dbc4636e54dfa46e467982ee9404210fdf99c9e", install = FALSE, overwrite = TRUE)

  ACS_YEAR <- 2024      # matches main pipeline ACS vintage (national_data.Rds)

  # State FIPS → ABBR (2-digit string) — needed for tidycensus ST variable
  pums_fips_xwalk <- tibble(
    ST   = sprintf("%02d", c(
      1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50,
      51, 53, 54, 55, 56, 72
    )),
    ABBR = c(
      "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
      "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
      "NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX",
      "UT","VT","VA","WA","WV","WI","WY","PR"
    )
  )

  # ── Retry helper: pull one state at a time with exponential back-off ─────────
  # Census PUMS API drops connections on large national pulls ("Connection reset
  # by peer"). Pulling state-by-state with retries + a checkpoint file means
  # the script can resume exactly where it left off if it fails mid-run.
  pull_pums_safe <- function(state_abbr, variables, year, survey,
                             max_retries = 3, wait_base = 15) {
    for (attempt in seq_len(max_retries)) {
      result <- tryCatch(
        get_pums(
          variables   = variables,
          state       = state_abbr,
          year        = year,
          survey      = survey,
          rep_weights = NULL   # NULL = no replicate weights; FALSE is not valid and triggers internal level= error
        ),
        error = function(e) {
          message("  [", state_abbr, "] attempt ", attempt, "/", max_retries,
                  " failed: ", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(result)) return(result)
      if (attempt < max_retries) {
        wait_sec <- wait_base * attempt     # 15 s, then 30 s
        message("  Waiting ", wait_sec, " s before retry...")
        Sys.sleep(wait_sec)
      }
    }
    warning("[", state_abbr, "] all ", max_retries, " attempts failed — skipping")
    NULL
  }

  state_abbrs <- pums_fips_xwalk$ABBR

  # ── Individual earnings (person level) ────────────────────────────────────
  # DIS: 1 = with disability, 2 = without disability (NOT 0 — common mistake)
  # ESR: 1 = civilian employed not at work; 2 = civilian employed at work
  # PERNP: total personal earnings (wages + self-employment)
  # Filter: working-age (18–64), employed, positive earnings
  person_ckpt <- file.path(EXT_DIR, "_pums_person_ckpt.rds")

  if (file.exists(person_ckpt)) {
    person_results <- readRDS(person_ckpt)
    message("Resuming person pull — already have ",
            length(person_results), " / ", length(state_abbrs), " states cached")
  } else {
    person_results <- list()
  }

  message("Starting PUMS person-level pull — this may take 30–60 min...")
  for (abbr in state_abbrs) {
    if (abbr %in% names(person_results)) {
      message("  [", abbr, "] already cached — skipping")
      next
    }
    message("  Pulling person PUMS: ", abbr, " (",
            which(state_abbrs == abbr), "/", length(state_abbrs), ")")
    df <- pull_pums_safe(
      abbr,
      variables = c("DIS", "AGEP", "ESR", "PERNP"),
      year      = ACS_YEAR,
      survey    = "acs5"
    )
    if (!is.null(df)) {
      df <- df %>% mutate(ABBR = abbr)   # ST not in 2024 PUMS API; tag state here
      person_results[[abbr]] <- df
    }
    # Checkpoint every 10 states so progress survives a dropped connection
    if (length(person_results) %% 10 == 0) {
      saveRDS(person_results, person_ckpt)
      message("  Checkpoint saved (", length(person_results), " states complete)")
    }
  }
  saveRDS(person_results, person_ckpt)   # final save
  message("Person pull complete — ", length(person_results),
          " / ", length(state_abbrs), " states")

  pums_person <- bind_rows(person_results) %>%
    # Coerce to numeric — tidycensus can return character for some states when
    # all values are NA/missing, causing type conflicts after bind_rows
    mutate(
      AGEP  = as.integer(AGEP),
      PERNP = as.numeric(PERNP)
    ) %>%
    filter(
      AGEP >= 18, AGEP <= 64,
      ESR  %in% c("1", "2"),      # employed civilians (at work + not at work)
      PERNP > 0                   # positive personal earnings only
    )

  ind_income <- pums_person %>%
    group_by(ABBR) %>%
    summarise(
      median_pernp_dis   = median(PERNP[DIS == "1"], na.rm = TRUE),
      median_pernp_nodis = median(PERNP[DIS == "2"], na.rm = TRUE),
      n_obs_dis          = sum(DIS == "1"),
      n_obs_nodis        = sum(DIS == "2"),
      .groups = "drop"
    )

  # ── Household income (household level) ────────────────────────────────────
  # DIS at HH level: 1 = any member has disability; 2 = no member has disability
  # HINCP: household income (past 12 months); filter positive values
  # Note: get_pums with level="household" is a separate API call
  hh_ckpt <- file.path(EXT_DIR, "_pums_hh_ckpt.rds")
  hh_required_vars <- c("DIS", "HINCP", "TEN", "GRPIP", "OCPIP")

  if (file.exists(hh_ckpt)) {
    hh_results <- readRDS(hh_ckpt)
    # Check if cached states have all required variables (TEN/GRPIP/OCPIP
    # were added in Apr 2026 — stale caches only have DIS+HINCP)
    if (length(hh_results) > 0) {
      cached_cols <- names(hh_results[[1]])
      missing_vars <- setdiff(hh_required_vars, cached_cols)
      if (length(missing_vars) > 0) {
        message("Household checkpoint is stale (missing: ",
                paste(missing_vars, collapse = ", "),
                ") — deleting and re-pulling all states")
        file.remove(hh_ckpt)
        hh_results <- list()
        # Also remove the stale final cache so Section G doesn't use it
        pums_hh_cache_stale <- file.path(EXT_DIR, "_pums_hh_temp.rds")
        if (file.exists(pums_hh_cache_stale)) file.remove(pums_hh_cache_stale)
      } else {
        message("Resuming household pull — already have ",
                length(hh_results), " / ", length(state_abbrs), " states cached")
      }
    } else {
      hh_results <- list()
    }
  } else {
    hh_results <- list()
  }

  message("Starting PUMS household-level pull...")
  for (abbr in state_abbrs) {
    if (abbr %in% names(hh_results)) {
      message("  [", abbr, "] already cached — skipping")
      next
    }
    message("  Pulling household PUMS: ", abbr, " (",
            which(state_abbrs == abbr), "/", length(state_abbrs), ")")
    df <- pull_pums_safe(
      abbr,
      variables = c("DIS", "HINCP", "TEN", "GRPIP", "OCPIP"),
      year      = ACS_YEAR,
      survey    = "acs5"
    )
    if (!is.null(df)) {
      df <- df %>% mutate(ABBR = abbr)   # ST not in 2024 PUMS API; tag state here
      hh_results[[abbr]] <- df
    }
    if (length(hh_results) %% 10 == 0) {
      saveRDS(hh_results, hh_ckpt)
      message("  Checkpoint saved (", length(hh_results), " states complete)")
    }
  }
  saveRDS(hh_results, hh_ckpt)   # final save
  message("Household pull complete — ", length(hh_results),
          " / ", length(state_abbrs), " states")

  pums_hh <- bind_rows(hh_results) %>%
    mutate(
      HINCP = as.numeric(HINCP),
      GRPIP = as.numeric(GRPIP),
      OCPIP = as.numeric(OCPIP)
    )   # coerce — same type-conflict risk as PERNP

  # For income analysis, filter to positive income
  pums_hh_income <- pums_hh %>% filter(HINCP > 0)

  hh_income <- pums_hh_income %>%
    group_by(ABBR) %>%
    summarise(
      median_hincp_dis   = median(HINCP[DIS == "1"], na.rm = TRUE),
      median_hincp_nodis = median(HINCP[DIS == "2"], na.rm = TRUE),
      .groups = "drop"
    )

  # ── Load RPP + join ───────────────────────────────────────────────────────
  rpp_state <- readr::read_csv(
    file.path(EXT_DIR, "rpp_by_state.csv"),
    show_col_types = FALSE
  ) %>%
    select(ABBR, rpp_2022)    # using 2022 per confirmed project decision

  # ── Compute COL-adjusted income indicators ────────────────────────────────
  income_adj <- ind_income %>%
    full_join(hh_income, by = "ABBR") %>%
    left_join(rpp_state, by = "ABBR") %>%
    mutate(
      # Divide nominal median by (RPP/100)
      # E.g. RPP=110 → prices 10% above national avg → real income 10% lower
      coladj_ind_income       = median_pernp_dis   / (rpp_2022 / 100),
      coladj_ind_income_nodis = median_pernp_nodis / (rpp_2022 / 100),
      coladj_hh_income        = median_hincp_dis   / (rpp_2022 / 100),
      coladj_hh_income_nodis  = median_hincp_nodis / (rpp_2022 / 100)
    ) %>%
    select(
      ABBR,
      coladj_ind_income, coladj_ind_income_nodis,
      coladj_hh_income,  coladj_hh_income_nodis
    ) %>%
    filter(!is.na(ABBR))

  # ── QC ────────────────────────────────────────────────────────────────────
  message("COL-adj ind income — states with PWD data:  ",
          sum(!is.na(income_adj$coladj_ind_income)))
  message("COL-adj HH  income — states with PWD data:  ",
          sum(!is.na(income_adj$coladj_hh_income)))
  # ind_income now has ABBR directly (ST removed from PUMS pull); join directly
  small_n <- income_adj %>%
    left_join(
      ind_income %>% select(ABBR, n_obs_dis),
      by = "ABBR"
    ) %>%
    filter(!is.na(n_obs_dis), n_obs_dis < 30)
  if (nrow(small_n) > 0)
    message("  WARN — small cell (<30 obs) for PWD individual income: ",
            paste(small_n$ABBR, collapse = ", "))

  # ── Save COL-adjusted income ──────────────────────────────────────────────
  out_path <- file.path(EXT_DIR, "coladj_income_bea_rpp.csv")
  readr::write_csv(income_adj, out_path)
  message("Saved COL-adjusted income to: ", out_path)

  # ── Cache pums_hh for Section G (avoids redundant 30-60 min pull) ─────────
  # NOTE: F2 now pulls DIS, HINCP, TEN, GRPIP, OCPIP so this cache
  #       contains all variables Section G needs (TEN, GRPIP, OCPIP).
  pums_hh_cache <- file.path(EXT_DIR, "_pums_hh_temp.rds")
  saveRDS(pums_hh, pums_hh_cache)
  message("Cached pums_hh to: ", pums_hh_cache, " (used by Section G)")

}  # end F2


# ============================================================
# SECTION G: ACS PUMS — Disability-Specific Housing Burden
#
#   Indicators produced:
#     renter_burden        — % renter HHs (DIS=1) with GRPIP >= 30
#     renter_burden_nodis  — % renter HHs (DIS=2) with GRPIP >= 30
#     owner_burden         — % owner HHs  (DIS=1) with OCPIP >= 30
#     owner_burden_nodis   — % owner HHs  (DIS=2) with OCPIP >= 30
#
#   All direction: lower_better (higher burden = worse outcome)
#
#   PUMS variable definitions:
#     DIS:   1 = any HH member w/ disability, 2 = no member w/ disability
#     TEN:   1 = owned w/ mortgage, 2 = owned free/clear, 3 = rented
#     GRPIP: gross rent as % of income (renters); burden threshold = 30
#     OCPIP: selected owner costs as % of income (owners); burden threshold = 30
#
#   NOTE: This block reuses the pums_hh RDS saved by Section F2.
#   If F2 did not run in this session, a state-by-state fallback pull runs.
#   ST is NOT a valid variable in the 2024 PUMS API; state is tagged via
#   mutate(ABBR = abbr) in the loop instead of being requested from the API.
# ============================================================

if (TRUE) {

  library(tidycensus)
  census_api_key("0dbc4636e54dfa46e467982ee9404210fdf99c9e", install = FALSE, overwrite = TRUE)

  ACS_YEAR <- 2024

  # State FIPS → ABBR (same crosswalk as F2; re-defined here for standalone use)
  pums_fips_xwalk_g <- tibble(
    ST   = sprintf("%02d", c(
      1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50,
      51, 53, 54, 55, 56, 72
    )),
    ABBR = c(
      "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA",
      "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
      "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
      "NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX",
      "UT","VT","VA","WA","WV","WI","WY","PR"
    )
  )

  # ── Load pums_hh: reuse F2 cache if available, else re-pull ───────────────
  #    The cache must contain TEN, GRPIP, OCPIP (added to F2 pull in Apr 2026).
  #    If a stale cache (DIS+HINCP only) is found, delete it and re-pull.
  pums_hh_cache <- file.path(EXT_DIR, "_pums_hh_temp.rds")
  g_required_cols <- c("TEN", "GRPIP", "OCPIP", "DIS", "ABBR")

  if (file.exists(pums_hh_cache)) {
    message("Section G: loading cached pums_hh from F2...")
    pums_hh_g <- readRDS(pums_hh_cache)
    missing_cols <- setdiff(g_required_cols, names(pums_hh_g))
    if (length(missing_cols) > 0) {
      message("  Cache is missing columns needed by Section G: ",
              paste(missing_cols, collapse = ", "),
              "\n  Deleting stale cache and pulling fresh data...")
      file.remove(pums_hh_cache)
      rm(pums_hh_g)
    } else {
      # Ensure numeric types (may have been character from PUMS API)
      pums_hh_g <- pums_hh_g %>%
        mutate(
          GRPIP = as.numeric(GRPIP),
          OCPIP = as.numeric(OCPIP)
        )
    }
  }

  if (!exists("pums_hh_g")) {
    # ST not valid in 2024 PUMS API — pull state-by-state and tag ABBR manually
    message("Section G: no F2 cache found — pulling household PUMS state-by-state (30-60 min)...")
    g_ckpt <- file.path(EXT_DIR, "_pums_g_ckpt.rds")
    if (file.exists(g_ckpt)) {
      g_results <- readRDS(g_ckpt)
      message("  Resuming G pull — ", length(g_results), " / ", length(pums_fips_xwalk_g$ABBR), " states cached")
    } else {
      g_results <- list()
    }
    for (abbr_g in pums_fips_xwalk_g$ABBR) {
      if (abbr_g %in% names(g_results)) next
      df_g <- tryCatch(
        get_pums(
          variables = c("DIS", "TEN", "GRPIP", "OCPIP"),
          state     = abbr_g,
          year      = ACS_YEAR,
          survey    = "acs5",
          rep_weights = NULL
        ),
        error = function(e) { message("  [", abbr_g, "] failed: ", conditionMessage(e)); NULL }
      )
      if (!is.null(df_g)) {
        df_g <- df_g %>% mutate(ABBR = abbr_g)
        g_results[[abbr_g]] <- df_g
      }
      if (length(g_results) %% 10 == 0) saveRDS(g_results, g_ckpt)
    }
    saveRDS(g_results, g_ckpt)
    pums_hh_g <- bind_rows(g_results) %>%
      mutate(                           # coerce — same type-conflict risk as PERNP/HINCP
        GRPIP = as.numeric(GRPIP),
        OCPIP = as.numeric(OCPIP)
      )
  }

  # ── Renter burden: TEN == "3" (renters), burden = GRPIP >= 30 ────────────
  renter_burden <- pums_hh_g %>%
    filter(TEN == "3", !is.na(GRPIP)) %>%
    group_by(ABBR) %>%
    summarise(
      renter_burden       = mean(GRPIP >= 30 & DIS == "1", na.rm = TRUE),
      renter_burden_nodis = mean(GRPIP >= 30 & DIS == "2", na.rm = TRUE),
      n_renter_dis   = sum(DIS == "1"),
      n_renter_nodis = sum(DIS == "2"),
      .groups = "drop"
    )

  # ── Owner burden: TEN in "1","2" (owners), burden = OCPIP >= 30 ──────────
  owner_burden <- pums_hh_g %>%
    filter(TEN %in% c("1", "2"), !is.na(OCPIP)) %>%
    group_by(ABBR) %>%
    summarise(
      owner_burden       = mean(OCPIP >= 30 & DIS == "1", na.rm = TRUE),
      owner_burden_nodis = mean(OCPIP >= 30 & DIS == "2", na.rm = TRUE),
      n_owner_dis    = sum(DIS == "1"),
      n_owner_nodis  = sum(DIS == "2"),
      .groups = "drop"
    )

  # ── Join, map ST → ABBR, clean ────────────────────────────────────────────
  housing_burden <- renter_burden %>%
    full_join(owner_burden, by = "ABBR") %>%
    filter(!is.na(ABBR)) %>%          # drops any unmatched records
    select(ABBR,
           renter_burden, renter_burden_nodis,
           owner_burden,  owner_burden_nodis)

  # ── QC ────────────────────────────────────────────────────────────────────
  message("Housing burden — states with renter data: ",
          sum(!is.na(housing_burden$renter_burden)))
  message("Housing burden — states with owner data:  ",
          sum(!is.na(housing_burden$owner_burden)))
  message("  Renter burden (PWD) range: ",
          round(min(housing_burden$renter_burden,       na.rm = TRUE) * 100, 1), "% – ",
          round(max(housing_burden$renter_burden,       na.rm = TRUE) * 100, 1), "%")
  message("  Owner burden  (PWD) range: ",
          round(min(housing_burden$owner_burden,        na.rm = TRUE) * 100, 1), "% – ",
          round(max(housing_burden$owner_burden,        na.rm = TRUE) * 100, 1), "%")

  missing_g <- setdiff(c(state.abb, "DC"), housing_burden$ABBR)
  if (length(missing_g) > 0)
    message("  Missing states: ", paste(missing_g, collapse = ", "))

  # ── Save ──────────────────────────────────────────────────────────────────
  out_path <- file.path(EXT_DIR, "housing_burden_pums.csv")
  readr::write_csv(housing_burden, out_path)
  message("Saved housing burden to: ", out_path)

}  # end G


# ============================================================
# INTEGRATION: Merge all completed external CSVs into national_data.Rds
#
#   Run this block AFTER all desired sections above have completed.
#   Uses "new columns only" join — will not overwrite existing columns
#   in national_data unless OVERWRITE_EXISTING is set to TRUE.
#
#   To replace the existing privatehealth and nohealthins variables
#   with the B18135-consistent versions from this pull, set:
#     OVERWRITE_EXISTING <- TRUE
# ============================================================

if (TRUE) {

  OVERWRITE_EXISTING <- FALSE   # set TRUE to replace any already-existing columns

  national_data <- readRDS(NATIONAL_DATA_PATH)
  message("national_data loaded. Rows: ", nrow(national_data),
          " | Cols: ", ncol(national_data))

  # List all completed external CSV files to merge into national_data.
  # The loop below silently skips any file that doesn't exist yet.
  #
  # STATUS (April 2026):
  #   crime   ✓  — fbi_crime_2024.csv          (Section B)
  #   hud     ✓  — hud_posh_2025.csv            (Section C)
  #   ssi_fmr ✓  — ssi_fmr_ratio.csv            (Section D)
  #   hcbs    ✓  — hcbs_kff_cms.csv             (Section E)
  #   income  ✓  — coladj_income_bea_rpp.csv    (Section F2 — ACS 2024 PUMS)
  #   burden  ✓  — housing_burden_pums.csv       (Section G  — ACS 2024 PUMS)
  #
  # NOTE: rpp_by_state.csv (Section F1) is an intermediate lookup table —
  #   it is NOT merged here because it contains RPP values (not indicator
  #   columns). It is read inside Section F2 to produce coladj_income_bea_rpp.csv.
  #
  # NOTE: ACS-sourced variables (b18135, b18130, b18131) are NOT listed here —
  #   those are already present in national_data from the main pipeline ACS run.
  ext_files <- list(
    crime   = file.path(EXT_DIR, "fbi_crime_2024.csv"),
    hud     = file.path(EXT_DIR, "hud_posh_2025.csv"),
    ssi_fmr = file.path(EXT_DIR, "ssi_fmr_ratio.csv"),
    hcbs    = file.path(EXT_DIR, "hcbs_kff_cms.csv"),
    income  = file.path(EXT_DIR, "coladj_income_bea_rpp.csv"),   # skip until F2 runs
    burden  = file.path(EXT_DIR, "housing_burden_pums.csv")       # skip until G runs
  )

  for (name in names(ext_files)) {
    path <- ext_files[[name]]
    if (!file.exists(path)) {
      message("  Skipping '", name, "' — file not found")
      next
    }

    ext_df <- readr::read_csv(path, show_col_types = FALSE)
    join_key <- if ("GEOID" %in% names(ext_df)) "GEOID" else "ABBR"

    # Determine which columns to add
    data_cols <- setdiff(names(ext_df), join_key)
    if (OVERWRITE_EXISTING) {
      # Drop existing versions of any columns we're about to add
      national_data <- national_data %>%
        select(-any_of(data_cols))
      cols_to_add <- data_cols
    } else {
      # Only add genuinely new columns
      cols_to_add <- setdiff(data_cols, names(national_data))
      already_have <- intersect(data_cols, names(national_data))
      if (length(already_have) > 0) {
        message("  '", name, "': keeping existing columns: ",
                paste(already_have, collapse = ", "))
      }
    }

    if (length(cols_to_add) == 0) {
      message("  '", name, "': no new columns to add — skipping")
      next
    }

    national_data <- national_data %>%
      left_join(
        ext_df %>% select(all_of(c(join_key, cols_to_add))),
        by = join_key
      )
    message("  '", name, "': added ", length(cols_to_add), " column(s): ",
            paste(cols_to_add, collapse = ", "))
  }

  saveRDS(national_data, NATIONAL_DATA_PATH)
  message("\nnational_data.Rds saved.")
  message("Total columns now: ", ncol(national_data))
  message("\nNEXT: re-run scripts 02 → 03 → 03b → 04 → 05 → 06.")

}  # end INTEGRATION
