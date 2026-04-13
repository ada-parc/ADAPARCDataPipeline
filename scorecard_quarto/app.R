# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  ADA-PARC Scorecard — Shiny App (2024 Revision)                         ║
# ║                                                                          ║
# ║  Run from within the scorecard_quarto/ directory:                        ║
# ║    shiny::runApp("scorecard_quarto")                                     ║
# ║  or open app.R in RStudio and click "Run App"                            ║
# ║                                                                          ║
# ║  Required packages: shiny, shinyjs, quarto, readr, dplyr                 ║
# ║                                                                          ║
# ║  2024 Revision — Index structure changes:                                ║
# ║    CL: living_arrangements + community_resource                          ║
# ║    CP: tech + insurance + education + commute + safety                   ║
# ║    WE: (positive_work + negative_work) + housing_affordability           ║
# ╚══════════════════════════════════════════════════════════════════════════╝

library(shiny)
library(shinyjs)
library(quarto)
library(readr)
library(dplyr)

# ── Output directory (Shiny serves www/ as static files automatically) ─────
dir.create("www", showWarnings = FALSE)

# Clean up leftover renders from any previous session
invisible(unlink(c("www/state_output.html", "www/cat_output.html")))

# ── Load dictionaries ───────────────────────────────────────────────────────
indicator_dict <- tryCatch(
  read_csv("dictionary/scorecard_indicator_dictionary.csv", show_col_types = FALSE),
  error = function(e) { warning("Could not load indicator dictionary: ", conditionMessage(e)); NULL }
)

index_dict <- tryCatch(
  read_csv("dictionary/scorecard_index_dictionary.csv", show_col_types = FALSE),
  error = function(e) { warning("Could not load index dictionary: ", conditionMessage(e)); NULL }
)

# ── Build state/territory choice list from the data ─────────────────────────
state_choices <- tryCatch({
  raw <- read_csv("scorecard_data/final/index_scores_wide.csv",
                  show_col_types = FALSE)
  df  <- raw |>
    filter(ABBR != "USA", !is.na(NAME), !is.na(ABBR)) |>
    mutate(NAME = as.character(NAME), ABBR = as.character(ABBR)) |>
    arrange(NAME)
  setNames(df$ABBR, df$NAME)
}, error = function(e) {
  warning("Could not read data file for state list: ", conditionMessage(e))
  c("Illinois" = "IL", "New York" = "NY", "California" = "CA")
})

cat_choices <- c(
  "Community Living"        = "CL",
  "Community Participation" = "CP",
  "Work & Economic"         = "WE"
)

# ── Index hierarchy definition (2024 revision) ──────────────────────────────
# Drives the Index Reference tab. Relative dimension only (equity scores share
# the same indicator pool). Structure matches scorecard_index_dictionary.csv.
index_hierarchy <- list(

  CL = list(
    label = "Community Living",
    color = "#C9A227",
    bg    = "#fdf8ec",
    category_index = "Community Living Index",
    indices = list(
      list(
        id         = "index_rel_living_arrangements",
        label      = "Living Arrangements",
        indicators = c("rel_score_home", "rel_score_noninstgroupquarters",
                       "rel_score_nursing_18to64", "rel_score_corrections")
      ),
      list(
        id         = "index_rel_community_resource",
        label      = "Community Resources",
        indicators = c("rel_score_hcbs_ratio", "rel_score_hcbs_per_individual",
                       "rel_score_hcv_hoh", "rel_score_pubhousing_hoh")
      )
    )
  ),

  CP = list(
    label = "Community Participation",
    color = "#CC5500",
    bg    = "#fdf4f0",
    category_index = "Community Participation Index",
    indices = list(
      list(
        id         = "index_rel_tech",
        label      = "Technology Access",
        indicators = c("rel_score_computer", "rel_score_smartphone", "rel_score_int")
      ),
      list(
        id         = "index_rel_insurance",
        label      = "Health Insurance",
        indicators = c("rel_score_privatehealth", "rel_score_publichealth",
                       "rel_score_nohealthins")
      ),
      list(
        id         = "index_rel_education",
        label      = "Education Access",
        indicators = c("rel_score_noGED", "rel_score_bachelors")
      ),
      list(
        id         = "index_rel_commute",
        label      = "Transportation Access",
        indicators = c("rel_score_transit", "rel_score_drivealone")
      ),
      list(
        id         = "index_rel_safety",
        label      = "Public Safety",
        indicators = c("rel_score_propertycrime", "rel_score_violentcrime")
      )
    )
  ),

  WE = list(
    label = "Work & Economic",
    color = "#800000",
    bg    = "#fdf0f0",
    category_index = "Work-Economic Index",
    indices = list(
      list(
        id         = "index_rel_positive_work",
        label      = "Positive Work Score",
        indicators = c("rel_score_employment", "rel_score_coladj_ind_income",
                       "rel_score_coladj_hh_income")
      ),
      list(
        id         = "index_rel_negative_work",
        label      = "Negative Work Score",
        indicators = c("rel_score_poverty", "rel_score_unemployed", "rel_score_labor")
      ),
      list(
        id         = "index_rel_housing_affordability",
        label      = "Housing Affordability",
        indicators = c("rel_score_ssi_rent", "rel_score_renter_burden",
                       "rel_score_owner_burden")
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# Indicator → data source lookup (used in About tab source table)
# ══════════════════════════════════════════════════════════════════════════════
ind_src <- c(
  # CL — Living Arrangements
  rel_score_home                 = "ACS PUMS",
  rel_score_noninstgroupquarters = "ACS PUMS",
  rel_score_nursing_18to64       = "ACS PUMS",
  rel_score_corrections          = "ACS PUMS",
  # CL — Community Resources
  rel_score_hcbs_ratio           = "KFF / CMS-64 (FY2024)",
  rel_score_hcbs_per_individual  = "KFF / CMS-64 (FY2024)",
  rel_score_hcv_hoh              = "HUD POSH (2023)",
  rel_score_pubhousing_hoh       = "HUD POSH (2023)",
  # CP — Technology Access
  rel_score_computer             = "ACS PUMS",
  rel_score_smartphone           = "ACS PUMS",
  rel_score_int                  = "ACS PUMS",
  # CP — Health Insurance
  rel_score_privatehealth        = "ACS B18135",
  rel_score_publichealth         = "ACS B18135",
  rel_score_nohealthins          = "ACS B18135",
  # CP — Education Access
  rel_score_noGED                = "ACS S1810",
  rel_score_bachelors            = "ACS S1810",
  # CP — Transportation Access
  rel_score_transit              = "ACS PUMS",
  rel_score_drivealone           = "ACS PUMS",
  # CP — Public Safety
  rel_score_propertycrime        = "FBI NIBRS (2024)",
  rel_score_violentcrime         = "FBI NIBRS (2024)",
  # WE — Positive Work
  rel_score_employment           = "ACS B18131",
  rel_score_coladj_ind_income    = "ACS PUMS + BEA RPP (2022)",
  rel_score_coladj_hh_income     = "ACS PUMS + BEA RPP (2022)",
  # WE — Negative Work
  rel_score_poverty              = "ACS B18130 (derived)",
  rel_score_unemployed           = "ACS B18131",
  rel_score_labor                = "ACS B18131",
  # WE — Housing Affordability
  rel_score_ssi_rent             = "SSA ASR 2024 + HUD FMR (FY2025)",
  rel_score_renter_burden        = "ACS PUMS",
  rel_score_owner_burden         = "ACS PUMS"
)

# ── Build the source table for the About tab ──────────────────────────────
build_source_table <- function(hierarchy, ind_df, src_lookup) {
  rel_df <- if (!is.null(ind_df)) ind_df |> filter(dimension == "relative") else NULL

  get_label <- function(ind_id) {
    if (is.null(rel_df)) return(ind_id)
    row <- rel_df |> filter(indicator_id == ind_id)
    if (nrow(row) == 0) return(ind_id)
    sub(":\\s*Relative$", "", row$indicator_label[1])
  }
  get_dir <- function(ind_id) {
    if (is.null(rel_df)) return("")
    row <- rel_df |> filter(indicator_id == ind_id)
    if (nrow(row) == 0) return("")
    if (!is.na(row$direction[1]) && row$direction[1] == "higher_better")
      "\u2191 Higher = better" else "\u2193 Lower = better"
  }
  get_src <- function(ind_id) {
    v <- src_lookup[ind_id]
    if (is.na(v)) "—" else v
  }

  cat_colors <- list(
    CL = "#C9A227",
    CP = "#CC5500",
    WE = "#800000"
  )

  rows <- list()
  for (cat_id in names(hierarchy)) {
    cat_info  <- hierarchy[[cat_id]]
    cat_color <- cat_colors[[cat_id]]
    # Category header row
    rows[[length(rows) + 1]] <- tags$tr(
      tags$td(
        colspan = "3",
        style = sprintf(
          paste0("background:%s;color:white;font-weight:700;font-size:1rem;",
                 "padding:0.5rem 0.9rem;letter-spacing:0.02em;"),
          cat_color
        ),
        cat_info$label
      )
    )
    for (idx_info in cat_info$indices) {
      # Sub-index subheader row
      rows[[length(rows) + 1]] <- tags$tr(
        tags$td(
          colspan = "3",
          style = sprintf(
            paste0("background:%s22;color:%s;font-weight:700;font-size:0.85rem;",
                   "padding:0.35rem 0.9rem 0.3rem 1.5rem;border-top:1px solid #eee;",
                   "text-transform:uppercase;letter-spacing:0.06em;"),
            cat_color, cat_color
          ),
          idx_info$label
        )
      )
      for (ind_id in idx_info$indicators) {
        rows[[length(rows) + 1]] <- tags$tr(
          style = "border-bottom:1px solid #f0f0f0;",
          tags$td(
            style = "padding:0.35rem 0.9rem 0.35rem 2.2rem;font-size:0.9rem;color:#1c2b3a;",
            get_label(ind_id)
          ),
          tags$td(
            style = "padding:0.35rem 0.6rem;font-size:0.82rem;color:#555;white-space:nowrap;",
            get_dir(ind_id)
          ),
          tags$td(
            style = "padding:0.35rem 0.9rem;font-size:0.82rem;color:#444;",
            get_src(ind_id)
          )
        )
      }
    }
  }

  div(
    style = "margin-top:1.5rem;",
    tags$h4(
      style = paste0("font-size:1rem;font-weight:700;text-transform:uppercase;",
                     "letter-spacing:0.07em;color:#1c2b3a;margin:0 0 0.6rem;"),
      "Indicators & Data Sources"
    ),
    div(
      style = paste0("background:white;border-radius:8px;",
                     "box-shadow:0 1px 4px rgba(0,0,0,0.07);overflow:hidden;"),
      tags$table(
        style = "width:100%;border-collapse:collapse;",
        tags$thead(
          tags$tr(
            style = "background:#f0f2f6;border-bottom:2px solid #dde2ea;",
            tags$th(style = "padding:0.5rem 0.9rem;text-align:left;font-size:0.8rem;text-transform:uppercase;letter-spacing:0.07em;color:#555;", "Indicator"),
            tags$th(style = "padding:0.5rem 0.6rem;text-align:left;font-size:0.8rem;text-transform:uppercase;letter-spacing:0.07em;color:#555;", "Direction"),
            tags$th(style = "padding:0.5rem 0.9rem;text-align:left;font-size:0.8rem;text-transform:uppercase;letter-spacing:0.07em;color:#555;", "Data Source")
          )
        ),
        tags$tbody(rows)
      )
    )
  )
}

# ── Pre-build source table ────────────────────────────────────────────────
source_table <- build_source_table(index_hierarchy, indicator_dict, ind_src)

# ══════════════════════════════════════════════════════════════════════════════
# Reference tab helper functions
# ══════════════════════════════════════════════════════════════════════════════

# ── Single indicator row ──────────────────────────────────────────────────────
indicator_row_ui <- function(ind_id, ind_df) {
  if (is.null(ind_df)) return(tags$li(style = "list-style:none;", ind_id))
  row <- ind_df |> filter(indicator_id == ind_id)
  if (nrow(row) == 0) return(tags$li(style = "list-style:none;", ind_id))
  # Strip dimension suffix for display
  label     <- sub(":\\s*(Relative|Equity)$", "", row$indicator_label[1])
  direction <- row$direction[1]
  dir_icon  <- if (!is.na(direction) && direction == "higher_better") "\u2191" else "\u2193"
  tags$li(
    style = "list-style:none;padding:3px 0;",
    tags$span(style = "font-size:0.9rem;color:#1c2b3a;", dir_icon, " ", label)
  )
}

# ── Single sub-index card ─────────────────────────────────────────────────────
sub_index_card_ui <- function(idx_info, ind_df, accent_color) {
  ind_rows <- lapply(idx_info$indicators, indicator_row_ui, ind_df = ind_df)
  div(
    style = sprintf(
      paste0("background:white;border-radius:7px;border-left:3px solid %s;",
             "padding:0.75rem 1rem;margin-bottom:0.65rem;",
             "box-shadow:0 1px 3px rgba(0,0,0,0.07);"),
      accent_color
    ),
    tags$h5(
      style = "font-size:0.95rem;font-weight:700;color:#1c2b3a;margin:0 0 0.3rem;",
      idx_info$label
    ),
    tags$ul(style = "margin:0;padding:0;", ind_rows)
  )
}

# ── Full category column ──────────────────────────────────────────────────────
category_column_ui <- function(cat_id, cat_info, ind_df) {
  body_content <- lapply(cat_info$indices, sub_index_card_ui,
                         ind_df = ind_df, accent_color = cat_info$color)
  div(
    style = "flex:1;min-width:270px;",
    div(
      style = sprintf(
        paste0("background:%s;color:white;border-radius:8px 8px 0 0;",
               "padding:0.75rem 1.1rem;margin-bottom:0.7rem;"),
        cat_info$color
      ),
      tags$h4(style = "margin:0;font-size:1.1rem;font-weight:700;", cat_info$label),
      tags$p(
        style = "margin:0.15rem 0 0;font-size:0.82rem;opacity:0.85;",
        "\u2192 ", cat_info$category_index
      )
    ),
    body_content
  )
}

# ── Assemble the full reference panel ────────────────────────────────────────
build_reference_panel <- function(hierarchy, ind_df) {

  # Filter indicator dict to relative dimension only (avoids duplicate labels)
  rel_df <- if (!is.null(ind_df)) ind_df |> filter(dimension == "relative") else NULL

  # Direction key
  key <- div(
    style = paste0("margin-bottom:1rem;padding:0.5rem 1rem;background:white;",
                   "border-radius:8px;box-shadow:0 1px 3px rgba(0,0,0,0.07);",
                   "display:inline-block;"),
    tags$span(style = "font-size:0.83rem;color:#555;",
              "\u2191 higher = better  \u00b7  \u2193 lower = better")
  )

  cols <- mapply(
    category_column_ui,
    cat_id   = names(hierarchy),
    cat_info = hierarchy,
    MoreArgs = list(ind_df = rel_df),
    SIMPLIFY = FALSE
  )

  tagList(
    key,
    div(
      style = "display:flex;gap:1.1rem;align-items:flex-start;flex-wrap:wrap;",
      cols
    )
  )
}

# ── Pre-build the reference panel at startup ──────────────────────────────────
reference_panel <- build_reference_panel(index_hierarchy, indicator_dict)

# ══════════════════════════════════════════════════════════════════════════════
# CSS
# ══════════════════════════════════════════════════════════════════════════════
app_css <- "
  @import url('https://fonts.googleapis.com/css2?family=EB+Garamond:wght@400;600;700&display=swap');

  body {
    font-family: 'EB Garamond', Georgia, serif;
    background: #f0f2f6;
    color: #1c2b3a;
  }

  /* ── App header ── */
  .app-header {
    padding: 1.4rem 0 0.6rem;
    border-bottom: 2px solid #e2e6ed;
    margin-bottom: 1.4rem;
  }
  .app-header h2 {
    font-size: 2.6rem;
    font-weight: 700;
    color: #1c2b3a;
    margin: 0 0 0.2rem;
    letter-spacing: -0.015em;
    line-height: 1;
  }
  .app-header p {
    color: #1c2b3a;
    font-size: 1.1rem;
    margin: 0;
  }
  .app-header .revision-badge {
    display: inline-block;
    background: #0d3b8e;
    color: white;
    font-size: 0.78rem;
    font-weight: 700;
    padding: 2px 8px;
    border-radius: 4px;
    margin-left: 10px;
    vertical-align: middle;
    letter-spacing: 0.04em;
  }

  /* ── Controls row ── */
  .controls-box {
    background: white;
    border-radius: 10px;
    padding: 1.1rem 1.6rem 1rem;
    margin-bottom: 1.2rem;
    box-shadow: 0 1px 4px rgba(0,0,0,0.07);
  }
  .controls-box label {
    font-weight: 700;
    font-size: 1rem;
    text-transform: uppercase;
    letter-spacing: 0.07em;
    color: #1c2b3a;
    margin-bottom: 0.4rem;
    display: block;
  }
  .controls-box .form-group { margin-bottom: 0; }
  .controls-box select {
    font-family: 'EB Garamond', Georgia, serif;
    font-size: 1.1rem;
    color: #1c2b3a;
  }

  /* ── Buttons ── */
  .btn-generate {
    background: #0d3b8e !important;
    color: white !important;
    border: none !important;
    border-radius: 6px !important;
    font-family: 'EB Garamond', Georgia, serif !important;
    font-size: 1.15rem !important;
    font-weight: 700 !important;
    padding: 9px 20px !important;
    margin-top: 24px;
    width: 100%;
  }
  .btn-generate:hover { background: #0a2d6e !important; }

  .btn-download {
    background: white !important;
    color: #000000 !important;
    border: 1.5px solid #1c2b3a !important;
    border-radius: 6px !important;
    font-family: 'EB Garamond', Georgia, serif !important;
    font-size: 1.15rem !important;
    font-weight: 700 !important;
    padding: 9px 20px !important;
    margin-top: 24px;
    width: 100%;
  }
  .btn-download:hover  { background: #f0f4fc !important; }
  .btn-download:disabled,
  .btn-download[disabled] {
    opacity: 0.45 !important;
    cursor: not-allowed !important;
  }

  /* ── Placeholder ── */
  .placeholder-box {
    background: white;
    border-radius: 10px;
    padding: 5rem 2rem;
    text-align: center;
    color: #1c2b3a;
    font-size: 1.15rem;
    box-shadow: 0 1px 4px rgba(0,0,0,0.07);
  }
  .placeholder-box .ph-icon {
    font-size: 2.4rem;
    margin-bottom: 0.8rem;
    display: block;
    opacity: 0.35;
  }

  /* ── Scorecard iframe ── */
  .scorecard-frame {
    width: 100%;
    border: none;
    border-radius: 10px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.09);
    display: block;
  }

  /* ── Tabs ── */
  .nav-tabs {
    border-bottom: 2px solid #dde2ea;
    margin-bottom: 0;
  }
  .nav-tabs .nav-link {
    font-family: 'EB Garamond', Georgia, serif;
    font-size: 1.2rem;
    font-weight: 600;
    color: #1c2b3a;
    border: none !important;
    border-radius: 0 !important;
    padding: 0.5rem 1.2rem 0.6rem;
  }
  .nav-tabs .nav-link:hover { color: #0d3b8e; background: transparent; }
  .nav-tabs .nav-link.active {
    color: #0d3b8e !important;
    border-bottom: 3px solid #0d3b8e !important;
    background: transparent !important;
  }
  .tab-content { padding-top: 1.2rem; }

  /* ── Index reference panel ── */
  .ref-section-note {
    font-size: 0.82rem;
    color: #666;
    font-style: italic;
    margin: 0 0 0.9rem;
  }
"

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(app_css))),

  div(
    style = "max-width: 1100px; margin: 0 auto; padding: 0 1.2rem 3rem;",

    # ── Header ────────────────────────────────────────────────────────────
    div(class = "app-header",
        tags$h2(
          "ADA-PARC Scorecard",
          tags$span(class = "revision-badge", "2024 Revision")
        ),
        tags$p(
          "Select a state or category to generate a scorecard report, or visit the ",
          tags$strong("About"), " tab to learn about the scorecard indices, indicators, and data sources."
        )
    ),

    # ── Tabs ──────────────────────────────────────────────────────────────
    tabsetPanel(id = "main_tabs", type = "tabs",

      # ── National Scorecard ───────────────────────────────────────────────
      tabPanel("National Scorecard",
        br(),
        div(class = "controls-box",
          fluidRow(
            column(6,
              selectInput("category", "Category",
                          choices  = cat_choices,
                          selected = "CL",
                          width    = "100%")
            ),
            column(3,
              actionButton("render_cat", "Generate Scorecard",
                           class = "btn-generate", width = "100%")
            ),
            column(3,
              downloadButton("download_cat", "Download HTML",
                             class = "btn-download")
            )
          )
        ),
        uiOutput("cat_output_ui")
      ),

      # ── State Scorecard ─────────────────────────────────────────────────
      tabPanel("State Scorecard",
        br(),
        div(class = "controls-box",
          fluidRow(
            column(6,
              selectInput("state_abbr", "State or Territory",
                          choices  = state_choices,
                          selected = "IL",
                          width    = "100%")
            ),
            column(3,
              actionButton("render_state", "Generate Scorecard",
                           class = "btn-generate", width = "100%")
            ),
            column(3,
              downloadButton("download_state", "Download HTML",
                             class = "btn-download")
            )
          )
        ),
        uiOutput("state_output_ui")
      ),

      # ── About ────────────────────────────────────────────────────────────
      tabPanel("About",
        br(),

        # ── Plain language intro ──────────────────────────────────────────
        div(
          style = paste0("background:white;border-radius:10px;padding:1.4rem 1.8rem 1.2rem;",
                         "margin-bottom:1.4rem;box-shadow:0 1px 4px rgba(0,0,0,0.07);"),
          tags$h3(
            style = "font-size:1.4rem;font-weight:700;color:#1c2b3a;margin:0 0 0.7rem;",
            "About the ADA-PARC Scorecard"
          ),
          tags$p(
            style = "font-size:1.05rem;color:#1c2b3a;line-height:1.65;margin:0 0 0.8rem;",
            "The ADA-PARC (Americans with Disabilities Act — Policy and Research Collaborative) ",
            "Scorecard is a comprehensive, evidence-based tool for measuring how well each U.S. ",
            "state supports the full inclusion and wellbeing of people with disabilities. ",
            "Grounded in the civil rights framework of the ADA, the scorecard evaluates states ",
            "not just on the presence of disability policy, but on measurable outcomes — ",
            "where people with disabilities actually live, work, participate, and thrive."
          ),
          tags$p(
            style = "font-size:1.05rem;color:#1c2b3a;line-height:1.65;margin:0 0 0.8rem;",
            "Scores are drawn from multiple federal data sources, including the U.S. Census ",
            "Bureau American Community Survey (ACS), the Department of Housing and Urban ",
            "Development (HUD), the FBI's National Incident-Based Reporting System (NIBRS), ",
            "the Social Security Administration (SSA), the Kaiser Family Foundation and Centers ",
            "for Medicare & Medicaid Services (KFF/CMS), and the Bureau of Economic Analysis ",
            "(BEA). All scores range from 0 to 100, where higher is always better."
          ),
          tags$p(
            style = "font-size:1.05rem;color:#1c2b3a;line-height:1.65;margin:0 0 0.8rem;",
            "The scorecard is organized into three domains:"
          ),
          tags$ul(
            style = "font-size:1.05rem;color:#1c2b3a;line-height:1.75;margin:0 0 0.8rem;padding-left:1.5rem;",
            tags$li(
              tags$strong("Community Living"), " — measures whether people with disabilities ",
              "live in integrated community settings rather than nursing homes or correctional ",
              "facilities, and whether states invest in community-based supports such as ",
              "Medicaid Home and Community-Based Services (HCBS) and affordable housing programs."
            ),
            tags$li(
              tags$strong("Community Participation"), " — captures access to the tools and ",
              "systems that enable people with disabilities to participate fully in community life, ",
              "including technology access, health insurance coverage, educational attainment, ",
              "transportation options, and public safety."
            ),
            tags$li(
              tags$strong("Work & Economic"), " — measures economic inclusion and security, ",
              "including employment rates, cost-of-living-adjusted income, poverty and ",
              "unemployment rates, and housing affordability for people with disabilities."
            )
          ),
          tags$p(
            style = "font-size:1.05rem;color:#1c2b3a;line-height:1.65;margin:0 0 0.8rem;",
            "Within each domain, individual indicators are standardized using z-scores and ",
            "combined into sub-index scores, which are then aggregated into a single domain ",
            "score. States are ranked among all 50 states, Washington D.C., and Puerto Rico, ",
            "and assigned a performance tier: ",
            tags$strong("Excellent"), " (ranks 1–12), ",
            tags$strong("Above Average"), " (13–25), ",
            tags$strong("Below Average"), " (26–39), or ",
            tags$strong("Poor"), " (40+)."
          ),
          tags$p(
            style = "font-size:1.05rem;color:#1c2b3a;line-height:1.65;margin:0;",
            "The ADA-PARC Scorecard is produced with the intention of informing policymakers, advocates, and the ",
            "public about the state of disability inclusion across the country, and to highlight ",
            "the states leading the way, as well as those with the most room to improve."
          )
        ),

        # ── Index diagram ─────────────────────────────────────────────────
        div(
          style = "margin-bottom:0.4rem;",
          tags$h4(
            style = paste0("font-size:1rem;font-weight:700;text-transform:uppercase;",
                           "letter-spacing:0.07em;color:#1c2b3a;margin:0 0 0.6rem;"),
            "Index Structure"
          ),
          tags$p(
            class = "ref-section-note",
            style = "margin:0 0 0.8rem;",
            paste0(
              "\u2191 higher = better  \u00b7  \u2193 lower = better  \u00b7  ",
              "Each category produces Relative and Equity scores from the same indicator pool."
            )
          ),
          reference_panel
        ),

        # ── Indicator / source table ──────────────────────────────────────
        source_table
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # Track whether a render has completed this session
  state_rendered <- reactiveVal(FALSE)
  cat_rendered   <- reactiveVal(FALSE)

  # Download buttons start disabled; enabled after first successful render
  shinyjs::disable("download_state")
  shinyjs::disable("download_cat")

  # ── Placeholder helper ───────────────────────────────────────────────────
  placeholder <- function(msg) {
    div(class = "placeholder-box",
        tags$span(class = "ph-icon", HTML("&#9634;")),
        msg)
  }

  # ════════════════════════════════════════════════════════════════════════
  # STATE SCORECARD
  # ════════════════════════════════════════════════════════════════════════

  observeEvent(input$render_state, {

    state_rendered(FALSE)
    shinyjs::disable("download_state")

    state_label <- names(state_choices)[state_choices == input$state_abbr]

    withProgress(
      message = paste0("Generating scorecard for ", state_label, "\u2026"),
      detail  = "This usually takes 15\u201325 seconds.",
      value   = 0.15,
      {
        tryCatch({
          quarto::quarto_render(
            input          = "state_scorecard.qmd",
            execute_params = list(
              state_abbr = input$state_abbr,
              year       = 2024
            )
          )
          file.copy("state_scorecard.html", "www/state_output.html",
                    overwrite = TRUE)
          setProgress(1)
          state_rendered(TRUE)
          shinyjs::enable("download_state")

        }, error = function(e) {
          showNotification(
            ui       = paste("Render failed:", conditionMessage(e)),
            type     = "error",
            duration = 15
          )
        })
      }
    )
  })

  output$state_output_ui <- renderUI({
    if (!state_rendered()) {
      placeholder("Select a state above and click \u201cGenerate Scorecard\u201d to view the report.")
    } else {
      cache_v <- as.integer(Sys.time())
      tags$iframe(
        src       = paste0("state_output.html?v=", cache_v),
        class     = "scorecard-frame",
        height    = "940px",
        scrolling = "yes"
      )
    }
  })

  output$download_state <- downloadHandler(
    filename = function() {
      state_label <- names(state_choices)[state_choices == input$state_abbr]
      paste0("ADAPARC_", gsub("[^A-Za-z0-9]+", "_", state_label),
             "_Scorecard_2024.html")
    },
    content = function(file) {
      if (file.exists("www/state_output.html")) {
        file.copy("www/state_output.html", file)
      } else {
        writeLines("<html><body>No scorecard generated yet.</body></html>", file)
      }
    }
  )

  # ════════════════════════════════════════════════════════════════════════
  # CATEGORY SCORECARD
  # ════════════════════════════════════════════════════════════════════════

  observeEvent(input$render_cat, {

    cat_rendered(FALSE)
    shinyjs::disable("download_cat")

    cat_label <- names(cat_choices)[cat_choices == input$category]

    withProgress(
      message = paste0("Generating ", cat_label, " scorecard\u2026"),
      detail  = "This usually takes 15\u201325 seconds.",
      value   = 0.15,
      {
        tryCatch({
          quarto::quarto_render(
            input          = "category_scorecard.qmd",
            execute_params = list(
              category = input$category,
              year     = 2024
            )
          )
          file.copy("category_scorecard.html", "www/cat_output.html",
                    overwrite = TRUE)
          setProgress(1)
          cat_rendered(TRUE)
          shinyjs::enable("download_cat")

        }, error = function(e) {
          showNotification(
            ui       = paste("Render failed:", conditionMessage(e)),
            type     = "error",
            duration = 15
          )
        })
      }
    )
  })

  output$cat_output_ui <- renderUI({
    if (!cat_rendered()) {
      placeholder("Select a category above and click \u201cGenerate Scorecard\u201d to view the report.")
    } else {
      cache_v <- as.integer(Sys.time())
      tags$iframe(
        src       = paste0("cat_output.html?v=", cache_v),
        class     = "scorecard-frame",
        height    = "1080px",
        scrolling = "yes"
      )
    }
  })

  output$download_cat <- downloadHandler(
    filename = function() {
      cat_label <- names(cat_choices)[cat_choices == input$category]
      paste0("ADAPARC_", gsub("[^A-Za-z0-9]+", "_", cat_label),
             "_Scorecard_2024.html")
    },
    content = function(file) {
      if (file.exists("www/cat_output.html")) {
        file.copy("www/cat_output.html", file)
      } else {
        writeLines("<html><body>No scorecard generated yet.</body></html>", file)
      }
    }
  )

}

shinyApp(ui = ui, server = server)
