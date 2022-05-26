library(tidyverse)
library(tidycensus)
options(scipen = 9999)
year = 2020
survey = "acs5"

source(here::here("scripts", "functions.R"))

# ---- Download National Data ----
tables <- c("S1810")
state_demographics <- downloadAndFormatAcs(tables, "state", year, survey)
national_demographics <- downloadAndFormatAcs(tables, "us", year, survey)
stacked_demographics <- bind_rows(national_demographics, state_demographics)
rm(state_demographics, national_demographics)

if(year > 2017) {
  tables <- c("S1810", "S2601A", "S2602", "B26108")
  state_living <- downloadAndFormatAcs(tables, "state", year, survey)
  national_living <- downloadAndFormatAcs(tables, "us", year, survey)
  stacked_living <- bind_rows(national_living, state_living)
  rm(state_living, national_living)
} else {
  message("ACS group quarters tables were not available until 2017 and cannot be included in current request.")
}

tables <- c("S1810", "S1811", "B18135") 
state_participation <- downloadAndFormatAcs(tables, "state", year, survey)
national_participation <- downloadAndFormatAcs(tables, "us", year, survey)
stacked_participation <- bind_rows(state_participation, national_participation)
rm(state_participation, national_participation)

tables <- c("S1810", "S1811", 
            "B18135", "B18140", "B25091", "B25070", 
            "C18120", "C18121", "C18130") 
state_economic <- downloadAndFormatAcs(tables, "state", year, survey)
national_economic <- downloadAndFormatAcs(tables, "us", year, survey)
stacked_economic <- bind_rows(state_economic, national_economic)
rm(state_economic, national_economic)


# ---- Code National Data ----
source(here::here("scripts", "national_data_clean.R"))
rm(stacked_demographics, stacked_living, stacked_participation, stacked_economic, tables)

# ---- Write workbook file ----
message(paste0("Writing data to national_data_", year,".xlsx"))
writexl::write_xlsx(list("Demographics" = demographics, "Community Living" = community_living, 
                         "Community Participation" = community_participation, "Work Economic" = work_economic),
                    here::here("Output", "National", paste0("national_data_", year,".xlsx")))

# ---- Generate national factsheets ----

source(here::here("Scripts", "generate_national_factsheets.R"))

# ---- Generate place-tract crosswalk ----
message("Beginning city data update process, please be patient. This process can take a long time.")
source(here::here("Scripts", "generate_places_tracts_crosswalk.R"))
