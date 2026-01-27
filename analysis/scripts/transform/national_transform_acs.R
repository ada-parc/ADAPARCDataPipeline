years <- config::get()$acs$years

############################################
### ---- Get base vars from raw data -----
############################################

map_of_variables <- getADAPARCBaseToSourceVariableMap(years)

tables <- map_of_variables %>%
  dplyr::filter(tolower(trimws(source)) == "acs") %>%
  dplyr::pull(source_var_code) %>%
  trimws() %>%
  stringr::str_extract("^[A-Z]+\\d+[A-Z]*") %>%
  unique() %>%
  sort()

tables

base_variables_national <- transformRawVariablesToADAPARCBaseVariables(
  map_of_variables,
  raw_data = readRawExtractedDataFile("raw_national_acs"),
  "ACS"
)

############################################
# Calculate variables from base variables and add to df's
############################################

transformed_acs_national <- addACSCalculatedVariables(base_variables_national)

#source(here::here("R", "combineMDAT_data.R"))

########################################
# ADD MDAT DATA (access_int)
########################################

# Read the access_int CSV file
access_int <- read.csv("access_int.csv")

# Common join key
common_key <- "NAME"  # update if your key changes

# Ensure access_int has unique columns (excluding the common key)
unique_columns_access <- setdiff(names(access_int), names(transformed_acs_national))
unique_columns_access <- unique_columns_access[unique_columns_access != common_key]

# Select only the unique columns and the common key from access_int
access_int_unique <- access_int %>%
  dplyr::select(dplyr::all_of(c(common_key, unique_columns_access)))

# Join access_int onto transformed_acs_national
transformed_acs_national <- dplyr::left_join(
  transformed_acs_national,
  access_int_unique,
  by = common_key
)

########################################
# ----- Save Output -----
########################################

saveTransformedDataFile(transformed_acs_national, "transformed_acs_national")
