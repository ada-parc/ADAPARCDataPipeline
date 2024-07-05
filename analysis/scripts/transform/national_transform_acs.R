years <- config::get()$acs$years

############################################
### ---- Get base vars from raw data -----
############################################

map_of_variables <- getADAPARCBaseToSourceVariableMap(years)

base_variables_national <- transformRawVariablesToADAPARCBaseVariables(map_of_variables, raw_data = readRawExtractedDataFile("raw_national_acs"), "acs")

############################################
# Calculate variables from base variables and add to df's
############################################

transformed_acs_national <- addACSCalculatedVariables(base_variables_national)

#source(here::here("R", "combineMDAT_data.R"))


########################################
 # ADD MDAT DATA #
#######################################

# Read the access_int CSV file
access_int <- read_csv("access_int.csv")

# Ensure access_int has unique columns (excluding the common key 'id')
common_key <- "NAME" # Update with the actual key column name
unique_columns <- setdiff(names(access_int), names(transformed_acs_national))
unique_columns <- unique_columns[unique_columns != common_key]

# Select only the unique columns and the common key from access_int
access_int_unique <- access_int %>% select(all_of(c(common_key, unique_columns)))

# Perform a join operation to combine the data
combined_data <- left_join(transformed_acs_national, access_int_unique, by = common_key)


transformed_acs_national <- combined_data

# ----- Save Output -----

saveTransformedDataFile(transformed_acs_national, "transformed_acs_national")
