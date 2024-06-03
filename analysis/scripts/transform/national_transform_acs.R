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

# ----- Save Output -----

saveTransformedDataFile(transformed_acs_national, "transformed_acs_national")
