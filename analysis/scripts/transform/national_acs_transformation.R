
years <- config::get()$acs$years

############################################
### ---- Get base vars from raw data -----
############################################

map_of_variables <- getADAPARCBaseToSourceVariableMap(years)

base_vars_demographics <- transformRawVariablesToADAPARCBaseVariables(map_of_variables, raw_data = readRawExtractedDataFile("national_demographics"), "acs")

base_vars_living <- transformRawVariablesToADAPARCBaseVariables(map_of_variables, raw_data = readRawExtractedDataFile("national_living"), "acs")

base_vars_economic <- transformRawVariablesToADAPARCBaseVariables(map_of_variables, raw_data = readRawExtractedDataFile("national_economic"), "acs")

base_vars_participation <- transformRawVariablesToADAPARCBaseVariables(map_of_variables, raw_data = readRawExtractedDataFile("national_participation"), "acs")

############################################
# Calculate variables from base variables and add to df's
############################################

# TODO: Long term, to facilitate analysis in Tableau, there should not be a distinction between these DFs; rather, we can combine our data with information on

demographics <- addCalculatedVariablesForBaseDemographics(base_vars_demographics)

community_living <- addCalculatedVariablesForBaseLiving(base_vars_living)

community_participation <- addCalculatedVariablesForBaseParticipation(base_vars_participation)

work_economic <- addCalculatedVariablesForBaseEconomic(base_vars_economic)

# ----- Save Output -----

saveTransformedDataFile(demographics, "transformed_acs_demographics")

saveTransformedDataFile(community_living, "transformed_acs_community_living")

saveTransformedDataFile(community_participation, "transformed_acs_community_participation")

saveTransformedDataFile(work_economic, "transformed_acs_work_economic")
