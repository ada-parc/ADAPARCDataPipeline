# This is the main file for updating the data for the ADA-PARC website.
# Before running this script, ensure that the year value in /national/import/hand/config.yaml is set to the desired year
# Running this script will take at least 8 hours to run and upwards of 12 - 13 hours
# If you want to break this process up it is recommended to run the scripts below according to their sections
# The city data takes significantly longer than the national data, with the bulk of the processing time happening in
# /city/places_tracts_crosswalk/src/places_tracts_crosswalk.R and /city/import_acs/src/city_import.R

# This repository is set up so that each task is contained within its own folder with a corresponding input, output, and src folder.
# You can read about the task-based workflow at this link:
# https://hrdag.org/2016/06/14/the-task-is-a-quantum-of-workflow/
# Each task is self-contained, with most tasks outputting at least a .Rda file containing the necessary environment variables
# for the subsequent task to function. In some cases .csv, .shp, .pdf, or other files are generated. This project does not require the
# user to move any files themselves, except to move the final .Rda file and set of .pdfs from the export task to the appropriate ADA-PARC Website
# folders.

# MAKE SURE YOU HAVE A CENSUS API KEY SET UP, THIS SCRIPT WILL NOT RUN WITHOUT ONE
# You can do so by following the instructions at the following link:
# https://walker-data.com/tidycensus/articles/basic-usage.html

# Warning:
# This script is likely to produce errors if the year value is set below 2018.
# Many of the tables this script uses did not exist prior to 2018.


## Jane Notes:
## run this regularly to clean up this file: usethis::use_tidy_description()
## devtools::document()
## devtools::check()

# Extractions (these are all independent and can be run in any order)
#TODO: Consider multithreaded strategies to increase speed/efficiency.
# rename: national_acs_extract
source(here::here("analysis", "scripts", "extract", "national_import_acs.R"))
source(here::here("analysis", "scripts", "extract", "city_import_acs.R"))
source(here::here("analysis", "scripts", "extract", "city_import_tigris.R"))

# Transformations--run transformations here. Can be multithreaded between flows.
# Output targets are national data and city data; within national and city, execution is linear.
source(here::here("analysis", "data", "transform", "national_acs_transformation"))
source(here::here("analysis", "data", "transform", "city_transform_tigris"))


########## Old Work saved as a note

# Section 1: National Data
source(here::here("analysis", "data", "scripts", "extract", "national_import.R"))
source(here::here("analysis", "data", "transform", "national_clean.R"))
# source(here::here("analysis", "generate_national_factsheets", "src", "generate_national_factsheets.R"))
# # Export
source(here::here("analysis", "data", "load", "src", "export_national.R"))


# Section 2: City Data
source(here::here("city", "import_sf", "src", "city_import_sf.R"))
source(here::here("city", "places_counties_crosswalk", "src", "places_counties_crosswalk.R"))
# Break
source(here::here("city", "places_tracts_crosswalk", "src", "places_tracts_crosswalk.R"))
# Break
source(here::here("city", "import_acs", "src", "city_import_acs.R"))
source(here::here("export", "src", "export_city.R"))
