# This is the main file for updating the data for the ADA-PARC website.
# Before running this script, ensure that the year value in /national/import/hand/config.yaml is set to the desired year

# MAKE SURE YOU HAVE A CENSUS API KEY SET UP, THIS SCRIPT WILL NOT RUN WITHOUT ONE
# You can do so by following the instructions at the following link:
# https://walker-data.com/tidycensus/articles/basic-usage.html

## Jane Notes:
## run this regularly to clean up this file: usethis::use_tidy_description()
## devtools::document()
## devtools::check()

#TODO: Consider multithreaded strategies to increase speed/efficiency.


# Extractions (these are all independent and can be run in any order)
source(here::here("analysis", "scripts", "extract", "national_import_acs.R"))
source(here::here("analysis", "scripts", "extract", "national_import_hud.R"))
source(here::here("analysis", "scripts", "extract", "city_import_acs.R"))
source(here::here("analysis", "scripts", "extract", "city_import_tigris.R"))


# Transformations (until the "final" script, execution of these scripts are independent and can be run in any order)
source(here::here("analysis", "scripts", "transform", "national_transform_acs.R"))
source(here::here("analysis", "scripts", "transform", "national_transform_final.R"))


source(here::here("analysis", "scripts", "transform", "city_transform_tigris.R")) # Dependency on "places_pop_est" to filter out locations and improve performance
source(here::here("analysis", "scripts", "transform", "city_transform_acs.R"))
source(here::here("analysis", "scripts", "transform", "city_transform_final.R")) # Combines outputs from previous "city_transform_*" scripts; must run last


# Load
