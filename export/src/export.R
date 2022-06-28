# Read in National data
load(here::here("national", "clean", "output", "national_clean.Rda"))
dict_vars <- read.csv(here::here("export", "hand", "dict_vars.csv"))

# Move factsheets
if(!dir.exists(paste0(here::here("export", "output", "factsheets"), "/"))) {
  dir.create(paste0(here::here("export", "output", "factsheets"), "/"))
}
purrr::walk(list.files(here::here("national", "generate_national_factsheets", "output")),
            function(x) {
              file.copy(from = here::here("national", "generate_national_factsheets", "output", x), 
                        to = here::here("export", "output", "factsheets", x))
            })

# Read in City data
load(here::here("city", "import_acs", "output", "city_import_acs.Rda"))
dict_location_crosswalk <- read.csv(here::here("city", "places_tracts_crosswalk", "output", "dict_location_crosswalk.txt"))

rm(dict_places, fips_codes_tidy, places_pop_est, 
   places_counties, places_split, tracts_split, 
   year, emitProgress, sourceWithProgress, 
   survey, places_acs, places_sf, 
   places_tracts, places_acs_s1810, places_acs_s1811)
# Environment should look like:
# community_living, community_participation, demographics, work_economic, dict_vars
# dict_location_crosswalk, tracts_data, tracts_sf, city_place_full

save.image(here::here("export", "output", "ADA-Website_data.Rda"))
# load("C:\\Users\\ethan\\Desktop\\ADA PARC\\ADA-PARC-Website-Design\\data\\all_2020N_2019C.rda")
# community_living, community_participation, demographics, work_economic, dict_vars
# dict_location_crosswalk, tracts_data, tracts_sf, city_place_full
