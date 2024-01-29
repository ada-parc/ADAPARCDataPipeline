# Read in National data
load(here::here("analysis", "data", "national_clean.Rda"))

# Move factsheets to output
if(!dir.exists(paste0(here::here("export", "output", "factsheets"), "/"))) {
  dir.create(paste0(here::here("export", "output", "factsheets"), "/"))
}
purrr::walk(list.files(here::here("analysis", "generate_national_factsheets", "output")),
            function(x) {
              file.copy(from = here::here("analysis", "generate_national_factsheets", "output", x),
                        to = here::here("export", "output", "factsheets", x))
            })

# Environment should look like:
# community_living, community_participation, demographics, work_economic

save.image(here::here("export", "output", "ADA-Website_tables_national.Rda"))

# load("C:\\Users\\ethan\\Desktop\\ADA PARC\\ADA-PARC-Website-Design\\data\\all_2020N_2019C.rda")
# community_living, community_participation, demographics, work_economic, dict_vars
# dict_location_crosswalk, tracts_data, tracts_sf, city_place_full
