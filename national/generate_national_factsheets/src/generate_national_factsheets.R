require(tidyverse)
require(rmarkdown)
require(pagedown)

# The template file and other necessary scripts for the factsheets can be found in /national/generate_national_factsheets/hand/

# Load variable dictionary for relating tables, variable codes,
# and readable values
dict_vars <- read_csv(here::here("national", "generate_national_factsheets", "input", "dict_vars.csv"))
load(here::here("national", "clean", "output", "national_clean.Rda"))

# functions ----
source(here::here("national", "generate_national_factsheets", "hand", "functions.R"))


# Factsheet parameters ----

factsheet_national_params <- dict_vars %>%
  mutate("row_number_temp" = row_number()) %>% 
  filter(!is.na(national_dropdown_label)) %>% 
  select(row_number_temp,
         starts_with("is_"), 
         "national_variable_selector" = var_readable,
         national_dropdown_label) %>% 
  pivot_longer(cols = -c(row_number_temp,
                         national_variable_selector,
                         national_dropdown_label),
               names_to = "national_category_selector") %>%
  filter(value == TRUE) %>% 
  select(row_number_temp,
         national_category_selector,
         national_variable_selector,
         national_dropdown_label) %>% 
  group_by(national_dropdown_label) %>% 
  slice(1) %>% 
  ungroup() %>%
  arrange(row_number_temp) %>%
  mutate("row_number" = str_pad(as.character(row_number()), 3, 
                                side = "left", pad = "0"),
         "output_file" = str_c(here::here("national", "generate_national_factsheets", "output"), "/",
                               row_number, "_",
                               national_dropdown_label, ".html"),
         "params" = pmap(list(national_category_selector,
                              national_variable_selector,
                              year), 
                         ~list(national_category_selector = ..1,
                               national_variable_selector = ..2,
                               year = year))) %>% 
  select(row_number, national_category_selector:params) 


# Create factsheets -------------------------------------------------------


# ----- Rmd to HTML -----

# Delete existing HTML and PDF files
file.remove(list.files(str_c(here::here("national", "generate_national_factsheets", "output"), "/"),
                       pattern = ".(html|pdf)$", 
                       full.names = TRUE)
)

# Walk through dataframe, create HTML documents
factsheet_national_params %>%
  select(output_file, params) %>%
  pwalk(rmarkdown::render,
        input = here::here("national", "generate_national_factsheets", "hand", "template.Rmd"))

# ----- HTML to PDF -----


# Get a vector of all the filepaths for the newly-generated HTML files
html_files <- list.files(here::here("national", "generate_national_factsheets", "output"),
                         pattern = ".html$", 
                         full.names = TRUE)

# Print HTMLs to PDFs
map(.x = html_files, 
    .f = ~chrome_print(input = .x))

# Delete HTML files
file.remove(list.files(here::here("national", "generate_national_factsheets", "output"),
                       pattern = ".html$",
                       full.names = TRUE)
)


# Write params to CSV for front-end table ----------------------------------


# Update output_file to location in GitHub repository
dashboard_factsheet_national <- factsheet_national_params %>% 
  select(national_dropdown_label,
         output_file) %>% 
  mutate("output_file" = str_replace_all(output_file,
                                         pattern = here::here(),
                                         replacement = "https://raw.githubusercontent.com/ada-parc/ADA-PARC-Website-Design/master")) %>% 
  mutate("output_file" = str_replace_all(output_file,
                                         pattern = ".html$",
                                         replacement = ".pdf"))

# Write CSV to file for dashboard front-end
write_csv(dashboard_factsheet_national,
          file = here::here("national", "generate_national_factsheets","output", "dashboard_factsheet_national.csv"))

rm(list = ls())
