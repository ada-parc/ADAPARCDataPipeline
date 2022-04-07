library(tidyverse)
library(tidycensus)

year = 2020

source(here::here("scripts", "functions.R"))

# ---- Download National Data ----

tables <- c("S1810")
national_demographics <- downloadAndFormatAcs(tables)

tables <- c("S1810", "S2601A", "S2602", "B26108")
national_living <- downloadAndFormatAcs(tables)

tables <- c("S1810", "S1811", "B18135") 
national_participation <- downloadAndFormatAcs(tables)

tables <- c("S1810", "S1811", 
            "B18135", "B18140", "B25091", "B25070", 
            "C18120", "C18121", "C18130") 
national_economic <- downloadAndFormatAcs(tables)


# ---- Code National Data ----

source(here::here("scripts", "national_data_clean.R"))

rm(national_demographics, national_living, national_participation, national_economic)

