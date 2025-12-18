# R/postal-tidy.R
# ------------------------------------------------
# Clean postal / location data from area-data-2021.csv
# Keep full city names (e.g. "København K", "København V")
# ------------------------------------------------

library(tidyverse)
library(janitor)
library(stringr)

# 1. Load raw data ---------------------------------------------------------
postal_raw <- read_csv("area-data-2021.csv") %>% 
  clean_names()   # LOCATION_ID, LOCATION_NAME -> location_id, location_name

# 2. Split postal code and city name --------------------------------------
postal_clean <- postal_raw %>%
  mutate(
    # postal code = first numeric token at start of string
    postal_code = as.integer(str_extract(location_name, "^\\d+")),
    
    # city part = everything after "<digits><space>"
    city_full   = str_squish(str_remove(location_name, "^\\d+\\s+"))
  ) %>%
  select(location_id, postal_code, city_full)

# optional quick check
postal_clean %>% head(10)

# 3. Save cleaned data -----------------------------------------------------
write_csv(postal_clean, "data_clean/postal-clean.csv")
saveRDS(postal_clean, "data_clean/postal-clean.rds")

