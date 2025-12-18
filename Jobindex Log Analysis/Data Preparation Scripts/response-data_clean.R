# Loading the used libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# Importing the .csv file, while omitting white spaces
raw_data <- read.csv("response-data.2021.csv", strip.white=TRUE)
raw_data <- as_tibble(raw_data)

# Removing rows with NAs in the essential columns
essential_cols <- c("JOB_ID", "CV_ID", "TIMESTAMP", "RESPONSE_TYPE")
essential_cols <- intersect(essential_cols, names(raw_data))

raw_data <- raw_data %>%
  drop_na(all_of(essential_cols))

# Dropping the duplicates + standardizing the datetime format
raw_data = raw_data %>% filter_all(any_vars(duplicated(.))) %>% 
  mutate(TIMESTAMP_FORMATTED = ymd_hms(TIMESTAMP_FORMATTED, quiet = TRUE))

# Dropping NA columns
empty_cols <- names(raw_data)[map_lgl(raw_data, ~ all(is.na(.x)))]
empty_cols <- unique(empty_cols)
clean_data <- raw_data %>% select(-any_of(empty_cols))

# View and save the cleaned data set
view(clean_data)
write.csv(clean_data, "response-data.2021-CLEAN.csv")