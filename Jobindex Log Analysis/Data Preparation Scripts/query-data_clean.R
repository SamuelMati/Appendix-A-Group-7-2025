# Loading the used libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# Importing the .csv file, while omitting white spaces
raw_data <- read.csv("query-data.2021.csv", strip.white=TRUE)
raw_data <- as_tibble(raw_data)

# VERIFICATIONS:
# Check for inconsistent "QUERY_COUNTER" increments in "JOB_ID" + "MESSAGE_ID" pairs(ChatGPT used for assistance)

if (all(c("JOB_ID", "MESSAGE_ID", "QUERY_COUNTER") %in% names(raw_data))) {
  df_counter <- raw_data %>%
    select(JOB_ID, MESSAGE_ID, QUERY_COUNTER) %>%
    filter(!is.na(QUERY_COUNTER)) %>%
    arrange(JOB_ID, MESSAGE_ID, QUERY_COUNTER)
  
  consistency_by_session <- df_counter %>%
    group_by(JOB_ID, MESSAGE_ID) %>%
    summarise(
      counter_ok = all(QUERY_COUNTER == seq_len(n())),
      .groups = "drop"
    )
  
  print(consistency_by_session %>% count(counter_ok))
} else {
  message("Some of JOB_ID, MESSAGE_ID, QUERY_COUNTER are missing from raw_data.")
}

# Checking for backward "TIMESTAMP" instances within a session
if (all(c("JOB_ID", "MESSAGE_ID", "TIMESTAMP") %in% names(raw_data))) {
  df_time <- raw_data %>%
    select(JOB_ID, MESSAGE_ID, TIMESTAMP) %>%
    filter(!is.na(TIMESTAMP)) %>%
    arrange(JOB_ID, MESSAGE_ID, TIMESTAMP)
  
  has_backward_timestamps <- function(x) {
    any(diff(x) < 0, na.rm = TRUE)
  }
  
  time_consistency_by_session <- df_time %>%
    group_by(JOB_ID, MESSAGE_ID) %>%
    summarise(
      backwards = has_backward_timestamps(TIMESTAMP),
      .groups = "drop"
    )
  
  print(time_consistency_by_session %>% count(backwards))
  
} else {
  message("Some of JOB_ID, MESSAGE_ID, TIMESTAMP are missing from raw_data.")
}

# Dropping the duplicates + standardizing the datetime format
raw_data = raw_data %>% filter_all(any_vars(duplicated(.))) %>% 
                        mutate(TIMESTAMP_FORMATTED = ymd_hms(TIMESTAMP_FORMATTED, quiet = TRUE))

# Dropping the rows that are missing a "JOB_ID", "TIMESTAMP" or "QUERY_COUNTER value
essential_cols <- c("JOB_ID", "TIMESTAMP", "QUERY_COUNTER")
essential_cols <- essential_cols[essential_cols %in% names(raw_data)]
raw_data <- raw_data %>% drop_na(all_of(essential_cols))

# Dropping NA columns
empty_cols <- names(raw_data)[map_lgl(raw_data, ~ all(is.na(.x)))]
empty_cols <- unique(empty_cols)
raw_data <- raw_data %>% select(-any_of(empty_cols))

# Converting to lowercase
raw_data <- lapply(raw_data, tolower)
raw_data <- as_tibble(raw_data)

# Dropping invalid sessions that do not have any unique values besides >>
# >> "TIMESTAMP", "TIMESTAMP_FORMATTED" or "QUERY_COUNTER"
session_cols <- intersect(c("JOB_ID", "MESSAGE_ID"), names(raw_data))
ignore_cols <- intersect(
  c("TIMESTAMP", "TIMESTAMP_FORMATTED", "QUERY_COUNTER"),
  names(raw_data))

content_cols <- setdiff(names(raw_data), c(session_cols, ignore_cols))

session_uniqueness <- raw_data %>%
  group_by(across(all_of(session_cols))) %>%
  summarise(
    n_unique_content_rows = n_distinct(across(all_of(content_cols))),
    .groups = "drop"
  )

sessions_to_drop <- session_uniqueness %>%
  filter(n_unique_content_rows == 1) %>%
  select(all_of(session_cols))
clean_data <- raw_data %>%
  anti_join(sessions_to_drop, by = session_cols)

# View and save the cleaned data set
view(clean_data)
write.csv(clean_data, "query-data.2021-CLEAN.csv")