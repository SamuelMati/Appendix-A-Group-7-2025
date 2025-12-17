# Loading libraries and defining a helper function
library(tidyverse)
library(stringdist)
library(ggplot2)

pick_col <- function(df, candidates) {
  x <- intersect(candidates, names(df))
  if (length(x)) x[1] else NA_character_}

fix_timestamp_units <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[x > 1e11] <- x[x > 1e11] / 1000
  x[x <= 0] <- NA_real_
  x}

# Reading .csv files and standardizing them
query <- read_csv("query-data.2021-CLEAN.csv", show_col_types = FALSE) %>%
  select(!starts_with("...")) %>% mutate(JOB_ID = as.character(JOB_ID))

response <- read_csv("response-data.2021-CLEAN.csv", show_col_types = FALSE) %>%
  select(!starts_with("...")) %>% mutate(JOB_ID = as.character(JOB_ID))

industry <- read_csv("industry-data.csv", show_col_types = FALSE) %>%
  select(!starts_with("...")) %>% mutate(JOB_ID = as.character(JOB_ID))

# Ensuring one industry row per session to avoid many-to-many join errors
ind_col <- pick_col(industry, "INDUSTRY_SECTOR_NAME")

industry_one <- if (!is.na(ind_col)) {industry %>% group_by(JOB_ID) %>%
    summarise(INDUSTRY_SECTOR_NAME =
                paste(unique(na.omit(.data[[ind_col]])), collapse = ";"),
              .groups = "drop")
} else {tibble(JOB_ID = unique(industry$JOB_ID),
               INDUSTRY_SECTOR_NAME = NA_character_)}

# Standardizing the response table
flag_col <- pick_col(response, "RESPONSE_TYPE")
ts_col   <- pick_col(response, "TIMESTAMP")

response <- response %>% mutate(RESPONSE_TYPE = if (!is.na(flag_col)) as.integer(.data[[flag_col]]) else NA_integer_,
                                TIMESTAMP = if (!is.na(ts_col)) as.numeric(.data[[ts_col]]) else NA_real_) %>%
  mutate(TIMESTAMP = fix_timestamp_units(TIMESTAMP))

response_summary <- response %>% group_by(JOB_ID) %>%
  summarise(any_positive_response = any(RESPONSE_TYPE == 1, na.rm = TRUE),
            total_positive_responses = sum(RESPONSE_TYPE == 1, na.rm = TRUE),
            first_positive_resp_ts =
              if (any(RESPONSE_TYPE == 1, na.rm = TRUE))
                min(TIMESTAMP[RESPONSE_TYPE == 1], na.rm = TRUE) else NA_real_,
            first_any_response_ts =
              if (any(!is.na(TIMESTAMP)))
                min(TIMESTAMP, na.rm = TRUE) else NA_real_,
            .groups = "drop")

# Preparing query
query <- query %>% mutate(QUERY_COUNTER = as.integer(QUERY_COUNTER),
                          TIMESTAMP = fix_timestamp_units(TIMESTAMP),
                          token_count = str_count(coalesce(QUERY, ""), "\\S+"),
                          boolean_flag = str_detect(coalesce(QUERY, ""),
                                                    "\\b(and|or|not)\\b|\\+|\\-|\\|")) %>%
  arrange(JOB_ID, QUERY_COUNTER)

# Limiting the session to 5minutes - 2hours
session_bounds <- query %>% group_by(JOB_ID) %>%
  summarise(start_ts = min(TIMESTAMP, na.rm = TRUE),
            end_ts   = max(TIMESTAMP, na.rm = TRUE),
            duration_s = end_ts - start_ts,
            .groups = "drop")

valid_sessions <- session_bounds %>%
  filter(between(duration_s, 60, 14400)) %>%
  pull(JOB_ID)

query            <- query %>% filter(JOB_ID %in% valid_sessions)
response_summary <- response_summary %>% filter(JOB_ID %in% valid_sessions)
industry_one     <- industry_one %>% filter(JOB_ID %in% valid_sessions)

# Query calculations (including Levenshtein method)
qr <- query %>% left_join(response_summary, by = "JOB_ID") %>%
  left_join(industry_one, by = "JOB_ID") %>%
  group_by(JOB_ID) %>% mutate(
    time_gap_s = TIMESTAMP - lag(TIMESTAMP),
    reformulation_dist =
      if_else(!is.na(lag(QUERY)),
              stringdist(lag(QUERY), QUERY, method = "lv"),
              NA_real_)) %>%
  ungroup()

# Calculating the time to first positive response
time_to_resp <- response_summary %>%
  left_join(session_bounds %>% select(JOB_ID, start_ts), by = "JOB_ID") %>%
  mutate(time_to_first_pos_response_min = (first_positive_resp_ts - start_ts) / 60) %>%
  filter(between(time_to_first_pos_response_min, 0, 240)) %>%
  select(JOB_ID, time_to_first_pos_response_min)

# Session-level aggregations
session_rq2 <- qr %>%
  group_by(JOB_ID) %>%
  summarise(n_queries = n(),
            avg_time_gap_s = mean(time_gap_s, na.rm = TRUE),
            median_time_gap_s = median(time_gap_s, na.rm = TRUE),
            avg_reformulation = mean(reformulation_dist, na.rm = TRUE),
            median_reformulation = median(reformulation_dist, na.rm = TRUE),
            boolean_usage_rate = mean(boolean_flag, na.rm = TRUE),
            any_positive_response = first(any_positive_response),
            total_positive_responses = first(total_positive_responses),
            industry_sector = first(INDUSTRY_SECTOR_NAME),
            .groups = "drop") %>%
  left_join(time_to_resp, by = "JOB_ID")

# Industry data for plot 4
industry_eff <- session_rq2 %>%
  filter(!is.na(industry_sector),
         !str_detect(industry_sector, ";"),
         time_to_first_pos_response_min > 0) %>%
  group_by(industry_sector) %>%
  summarise(sessions = n(),
            mean_time_to_first_pos_response_min =
              mean(time_to_first_pos_response_min),
            .groups = "drop") %>%
  arrange(desc(sessions))

# Plots

# Plot 1: Response vs no response
session_rq2 %>%
  mutate(status = if_else(any_positive_response,"At least one response", "No response")) %>%
  count(status) %>% mutate(share = n / sum(n)) %>%
  ggplot(aes(status, n, fill = status)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(share, 0.1)),
            vjust = -0.4) +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  labs(title = "Sessions With and Without a Positive Response",
       y = "Number of sessions") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Boolean usage vs effort
ggplot(session_rq2,aes(boolean_usage_rate, avg_time_gap_s)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Boolean Query Usage and Search Effort",
       y = "Average time gap between queries (seconds)") +
  theme_minimal()

# Plot 3: Boolean usage groups vs response time
session_rq2 %>% mutate(boolean_group = case_when(
  boolean_usage_rate == 0 ~ "None",
  boolean_usage_rate <= 0.33 ~ "Low",
  boolean_usage_rate <= 0.66 ~ "Medium",
  TRUE ~ "High")) %>%
  ggplot(aes(boolean_group, time_to_first_pos_response_min)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.05) +
  coord_cartesian(ylim = c(0, 120)) +
  labs(title = "Time to First Positive Response by Boolean Usage",
       y = "Minutes") +
  theme_minimal()

# Plot 4: Industry comparison
industry_eff %>% slice_head(n = 8) %>%
  ggplot(aes(mean_time_to_first_pos_response_min,
             reorder(industry_sector,
                     mean_time_to_first_pos_response_min))) +
  geom_col(fill = "grey") +
  labs(title = "Average Time to First Positive Response by Industry",
       x = "Minutes", y = "Industry sector") +
  theme_minimal()
