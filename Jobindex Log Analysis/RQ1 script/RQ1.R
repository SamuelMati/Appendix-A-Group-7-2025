# Loading the used libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)

# Importing the .csv file
query <- read_csv("query-data.2021-CLEAN.csv", show_col_types = FALSE)

# Standardizing columns and ordering the file
query <- query %>% mutate(JOB_ID = as.character(JOB_ID),
    QUERY_COUNTER = as.numeric(QUERY_COUNTER),
    TIMESTAMP = as.numeric(TIMESTAMP)) %>%
  arrange(JOB_ID, QUERY_COUNTER)

# Breaking down the characteristics of a cells
query <- query %>% mutate(token_count = str_count(QUERY, "\\S+"),
    contains_boolean = str_detect(QUERY, "\\b(and|or|not)\\b|\\+|\\-|\\|"),
    contains_wildcard = str_detect(QUERY, "\\*|\\?"))

# Unique tokens per session 
query_tokens <- query %>% mutate(QUERY = str_to_lower(QUERY)) %>%
  separate_rows(QUERY, sep = "\\s+") %>% filter(!is.na(QUERY), QUERY != "")
unique_tokens_per_session <- query_tokens %>%
  group_by(JOB_ID) %>% summarise(unique_token_count = n_distinct(QUERY))

# Creating filtered query-level dataset for plot analysis
query_filtered <- query %>% filter(!is.na(token_count),
    token_count > 0,
    TIMESTAMP > 0)

# Creating the calculations and summarizing them
session_summary <- query %>% group_by(JOB_ID) %>%
  summarise(n_queries = n(),

    # Query length statistics
    avg_query_length = mean(token_count, na.rm = TRUE),
    median_query_length = median(token_count, na.rm = TRUE),
    
    # Boolean utilization statistics
    boolean_usage_rate = mean(coalesce(as.logical(contains_boolean), FALSE)),
    wildcard_usage_rate = mean(coalesce(as.logical(contains_wildcard), FALSE)),
    
    # Session duration calculation
    session_start = if ( "TIMESTAMP" %in% names(.) && any(!is.na(TIMESTAMP)) ) min(TIMESTAMP, na.rm = TRUE) else NA_real_,
    session_end   = if ( "TIMESTAMP" %in% names(.) && any(!is.na(TIMESTAMP)) ) max(TIMESTAMP, na.rm = TRUE) else NA_real_,
    session_duration_min = if (!is.na(session_start) && !is.na(session_end)) (session_end - session_start) / 60 else NA_real_,
    
    # First and last query lengths
    first_query_len = ifelse(n() >= 1, first(token_count), NA_real_),
    last_query_len  = ifelse(n() >= 1, last(token_count), NA_real_),) %>%
  
  # Convering all NaN to either NA or 0, depending on the variable
  mutate(avg_query_length = ifelse(is.nan(avg_query_length), NA_real_, avg_query_length),
    median_query_length = ifelse(is.nan(median_query_length), NA_real_, median_query_length),
    boolean_usage_rate = ifelse(is.nan(boolean_usage_rate), 0, boolean_usage_rate),
    wildcard_usage_rate = ifelse(is.nan(wildcard_usage_rate), 0, wildcard_usage_rate))

# Joining unique tokens
session_summary <- session_summary %>%
  left_join(unique_tokens_per_session, by = "JOB_ID")

# Filtering out queries below 5 min. and beyond 2 hours
session_summary <- session_summary %>% filter(session_duration_min >= 1,
         session_duration_min <= 240) %>%
  mutate(session_type = case_when(
      session_duration_min <= 60 ~ "Matching",
      session_duration_min > 60 & session_duration_min <= 240 ~ "Recruiting",
      TRUE ~ "Other"))

# Flagging session that use/don't use booleans and wildcards
session_summary <- session_summary %>%mutate(used_boolean = boolean_usage_rate > 0,
    used_wildcard = wildcard_usage_rate > 0)

# Syntax that outputs a highlight of the important metrics using the descriptive statistics
summary_stats <- session_summary %>% summarise(avg_queries_per_session = mean(n_queries, na.rm = TRUE),
    median_queries_per_session = median(n_queries, na.rm = TRUE),
    sd_queries = sd(n_queries, na.rm = TRUE),
    avg_boolean_rate = mean(boolean_usage_rate, na.rm = TRUE),
    avg_wildcard_usage = mean(wildcard_usage_rate, na.rm = TRUE),
    avg_query_length = mean(avg_query_length, na.rm = TRUE),
    median_query_length = median(median_query_length, na.rm = TRUE),
    avg_session_duration = mean(session_duration_min, na.rm = TRUE),
    median_session_duration = median(session_duration_min, na.rm = TRUE))

options(tibble.width = Inf)
cat("The summary of statistics:\n")
print(summary_stats)

# Boolean statisticss
boolean_query_stats <- query_filtered %>%
  summarise(total_queries = n(),
    boolean_queries = sum(contains_boolean, na.rm = TRUE),
    non_boolean_queries = sum(!contains_boolean, na.rm = TRUE),
    pct_boolean = mean(contains_boolean, na.rm = TRUE) * 100)

print(boolean_query_stats)

# Matching vs Recruiting statistics
session_type_stats <- session_summary %>% group_by(session_type) %>%
  summarise(n_sessions = n(),
    mean_duration = mean(session_duration_min, na.rm = TRUE),
    median_duration = median(session_duration_min, na.rm = TRUE),
    sd_duration = sd(session_duration_min, na.rm = TRUE),
    .groups = "drop")

cat("\nSession duration statistics by session type:\n")
print(session_type_stats)

# Session duration and number of queries correlation
duration_query_correlation <- cor(session_summary$session_duration_min,
  session_summary$n_queries,
  use = "complete.obs",
  method = "pearson")

print(duration_query_correlation)

# Plot 1: Distribution of queries per session
mean_queries <- mean(session_summary$n_queries, na.rm = TRUE)
median_queries <- median(session_summary$n_queries, na.rm = TRUE)

ggplot(session_summary %>% filter(n_queries >= 1, n_queries <= 50),
       aes(x = n_queries)) +
  geom_histogram(bins = 25, fill = "grey", color = "black") +
  geom_vline(xintercept = mean_queries, linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = median_queries, linetype = "solid", linewidth = 0.8) +
  labs(title = "Distribution of Queries per Session",
    subtitle = "Full line = Average, Dashed line = Median",
    x = "Number of queries",
    y = "Number of sessions") +
  theme_minimal()

# Plot 2: Relationship between session duration and number of queries
ggplot(session_summary,
       aes(x = session_duration_min, y = n_queries)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Session Duration and Number of Queries",
    subtitle = "Dashed line indicates linear trend across sessions",
    x = "Session duration (minutes)",
    y = "Number of queries in the session") +
  coord_cartesian(xlim = c(1, 240)) +
  theme_minimal()

# Plot 3: Unique tokens per session
ggplot(session_summary, aes(x = unique_token_count)) +
  geom_histogram(bins = 25, fill = "grey", color = "black") +
  labs(title = "Distribution of Unique Tokens per Search Session",
    x = "Number of unique tokens",
    y = "Number of sessions") +
  theme_minimal()

# Plot 4: Session duration by task type
ggplot(session_summary,
       aes(x = session_duration_min, fill = session_type)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "Distribution of Session Duration by Job Type",
    x = "Session duration (minutes)",
    y = "Number of sessions",
    fill = "Session type") +
  theme_minimal()
