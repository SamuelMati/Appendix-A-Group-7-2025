# R/job-ad-tidy.R
# ----------------------------
# Clean and tidy the job ad text dataset
# ----------------------------

library(tidyverse)
library(janitor)
library(stringr)

# 1. Load raw data ---------------------------------------------------------
# Adjust file path/name if needed
job_ads_raw <- read_csv("job-ad-data-2021.csv") %>%
  clean_names()   # JOB_ID, TEXT -> job_id, text


# 2. Basic text cleaning ---------------------------------------------------

job_ads <- job_ads_raw %>%
  mutate(
    # ensure text is character and trim weird whitespace
    text = as.character(text),
    text = str_squish(text),
    
    # remove mailto: fragments and naked URLs
    text = str_remove_all(text, "mailto:[^ ]+"),
    text = str_remove_all(text, "http[^ ]+"),
    
    # optional: normalize some odd characters
    text = str_replace_all(text, "", "-"),
    text = str_replace_all(text, "", "'")
  )


# 3. Derive simple meta-features -------------------------------------------

job_ads <- job_ads %>%
  mutate(
    # number of characters and words
    n_chars    = nchar(text),
    n_words    = str_count(text, "\\S+"),
    
    # crude job title proxy: first sentence-like chunk before first period
    job_title_raw = str_extract(text, "^[^.\\n]{10,120}"),
    job_title     = str_squish(job_title_raw)
  )


# 4. Quick sanity checks (optional) ----------------------------------------

# top 10 longest ads by word count
job_ads %>%
  arrange(desc(n_words)) %>%
  select(job_id, n_words, job_title) %>%
  head(10)

# simple distribution of word counts
job_ads %>%
  summarise(
    min_words = min(n_words, na.rm = TRUE),
    q1_words  = quantile(n_words, 0.25, na.rm = TRUE),
    median_words = median(n_words, na.rm = TRUE),
    q3_words  = quantile(n_words, 0.75, na.rm = TRUE),
    max_words = max(n_words, na.rm = TRUE)
  )


# 5. Save cleaned data -----------------------------------------------------

# Make sure 'data_clean' exists
write_csv(job_ads, "data_clean/job-ad-clean.csv")
saveRDS(job_ads, "data_clean/job-ad-clean.rds")
