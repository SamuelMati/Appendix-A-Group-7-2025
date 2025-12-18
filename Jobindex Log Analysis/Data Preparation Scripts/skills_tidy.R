# R/skills-tidy.R
# ----------------------------
# Clean and tidy the skills dataset
# ----------------------------

library(tidyverse)
library(janitor)
library(stringr)

# 1. Load raw data ---------------------------------------------------------

# Adjust the file name if yours is different, e.g. "skills-data-2021.csv"
skills_raw <- read_csv("skills-data-2021.csv") %>%
  clean_names()   # makes columns: job_id, skill


# 2. Clean SKILL column ----------------------------------------------------

skills <- skills_raw %>%
  mutate(
    # trim whitespace
    skill = str_trim(skill),
    
    # turn literal "NA" into real NA
    skill = na_if(skill, "NA"),
    
    # lowercase everything for consistency
    skill = str_to_lower(skill),
    
    # treat empty strings as NA
    skill = if_else(skill == "", NA_character_, skill)
  ) %>%
  # drop rows with missing skills
  filter(!is.na(skill)) %>%
  # remove exact duplicates (same job_id + skill)
  distinct()


# 3. Create useful summary tables -----------------------------------------

# 3a. How many skills per job?
skills_per_job <- skills %>%
  count(job_id, name = "n_skills")

# 3b. Global skill frequency
skill_freq <- skills %>%
  count(skill, sort = TRUE)


# 4. (Optional) print a quick check ---------------------------------------

skills_per_job %>% arrange(desc(n_skills)) %>% head(10)
skill_freq %>% head(20)


# 5. Save cleaned data -----------------------------------------------------

# Make sure the 'data_clean' folder exists before running this
write_csv(skills,         "data_clean/skills-clean.csv")
write_csv(skills_per_job, "data_clean/skills-per-job.csv")
write_csv(skill_freq,     "data_clean/skills-frequency.csv")
