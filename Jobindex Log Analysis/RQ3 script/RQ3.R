
#
# Research Question 3:
# Where do mismatches arise between recruiter search practices
# and the Jobindex system?
#
# This script:
#  - Loads cleaned datasets
#  - Computes core RQ3 measures
#  - Produces summary tables and figures used in the analysis

library(tidyverse)
library(stringr)
library(janitor)
library(scales)

skills   <- read_csv("skills-clean.csv",   show_col_types = FALSE)
industry <- read_csv("industry-clean.csv", show_col_types = FALSE)
jobads   <- read_csv("job-ad-clean.csv",   show_col_types = FALSE)
queries  <- read_csv("query-clean.csv",    show_col_types = FALSE) %>%
  clean_names()

print("Loaded datasets:")
print(list(
  skills   = nrow(skills),
  industry = nrow(industry),
  jobads   = nrow(jobads),
  queries  = nrow(queries)
))

# Combine job ads with industry and skill information
# ------
job_master <- jobads %>%
  left_join(industry %>% select(job_id, industry_sector_name), by = "job_id") %>%
  left_join(skills, by = "job_id", relationship = "many-to-many")

print("Preview of combined job data:")
print(job_master, n = 20)

# --
# Skill frequency (raw labels)

skill_freq <- skills %>%
  count(skill, sort = TRUE)

print("Skill frequency summary:")
print(skill_freq, n = 20)

# Skill normalization to identify duplicates

skill_normalized <- skills %>%
  mutate(
    skill_clean = skill %>%
      str_to_lower() %>%
      str_replace_all("[-_ ]", "")
  )

skill_concept_counts <- skill_normalized %>%
  count(skill_clean, name = "total_frequency") %>%
  arrange(desc(total_frequency))

print("Most common normalized skill concepts:")
print(skill_concept_counts, n = 30)

# Match job ad skills with recruiter query terms
query_terms <- queries %>%
  filter(!is.na(query)) %>%
  mutate(
    query = str_replace_all(query, "[+()\"']", " "),
    query = str_replace_all(query, "\\*", ""),
    query = str_to_lower(query)
  ) %>%
  separate_rows(query, sep = "\\s+") %>%
  filter(query != "") %>%
  transmute(
    job_id,
    term_clean = str_replace_all(query, "[-_ ]", "")
  ) %>%
  distinct(job_id, term_clean)

job_skill_pairs <- skill_normalized %>%
  distinct(job_id, skill, skill_clean)

skill_query_overlap <- job_skill_pairs %>%
  left_join(query_terms, by = c("job_id", "skill_clean" = "term_clean")) %>%
  mutate(skill_queried = if_else(!is.na(term_clean), 1L, 0L)) %>%
  select(job_id, skill, skill_clean, skill_queried)

# ................................
# Skill-level mismatch overview

skill_mismatch_overview <- skill_query_overlap %>%
  group_by(skill, skill_clean) %>%
  summarise(
    n_jobs_with_skill    = n(),
    n_jobs_where_queried = sum(skill_queried),
    share_queried        = n_jobs_where_queried / n_jobs_with_skill,
    .groups = "drop"
  ) %>%
  arrange(share_queried, desc(n_jobs_with_skill))

print("Skills least often queried despite appearing in job ads:")
print(head(skill_mismatch_overview, 30))

# Job-level skill coverage
job_skill_coverage <- skill_query_overlap %>%
  group_by(job_id) %>%
  summarise(
    n_skills             = n(),
    n_skills_queried     = sum(skill_queried),
    share_skills_queried = n_skills_queried / n_skills,
    .groups = "drop"
  )

# ..............
# Industry-level mismatch
industry_skill_mismatch <- job_master %>%
  distinct(job_id, industry_sector_name) %>%
  left_join(job_skill_coverage, by = "job_id") %>%
  group_by(industry_sector_name) %>%
  summarise(
    avg_share_skills_queried = mean(share_skills_queried, na.rm = TRUE),
    n_jobs                   = n_distinct(job_id),
    .groups = "drop"
  ) %>%
  arrange(avg_share_skills_queried)

print("Industries with lowest alignment between job skills and queries:")
print(industry_skill_mismatch, n = 30)

# Export core RQ3 results
# ------------------------------------------------------------
write_csv(skill_freq,              "RQ3/skill_frequency.csv")
write_csv(skill_concept_counts,    "RQ3/skill_concept_counts.csv")
write_csv(skill_mismatch_overview, "RQ3/skill_mismatch_overview.csv")
write_csv(job_skill_coverage,      "RQ3/job_skill_coverage.csv")
write_csv(industry_skill_mismatch, "RQ3/industry_skill_mismatch.csv")

# Visualizations

# Distribution of raw skill frequencies
ggplot(skill_freq, aes(n)) +
  geom_histogram(bins = 40) +
  scale_x_log10() +
  labs(
    title = "Skill Frequency Distribution",
    x = "Number of job ads (log scale)",
    y = "Number of skills"
  ) +
  theme_minimal()

# Distribution of normalized skill concepts
ggplot(skill_concept_counts, aes(total_frequency)) +
  geom_histogram(bins = 40) +
  scale_x_log10() +
  labs(
    title = "Normalized Skill Frequency Distribution",
    x = "Number of job ads (log scale)",
    y = "Number of skills"
  ) +
  theme_minimal()

# Top skills: share queried
top_common_skills <- skill_mismatch_overview %>%
  slice_max(n_jobs_with_skill, n = 30) %>%
  arrange(share_queried)

ggplot(top_common_skills,
       aes(x = reorder(skill, share_queried), y = share_queried)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "How Often Job Skills Are Queried",
    x = "Skill",
    y = "Share of jobs where skill is queried"
  ) +
  theme_minimal()

# Skill coverage per job
ggplot(job_skill_coverage, aes(share_skills_queried)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Skill Coverage per Job",
    x = "Share of job skills queried",
    y = "Number of jobs"
  ) +
  theme_minimal()

# Industry-level alignment
ggplot(industry_skill_mismatch,
       aes(x = reorder(industry_sector_name, avg_share_skills_queried),
           y = avg_share_skills_queried)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Industry-Level Skill–Query Alignment",
    x = "Industry",
    y = "Average share of skills queried"
  ) +
  theme_minimal()

print("RQ3 analysis and figures completed.")
