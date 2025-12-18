library(dplyr)
library(tidyverse)


industry <- read_csv("data_clean/industry-clean.csv")
company  <- read_csv("data_clean/company-clean.csv")
skills   <- read_csv("data_clean/skills-clean.csv")
jobads   <- read_csv("data_clean/job-ad-clean.csv")
area     <- read_csv("data_clean/area-clean.csv")


job_company <- industry %>%
  select(
    job_id,
    company_id,
    company_name,
    industry_sector_id,
    industry_sector_name,
    industry_division_id,
    industry_division_name
  ) %>%
  distinct()


job_master <- jobads %>%
  # 1) attach company + industry info via job_id
  left_join(job_company, by = "job_id") %>%
  
  # 2) attach skills (many skills per job is expected)
  left_join(skills, by = "job_id", relationship = "many-to-many")

job_master %>% glimpse()
job_master %>% count(!is.na(skill))
job_master %>% count(industry_sector_name, sort = TRUE) %>% head(20)

job_level <- job_master %>%
  group_by(job_id, job_title, company_id, company_name,
           industry_sector_name, industry_division_name) %>%
  summarise(
    n_skills = n_distinct(skill),
    n_words  = first(n_words),
    n_chars  = first(n_chars),
    .groups = "drop"
  )
job_master %>%
  count(skill, sort = TRUE) %>%
  head(20)
job_master %>%
  filter(!is.na(skill)) %>%
  count(industry_sector_name, skill, sort = TRUE)
job_master %>%
  group_by(industry_sector_name) %>%
  summarise(
    avg_words = mean(n_words, na.rm = TRUE),
    median_words = median(n_words, na.rm = TRUE),
    n_ads = n_distinct(job_id),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_words))


# RQS1 - Search behaviour

# this one is going to be done by Samo 

# RQS2 - cannot really answer without query and response BUT 
# which job posting find it harder/easier to find candidates?


# if the job ad has many words, more complex and more details + more mental load

industry_complexity <- job_master %>%
  group_by(industry_sector_name) %>%
  summarise(
    avg_words = mean(n_words, na.rm = TRUE),
    n_ads = n_distinct(job_id)
  ) %>% arrange(desc(avg_words))
# medical industry - highest number of words 
#Recruiters working with medicinal sector job postings face the highest cognitive load, as these ads are significantly longer and more complex than all other industries.
# Energy, IT and Public Sector high in words - also high complexity
#transportmiddelindustri Bygge og anlæg Metalindustri Plastindustri - low on words but still 
# top 4 = 979
# low 4 = 604 
###thats HALF A PAGE

# Which factors influence the efficiency and effectiveness of recruiter search processes across job categories?
# industry differences

# SKILLS PER JOB
skills_per_job <- skills %>%
  count(job_id, name = "n_skills")

skills_per_job %>%
  arrange(desc(n_skills)) %>%
  head(20)

job_industry <- industry %>%
  select(job_id, industry_sector_name)

skills_with_industry <- skills_per_job %>%
  left_join(job_industry, by = "job_id")

skills_with_industry %>%
  group_by(industry_sector_name) %>%
  summarise(
    avg_skills = mean(n_skills, na.rm = TRUE),
    max_skills = max(n_skills, na.rm = TRUE),
    n_jobs     = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_skills)) %>%
  head(15)

summary(skills_per_job$n_skills)

library(ggplot2)

ggplot(skills_per_job, aes(n_skills)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Required Skills per Job",
    x = "Number of Skills",
    y = "Count of Job Ads"
  )

# most jobs do not require many skills 2-7 skills
#Matching between candidates and job requirements is simpler
#Most recruitment tasks on Jobindex are cognitively light.

#The majority of job ads list only a small, concentrated set of skills (typically 2–7), meaning that most recruiter searches involve low information complexity, fast decision cycles, and simple query strategies. As a result, Jobindex’s basic search tools are sufficient for typical roles, and inefficiencies primarily arise only in a minority of highly specialized searches.


rare_skills <- skill_freq %>%
  filter(n < 20)         # threshold adjustable

industry_rare <- job_master %>%
  filter(skill %in% rare_skills$skill) %>%
  count(industry_sector_name, skill, sort = TRUE)

ggplot(skill_freq, aes(n)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +   # log scale makes the long tail visible
  labs(
    title = "Distribution of Skill Frequencies",
    x = "Number of Times a Skill Appears (log scale)",
    y = "Number of Skills"
  ) +
  theme_minimal()

skill_freq %>%
  count(n, name = "n_skills") %>%
  arrange(n)
#
#
#
company_complexity <- job_master %>%
  group_by(employee_count_low) %>%
  summarise(
    avg_words  = mean(n_words, na.rm = TRUE),
    avg_skills = mean(n_skills, na.rm = TRUE)
  )
ggplot(company_complexity, aes(employee_count_low, avg_skills)) +
  geom_point() +
  geom_smooth()
#
#
#
location_complexity <- job_master %>%
  group_by(location_name) %>%
  summarise(
    avg_words = mean(n_words, na.rm = TRUE),
    avg_skills = mean(n_skills, na.rm = TRUE),
    n_ads = n_distinct(job_id)
  )

###
#
###

job_level <- job_level %>%
  mutate(
    efficiency_score = 0.5 * scale(n_words) +
      0.5 * scale(n_skills)
  )
job_level %>%
  group_by(industry_sector_name) %>%
  summarise(avg_efficiency_score = mean(efficiency_score)) %>%
  arrange(desc(avg_efficiency_score))


# RQS3 - System Fit & Mismatch
# The Jobindex skill tags are very inconsistent:
# e.g dansinsh dansks dengelsk
# Recruiters need broad conceptual filters; system only offers literal matching
# "Communication" ----- kommunikation, communication atď
# some industries have multiple jobs per sector for example it atd
# soft vs hard skills.... that iz the same picture
# 510 skills only appeared once
# Recruiters expect standardized skill categories……but Jobindex stores skills as free text.
# Not find the job if the spelling or phrasing is slightly different
# “More than half of all unique skills in the Jobindex dataset appear only once.
#This indicates that the platform lacks skill standardization, forcing recruiters to rely on domain knowledge, guesswork and repeated manual searches. The system’s literal, unstructured skill tagging is misaligned with the conceptual and category-based way recruiters think about competencies.”