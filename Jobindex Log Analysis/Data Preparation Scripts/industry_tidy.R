library(tidyverse)
library(janitor)
library(stringr)

industry_clean <- read_csv("industry-data.csv") %>%
  clean_names() %>%
  mutate(
    employee_count = str_trim(employee_count),
    employee_count = na_if(employee_count, "NA"),
    employee_count = as.numeric(str_extract(employee_count, "\\d+"))
  ) %>%
  select(
    job_id,
    company_id,
    company_name,
    industry_sector_name,
    employee_count
  ) %>%
  distinct()

write_csv(industry_clean, "data_clean/industry-clean.csv")
