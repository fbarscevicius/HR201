# BUSINESS UNDERSTANDING ----

library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Load Data
path_train    <- '00_Data/telco_train.xlsx'
train_raw_tbl <- read_xlsx(path_train)

# Data Subset
dept_job_role_tbl <- train_raw_tbl %>% 
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)


# 1.  Business Science Problem Framework ----

# 1A. View Business As Machine ----

# BSU's:              Department and Job Role
# Define Objectives:  Retaing High Performers
# Assess Outcomes:    TBD

dept_job_role_tbl %>% 
  
  group_by(Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n))

# 1B. Understand the Drivers ----

# Investigate Objectives: 16% Attrition
# Synthesize Outcomes:    High Counts and High % on some Job Roles
# Hypothesize Drivers:    Job Role, Departments

# Department
dept_job_role_tbl %>% 
 
  group_by(Department, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department) %>% 
  mutate(pct = n/sum(n))

# Job Role
dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition == 'Yes')

# 1C. Measure the Drivers ----

# Collect information on employee attrition:  Ongoing
# Develop KPIs:                               Industry KPIs -> 8.8%

dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition == 'Yes') %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ 'Yes', 
      TRUE ~ 'No'
    )
  )

# 1D. Uncover Problems and Opportunities ----

calculate_attrition_cost <- function(
  
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}

# Calculate cost by job role ----
  
dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition == 'Yes') %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ 'Yes', 
      TRUE ~ 'No'
    )
  ) %>% 
  
  mutate(
    cost = calculate_attrition_cost(n = n)
  )
