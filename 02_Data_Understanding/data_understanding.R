# DATA UNDERSTANDING ----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

# Load Data
path_train            <- '00_Data/telco_train.xlsx'
path_data_definition  <- '00_Data/telco_data_definitions.xlsx'

train_raw_tbl         <- read_excel(path_train)
definitions_raw_tbl   <- read_excel(path_data_definition, col_names = FALSE)

glimpse(train_raw_tbl)

# EDA ----
# Step 1: Data Summarization ----

skim(train_raw_tbl)

# Character Data Type
train_raw_tbl %>% 
  select_if(is.character) %>% 
  glimpse()

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(unique)

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(~ table(.) %>% prop.table)

# Numeric Data
train_raw_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(~ unique(.) %>% length) %>% 
  gather() %>% 
  arrange(value) %>% 
  filter(value <= 10)

# Step 2: Data Visualization ----

train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  ggpairs()

train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  ggpairs(aes(color = Attrition), lower = 'blank', legend = 1, 
          diag = list(continuous = wrap('densityDiag', alpha = 0.5))) +
  theme(legend.position = 'bottom')


plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}

# Explore features by category ----
# 1. Descriptive Features: age, gender, marital status
train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  plot_ggpairs(color = Attrition)

# 2. Employment features: department, job role, job level
train_raw_tbl %>% 
  select(Attrition, contains('employee'), contains('department'), contains('job')) %>% 
  plot_ggpairs(color = Attrition)

# 3. Compensation features: hourly rate, monthly income, stock option level
train_raw_tbl %>% 
  select(Attrition, contains('income'), contains('rate'), contains('salary'), contains('stock')) %>% 
  plot_ggpairs(color = Attrition)

# 4. Survey results: satisfaction level, work-life balance
train_raw_tbl %>% 
  select(Attrition, contains('satisfaction'), contains('life')) %>% 
  plot_ggpairs(color = Attrition)

# 5. Performance data: job involvement, performance rating
train_raw_tbl %>% 
  select(Attrition, contains('performance'), contains('involvement')) %>% 
  plot_ggpairs(color = Attrition)

# 6. Work-life features
train_raw_tbl %>% 
  select(Attrition, contains('overtime'), contains('travel')) %>% 
  plot_ggpairs(color = Attrition)

# 7. Training and education
train_raw_tbl %>% 
  select(Attrition, contains('training'), contains('education')) %>% 
  plot_ggpairs(color = Attrition)

# 8. Time-based features: years at company, years in current role
train_raw_tbl %>% 
  select(Attrition, contains('years')) %>% 
  plot_ggpairs(color = Attrition)
