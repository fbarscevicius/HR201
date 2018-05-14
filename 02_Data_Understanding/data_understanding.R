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
