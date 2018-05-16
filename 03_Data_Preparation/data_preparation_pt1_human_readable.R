# DATA PREPARATION ----
# Human readable

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

# Data
path_train        <- '00_Data/telco_train.xlsx'
path_data_def     <- '00_Data/telco_data_definitions.xlsx'

train_raw_tbl     <- read_excel(path_train, sheet = 1)
def_raw_tbl       <- read_excel(path_data_def, sheet = 1, col_names = FALSE)

# Tidying the Data ----
def_raw_tbl %>% 
  fill(X__1, .direction = 'down') %>% 
  filter(!is.na(X__2)) %>% 
  separate(X__2, into = c('key', 'value'), sep = " '", remove = TRUE) %>% 
  rename(column_name = X__1) %>% 
  mutate(key = as.numeric(key)) %>% 
  mutate(value = value %>% str_replace(pattern = "'", replacement = '')) -> def_tbl

def_tbl %>% 
  split(.$column_name) %>% 
  map(~ select(., -column_name)) %>% 
  map(~ mutate(., value = as_factor(value))) -> def_list

for (i in seq_along(def_list)) {
  
  list_name <- names(def_list)[i]
  colnames(def_list[[i]]) <- c(list_name, paste0(list_name, '_value'))

}

list(HR_Data = train_raw_tbl) %>% 
  append(def_list, after = 1) %>% 
  reduce(left_join) %>% 
  select(-one_of(names(def_list))) %>% 
  set_names(str_replace_all(names(.), pattern = '_value', replacement = '')) %>% 
  select(sort(names(.))) -> data_merged_tbl

data_merged_tbl %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel('Non-Travel', 'Travel_Rarely', 'Travel_Frequently'), 
    MaritalStatus  = MaritalStatus %>% fct_relevel('Single', 'Married', 'Divorced')
  ) -> data_processed_tbl

# Processing Pipeline ----

process_hr_data_readable <- function(data, def_raw_tbl) {
  
  def_raw_tbl %>% 
    fill(X__1, .direction = 'down') %>% 
    filter(!is.na(X__2)) %>% 
    separate(X__2, into = c('key', 'value'), sep = " '", remove = TRUE) %>% 
    rename(column_name = X__1) %>% 
    mutate(key = as.numeric(key)) %>% 
    mutate(value = value %>% str_replace(pattern = "'", replacement = '')) %>% 
    split(.$column_name) %>% 
    map(~ select(., -column_name)) %>% 
    map(~ mutate(., value = as_factor(value))) -> def_list
  
  for (i in seq_along(def_list)) {
    
    list_name <- names(def_list)[i]
    colnames(def_list[[i]]) <- c(list_name, paste0(list_name, '_value'))
    
  }
  
  list(HR_Data = data) %>% 
    append(def_list, after = 1) %>% 
    reduce(left_join) %>% 
    select(-one_of(names(def_list))) %>% 
    set_names(str_replace_all(names(.), pattern = '_value', replacement = '')) %>% 
    select(sort(names(.))) %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel('Non-Travel', 'Travel_Rarely', 'Travel_Frequently'), 
      MaritalStatus  = MaritalStatus %>% fct_relevel('Single', 'Married', 'Divorced')
    ) -> data_processed_tbl
  
  return(data_processed_tbl)
  
}

processed_data_tbl <- process_hr_data_readable(train_raw_tbl, def_raw_tbl = def_raw_tbl)
