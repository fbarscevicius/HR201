# H2O MODELING ----

# Libraries
library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)
library(cowplot)
library(fs)
library(glue)

# Load Data
path_train        <- '00_Data/telco_train.xlsx'
path_test         <- '00_Data/telco_test.xlsx'
path_data_def     <- '00_Data/telco_data_definitions.xlsx'

train_raw_tbl     <- read_excel(path_train, sheet = 1)
test_raw_tbl      <- read_excel(path_train, sheet = 1)
def_raw_tbl       <- read_excel(path_data_def, sheet = 1, col_names = FALSE)

