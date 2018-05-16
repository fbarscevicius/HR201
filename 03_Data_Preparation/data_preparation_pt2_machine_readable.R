# DATA PREPARATION ----
# Machine readable

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)
library(magrittr)

# Setup ----
# Data
path_train        <- '00_Data/telco_train.xlsx'
path_test         <- '00_Data/telco_test.xlsx'
path_data_def     <- '00_Data/telco_data_definitions.xlsx'

train_raw_tbl     <- read_excel(path_train, sheet = 1)
test_raw_tbl      <- read_excel(path_train, sheet = 1)
def_raw_tbl       <- read_excel(path_data_def, sheet = 1, col_names = FALSE)

# Processing Pipeline
source('00_Scripts/data_processing_pipeline.R')

train_readable_tbl  <- process_hr_data_readable(train_raw_tbl, def_raw_tbl)
test_readable_tbl   <- process_hr_data_readable(test_raw_tbl, def_raw_tbl)

# Plot Faceted Histogram function ----
plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = palette_light()[[3]], color = "white", ncol = 5, scale = "free") {
  
  data_factored <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = key, value = value, factor_key = TRUE) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale) + 
    theme_tq()
  
  return(g)
  
}

train_raw_tbl %>% 
  plot_hist_facet()

# Data Preprocessing with Recipes ----

# Plan: Correlation Analysis
# 1. Zero Variance Features ----
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors())

# 2. Transformations ----
train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(skewness) %>% 
  gather(factor_key = TRUE) %>% 
  arrange(desc(value)) %>% 
  filter(value >= 0.8) %>% 
  pull(key) %>% 
  as.character() -> skewed_feature_names

train_readable_tbl %>% 
  select(skewed_feature_names) %>% 
  plot_hist_facet()

train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(skewness) %>% 
  gather(factor_key = TRUE) %>% 
  arrange(desc(value)) %>% 
  filter(value >= 0.8) %>% 
  filter(!key %in% c('JobLevel', 'StockOptionLevel')) %>% 
  pull(key) %>% 
  as.character() -> skewed_feature_names

factor_names <- c('JobLevel', 'StockOptionLevel')

recipe_obj %<>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_num2factor(factor_names)

# 3. Center and Scale ----
train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  plot_hist_facet()

recipe_obj %<>%  
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())

# 4. Dummy variables ----
recipe_obj %<>% 
  step_dummy(all_nominal())

# Final Recipe ----
recipe_obj %<>%
  prep()

train_tbl <- bake(recipe_obj, train_readable_tbl)
test_tbl  <- bake(recipe_obj, test_readable_tbl)

# Correlation Analysis ----
# Get Correlations function ----
get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)
  
  if (fct_reorder) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }
  
  if (fct_rev) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }
  
  return(data_cor)
  
}

# Plot Correlations function ----
plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], color_neg = palette_light()[[2]]) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())
  
  g <- data_cor %>%
    ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    theme_tq() +
    scale_color_manual(values = c(color_neg, color_pos)) 
  
  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  
  return(g)
  
}
# Correlation Evaluation ----
# Explore features by category ----
# 1. Descriptive Features: age, gender, marital status
train_tbl %>% 
  select(Attrition_Yes, Age, contains('Gender'), contains('MaritalStatus'), 
         NumCompaniesWorked, DistanceFromHome) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

# 2. Employment features: department, job role, job level
train_tbl %>% 
  select(Attrition_Yes, contains('employee'), contains('department'), contains('job')) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

# 3. Compensation features: hourly rate, monthly income, stock option level
train_tbl %>% 
  select(Attrition_Yes, contains('income'), contains('rate'), contains('salary'), contains('stock')) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

# 4. Survey results: satisfaction level, work-life balance
train_tbl %>% 
  select(Attrition_Yes, contains('satisfaction'), contains('life')) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

# 5. Performance data: job involvement, performance rating
train_tbl %>% 
  select(Attrition_Yes, contains('performance'), contains('involvement')) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

# 6. Work-life features
train_tbl %>% 
  select(Attrition_Yes, contains('overtime'), contains('travel')) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

# 7. Training and education
train_tbl %>% 
  select(Attrition_Yes, contains('training'), contains('education')) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

# 8. Time-based features: years at company, years in current role
train_tbl %>% 
  select(Attrition_Yes, contains('years')) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = TRUE)

