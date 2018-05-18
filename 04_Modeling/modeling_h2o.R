# H2O MODELING ----

# 1. Setup ----
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

# Processing pipeline
source('00_Scripts/data_processing_pipeline.R')
train_readable_tbl  <- process_hr_data_readable(train_raw_tbl, def_raw_tbl)
test_readable_tbl   <- process_hr_data_readable(test_raw_tbl, def_raw_tbl)

# ML Preprocessing
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_num2factor(JobLevel, StockOptionLevel) %>% 
  prep()

train_tbl <- bake(recipe_obj, newdata = train_readable_tbl)
test_tbl  <- bake(recipe_obj, newdata = test_readable_tbl)

# 2. Modeling ----
h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- 'Attrition'
x <- setdiff(names(train_h2o), y)

automlmodels_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 1800,
  nfolds = 10
)

extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) {
  
  name <- h2o_leaderboard %>% 
    as.tibble() %>% 
    slice(n) %>% 
    pull(model_id)
  
  if (verbose) message(name)
    
  return(name)
  
}

automlmodels_h2o@leaderboard %>%
  extract_h2o_model_name_by_position(3) %>% 
  h2o.getModel()

automlmodels_h2o@leader %>% 
  h2o.saveModel(path = '04_Modeling/h2o_models/')

# Visualizing the leaderboard ----
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
  
  # Setup inputs
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
  
  # Transformation
  if (order_by == "auc") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
      ) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname), factor_key = T)
    
  } else if (order_by == "logloss") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as.factor(model_type)
      ) %>%
      gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)
    
  } else {
    stop(paste0("order_by = '", order_by, "' is not a permitted option."))
  }
  
  # Visualization
  g <- data_transformed_tbl %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~ key, scales = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Leaderboard Metrics",
         subtitle = paste0("Ordered by: ", toupper(order_by)),
         y = "Model Postion, Model ID", x = "")
  
  if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
  
  return(g)
  
}

plot_h2o_leaderboard(automlmodels_h2o@leaderboard, order_by = 'auc', n_max = 20)

# Predictions ----
preds <- h2o.predict(automlmodels_h2o@leader, newdata = test_h2o)


