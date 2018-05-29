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
test_raw_tbl      <- read_excel(path_test, sheet = 1)
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
  max_runtime_secs = 120,
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
  h2o.getModel() %>% 
  h2o.saveModel(path = '04_Modeling/h2o_models/')

automlmodels_h2o@leader %>% 
  h2o.saveModel(path = '04_Modeling/h2o_models/')

# 3. Visualizing the leaderboard ----
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
preds_tbl <- h2o.predict(automlmodels_h2o@leader, newdata = test_h2o) %>% 
  as.tibble()

# 4. GRID SEARCH ----
# Test performance
h2o.performance(deeplearning, newdata = test_h2o)

# Grid search
deep_grid_01 <- h2o.grid(
  algorithm = 'deeplearning', 
  x = x,
  y = y, 
  grid_id = 'deep_grid_01',
  
  training_frame = train_h2o,
  validation_frame = valid_h2o, 
  nfolds = 7,
  
  hyper_params = list(
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)), 
    epochs = list(10, 50, 100)
  )
)

h2o.getGrid(grid_id = 'deep_grid_01', sort_by = 'auc', decreasing = T)

deep_grid_01_model_3 <- h2o.getModel('deep_grid_01_model_3')
h2o.saveModel(deep_grid_01_model_3, path = '04_Modeling/h2o_models/')

deep_grid_01_model_3 %>% h2o.auc(train = T, valid = T, xval = T)

deep_grid_01_model_3 %>% h2o.performance(newdata = test_h2o)

# 5. ASSESSING PERFORMANCE ----
xrt_h2o   <- h2o.loadModel('04_Modeling/h2o_models/XRT_0_AutoML_20180528_205227')
gbm_h2o   <- h2o.loadModel('04_Modeling/h2o_models/GBM_grid_0_AutoML_20180528_205227_model_0')
deep_h2o  <- h2o.loadModel('04_Modeling/h2o_models/deep_grid_01_model_3')

perf <- h2o.performance(deep_h2o, newdata = test_h2o)

# Classifier Summary Metrics
h2o.auc(perf)
h2o.confusionMatrix(perf)

performance_tbl <- h2o.metric(perf) %>% 
  as.tibble()

# Precision vs Recall
performance_tbl %>% 
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = 'blue', size = 2) +
  geom_line(aes(y = recall), color = 'red', size = 1) +
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(perf, 'f1')) +
  theme_tq() +
  labs(title = 'Precision vs Recall', 
       y = 'Value')

# ROC plot

load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = test_tbl)
  
  perf_h2o %>% 
    h2o.metric() %>% 
    as.tibble() %>% 
    mutate(auc = h2o.auc(perf_h2o)) %>% 
    select(tpr, fpr, auc, precision, recall)
  
}

model_metrics_tbl <- fs::dir_info(path = '04_Modeling/h2o_models/') %>% 
  select(path) %>% 
  mutate(metrics = map(path, load_model_performance_metrics, test_h2o)) %>% 
  unnest()

model_metrics_tbl %>% 
  mutate(path = str_split(path, pattern = '/', simplify = T)[, 3] %>% as_factor(), 
         auc  = auc %>% round(3) %>% as.character() %>% as_factor
         ) %>% 
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) +
  geom_line(size = 1) + 
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = 'vertical') +
  labs(
    title = 'ROC Plot', 
    subtitle = 'Performance of 3 top performing models'
  )

# Precision vs Recall
model_metrics_tbl %>% 
  mutate(path = str_split(path, pattern = '/', simplify = T)[, 3] %>% as_factor(), 
         auc  = auc %>% round(3) %>% as.character() %>% as_factor
  ) %>% 
  ggplot(aes(recall, precision, color = path, linetype = auc)) +
  geom_line(size = 1) + 
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = 'vertical') +
  labs(
    title = 'Precision vs Recall Plot', 
    subtitle = 'Performance of 3 top performing models'
  )

# Gain & Lift
preds_tbl <- h2o.predict(deep_h2o, newdata = test_h2o) %>% 
  as.tibble()

ranked_predictions_tbl <- preds_tbl %>% 
  bind_cols(test_tbl) %>% 
  select(predict:Yes, Attrition) %>% 
  arrange(desc(Yes))

calculated_gain_lift_tbl <- ranked_predictions_tbl %>% 
  mutate(ntile = ntile(Yes, n = 10)) %>% 
  group_by(ntile) %>% 
  summarise(
    cases = n(), 
    responses = sum(Attrition == 'Yes')
  ) %>% 
  arrange(desc(ntile)) %>% 
  mutate(group = row_number()) %>% 
  select(group, cases, responses) %>% 
  mutate(
    cumulative_response = cumsum(responses), 
    pct_responses       = responses / sum(responses), 
    gain                = cumsum(pct_responses), 
    cumulative_pct_case = cumsum(cases) / sum(cases),
    lift                = gain / cumulative_pct_case, 
    gain_baseline       = cumulative_pct_case, 
    lift_baseline       = gain_baseline / cumulative_pct_case
  )

gain_lift_tbl <- perf %>% 
  h2o.gainsLift() %>% 
  as.tibble()

gain_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
  select(-contains('lift')) %>% 
  mutate(baseline = cumulative_data_fraction) %>% 
  rename(gain = cumulative_capture_rate) %>% 
  gather(key = key, value = value, gain, baseline)

gain_transformed_tbl %>% 
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = 'Gain Chart', 
    x = 'Cumulative Data Fraction', 
    y = 'Gain'
  )

lift_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
  select(-contains('capture')) %>% 
  mutate(baseline = 1) %>% 
  rename(lift = cumulative_lift) %>% 
  gather(key = key, value = value, lift, baseline)

lift_transformed_tbl %>% 
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = 'Lift Chart', 
    x = 'Cumulative Data Fraction', 
    y = 'Gain'
  )

# 6. Performance visualization ----
plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
  
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as.tibble()
  
  order_by <- tolower(order_by[[1]])
  order_by_expr <- rlang::sym(order_by)
  
  h2o.no_progress()
  
  # 1. Model metrics
  
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest() %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    )
  
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical")
  
  # 1B. Precision vs Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes_string("recall", "precision", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position = "none")
  
  
  # 2. Gain / Lift
  
  get_gain_lift <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
    
    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
  }
  
  gain_lift_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
    unnest() %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    ) %>%
    rename(
      gain = cumulative_capture_rate,
      lift = cumulative_lift
    ) 
  
  # 2A. Gain Plot
  
  p3 <- gain_lift_tbl %>%
    ggplot(aes_string("cumulative_data_fraction", "gain", 
                      color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 color = "black", size = size) +
    theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Gain",
         x = "Cumulative Data Fraction", y = "Gain") +
    theme(legend.position = "none")
  
  # 2B. Lift Plot
  
  p4 <- gain_lift_tbl %>%
    ggplot(aes_string("cumulative_data_fraction", "lift", 
                      color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 color = "black", size = size) +
    theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Lift",
         x = "Cumulative Data Fraction", y = "Lift") +
    theme(legend.position = "none")
  
  
  # Combine using cowplot
  p_legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position = "none")
  
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2) 
  
  p_title <- ggdraw() + 
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               colour = palette_light()[[1]])
  
  p_subtitle <- ggdraw() + 
    draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
               colour = palette_light()[[1]])
  
  ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
  
}

automlmodels_h2o@leaderboard %>% 
  plot_h2o_performance(newdata = test_h2o)
