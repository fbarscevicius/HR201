# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(JobLevel, StockOptionLevel) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, newdata = train_readable_tbl)
test_tbl  <- bake(recipe_obj, newdata = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("04_Modeling/h2o_models/deep_grid_01_model_3")

automl_leader

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl


test_tbl %>%
  slice(5) %>%
  glimpse()

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           = automl_leader,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )

explainer

explanation <- test_tbl %>%
  slice(5) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 1
  )

explanation %>%
  as.tibble() %>%
  select(feature:prediction) 

plot_features(explanation = explanation, ncol = 1)


# 3.3 Multiple Explanations ----

explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 1
  )

explanation %>%
  as.tibble()


plot_features(explanation, ncol = 4)

plot_explanations(explanation)


# 4. Challenge Solutions ----

# 4.1 Recreating plot_features() -----

explanation %>%
  as.tibble()

case_1 <- explanation %>%
  filter(case == 1)

case_1 %>%
  plot_features()


library(glue)

# Transformation
data_transformed <- case_1 %>%
  as.tibble() %>%
  mutate(
    feature_desc = as_factor(feature_desc) %>% 
      fct_reorder(abs(feature_weight), .desc = FALSE),
    key     = ifelse(feature_weight > 0, "Supports", "Contradicts") %>% 
      fct_relevel("Supports"),
    case_text    = glue("Case: {case}"),
    label_text   = glue("Label: {label}"),
    prob_text    = glue("Probability: {round(label_prob, 2)}"),
    r2_text      = glue("Explanation Fit: {model_r2 %>% round(2)}")
  ) %>%
  select(feature_desc, feature_weight, key, case_text:r2_text)

data_transformed


data_transformed %>%
  ggplot(aes(feature_desc, feature_weight, fill = key)) +
  geom_col() +
  coord_flip() +
  theme_tq() +
  scale_fill_tq() +
  labs(y = "Weight", x = "Feature") +
  facet_wrap(~ case_text + label_text + prob_text + r2_text,
             ncol = 1, scales = "free")


plot_features_tq <- function(explanation, ncol) {
  
  data_transformed <- explanation %>%
    as.tibble() %>%
    mutate(
      feature_desc = as_factor(feature_desc) %>% 
        fct_reorder(abs(feature_weight), .desc = FALSE),
      key     = ifelse(feature_weight > 0, "Supports", "Contradicts") %>% 
        fct_relevel("Supports"),
      case_text    = glue("Case: {case}"),
      label_text   = glue("Label: {label}"),
      prob_text    = glue("Probability: {round(label_prob, 2)}"),
      r2_text      = glue("Explanation Fit: {model_r2 %>% round(2)}")
    ) %>%
    select(feature_desc, feature_weight, key, case_text:r2_text)
  
  
  data_transformed %>%
    ggplot(aes(feature_desc, feature_weight, fill = key)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_tq() +
    scale_fill_tq() +
    labs(y = "Weight", x = "Feature") +
    theme(title = element_text(size = 9)) +
    facet_wrap(~ case_text + label_text + prob_text + r2_text,
               ncol = ncol, scales = "free")
  
}

explanation %>%
  filter(case %in% 1) %>%
  plot_features_tq(ncol = 2)

explanation %>%
  filter(case %in% 1:6) %>%
  plot_features(ncol = 2)



# 4.2 Recreating plot_explanations ----



explanation %>%
  as.tibble()

plot_explanations(explanation)


data_transformed <- explanation %>%
  as.tibble() %>%
  mutate(
    case    = as_factor(case),
    order_1 = rank(feature) 
  ) %>%
  # select(case, feature, feature_value, order_1) %>%
  # arrange(order_1)
  group_by(feature) %>%
  mutate(
    order_2 = rank(feature_value)
  ) %>%
  ungroup() %>%
  # select(case, feature, feature_value, order_1, order_2) %>%
  # arrange(order_1, order_2)
  mutate(
    order = order_1 * 1000 + order_2
  ) %>%
  # select(case, feature, feature_value, order_1, order_2, order) %>%
  # arrange(order)
  mutate(
    feature_desc = as.factor(feature_desc) %>% 
      fct_reorder(order, .desc =  T) 
  ) %>%
  select(case, feature_desc, feature_weight, label)

data_transformed %>%
  ggplot(aes(case, feature_desc)) +
  geom_tile(aes(fill = feature_weight)) +
  facet_wrap(~ label) +
  theme_tq() +
  scale_fill_gradient2(low = palette_light()[[2]], 
                       mid = "white",
                       high = palette_light()[[1]]) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(y = "Feature", x = "Case", 
       fill = glue("Feature
                   Weight"))

plot_explanations_tq <- function(explanation) {
  
  data_transformed <- explanation %>%
    as.tibble() %>%
    mutate(
      case    = as_factor(case),
      order_1 = rank(feature) 
    ) %>%
    group_by(feature) %>%
    mutate(
      order_2 = rank(feature_value)
    ) %>%
    ungroup() %>%
    mutate(
      order = order_1 * 1000 + order_2
    ) %>%
    mutate(
      feature_desc = as.factor(feature_desc) %>% 
        fct_reorder(order, .desc =  T) 
    ) %>%
    select(case, feature_desc, feature_weight, label)
  
  data_transformed %>%
    ggplot(aes(case, feature_desc)) +
    geom_tile(aes(fill = feature_weight)) +
    facet_wrap(~ label) +
    theme_tq() +
    scale_fill_gradient2(low = palette_light()[[2]], mid = "white",
                         high = palette_light()[[1]]) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    ) +
    labs(y = "Feature", x = "Case", 
         fill = glue("Feature
                         Weight"))
  
}

plot_explanations(explanation)

plot_explanations_tq(explanation)