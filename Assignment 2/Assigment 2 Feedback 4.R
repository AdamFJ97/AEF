install.packages("furrr")
install.packages("glmnet")
install.packages("broom")
install.packages("timetk")
install.packages("tidymodels")
install.packages("keras")
install.packages("hardhat")
install.packages("ranger")

library(furrr) 
library(glmnet)
library(broom)
library(timetk)
library(scales)
library(tidymodels)

library(keras)
library(hardhat)
library(ranger)


library(stats)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(scales)
library(RSQLite)
library(RPostgres)
library(lmtest)
library(tidyverse)
library(RSQLite)
library(lubridate)
library(sandwich)
library(lmtest)
library(scales)
library(knitr) 
library(kableExtra)



tidy_finance_ML <- dbConnect(SQLite(), "/Users/adrianmoise/Library/CloudStorage/OneDrive-Personal/AEF/data/tidy_finance_ML.sqlite",
                             extended_types = TRUE)


data <- tbl(tidy_finance_ML, "stock_characteristics_monthly") %>%
  select(permno:sic2, macro_bm,macro_ntis,macro_tbl,macro_tms, characteristic_mom1m, characteristic_mvel1,characteristic_mom12m,characteristic_chmom,characteristic_maxret) %>% 
  collect()

data <- data %>% 
  drop_na(sic2) %>% 
  filter(month >= "2005-01-01") %>% 
  arrange(month, permno)


# Recipes

rec <- recipe(ret_excess ~ ., data = data) %>%
  #step_rm(month) %>% 
  step_interact(terms = ~ contains("characteristic"):contains("macro")) %>% 
  step_normalize(macro_bm:characteristic_maxret_x_macro_tms) %>%
  step_center(ret_excess, skip = TRUE)

tmp_data <- bake(prep(rec), new_data = data)
tmp_data



# One-hot encoding
data <- tmp_data %>% mutate(value = 1)  %>% spread(sic2, value,  fill = 0 )



rec <- recipe(ret_excess ~ ., data = data) 


## 20% Out-Of-Sample split
split_OOS <- initial_time_split(
  data, prop = 1 / 5 
)

data_OOS <- bake(prep(rec), new_data = training(split_OOS))

## 60-20 split
split <- initial_time_split(
  testing(split_OOS), prop = 3 / 4  
)

data_training <- bake(prep(rec), new_data = training(split))



## 4
# NN
# nnet_model <- mlp(
#   epochs = 200,
#   hidden_units = 20
# ) %>%
#   set_mode("regression") %>%
#   set_engine("keras", verbose = 0)
# 
# nn_fit <- workflow() %>%
#   add_recipe(rec) %>%
#   add_model(nnet_model) %>%
#   fit(data = training(split_2))

## Random Forest
rf_model <- rand_forest(
  trees = 5,
  min_n = 5
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_fit <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_model) %>%
  fit(data = training(split))

predicted_values <- rf_fit %>%
  fit(data = training(split)) %>%
  predict(data) %>%
  bind_cols(data) %>%
  select(month, .pred, ret_excess) %>%
  pivot_longer(-month, names_to = "Variable") %>%
  mutate(Variable = case_when(
    Variable == ".pred" ~ "Fitted value",
    Variable == "ret_excess" ~ "Realization"
  )) 

predicted_values$Variable <- predicted_values$Variable %>% replace_na('Realization')

predicted_values %>%
  ggplot(aes(x = month, y = value, color = Variable)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Monthly realized and fitted risk premia all industries"
  ) +
  scale_x_date(
    breaks = function(x) seq.Date(from = min(x), to = max(x), by = "5 years"),
    minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 years"),
    expand = c(0, 0),
    labels = date_format("%Y")
  ) +
  scale_y_continuous(
    labels = percent
  ) + 
  annotate("rect", 
           xmin = testing(split) %>% pull(month) %>% min(), 
           xmax = testing(split) %>% pull(month) %>% max(), 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.5, fill="grey70")

## Tune
rf_model <- rand_forest(
  trees = tune(),
  mtry = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")
rf_model


rf_fit <- rf_fit %>%
  update_model(rf_model)


rf_grid <- expand_grid(mtry = 3:5, trees = seq(4, 6, by = 1))


rf_folds <- vfold_cv(training(split), v = 5)

rf_grid_results <- rf_fit %>% 
  tune_grid(
    resamples = rf_folds,
    grid = rf_grid
  )



# data_folds <- time_series_cv(
#   data        = training(split),
#   date_var    = month,
#   initial     = "60 months",
#   assess      = "48 months",
#   cumulative  = FALSE,
#   skip        = 1,
#   slice_limit = 20
# )
# 
# data_folds
# 
# lm_tune <- rf_fit %>%
#   tune_grid(
#     resample = data_folds,
#     grid = grid_regular(penalty(), mixture(), levels = c(9, 2)),
#     metrics = metric_set(rmse)
#   )
# 
# autoplot(lm_tune) +
#   labs(y = "Root mean-squared prediction error",
#        title = "MSPE for Manufacturing excess returns",
#        subtitle = "Lasso (1.0), Ridge (0.0), and Elastic Net (0.5) with different levels of regularization.")
# ## NN
# 
# model <- keras_model_sequential() %>%
#   layer_dense(units = 20, activation = "sigmoid", input_shape = 5) %>%
#   layer_dense(units = 20, activation = "sigmoid") %>%
#   layer_dense(units = 20, activation = "sigmoid") %>%
#   layer_dense(units = 1, activation = "linear") %>%
#   compile(
#     loss = "mean_absolute_error"
#   )
# model
# 
# model %>%
#   fit(
#     x = extract_mold(rf_fit)$predictors %>% as.matrix(),
#     y = extract_mold(rf_fit)$outcomes %>% pull(ret_excess),
#     epochs = 150, verbose = 0
#   )