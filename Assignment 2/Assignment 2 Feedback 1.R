library(tidyverse)
library(tidymodels)
library(purrr)
library(RSQLite)
library(reshape2)
library(timetk)

setwd("C:/Users/jacob/OneDrive/Skrivebord/Advanced Empirical Finance/R")

tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_ML.sqlite",
                          extended_types = TRUE)

# Define characteristics and macro variables
charactheristics <- c("characteristic_mom1m", "characteristic_mom12m", "characteristic_retvol", "characteristic_mvel1", "characteristic_maxret")
macrovars <- c("macro_dp", "macro_ep", "macro_tms", "macro_dfy")

# Load data
data <- tbl(tidy_finance, "stock_characteristics_monthly") %>% 
  select(permno:sic2, all_of(charactheristics), all_of(macrovars)) %>% 
  collect()

# Slice data
data <- data %>% 
  drop_na(sic2) %>% 
  filter(month >= "2005-01-01") %>% 
  arrange(month,permno)


# Cross-sectional summary statistics
cross_sec_summary_stat <- data %>% 
  pivot_longer(cols = c(all_of(charactheristics), all_of(macrovars))) %>% 
  select(month, name, value) %>% 
  group_by(name, month) %>% 
  summarize(mean = mean(value),
            sd = sd(value),
            min = min(value),
            q05 = quantile(value, 0.05),
            q25 = quantile(value, 0.25),
            q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            q95 = quantile(value, 0.95),
            max = max(value),
            n = n())

# Table 1 - Time series summary statistics (avg. cross-sectional)
avg_summary_mktc_ret <- cross_sec_summary_stat %>% 
  select(-month) %>% 
  group_by(name) %>% 
  summarize_all(list(~mean(.)))


# Plot macro time series - Temporary working plot
data %>% 
  select(month, starts_with('macro')) %>%
  melt(. , id.vars = 'month', variable.name = 'series') %>% 
  ggplot(aes(x = month, y = value)) +
  geom_line(aes(colour = series)) 
 #scale_y_continuous("Pct (left axis)", sec.axis = sec_axis(~ . * 0, name = "Pct (right axis)"))



# Complete for one data series first (trouble with multiple stocks / panel data)
tmp_data <- data %>% 
  filter(permno == 14593) %>% # Apple stock
  select(-permno, -sic2)

# Define the split
split <- initial_time_split(
  tmp_data,
  prop = 4 / 5
)
split

# Add pre-processing recipe (normalize after one-hot encoding)
rec <- recipe(ret_excess ~ ., data = training(split)) %>% 
  step_rm(month, mktcap_lag) %>%
  step_normalize(all_predictors()) %>% 
  step_center(ret_excess, skip = TRUE)
  #step_interact(terms = ~ contains("factor"):contains("macro")) #%>% 
#  step_dummy(sic2, one_hot = T)


### Exercise 2 ###

# See pdf


### Exercise 3 ###

# 3.3 - Split data into train/valid/test

# Initial - set training data size to 60 pct of the original size
initial <- floor((training(split) %>% count(month) %>% nrow() + 
                    testing(split) %>% count(month) %>% nrow())*0.6)

data_folds <- time_series_cv(
  data = training(split),
  date_var = month,
  initial = floor((training(split) %>% count(month) %>% nrow() + testing(split) %>% count(month) %>% nrow())*0.6),
  assess = training(split) %>% count(month) %>% nrow() - initial,
  cumulative = FALSE,
  slice_limit = 1
)



### Exercise 4 ###

# Setup for the one stock
data_folds <- time_series_cv(
  data = training(split),
  date_var = month,
  initial = "114 months",
  assess = "38 months",
  cumulative = FALSE,
  slice_limit = 1
)

# Define Elastic-Net model
lm_model <- linear_reg(
  penalty = 0.0001,
  mixture = 1
) %>% 
  set_engine("glmnet", intercept = FALSE)

# Define workflow
lm_fit <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(lm_model)

# Cross Validation
lm_model <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>% set_engine("glmnet", intercept = FALSE)
lm_fit <- lm_fit %>% update_model(lm_model)

# Tune model
lm_tune <- lm_fit %>% 
  tune_grid(
    resample = data_folds,
    grid = grid_regular(penalty(), mixture(), levels = c(10,3)),
    metrics = metric_set(rmse)
  )

# Plot level of mixture
autoplot(lm_tune) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Setup random forest
rf_model <- rand_forest(
  trees = 50,
  min_n = 20
) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# Define workflow
rf_fit <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf_model) %>% 
  fit(data = training(split))

lm_model <- linear_reg(
  penalty = 0.0001,
  mixture = 1
) %>% 
  set_engine("glmnet", intercept = FALSE)

lm_fit <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(lm_model) %>% 
  fit(data = training(split))


# Normalize test data
tmp_test_data <- bake(prep(rec, training(split)), new_data = testing(split))
tmp_test_data

# Compute RMSE for both Lasso and RF.
predictive_performance <- testing(split) %>% 
  select(month, ret_excess) %>% 
  bind_cols(lm_fit %>% predict(testing(split))) %>% 
  rename("Lasso" = .pred) %>%
  bind_cols(rf_fit %>% predict(testing(split))) %>% 
  rename("Random_Forest" = .pred) %>% 
  mutate(lasso_sq_error = (Lasso - ret_excess)^2,
         rf_sq_error = (Random_Forest - ret_excess)^2)
  



