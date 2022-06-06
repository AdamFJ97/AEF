library(tidyverse)
library(lubridate)
library(scales)
library(tidyquant)
library(RSQLite)
library(dbplyr)
library(zoo)
library(kableExtra)
library(latex2exp)
library(quadprog)
library(base)
library(janitor)
library(matrixStats)

# 1
start_date <- as.Date("1962-01-01")
end_date <- as.Date("2020-11-30")

setwd('/Users/marcuspiil/Desktop/OneDrive - University of Copenhagen/MatØk_Bach/4. år/Advanced Empirical Finance/R_WD')
dir.create("HandIn3plots", showWarnings = FALSE)
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_ML.sqlite", extended_types = TRUE)
stock_characteristics_monthly <- tbl(tidy_finance, "stock_characteristics_monthly") %>%
  select(c(permno, sic2, mktcap_lag, ret_excess, month)) %>%
  collect()

no_months <- year(end_date)*12 + month(end_date) - year(start_date)*12 + month(start_date) - 1

stock_characteristics_monthly1 <- stock_characteristics_monthly %>% # Only keep stocks that exist in whole period
  filter((month<=end_date) & (month>=start_date))%>% 
  group_by(permno) %>%
  mutate(count=n(),
         min_date=min(month),
         max_date=max(month)) %>%
  filter(count==no_months)
  
stock_characteristics_monthly1 <- stock_characteristics_monthly1 %>%
  mutate(industry = case_when(
    sic2 >= 1 & sic2 < 10 ~ "Agriculture",
    sic2 >= 10 & sic2 < 15 ~ "Mining",
    sic2 >= 15 & sic2 < 18 ~ "Construction",
    sic2 >= 20 & sic2 < 40 ~ "Manufacturing",
    sic2 >= 40 & sic2 < 49 ~ "Transportation",
    sic2 >= 49 & sic2 < 50 ~ "Utilities",
    sic2 >= 50 & sic2 < 52 ~ "Wholesale",
    sic2 >= 52 & sic2 < 60 ~ "Retail",
    sic2 >= 60 & sic2 < 68 ~ "Finance",
    sic2 >= 70 & sic2 < 90 ~ "Services",
    sic2 >= 90 & sic2 <= 99 ~ "Public",
    TRUE ~ "Missing"
  ))

stock_characteristics_monthly1 %>% 
  group_by(industry) %>% 
  summarize(count_as_of_2020=sum(month==as.Date('2020-11-01')),
            max_return=max(ret_excess),
            min_return=min(ret_excess),
            mean_return=mean(ret_excess)) %>% 
  ungroup %>%
  bind_rows(summarize(., , industry = "Total",
                      count_as_of_2020=sum(count_as_of_2020), # total count
                      max_return = max(max_return), # actual max
                      min_return = min(min_return), # actual min
                      mean_return = sum(mean_return*.$count_as_of_2020)/count_as_of_2020)) %>% # weighted average
  knitr::kable(digits = 4, caption = "Descriptive summary of balanced panel of stocks"
            ) %>%
  kable_styling(latex_options = "hold_position") %>%
#  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("HandIn3plots/table1.png")
  
# 2
mu_hat <- stock_characteristics_monthly1 %>%
  select(permno, ret_excess) %>%
  group_by(permno) %>%
  summarize(ret_excess_mean=mean(ret_excess)) %>% 
  select(-permno)

sigma_hat <- stock_characteristics_monthly1 %>%
  select(permno, ret_excess, month) %>%
  pivot_wider(names_from = permno, values_from = ret_excess) %>%
  select(-c(month)) %>%
  cov

w_mvp <- solve(sigma_hat) %*% rep(1, ncol(sigma_hat))
w_mvp <- as.vector(w_mvp / sum(w_mvp))


### 
Sigma <- sigma_hat
mu <- mu_hat
w_naive <- 1/ncol(Sigma) * rep(1, ncol(Sigma))


compute_efficient_weight <- function(Sigma,
                                     mu,
                                     gamma = 4, 
                                     lambda = 0, # transaction costs
                                     w_prev = w_naive){ 
  iota <- rep(1, ncol(Sigma))
  Sigma_processed <- Sigma + lambda / gamma * Sigma
  mu_processed <- as.matrix(mu + lambda * Sigma%*% w_prev)
  
  Sigma_inverse <- solve(Sigma_processed)
  
  w_mvp <- Sigma_inverse %*% iota
  w_mvp <- as.vector(w_mvp / sum(w_mvp))
  w_opt <- w_mvp  + 1/gamma * (Sigma_inverse - 1 / sum(Sigma_inverse) * Sigma_inverse %*% iota %*% t(iota) %*% Sigma_inverse) %*% mu_processed
  return(as.vector(w_opt))
}

w_eff <- compute_efficient_weight(sigma_hat, mu_hat, gamma = 4, lambda = 0, w_prev = w_naive)

# test
# lambda <- 200
# w_prev_test <- w_eff
# 
# w_eff <- compute_efficient_weight(sigma_hat, mu_hat, gamma = 4, lambda = lambda, w_prev = w_prev_test)
# 
# 
# 
# t(w_eff)%*% as.matrix(mu_hat) - lambda * t(w_eff-w_prev_test)%*%(sigma_hat)%*%(w_eff-w_prev_test) - gamma/2*t(w_eff)%*%sigma_hat%*%w_eff
# t(w_prev_test)%*% as.matrix(mu_hat) - 
#   lambda * t(w_prev_test-w_prev_test)%*%(sigma_hat)%*%(w_prev_test-w_prev_test) - 
#   gamma/2*t(w_prev_test)%*%sigma_hat%*%w_prev_test
# 
# 
# 
# 
# sum(abs(w_eff-w_prev_test))



transaction_costs <- expand_grid(gamma = 4,
                                 lambda = 20 * qexp((1:99)/100)) %>% 
  mutate(weights = map2(.x = gamma, 
                        .y = lambda,
                        ~compute_efficient_weight(sigma_hat,
                                                  mu_hat,
                                                  gamma = 4,
                                                  lambda = .y,
                                                  w_prev = w_naive)),
         concentration = map_dbl(weights, ~sum(abs(. - w_eff))))

transaction_costs  %>% 
  ggplot(aes(x = lambda, y = concentration)) + 
  geom_line() +
  scale_x_sqrt() +
  labs(x = TeX("Transaction cost parameter, $\\lambda $"),
       y = "Distance from the efficient portfolio",
       title = "Optimal portfolio weights from naive portfolio against transaction cost")
ggsave('HandIn3plots/plot1.pdf',
        plot = last_plot(),
        width = 8, height = 4)

print(paste("L1 norm between naive and efficient portfolio:",sum(abs(w_naive - w_eff))))

# 3

# Backtest
returns <- stock_characteristics_monthly1 %>% # some filter
  select(c(permno, ret_excess, month)) %>%
  pivot_wider(names_from = permno, values_from = ret_excess) %>%
  select(-month)

window_length <- 432
periods <- nrow(returns) - window_length 

lambda <- 200/10000
gamma <- 4

performance_values <- matrix(NA, 
                             nrow = periods, 
                             ncol = 3) # A matrix to collect all returns
colnames(performance_values) <- c("raw_return", "turnover", "net_return") 

performance_values <- list("a. Naive" = performance_values, 
                           "b. MV with TC" = performance_values,
                           "c. MV with TC, no short selling" = performance_values)

n_stocks <- ncol(returns)

w_prev_1 <- w_prev_2 <- w_prev_3 <- rep(1 / n_stocks , n_stocks)

# Adjusting portfolio weights
adjust_weights <- function(w, next_return){
  w_prev <- w * (1+next_return)
  as.numeric(w_prev / sum(as.vector(w_prev)))
}

# set up to evaluate performance
evaluate_performance <- function(w, w_previous, next_return, lambda, Sigma){
  raw_return <- as.matrix(next_return) %*% w
  turnover <- sum(abs(w - w_previous))
  
  transaction_costs <- lambda * t(w-w_previous)%*%(Sigma)%*%(w-w_previous) # transaction cost formula from exercise 2
  
  net_return <- raw_return - transaction_costs
  c(raw_return, turnover, net_return)
}

#
w_no_short_sale<-function(Sigma,
         mu,
         gamma = 4, 
         lambda = 0, # transaction costs
         w_prev = w_naive){
  n_stocks <- dim(Sigma)[1]
  Sigma_processed <- Sigma + lambda / gamma * Sigma

QuadraticProgramming <- solve.QP(Dmat = 2 * Sigma_processed,
                            dvec = t(mu), 
                            Amat = cbind(1, diag(n_stocks)), 
                            bvec = c(1, rep(0, n_stocks)), 
                            meq = 1)
return(QuadraticProgramming$solution)
}

mu_hat_achieve <- matrix(rep(NA,periods*n_stocks),
                         nrow = n_stocks)

# rolling window estimation
for(p in 1:periods){
  returns_window <- returns[p : (p + window_length - 1), ]
  next_return <- returns[p + window_length, ] 

  mu_hat_temp <- colMeans(returns_window)
  
  mu_hat_achieve[,p] <- mu_hat_temp
  
  sigma_hat_train <- cov(returns_window[1:(window_length/2),])
  
  rho_hat <- mean(cor(returns_window[1:(window_length/2),])[lower.tri(cor(returns_window[1:(window_length/2),]))]) 
  # average rho; all the values in the lower triangular of the matrix
  
  F_matrix <- diag(1-rho_hat,dim(sigma_hat_train)[1]) + rho_hat
  
  sigma_hat_valid <- cov(returns_window[(window_length/2+1):window_length,])
  
  error <- function(alpha){
    sum((alpha*F_matrix+(1-alpha)*sigma_hat_train-sigma_hat_valid)^2)
  }
  alpha <- optimize(error, interval=c(0, 1))$minimum
  
  sigma_hat_temp <- alpha*F_matrix+(1-alpha)*sigma_hat_train
  
  # Transaction-cost adjusted portfolio
  w_1 <- w_no_short_sale(mu = mu_hat_temp, 
                        Sigma = sigma_hat_temp, 
                        gamma = gamma,
                        lambda = lambda, 
                        w_prev = w_prev_1)
  
  performance_values[["c. MV with TC, no short selling"]][p, ] <- evaluate_performance(w_1, 
                                                       w_prev_1, 
                                                       next_return, 
                                                       lambda = lambda,
                                                       Sigma=sigma_hat_temp)
  
  w_prev_1 <- adjust_weights(w_1, next_return)
  
  # Naive portfolio
  w_2 <- rep(1 / n_stocks, n_stocks)
  
  performance_values[["a. Naive"]][p, ] <- evaluate_performance(w_2, 
                                                       w_prev_2, 
                                                       next_return, 
                                                       lambda = lambda,
                                                       Sigma=sigma_hat_temp)
  
  w_prev_2 <- adjust_weights(w_2, next_return)
  
  # Mean-variance efficient portfolio (w/o transaction costs)
  w_3 <- compute_efficient_weight(Sigma = sigma_hat_temp,
                                  mu = mu_hat_temp, 
                                  gamma = gamma,
                                  lambda = lambda,
                                  w_prev = w_prev_3)
  
  performance_values[["b. MV with TC"]][p, ] <- evaluate_performance(w_3, 
                                                       w_prev_3, 
                                                       next_return, 
                                                       lambda = lambda,
                                                       Sigma=sigma_hat_temp)
  
  w_prev_3 <- adjust_weights(w_3, next_return)
}

# evaluating performance
performance <- lapply(performance_values, as_tibble) %>% 
  bind_rows(.id = "strategy")

performance %>%
  group_by(strategy) %>%
  summarize(Mean = 12 * mean(100 * net_return),
            SD = sqrt(12) * sd(100 * net_return), 
            `Sharpe ratio` = if_else(Mean > 0, Mean / SD, NA_real_),
            Turnover = 100 * mean(turnover)) %>%
  knitr::kable(digits = 2, caption = "Descriptive summary of balanced panel of stocks"
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("HandIn3plots/table2.png")


mu_hat_achieve <- t(mu_hat_achieve) 
colnames(mu_hat_achieve) <- colnames(returns)
mu_hat_achieve <- data.frame(mu_hat_achieve)

max_mu <- mu_hat_achieve %>% 
  summarize(across(, ~max(.x)))
min_mu <- mu_hat_achieve %>% 
  summarize(across(, ~min(.x)))
difference <- t(max_mu-min_mu)
ggplot(mapping=aes(x=100*difference)) +
  geom_histogram(bins=15) +
  ggtitle(TeX("Histogram of variation in $\\mu$-estimates")) +
  xlab(TeX("Difference between max and minimum $\\mu$-estimates in %.")) +
  ylab('Count')
ggsave('HandIn3plots/plot2.pdf',
       plot = last_plot(),
       width = 8, height = 4)

    