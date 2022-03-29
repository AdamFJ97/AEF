library(tidyverse)
library(tidyquant)
library(RSQLite)
library(lmtest)
library(kableExtra)
library(knitr)
library(scales)
library(sandwich)

setwd("C:/Users/adam/Documents/R")


tidy_finance <- dbConnect(SQLite(), "data/tidy_finance.sqlite",
                          extended_types = TRUE)

# Read in CRSP data (crsp_monthly)
crsp_monthly <- tbl(tidy_finance, "crsp_monthly") %>%  collect()

# Read in Fama-French monthly market returns
factors_ff_monthly <- tbl(tidy_finance, "factors_ff_monthly") %>% collect()

# Merge the two datasets
crsp_monthly <- crsp_monthly %>% 
  left_join(factors_ff_monthly, by = "month")





### 1 - Summary statistics for cross-section of excess returns + market cap

# Cross-sectional summary statistics
cross_sec_summary_mktc_ret <- crsp_monthly %>% 
  pivot_longer(cols = c(ret_excess, mktcap)) %>% 
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
avg_summary_mktc_ret <- cross_sec_summary_mktc_ret %>% 
  select(-month) %>% 
  group_by(name) %>% 
  summarize_all(list(~mean(.)))

# Print table
avg_summary_mktc_ret

#####################################




  
### 2 - Generate ret_excess_lag and summary statistics / visualizations

# Method 1 - Join operation
ret_excess_lag <- crsp_monthly %>% 
  mutate(month = month %m+% months(1)) %>%
  select(permno, month, ret_excess_lag = ret_excess)

crsp_monthly <- crsp_monthly %>% 
  left_join(ret_excess_lag, by = c("permno", "month"))


# (Alternative) Method 2 - lag() operator - Not preferred method as described in assignment
#crsp_lag_method <- crsp_monthly %>% 
#  arrange(permno, date) %>% 
#  group_by(permno) %>% 
#  mutate(ret_excess_lag_2 = lag(ret_excess))



# Drop NA values for ret_excess_lag
crsp_monthly_expanded <- crsp_monthly %>%
  drop_na(ret_excess_lag)

# Plot persistence of returns - Consider for assignment
#crsp_monthly_expanded %>%
#  group_by(month) %>% 
#  ggplot(aes(x = ret_excess_lag, y = ret_excess)) +
#  geom_point() +
#  geom_abline(aes(intercept = 0, slope = 1),
#        color = "red",
#        linetype = "dotted"
#  ) +
#  labs(
#    x = "Returns at time t-1",
#    y = "Returns at time t",
#    title = "Persistence of returns"
#  )



# Method one - Calculate periodic avg. cross-sectional returns
#cross_avg_returns <- crsp_monthly_expanded %>% 
#  pivot_longer(cols = ret_excess) %>% 
#  select(month, name, value) %>% 
#  group_by(name, month) %>% 
#  summarize(mean = mean(value))

# Look at autocorrelation function for time series of avg. cross-sectional returns
# Inference can be made by making a suitable AR-model (AR5?) to test for significance
#acf(cross_avg_returns$mean, lag=5)
#arima(cross_avg_returns$mean, order = c(5,0,0))

# Method two - Calculating avg returns -> preferred, as more simple - Also Figure 1
cross_avg_returns2 <- crsp_monthly_expanded %>% 
  group_by(month) %>% 
  mutate(cross_sec_avg_return = mean(ret_excess)) %>% 
  select(month, cross_sec_avg_return) %>% 
  distinct() %>% 
  arrange(month)
autocorr <- acf(cross_avg_returns2$cross_sec_avg_return, lag=5)
plot(autocorr, main = "Autocorrelation function for average excess returns")



# Calculate a lenght T vector of cross-sectional correlations between ret_excess and ret_excess_lag
return_cross_correlations <- crsp_monthly_expanded %>% 
  group_by(month) %>% 
  mutate(cross_sec_correlation = cor(ret_excess, ret_excess_lag)) %>% 
  select(month, cross_sec_correlation) %>% 
  distinct() %>% 
  arrange(month)

# Mean across all the correlations (avg time series)
mean(return_cross_correlations$cross_sec_correlation)

#####################################





### 3 - Generate mktcap_lag_12, mom and produce summary statistics
mktcap_lag12 <- crsp_monthly %>%
  mutate(month = month %m+% months(12)) %>%
  select(permno, month, mktcap_lag12 = mktcap)

crsp_monthly <- crsp_monthly %>%
  left_join(mktcap_lag12, by = c("permno", "month"))

# Create backup
#crsp_backup <- crsp_monthly
#crsp_monthly <- crsp_backup


# Create momentum variable
crsp_monthly <- crsp_monthly %>%
  group_by(permno) %>% 
  mutate(mom = 100*(mktcap_lag - mktcap_lag12)/mktcap_lag12)
  
# Drop NA values
crsp_monthly_mom <- crsp_monthly %>% 
  drop_na(mom)

# Argument: Since excess returns and market capitalization are crucial for all our analyses, we can safely exclude all observations with missing returns or market capitalization


# Cross-sectional summary statistics for momentum
cross_sec_summary_mom <- crsp_monthly_mom %>% 
  pivot_longer(cols = mom) %>% 
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
            max = max(value))


# Table 2 - Time series summary statistics (avg. cross-sectional) for momentum
avg_summary_mom <- cross_sec_summary_mom %>% 
  select(-month) %>% 
  group_by(name) %>% 
  summarize_all(list(~mean(.)))

# Print table
avg_summary_mom


# Calculate correlation between momentum (MOM) and log(mc_i,t) - First, create size variable (log(mktcap))
crsp_monthly_mom <- crsp_monthly_mom %>% 
  mutate(logmktcap = log(mktcap))

# Calculate vector of cross-sectional correlations between momentum and size  
logmktcap_cross_correlations <- crsp_monthly_mom %>% 
  group_by(month) %>% 
  mutate(cross_sec_correlation = cor(mom, logmktcap)) %>% 
  select(month, cross_sec_correlation) %>% 
  distinct() %>% 
  arrange(month)

# Mean across all the correlations
mean(logmktcap_cross_correlations$cross_sec_correlation)

# Plot vector across time to see how it varies
logmktcap_cross_correlations %>%
  ggplot(aes(x = month, y = cross_sec_correlation)) +
  geom_line()






### 4 - Examine relationship between momentum and future stock returns (1 month)

# Function from exercises (Size chapter) to find breakpoints and form portfolios
assign_portfolio <- function(data, var, n_portfolios) {
  breakpoints <- data %>% 
    summarize(breakpoint = quantile({{ var}},
              probs = seq(0, 1, length.out = n_portfolios + 1),
              na.rm = TRUE
              )) %>% 
    pull(breakpoint) %>% 
    as.numeric()
  
  data %>% 
    mutate(portfolio = findInterval({{ var }},
        breakpoints,
        all.inside = TRUE
        )) %>% 
    pull(portfolio)
}

# Use function to actually compute 10 portfolios at a given month, similarly to the exercises. Also computes equal-weight return in addition to value-weighted 
mom_portfolios <- crsp_monthly_mom %>% 
  group_by(month) %>% 
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>% 
  group_by(portfolio, month) %>% 
  summarize(equal_mom = mean(mom),
            equal_mktcap = mean(mktcap),
            weighted_ret = weighted.mean(ret_excess, mktcap_lag),
            equal_ret = mean(ret_excess),
            .groups = "drop")

# Construct Table 3 - Summary statistics for momentum and mktcap variable for each portfolio
mom_portfolios_intro_summary <- mom_portfolios %>% 
  left_join(factors_ff_monthly, by = "month") %>% 
  group_by(portfolio) %>% 
  summarise(
    equal_mom = mean(equal_mom),
    equal_mktcap = mean(equal_mktcap)
  ) %>% 
  pivot_longer(cols = c(equal_mom, equal_mktcap)) %>% 
  group_by(name) %>% 
  pivot_wider(names_from = portfolio, values_from = value)

# Print table
mom_portfolios_intro_summary %>% 
  knitr::kable(digits = 2, caption = "Average cross-sectional summary statistics")

# Construct Table 4 - Summary statistics for alpha, market beta and value-weighted return for each portfolio
mom_portfolios_summary <- mom_portfolios %>% 
  left_join(factors_ff_monthly, by = "month") %>% 
  group_by(portfolio) %>% 
  summarise(
    alpha = as.numeric(lm(weighted_ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(weighted_ret ~ 1 + mkt_excess)$coefficients[2]),
    vw_ret = mean(weighted_ret)
  ) %>% 
  pivot_longer(cols = c(alpha, beta, vw_ret)) %>% 
  group_by(name) %>% 
  pivot_wider(names_from = portfolio, values_from = value)

# Setup for plot alphas for momentum-sorted portfolios
mom_portfolios_summary_plot <- mom_portfolios %>% 
  left_join(factors_ff_monthly, by = "month") %>% 
  group_by(portfolio) %>% 
  summarise(
    alpha = as.numeric(lm(weighted_ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(weighted_ret ~ 1 + mkt_excess)$coefficients[2]),
    vw_ret = mean(weighted_ret)
  )

# Figure 2 - Print alphas for each portfolio
mom_portfolios_summary_plot %>% 
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") + 
  labs(
    title = "Alphas of momentum-sorted portfolios",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
 
# Create long-short portfolio (long top decile - short bottom decile)
mom_longshort <- mom_portfolios %>% 
  ungroup() %>% 
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low",
  )) %>% 
  filter(portfolio %in% c("low", "high")) %>% 
  pivot_wider(month, names_from = portfolio, values_from = weighted_ret) %>% 
  mutate(long_short = high - low) %>% 
  left_join(factors_ff_monthly, by = "month")

# Test for stat. sig. excess returns
coeftest(lm(long_short ~ 1, data = mom_longshort), vcov = NeweyWest)

# Test for risk-adjusted excess performance (alpha)
coeftest(lm(long_short ~ 1 + mkt_excess, data = mom_longshort), vcov = NeweyWest)

# Figure 3 - Create annual returns for momentum long-short strategy
mom_longshort %>% 
  group_by(year = year(month)) %>% 
  summarize(
    low = prod(1 + low),
    high = prod(1 + high),
    long_short = prod(1 + long_short) 
  ) %>% 
  pivot_longer(cols = -year) %>% 
  ggplot(aes(x = year, y = 1 - value, fill = name)) + 
  geom_col(position = "dodge") +
  facet_wrap(~name, ncol = 1) +
  theme(legend.position = "none") + 
  scale_y_continuous(labels = percent) +
  labs(
    title = "Annual returns of momentum portfolios",
    x = NULL, y = NULL
  )

#####################################






### 5 - Repeat 4, but repeat for k-month-ahead excess returns for k={1,3,6,12} for outcome variable for sorting


# Indices for future excess stocks - Use 0,2,5,11 instead of 1,3,6,12 -> argument: r_t is already t+1 wrt momentum (t-1 measurable). 
k <- c(3,6,12)

# Select relevant columns
crsp_monthly_future <- crsp_monthly_mom %>% select(permno, month, mktcap, mktcap_lag, ret_excess, mkt_excess, mom)

# Get future k-ahead excess returns -> For loop does the lubridate-month-lag procedure for each k element in vector k
for (i in k) {
  
  fut_ret <- crsp_monthly_mom %>% 
    mutate(month = month %m-% months(i-1)) %>%
    select(permno, month, fut_ret = ret_excess)
    
  crsp_monthly_future <- crsp_monthly_future %>% 
    left_join(fut_ret, by = c("permno", "month"))
  
  colnames(crsp_monthly_future)[ncol(crsp_monthly_future)] <- paste("future_excess_ret_", i-1, sep = "")
}

# Drop NA values to avoid backwards-extrapolating
crsp_monthly_future_ <- crsp_monthly_future %>% 
  drop_na()

# Do portfolio sorts with new outcome-variable -> future returns at k={1,3,6,12} i.e. t+{0,2,5,11}
fut_mom_portfolios <- crsp_monthly_future_ %>% 
  group_by(month) %>% 
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>% 
  group_by(portfolio, month) %>% 
  summarize(ew_ret_k1 = mean(ret_excess),
            vw_ret_k1 = weighted.mean(ret_excess, mktcap_lag),
            ew_ret_k3 = mean(ret_excess),
            vw_ret_k3 = weighted.mean(future_excess_ret_2, mktcap_lag),
            ew_ret_k6 = mean(ret_excess),
            vw_ret_k6 = weighted.mean(future_excess_ret_5, mktcap_lag),
            ew_ret_k12 = mean(ret_excess),
            vw_ret_k12 = weighted.mean(future_excess_ret_11, mktcap_lag),
            .groups = "drop")


# Table 7 - Construct summary table with alphas, betas and value-weighted returns for each of the 10 portfolios for each k.
fut_mom_portfolios_summary <- fut_mom_portfolios %>% 
  left_join(factors_ff_monthly, by = "month") %>% 
  group_by(portfolio) %>% 
  summarise(
    alpha_k1 = as.numeric(lm(vw_ret_k1 ~ 1 + mkt_excess)$coefficients[1]),
    beta_k1 = as.numeric(lm(vw_ret_k1 ~ 1 + mkt_excess)$coefficients[2]),
    ret_k1 = mean(vw_ret_k1),
    alpha_k3 = as.numeric(lm(vw_ret_k3 ~ 1 + mkt_excess)$coefficients[1]),
    beta_k3 = as.numeric(lm(vw_ret_k3 ~ 1 + mkt_excess)$coefficients[2]),
    ret_k3 = mean(vw_ret_k3),
    alpha_k6 = as.numeric(lm(vw_ret_k6 ~ 1 + mkt_excess)$coefficients[1]),
    beta_k6 = as.numeric(lm(vw_ret_k6 ~ 1 + mkt_excess)$coefficients[2]),
    ret_k6 = mean(vw_ret_k6),
    alpha_k12 = as.numeric(lm(vw_ret_k12 ~ 1 + mkt_excess)$coefficients[1]),
    beta_k12 = as.numeric(lm(vw_ret_k12 ~ 1 + mkt_excess)$coefficients[2]),
    ret_k12 = mean(vw_ret_k12)
  )

# Construct long-short portfolios for each k
fut_mom_longshort <- fut_mom_portfolios %>% 
  ungroup() %>% 
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low",
  )) %>% 
  filter(portfolio %in% c("low", "high")) %>% 
  pivot_wider(month, names_from = portfolio, values_from = c("vw_ret_k1", "vw_ret_k3", "vw_ret_k6", "vw_ret_k12")) %>% 
  mutate(long_short_k1 = vw_ret_k1_high - vw_ret_k1_low,
         long_short_k3 = vw_ret_k3_high - vw_ret_k3_low,
         long_short_k6 = vw_ret_k6_high - vw_ret_k6_low,
         long_short_k12 = vw_ret_k12_high - vw_ret_k12_low) %>% 
  left_join(factors_ff_monthly, by = "month")


# Save variables for alphas and t-statistics after testing CAPM regression on each long-short strategy
alpha_k1 <- coeftest(lm(long_short_k1 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[1]
alpha_k1_tval <- coeftest(lm(long_short_k1 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[5]
alpha_k3 <- coeftest(lm(long_short_k3 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[1]
alpha_k3_tval <- coeftest(lm(long_short_k3 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[5]
alpha_k6 <- coeftest(lm(long_short_k6 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[1]
alpha_k6_tval <- coeftest(lm(long_short_k6 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[5]
alpha_k12 <- coeftest(lm(long_short_k12 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[1]
alpha_k12_tval <- coeftest(lm(long_short_k12 ~ 1 + mkt_excess, data = fut_mom_longshort), vcov = NeweyWest)[5]

# Names for plot
names <- c("Alpha_k=1", "Alpha_k=3", "Alpha_k=6", "Alpha_k=12")

# Table 8 - Alphas for each k and t-stat
fut_mom_perform <- tibble(k__1 = c(alpha_k1, alpha_k1_tval),
                          k__3 = c(alpha_k3, alpha_k3_tval),
                          k__6 = c(alpha_k6, alpha_k6_tval),
                          k_12 = c(alpha_k12, alpha_k12_tval))

# Make column with row names
Alpha_column <- tibble(c("Alpha","t-stat"))
names(Alpha_column) <- "Name"

# Print table
fut_mom_perform %>% 
  cbind(Alpha_column,.) %>% 
  knitr::kable(digits = 3, caption = "Test on alphas for long-short strategies for different k's") 

# Figure 4 - Construct plot with alphas for different long-short strategies for each k.
fut_mom_perform %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = -rowname) %>% 
  filter(rowname == 1) %>%
  ggplot(aes(x = name, y = value, fill = name)) + 
  geom_bar(stat = "identity") + 
  labs(
    title = "Alphas of long-short portfolios for different values of k",
    x = "k",
    y = "CAPM alpha",
    fill = "k"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none")






### 6 - Propose new strategy which captures momentum, but reduces problems with it. Implement and test against #4



