# Working directory, packages and such:
library(tidyverse) 
library(RSQLite) 
library(lubridate)
getwd()
setwd("C:/Users/adam/Documents/R") # Change to your own path
tidy_finance <- dbConnect(SQLite(), 
                          'data/tidy_finance.sqlite',
                          extended_types = TRUE) # Connect to server

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") %>% collect() # Collect data

## Describe excess return
crsp_monthly %>%
  mutate(ret_excess=ret_excess*100) %>% # Adjust to percentage
  group_by(month) %>%
  summarise(
    x = quantile(ret_excess, seq(0.1, 0.9, 0.4)),
    quantile = 100 * seq(0.1, 0.9, 0.4),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = month, y = x, color = as_factor(quantile))) +
  geom_line() +
  labs(
    x = NULL, y = 'Excess return in Percentage', color = NULL,
    title = "Distribution of excess returns",
    subtitle = "Monthly 10th, 50th and 90th percentile for CRSP cross-section"
  )

## Describe market cap
crsp_monthly %>%
  group_by(month) %>%
  summarise(
    x = quantile(mktcap, seq(0.1, 0.9, 0.4)),
    quantile = 100 * seq(0.1, 0.9, 0.4),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = month, y = x, color = as_factor(quantile))) +
  geom_line() +
  labs(
    x = NULL, y = 'Market cap', color = NULL,
    title = "Distribution of Market cap",
    subtitle = "Monthly 10th, 50th and 90th percentile for CRSP cross-section"
  )

## 2 

## Find those observations with gaps.
crsp_monthly %>% 
  mutate(lag_month_plus = lag(month) + months(1), 
         lead_month_minus = lead(month) - months(1),
         lag_permno = lag(permno),
         lead_permno = lead(permno)) %>%
  filter(((month!=lag_month_plus & permno==lag_permno) | #Same firm, but previous month is not 1 month ago
            (month!=lead_month_minus & permno==lead_permno))) #Same firm, but next month is not 1 month ahead.

crsp_monthly <- crsp_monthly %>% 
  group_by(permno) %>%
  mutate(ret_excess_lag = ifelse((month!=lag(month) %m+% months(1)), as.numeric(NA), lag(ret_excess)))

regression <- lm(ret_excess~ret_excess_lag-1, 
                 data=crsp_monthly)
summary(regression)

## beta estimation
regression$coefficients[1]

# p-value
summary(regression)$coefficients[,'Pr(>|t|)']

## 3 
# Generate Momentum
crsp_monthly <- crsp_monthly %>% 
  group_by(permno) %>%
  mutate(mktcap_lag_12 = ifelse((month!=lag(month, 12) %m+% years(1)),
                                as.numeric(NA), 
                                lag(mktcap, 12)),
         Mom_1_12 = ifelse(((month!=lag(month, 12) %m+% years(1)) & (month!=lag(month, 1) %m+% months(1))),
                           as.numeric(NA),
                           100*(mktcap_lag-mktcap_lag_12)/mktcap_lag_12))
# Table 1
p <- c(0.05, 0.25,0.5,0.75,0.95) # percentiles
mean_statistics <- t(crsp_monthly %>%
                       filter(!is.na(Mom_1_12)) %>%
                       group_by(month) %>%
                       summarise(across(
                         Mom_1_12,
                         list(
                           Mean = mean,
                           sd = sd,
                           min = min,
                           max = max
                         ), .names = "{.fn}"), 
                         quant5 = quantile(Mom_1_12, probs = p[1]),
                         quant25 = quantile(Mom_1_12, probs = p[2]),
                         quant50 = quantile(Mom_1_12, probs = p[3]),
                         quant75 = quantile(Mom_1_12, probs = p[4]),
                         quant95 = quantile(Mom_1_12, probs = p[5]) # all the stats asked for 
                       ) %>% 
                       ungroup() %>%
                       select(-month) %>%
                       summarize(across(.cols = everything(),
                                        list(mean=mean))) %>% # take mean across month
                       mutate(across(.cols = everything(), round, 2))) # round answers 
mean_statistics

# Correlation
noNanValues <- crsp_monthly %>% 
  filter(!is.na(Mom_1_12), !is.na(mktcap))
cor(noNanValues$Mom_1_12, log(noNanValues$mktcap))

## 4
# Portfolios mean market cap :
crsp_monthly %>%
  filter(!is.na(Mom_1_12)) %>%
  group_by(month) %>%
  mutate(decile_rank=ntile(Mom_1_12,10)) %>%
  group_by(decile_rank) %>%
  summarise(
    Mom_1_12 = mean(Mom_1_12),
    mktcap = mean(mktcap)
  )

# Create portfolios with value-weighted returns
return_by_PF <- crsp_monthly %>%
  filter(!is.na(Mom_1_12)) %>%
  group_by(month) %>%
  mutate(decile_rank=ntile(Mom_1_12,10)) %>%
  group_by(month, decile_rank) %>%
  summarise(
    ret_excess = round(sum(mktcap_lag*ret_excess*100)/sum(mktcap_lag),2) # Weighted with start-of-month market value
  )

## 
factors_ff_monthly <- tbl(tidy_finance, "factors_ff_monthly") %>% collect() # Macro factors

reg_data <- return_by_PF %>%
  left_join(factors_ff_monthly, by='month') %>%
  select(month, decile_rank, ret_excess, mkt_excess) %>%
  mutate(decile_rank = as.factor(decile_rank),
         mkt_excess = 100 * mkt_excess) 
# Make decile rank a categorical variable for interaction linear regression
# could alternatively just create a dataset for each portfolio.
reg <- lm(ret_excess ~ decile_rank:mkt_excess + decile_rank - 1,
          data = reg_data)
summary(reg)

# sharpe ratio:
mean(reg_data$ret_excess)/sd(reg_data$ret_excess)

# reg <- lm(ret_excess ~ mkt_excess,
#           data = reg_data %>%
#             filter(decile_rank==1))
## Alphas:
data.frame(portfolio=as.factor(1:10),
           alpha=reg$coefficients[1:10],
           beta=reg$coefficients[11:20]) %>% 
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

# betas
data.frame(portfolio=as.factor(1:10),
           alpha=reg$coefficients[1:10],
           beta=reg$coefficients[11:20]) %>% 
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Betas of momentum-sorted portfolios",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")

# Short-long strategy:
reg_data_2 <- reg_data %>%
  filter((decile_rank==1) | (decile_rank==10)) %>% 
  mutate(ret_excess=ifelse(decile_rank==1,-1*ret_excess, ret_excess)) %>% # Short decile 1
  group_by(month) %>%
  summarise(ret_excess = sum(ret_excess),
            mkt_excess = first(mkt_excess))

reg_2 <- lm(ret_excess ~ mkt_excess,
            data = reg_data_2)
summary(reg_2)

## 5 
i=0
sharpe_ratio <- rep(0,4)
for (k in c(1,3,6,12)){
  i = i + 1
  temp_data <- crsp_monthly %>%
    filter(!is.na(Mom_1_12)) %>%
    mutate(ret_excess=lead(ret_excess, k),
           mktcap_lag = lead(mktcap_lag, k)) %>%  # we must also update mktcap so our weights are correct
    filter(!is.na(ret_excess)) %>%  # lead function causes NA
    group_by(month) %>%
    mutate(decile_rank=ntile(Mom_1_12,10))%>%
    filter((decile_rank==1) | (decile_rank==10)) %>%  ## Only keep the lowest and highest portfolio
    group_by(month, decile_rank) %>%
    summarise(
      ret_excess = round(sum(mktcap_lag*ret_excess*100)/sum(mktcap_lag),2) # Weighted with start-of-month market value
    ) %>%
    mutate(ret_excess=ifelse(decile_rank==1,-1*ret_excess, ret_excess)) %>% # Short decile 1
    group_by(month) %>%
    summarise(ret_excess = sum(ret_excess))
  sharpe_ratio[i] <- mean(temp_data$ret_excess)/sd(temp_data$ret_excess)
}


## 6 
new_strategy <- crsp_monthly %>%
  mutate(Avg_Mom_1_12 = (Mom_1_12 + lag(Mom_1_12,1) + lag(Mom_1_12,2) + lag(Mom_1_12,3))/4) %>% # average moment over 4 last months
  filter(!is.na(Mom_1_12)) %>%
  group_by(month) %>%
  mutate(decile_rank=ntile(Mom_1_12,10))%>%
  filter((decile_rank==1) | (decile_rank==10)) %>%  ## Only keep the lowest and highest portfolio
  group_by(month, decile_rank) %>%
  summarise(
    ret_excess = round(sum(mktcap_lag*ret_excess*100)/sum(mktcap_lag),2) # Weighted with start-of-month market value
  ) %>%
  mutate(ret_excess=ifelse(decile_rank==1,-1*ret_excess, ret_excess)) %>% # Short decile 1
  group_by(month) %>%
  summarise(ret_excess = sum(ret_excess))
mean(new_strategy$ret_excess)/sd(new_strategy$ret_excess)





