## ---------------------------------------------------------------------------------------------------------------
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("tibble")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(ggplot2)


## ---------------------------------------------------------------------------------------------------------------
symbols <- c("PYPL", "SE", "SQ", "SHOP", "MELI")


## ---------------------------------------------------------------------------------------------------------------

prices <- getSymbols(symbols, source = 'yahoo', from = "2018-07-31", to = "2022-07-31") %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols)
             


## ---------------------------------------------------------------------------------------------------------------
head(prices,10)


## ---------------------------------------------------------------------------------------------------------------
prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)
head(prices_monthly)


## ---------------------------------------------------------------------------------------------------------------
asset_returns_xts <- 
  Return.calculate(prices_monthly, method = "log") %>% 
  na.omit()
head(asset_returns_xts)


## ---------------------------------------------------------------------------------------------------------------
w <- c(0.25, 0.25, 0.20, 0.20, 0.10)


## ---------------------------------------------------------------------------------------------------------------
tibble(w, symbols)


## ---------------------------------------------------------------------------------------------------------------
tibble(w, symbols) %>% 
  summarise(total_weight=sum(w))


## ---------------------------------------------------------------------------------------------------------------
w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]


## ---------------------------------------------------------------------------------------------------------------
asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]
portfolio_returns_byhand <- 
  (w_1*asset1) +
  (w_2*asset2) +
  (w_3*asset3) +
  (w_4*asset4) +
  (w_5*asset5)
names(portfolio_returns_byhand) <- "returns"
head(portfolio_returns_byhand)


## ---------------------------------------------------------------------------------------------------------------
portfolio_returns_xts_rebalanced_monthly <- 
  Return.portfolio(asset_returns_xts, weights = w,rebalance_on = "months") %>% 
  `colnames<-`("returns")

head(portfolio_returns_xts_rebalanced_monthly)


## ---------------------------------------------------------------------------------------------------------------
asset_returns_dplyr_byhand <- prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>%
  # now remove the index because it got converted to row names 
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols)

asset_returns_long <- 
  asset_returns_dplyr_byhand %>% 
  gather(asset, returns, -date) %>% 
  group_by(asset)

asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2], 
                             asset == symbols[3] ~ w[3], 
                             asset == symbols[4] ~ w[4], 
                             asset == symbols[5] ~ w[5])) %>%
  head(5)



## ---------------------------------------------------------------------------------------------------------------
covariance_matrix <- cov(asset_returns_xts) 
round(covariance_matrix, 5)


## ---------------------------------------------------------------------------------------------------------------
sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)
sd_matrix_algebra_percent <- round(sd_matrix_algebra * 100, 2) %>% `colnames<-`("standard deviation")
sd_matrix_algebra_percent[1,]


## ---------------------------------------------------------------------------------------------------------------
portfolio_sd_xts_builtin <- StdDev(asset_returns_xts, weights = w)
portfolio_sd_xts_builtin_percent <- round(portfolio_sd_xts_builtin * 100, 2)
portfolio_sd_xts_builtin_percent[1,1]


## ---------------------------------------------------------------------------------------------------------------
window <- 24


## ---------------------------------------------------------------------------------------------------------------
port_rolling_sd_xts <- 
  rollapply(portfolio_returns_xts_rebalanced_monthly,FUN = sd,width = window) %>%
  na.omit() %>% `colnames<-`("rolling_sd")

tail(port_rolling_sd_xts)


## ---------------------------------------------------------------------------------------------------------------
skew_xts <- skewness(portfolio_returns_xts_rebalanced_monthly$returns)
skew_xts


## ---------------------------------------------------------------------------------------------------------------
window <- 24
rolling_skew_xts <- rollapply(portfolio_returns_xts_rebalanced_monthly, FUN = skewness, width = window) %>% 
  na.omit()
tail(rolling_skew_xts)


## ---------------------------------------------------------------------------------------------------------------
kurt_xts <- kurtosis(portfolio_returns_xts_rebalanced_monthly$returns)

kurt_xts


## ---------------------------------------------------------------------------------------------------------------
window <- 24
rolling_kurt_xts <- rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = kurtosis,
            width = window) %>% 
na.omit()

tail(rolling_kurt_xts)


## ---------------------------------------------------------------------------------------------------------------
highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log Returns") %>% 
  hc_add_series(asset_returns_xts[, symbols[1]],
                name = symbols[1]) %>% 
  hc_add_series(asset_returns_xts[, symbols[2]],
                name = symbols[2]) %>% 
  hc_add_series(asset_returns_xts[,symbols[3]],
                name = symbols[3]) %>% 
  hc_add_series(asset_returns_xts[, symbols[4]],
                name = symbols[4]) %>%
  hc_add_series(asset_returns_xts[, symbols[5]], 
                name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)


## ---------------------------------------------------------------------------------------------------------------
asset_returns_long %>%
ggplot(aes(x = returns, fill = asset)) + geom_histogram(alpha = 0.45, binwidth = .005) + ggtitle("Monthly Returns Since August, 2018")


## ---------------------------------------------------------------------------------------------------------------
asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since August 2018") + 
  theme_update(plot.title = element_text(hjust = 0.5))


## ---------------------------------------------------------------------------------------------------------------
highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly", color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_exporting(enabled = TRUE)


## ---------------------------------------------------------------------------------------------------------------
portfolio_returns_dplyr_byhand <- 
  asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2], 
                             asset == symbols[3] ~ w[3], 
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>%
  group_by(date) %>%
  summarise(returns = sum(weighted_returns))

portfolio_returns_dplyr_byhand %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(color = "cornflowerblue") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  ggtitle("Scatterplot of Returns by Date") +
  theme(plot.title = element_text(hjust = 0.5))


## ---------------------------------------------------------------------------------------------------------------
portfolio_returns_tq_rebalanced_monthly <- asset_returns_long %>% 
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")
sd_plot <- sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot <- mean(portfolio_returns_tq_rebalanced_monthly$returns)
portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot), 
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot), 
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot), 
                    returns, as.numeric(NA))) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") +
  geom_point(aes(y = hist_col_blue),
           color = "blue") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
           color = "purple",
           linetype = "dotted") +
  labs(title = "Colored Scatter with Line", y = "monthly returns") + 
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))


## ---------------------------------------------------------------------------------------------------------------
port_rolling_sd_xts_hc <- 
  round(port_rolling_sd_xts, 4) * 100


## ---------------------------------------------------------------------------------------------------------------
highchart(type = "stock") %>%
  hc_title(text = "24-Month Rolling Volatility") %>% 
  hc_add_series(port_rolling_sd_xts_hc,
                color = "cornflowerblue") %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(labels = list(format = "{value}%"),
            opposite = FALSE) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled= TRUE) %>% 
  hc_legend(enabled = TRUE)


## ---------------------------------------------------------------------------------------------------------------
portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) + 
  geom_histogram(alpha = .05,binwidth = .01,fill = "cornflowerblue", color = "cornflowerblue") +
  scale_x_continuous(breaks =pretty_breaks(n = 10))


## ---------------------------------------------------------------------------------------------------------------
skew_tidy <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  summarise(skew_builtin = skewness(returns),
            skew_byhand =
(sum((returns - mean(returns))^3)/length(returns))/ 
((sum((returns - mean(returns))^2)/length(returns)))^(3/2)) %>%
  select(skew_builtin, skew_byhand)

skew_tidy %>%
  mutate(xts = coredata(skew_xts)) %>% 
  mutate_all(funs(round(., 3)))

asset_returns_long %>%
  summarize(skew_assets = skewness(returns)) %>%
  add_row(asset = "Portfolio",skew_assets = skew_tidy$skew_byhand)%>%
  ggplot(aes(x = asset,y = skew_assets,colour = asset)) +
  geom_point() +
  geom_text(aes(x = "Portfolio", y=skew_tidy$skew_builtin + .04),label = "Portfolio", color = "cornflowerblue") +
labs(y = "skewness")


## ---------------------------------------------------------------------------------------------------------------
highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month Skewness") %>% 
  hc_add_series(rolling_skew_xts,name = "Rolling skewness",color = "cornflowerblue") %>%
  hc_yAxis(title = list(text = "skewness"),
           opposite = FALSE,
           max = 2,
           min = -2) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)


## ---------------------------------------------------------------------------------------------------------------
portfolio_density_plot <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) + 
  stat_density(geom = "line",
               alpha = 1,
               colour = "cornflowerblue")


## ---------------------------------------------------------------------------------------------------------------

median <- 
  median(portfolio_returns_tq_rebalanced_monthly$returns)
mean <- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

shaded_area_data <- 
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x < mean)

median_line_data <- 
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x <= median)

sd_pos <- 
  mean + 
  (sd(portfolio_returns_tq_rebalanced_monthly$returns))
sd_neg <-
  mean - 
  (sd(portfolio_returns_tq_rebalanced_monthly$returns))

sd_pos_shaded_area <- ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x > sd_pos)

sd_neg_shaded_area <- ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x < sd_neg)


## ---------------------------------------------------------------------------------------------------------------
portfolio_density_plot + 
  geom_area(data = sd_pos_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_area(data = sd_neg_shaded_area, 
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) + 
  geom_segment(data = shaded_area_data,
               aes(x = mean, y = 0,xend = mean,yend = density),
               color = "red",
               linetype = "dotted") +
  annotate(geom = "text", 
           x = mean, 
           y = 2,
           label = "mean",
           color = "red",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = -1.75) +
  geom_segment(data = median_line_data, 
               aes(x = median,
                 y = 0,
                 xend = median,
                 yend = density),
               color = "black", 
               linetype = "dotted") +
  annotate(geom = "text", 
           x = median,
           y = 2,
           label = "median", 
           fontface = "plain", 
           angle = 90,
           alpha = .8,
           vjust = 1.75) +
scale_x_continuous(breaks = pretty_breaks(n = 10))

  


## ---------------------------------------------------------------------------------------------------------------
kurt_tidy <- portfolio_returns_tq_rebalanced_monthly %>% 
  summarise(kurt_builtin = kurtosis(returns), kurt_byhand =((sum((returns - mean(returns))^4)/
length(returns))/ ((sum((returns - mean(returns))^2)/
length(returns))^2)) - 3) %>% 
  select(kurt_builtin, kurt_byhand)

asset_returns_long %>%
summarize(kurt_assets = kurtosis(returns)) %>% add_row(asset = "Portfolio",
kurt_assets = kurt_tidy$kurt_byhand) %>% ggplot(aes(x = asset,
             y = kurt_assets,
colour = asset)) + geom_point() +
geom_text(
aes(x = "Portfolio",
y=
kurt_tidy$kurt_byhand + .06),
label = "Portfolio", color = "cornflowerblue") +
labs(y = "kurtosis")


## ---------------------------------------------------------------------------------------------------------------
highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month kurtosis") %>%
  hc_add_series(rolling_kurt_xts,
                name = "Rolling 24-Month kurtosis",
                color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "kurtosis"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE)


## ---------------------------------------------------------------------------------------------------------------
mean_port_return <- mean(portfolio_returns_tq_rebalanced_monthly$returns)
stddev_port_return <- sd(portfolio_returns_tq_rebalanced_monthly$returns)


## ---------------------------------------------------------------------------------------------------------------
simulated_monthly_returns <- rnorm(60, mean_port_return, stddev_port_return)

tail(simulated_monthly_returns)


## ---------------------------------------------------------------------------------------------------------------
simulated_returns_add_1 <- tibble(c(1, 1 + simulated_monthly_returns)) %>% 
  `colnames<-`("returns")
head(simulated_returns_add_1, 3)


## ---------------------------------------------------------------------------------------------------------------
simulated_growth <- simulated_returns_add_1 %>%
  
mutate(growth1 = accumulate(returns, function(x, y) x * y), 
       growth2 = accumulate(returns, `*`),
       growth3 = cumprod(returns)) %>%
select(-returns) 
head(simulated_growth, 3)


## ---------------------------------------------------------------------------------------------------------------
cagr <- ((simulated_growth$growth1[nrow(simulated_growth)]^(1/5)) -1)*100
cagr


## ---------------------------------------------------------------------------------------------------------------
simulation_accum_1 <- function(init_value, N, mean, stdev) { 
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
  `colnames<-`("returns") %>%
  mutate(growth = accumulate(returns, function(x, y) x * y)) %>%
  select(growth) 
  }


## ---------------------------------------------------------------------------------------------------------------
simulation_accum_2 <- function(init_value, N, mean, stdev) { 
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
  `colnames<-`("returns") %>%
  mutate(growth = accumulate(returns, `*`)) %>%
  select(growth)
}


## ---------------------------------------------------------------------------------------------------------------
simulation_cumprod <- function(init_value, N, mean, stdev) { 
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
  `colnames<-`("returns") %>% 
  mutate(growth = cumprod(returns)) %>%
  select(growth) }


## ---------------------------------------------------------------------------------------------------------------
simulation_confirm_all <- function(init_value, N, mean, stdev) { 
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
  `colnames<-`("returns") %>%
  mutate(growth1 = accumulate(returns, function(x, y) x * y),
         growth2 = accumulate(returns, `*`),
         growth3 = cumprod(returns)) %>% 
  select(-returns)
}


## ---------------------------------------------------------------------------------------------------------------
simulation_confirm_all_test <- simulation_confirm_all(1, 60,mean_port_return, stddev_port_return) 

head(simulation_confirm_all_test)


## ---------------------------------------------------------------------------------------------------------------
sims <- 100
starts <- rep(1, sims) %>% 
  set_names(paste("sim", 1:sims, sep = ""))

tail(starts, 10)
head(starts, 10)


## ---------------------------------------------------------------------------------------------------------------
monte_carlo_sim_100 <-
map_dfc(starts, simulation_cumprod,
          N = 60,
          mean = mean_port_return,
          stdev = stddev_port_return) 
for ( col in 1:ncol(monte_carlo_sim_100)) 
  colnames(monte_carlo_sim_100)[col] <-  sub(".........", "growth", colnames(monte_carlo_sim_100)[col])


tail(monte_carlo_sim_100 %>% 
  select(growth1, growth2, growth98, growth99), 3)


## ---------------------------------------------------------------------------------------------------------------
monte_carlo_sim_100 <- monte_carlo_sim_100 %>%
  mutate(month = seq(1:nrow(.))) %>% 
  select(month, everything()) %>% 
  `colnames<-`(c("month", names(starts))) %>%
  mutate_all(funs(round(., 2)))

head(monte_carlo_sim_100 %>% select(month, sim1, sim2, sim98, sim99), 3)
tail(monte_carlo_sim_100 %>% select(month, sim1, sim2, sim98, sim99), 3)


## ---------------------------------------------------------------------------------------------------------------
monte_carlo_sim_100 %>%
  gather(sim, growth, -month) %>%
  group_by(sim) %>%
  ggplot(aes(x = month, y = growth, color = sim)) +
  geom_line() + 
  theme(legend.position="none")
  


## ---------------------------------------------------------------------------------------------------------------
sim_summary <- monte_carlo_sim_100 %>%
  gather(sim, growth, -month) %>% 
  group_by(sim) %>%
  summarise(final = last(growth)) %>% 
  summarise(max = max(final),
            median = median(final), 
            min = min(final))
sim_summary

monte_carlo_sim_100 %>% 
  gather(sim, growth, -month) %>%
  group_by(sim) %>%
  filter(
    last(growth) == sim_summary$max||
    last(growth) == sim_summary$median||
    last(growth) == sim_summary$min) %>%
  ggplot(aes(x = month, y = growth)) + 
  geom_line(aes(color = sim))


## ---------------------------------------------------------------------------------------------------------------
mc_gathered <- monte_carlo_sim_100 %>%
  gather(sim, growth, -month) %>% 
  group_by(sim)

hchart(mc_gathered, 
       type = 'line',
       hcaes(y = growth, x = month,group = sim)) %>% 
  hc_title(text = "100 Simulations") %>%
  hc_xAxis(title = list(text = "months")) %>% 
  hc_yAxis(title = list(text = "dollar growth"),
           labels = list(format = "${value}")) %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = FALSE)


## ---------------------------------------------------------------------------------------------------------------
mc_max_med_min <- mc_gathered %>% 
  filter(
      last(growth) == sim_summary$max || 
      last(growth) == sim_summary$median || 
      last(growth) == sim_summary$min) %>%
  group_by(sim)

mc_max_med_min

hchart(mc_max_med_min, 
       type = 'line',
       hcaes(y = growth, 
             x = month,
             group = sim)) %>%
  hc_title(text = "Min, Max, Median Simulations") %>% 
  hc_xAxis(title = list(text = "months")) %>% 
  hc_yAxis(title = list(text = "dollar growth"),
           labels = list(format = "${value}")) %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = FALSE)

