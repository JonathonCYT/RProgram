library(tidyquant)
library(tidyverse)
library(timetk)
library(tibbletime)
library(dplyr)
library(tidymodels)

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
prices <- getSymbols(symbols, src = 'yahoo',
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE, warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

asset_returns_long <-
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  na.omit()

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset,
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")

Global_3_Factors <-
  read_csv("data/F-F_Research_Data_Factors.CSV", skip = 4, col_names = c("date","Mkt-RF","SMB","HML","RF")) %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date = rollback(ymd(parse_date_time(date, "%Y%m") + months(1)))) %>%
  dplyr::filter(date >= first(portfolio_returns_tq_rebalanced_monthly$date)
         & date <= last(portfolio_returns_tq_rebalanced_monthly$date))

ff_portfolio_returns <-
  portfolio_returns_tq_rebalanced_monthly %>%
  left_join(Global_3_Factors, by = "date") %>%
  mutate(MKT_RF = Global_3_Factors$`Mkt-RF`/100,
         SMB = Global_3_Factors$SMB/100,
         HML = Global_3_Factors$HML/100,
         RF = Global_3_Factors$RF/100,
         R_excess = round(returns - RF, 4))

window <- 24
rolling_lm <-
  rollify(.f = function(R_excess, MKT_RF, SMB, HML) {
  lm(R_excess ~ MKT_RF + SMB + HML)
  }, window = window, unlist = FALSE)

rolling_ff_betas <-
  ff_portfolio_returns %>%
  mutate(rolling_ff =
           rolling_lm(R_excess,
                      MKT_RF,
                      SMB,
                      HML)) %>%
  slice(-1:-23) %>%
  select(date, rolling_ff)

rolling_ff_betas <-
  ff_portfolio_returns %>%
  mutate(rolling_ff =
           rolling_lm(R_excess,
                      MKT_RF,
                      SMB,
                      HML)) %>%
  mutate(tidied = map(rolling_ff,
                      tidymodels::tidy,
                      conf.int = T)) %>%
  unnest(tidied) %>%
  slice(-1:-23) %>%
  select(date, term, estimate, conf.low, conf.high) %>%
  filter(term != "(Intercept)") %>%
  rename(beta = estimate, factor = term) %>%
  group_by(factor)

rolling_ff_rsquared <-
  ff_portfolio_returns %>%
  mutate(rolling_ff =
           rolling_lm(R_excess,
                      MKT_RF,
                      SMB,
                      HML)) %>%
  slice(-1:-23) %>%
  mutate(glanced = map(rolling_ff,
                      glance)) %>%
  unnest(glanced) %>%
  select(date, r.squared, adj.r.squared, p.value)

rolling_ff_betas %>%
  ggplot(aes(x = date,
             y = beta,
             color = factor)) +
  geom_line() +
  labs(title= "24-Month Rolling FF Factor Betas",
       x = "rolling betas") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

rolling_ff_rsquared_xts <-
  rolling_ff_rsquared %>%
  tk_xts(date_var = date, silent = TRUE)

highchart(type = "stock") %>%
  hc_add_series(rolling_ff_rsquared_xts$r.squared,
                color = "cornflowerblue",
                name = "r-squared") %>%
  hc_title(text = "Rolling FF 3-Factor R-Squared") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE)

highchart(type = "stock") %>%
  hc_add_series(rolling_ff_rsquared_xts$r.squared,
                color = "cornflowerblue",
                name = "r-squared") %>%
  hc_title(text = "Rolling FF 3-Factor R-Squared") %>%
  hc_yAxis( max = 2, min = 0) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE)
