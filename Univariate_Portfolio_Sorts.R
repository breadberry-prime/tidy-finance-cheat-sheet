# Load necessary libraries
library(tidyverse)
library(RSQLite)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

# Connect to the SQLite database and load required data
load_data <- function() {
  tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

  crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
    select(permno, month, ret_excess, mktcap_lag) |>
    collect()

  factors_ff3_monthly <- tbl(tidy_finance, "factors_ff3_monthly") |>
    select(month, mkt_excess) |>
    collect()

  beta <- tbl(tidy_finance, "beta") |>
    select(permno, month, beta_monthly) |>
    collect()

  return(list(crsp_monthly = crsp_monthly, factors_ff3_monthly = factors_ff3_monthly, beta = beta))
}

# Prepare data for sorting by market beta
prepare_sorting_data <- function(beta, crsp_monthly) {
  beta_lag <- beta |>
    mutate(month = month %m+% months(1)) |>
    select(permno, month, beta_lag = beta_monthly) |>
    drop_na()

  data_for_sorts <- crsp_monthly |>
    inner_join(beta_lag, by = c("permno", "month"))

  return(data_for_sorts)
}

# Function to perform univariate portfolio sorts
univariate_portfolio_sorts <- function(data_for_sorts) {
  beta_portfolios <- data_for_sorts |>
    group_by(month) |>
    mutate(breakpoint = median(beta_lag, na.rm = TRUE)) |>
    mutate(portfolio = if_else(beta_lag <= breakpoint, "low", "high")) |>
    group_by(month, portfolio) |>
    summarize(ret = weighted.mean(ret_excess, mktcap_lag, na.rm = TRUE), .groups = "drop")

  return(beta_portfolios)
}

# Performance evaluation of the long-short strategy
evaluate_performance <- function(beta_portfolios, factors_ff3_monthly) {
  beta_longshort <- beta_portfolios |>
    pivot_wider(id_cols = month, names_from = portfolio, values_from = ret) |>
    mutate(long_short = high - low) |>
    left_join(factors_ff3_monthly, by = "month")

  model_fit <- lm(long_short ~ 1, data = beta_longshort)
  test_results <- coeftest(model_fit, vcov = NeweyWest(model_fit))

  return(test_results)
}

# Example usage of functions
data <- load_data()
sorted_data <- prepare_sorting_data(data$beta, data$crsp_monthly)
portfolio_sorts <- univariate_portfolio_sorts(sorted_data)
performance_results <- evaluate_performance(portfolio_sorts, data$factors_ff3_monthly)

# Print the results
print(performance_results)

# Additional functional programming for more complex portfolio sorts and further analyses can be added as per the chapter details
