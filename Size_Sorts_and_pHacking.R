# Load necessary libraries
library(tidyverse)
library(RSQLite)
library(scales)
library(sandwich)
library(lmtest)
library(furrr)
library(rlang)

# Connect to the SQLite database and load required data
load_data <- function() {
  tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

  crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect()
  factors_ff3_monthly <- tbl(tidy_finance, "factors_ff3_monthly") |> select(smb) |> collect()

  return(list(crsp_monthly = crsp_monthly, factors_ff3_monthly = factors_ff3_monthly))
}

# Data preparation for size distribution analysis
analyze_size_distribution <- function(crsp_monthly) {
  size_distribution <- crsp_monthly |>
    group_by(month) |>
    # Additional analysis steps as described in the chapter
    # ...
  return(size_distribution)
}

# Function to assign portfolios based on size
assign_size_portfolio <- function(data, n_portfolios, exchanges) {
  breakpoints <- data |>
    filter(exchange %in% exchanges) |>
    pull(mktcap_lag) |>
    quantile(probs = seq(0, 1, length.out = n_portfolios + 1), na.rm = TRUE, names = FALSE)

  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(mktcap_lag, breakpoints, all.inside = TRUE)) |>
    pull(portfolio)

  return(assigned_portfolios)
}

# Function to compute portfolio returns with flexible weighting schemes
compute_portfolio_returns <- function(data, n_portfolios, exchanges, value_weighted) {
  data |>
    group_by(month) |>
    mutate(portfolio = assign_size_portfolio(data, n_portfolios, exchanges)) |>
    group_by(month, portfolio) |>
    summarize(
      ret = if_else(value_weighted,
                    weighted.mean(ret_excess, mktcap_lag),
                    mean(ret_excess)),
      .groups = "drop"
    ) |>
    # Additional steps for performance evaluation
    # ...
}

# Perform p-hacking analysis
p_hacking_analysis <- function(crsp_monthly) {
  p_hacking_setup <- expand_grid(
    n_portfolios = c(2, 5, 10),
    exchanges = list("NYSE", c("NYSE", "NASDAQ", "AMEX")),
    value_weighted = c(TRUE, FALSE),
    data = parse_exprs(
      'crsp_monthly;
       crsp_monthly |> filter(industry != "Finance");
       crsp_monthly |> filter(month < "1990-06-01");
       crsp_monthly |> filter(month >="1990-06-01")'
    )
  )

  n_cores <- availableCores() - 1
  plan(multisession, workers = n_cores)

  p_hacking_setup <- p_hacking_setup |>
    mutate(size_premium = future_pmap(
      .l = list(
        n_portfolios,
        exchanges,
        value_weighted,
        data
      ),
      .f = ~ compute_portfolio_returns(
        n_portfolios = ..1,
        exchanges = ..2,
        value_weighted = ..3,
        data = eval_tidy(..4)
      )
    ))

  p_hacking_results <- p_hacking_setup |>
    mutate(data = map_chr(data, deparse)) |>
    unnest(size_premium) |>
    arrange(desc(size_premium))

  return(p_hacking_results)
}

# Example usage of functions
data <- load_data()
size_distribution <- analyze_size_distribution(data$crsp_monthly)
portfolio_returns <- compute_portfolio_returns(data$crsp_monthly, 5, c("NYSE", "NASDAQ", "AMEX"), TRUE)
p_hacking_results <- p_hacking_analysis(data$crsp_monthly)

# Print results
print(size_distribution)
print(portfolio_returns)
print(p_hacking_results)