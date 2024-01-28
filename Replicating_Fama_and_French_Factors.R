# Load necessary libraries
library(tidyverse)
library(RSQLite)

# Connect to the SQLite database and load required data
load_data <- function() {
    tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

    crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
        select(permno, gvkey, month, ret_excess, mktcap, mktcap_lag, exchange) |>
        collect()

    compustat <- tbl(tidy_finance, "compustat") |>
        select(gvkey, datadate, be, op, inv) |>
        collect()

    factors_ff3_monthly <- tbl(tidy_finance, "factors_ff3_monthly") |>
        select(month, smb, hml) |>
        collect()

    factors_ff5_monthly <- tbl(tidy_finance, "factors_ff5_monthly") |>
        select(month, smb, hml, rmw, cma) |>
        collect()

    return(list(crsp_monthly = crsp_monthly, compustat = compustat,
                factors_ff3_monthly = factors_ff3_monthly, factors_ff5_monthly = factors_ff5_monthly))
}

# Data preparation and calculation of size, market equity, and book-to-market
prepare_data <- function(crsp_monthly, compustat) {
    # Prepare size data
    size <- crsp_monthly |>
        filter(month(month) == 6) |>
        mutate(sorting_date = month %m+% months(1)) |>
        select(permno, exchange, sorting_date, size = mktcap)

    # Prepare market equity data
    market_equity <- crsp_monthly |>
        filter(month(month) == 12) |>
        mutate(sorting_date = ymd(str_c(year(month) + 1, "0701"))) |>
        select(permno, gvkey, sorting_date, me = mktcap)

    # Prepare book-to-market data
    book_to_market <- compustat |>
        mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
        select(gvkey, sorting_date, be) |>
        inner_join(market_equity, by = c("gvkey", "sorting_date")) |>
        mutate(bm = be / me) |>
        select(permno, sorting_date, me, bm)

    # Combine all sorting variables
    sorting_variables <- size |>
        inner_join(book_to_market, by = c("permno", "sorting_date")) |>
        drop_na() |>
        distinct(permno, sorting_date, .keep_all = TRUE)

    return(sorting_variables)
}

# Function to assign portfolios based on size and book-to-market
assign_portfolio <- function(data, sorting_variable, percentiles) {
    breakpoints <- data |>
        filter(exchange == "NYSE") |>
        pull({{ sorting_variable }}) |>
        quantile(probs = percentiles, na.rm = TRUE, names = FALSE)

    assigned_portfolios <- data |>
        mutate(portfolio = findInterval({{ sorting_variable }}, breakpoints, all.inside = TRUE)) |>
        pull(portfolio)

    return(assigned_portfolios)
}

# Function to compute Fama and French factors
compute_ff_factors <- function(portfolios, crsp_monthly) {
    factors_replicated <- portfolios |>
        group_by(portfolio_size, portfolio_bm, month) |>
        summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
        group_by(month) |>
        summarize(
            smb_replicated = mean(ret[portfolio_size == 1]) - mean(ret[portfolio_size == 2]),
            hml_replicated = mean(ret[portfolio_bm == 3]) - mean(ret[portfolio_bm == 1])
        )

    return(factors_replicated)
}

# Replication evaluation
evaluate_replication <- function(original_factors, replicated_factors) {
    model_smb <- lm(smb ~ smb_replicated, data = replicated_factors)
    model_hml <- lm(hml ~ hml_replicated, data = replicated_factors)

    summary_smb <- summary(model_smb)
    summary_hml <- summary(model_hml)

    return(list(summary_smb = summary_smb, summary_hml = summary_hml))
}

# Example usage of functions
data <- load_data()
prepared_data <- prepare_data(data$crsp_monthly, data$compustat)

# Assign portfolios based on size and book-to-market
portfolios <- prepared_data |>
    group_by(sorting_date) |>
    mutate(
        portfolio_size = assign_portfolio(data, size, c(0, 0.5, 1)),
        portfolio_bm = assign_portfolio(data, bm, c(0, 0.3, 0.7, 1))
    )

# Compute Fama and French factors
ff_factors <- compute_ff_factors(portfolios, data$crsp_monthly)

# Evaluate replication
evaluation_results <- evaluate_replication(data$factors_ff3_monthly, ff_factors)

# Print the results
print(evaluation_results)