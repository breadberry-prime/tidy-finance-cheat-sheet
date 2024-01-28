# Load necessary libraries
library(tidyverse)
library(RSQLite)
library(sandwich)
library(broom)

# Connect to the SQLite database and load required data
load_data <- function() {
    tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

    crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
        select(permno, gvkey, month, ret_excess, mktcap) |>
        collect()

    compustat <- tbl(tidy_finance, "compustat") |>
        select(datadate, gvkey, be) |>
        collect()

    beta <- tbl(tidy_finance, "beta") |>
        select(month, permno, beta_monthly) |>
        collect()

    return(list(crsp_monthly = crsp_monthly, compustat = compustat, beta = beta))
}

# Prepare characteristics data
prepare_characteristics <- function(crsp_monthly, compustat, beta) {
    characteristics <- compustat |>
        mutate(month = floor_date(ymd(datadate), "month")) |>
        left_join(crsp_monthly, by = c("gvkey", "month")) |>
        left_join(beta, by = c("permno", "month")) |>
        transmute(gvkey, bm = be / mktcap, log_mktcap = log(mktcap), beta = beta_monthly, sorting_date = month %m+% months(6))

    data_fama_macbeth <- crsp_monthly |>
        left_join(characteristics, by = c("gvkey", "month" = "sorting_date")) |>
        group_by(permno) |>
        arrange(month) |>
        fill(c(beta, bm, log_mktcap), .direction = "down") |>
        ungroup() |>
        left_join(crsp_monthly |>
                  select(permno, month, ret_excess_lead = ret_excess) |>
                  mutate(month = month %m-% months(1)),
                  by = c("permno", "month")
        ) |>
        select(permno, month, ret_excess_lead, beta, log_mktcap, bm) |>
        drop_na()

    return(data_fama_macbeth)
}

# Perform the cross-sectional regression for each month
cross_sectional_regression <- function(data_fama_macbeth) {
    risk_premiums <- data_fama_macbeth |>
        nest(data = c(ret_excess_lead, beta, log_mktcap, bm, permno)) |>
        mutate(estimates = map(
            data,
            ~ tidy(lm(ret_excess_lead ~ beta + log_mktcap + bm, data = .x))
        )) |>
        unnest(estimates)

    return(risk_premiums)
}

# Time-series aggregation to estimate risk premiums
aggregate_time_series <- function(risk_premiums) {
    price_of_risk <- risk_premiums |>
        group_by(factor = term) |>
        summarize(
            risk_premium = mean(estimate) * 100,
            t_statistic = mean(estimate) / sd(estimate) * sqrt(n())
        )

    regressions_for_newey_west <- risk_premiums |>
        select(month, factor = term, estimate) |>
        nest(data = c(month, estimate)) |>
        mutate(
            model = map(data, ~ lm(estimate ~ 1, .)),
            mean = map(model, tidy)
        )

    price_of_risk_newey_west <- regressions_for_newey_west |>
        mutate(newey_west_se = map_dbl(model, ~ sqrt(NeweyWest(.)))) |>
        unnest(mean) |>
        mutate(t_statistic_newey_west = estimate / newey_west_se) |>
        select(factor, risk_premium = estimate, t_statistic_newey_west)

    final_results <- left_join(price_of_risk, price_of_risk_newey_west |>
                               select(factor, t_statistic_newey_west),
                               by = "factor")

    return(final_results)
}

# Example usage of functions
data <- load_data()
characteristics <- prepare_characteristics(data$crsp_monthly, data$compustat, data$beta)
risk_premiums <- cross_sectional_regression(characteristics)
final_results <- aggregate_time_series(risk_premiums)

# Print final results
print(final_results)
