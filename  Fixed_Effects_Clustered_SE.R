# Load necessary libraries
library(tidyverse)
library(RSQLite)
library(fixest)

# Connect to SQLite database and load data
load_data <- function() {
    tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

    crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
        select(gvkey, month, mktcap) |>
        collect()

    compustat <- tbl(tidy_finance, "compustat") |>
        select(datadate, gvkey, year, at, be, capx, oancf, txdb) |>
        collect()

    return(list(crsp_monthly = crsp_monthly, compustat = compustat))
}

# Prepare investment data
prepare_investment_data <- function(crsp_monthly, compustat) {
    data_investment <- compustat |>
        mutate(month = floor_date(datadate, "month")) |>
        left_join(crsp_monthly, by = c("gvkey", "month")) |>
        mutate(tobins_q = (mktcap + at - be + txdb) / at) |>
        select(gvkey, year, investment = capx / at, cash_flows = oancf / at, tobins_q) |>
        drop_na()

    return(data_investment)
}

# Winsorize data at specified quantiles
winsorize <- function(x, cut) {
    x <- replace(x, x > quantile(x, 1 - cut, na.rm = TRUE), quantile(x, 1 - cut, na.rm = TRUE))
    x <- replace(x, x < quantile(x, cut, na.rm = TRUE), quantile(x, cut, na.rm = TRUE))
    return(x)
}

# Apply winsorization to investment data
winsorize_data <- function(data_investment) {
    data_investment <- data_investment |>
        mutate(across(
            c(investment, cash_flows, tobins_q),
            ~ winsorize(., 0.01)
        ))

    return(data_investment)
}

# Perform fixed effects regression
fixed_effects_regression <- function(data_investment) {
    # OLS model
    model_ols <- feols(investment ~ cash_flows + tobins_q, se = "iid", data = data_investment)

    # Firm fixed effects model
    model_fe_firm <- feols(investment ~ cash_flows + tobins_q | gvkey, se = "iid", data = data_investment)

    # Firm and year fixed effects model
    model_fe_firmyear <- feols(investment ~ cash_flows + tobins_q | gvkey + year, se = "iid", data = data_investment)

    return(list(model_ols = model_ols, model_fe_firm = model_fe_firm, model_fe_firmyear = model_fe_firmyear))
}

# Cluster standard errors
cluster_standard_errors <- function(data_investment) {
    # Cluster by firm
    model_cluster_firm <- feols(investment ~ cash_flows + tobins_q | gvkey + year, cluster = "gvkey", data = data_investment)

    # Cluster by firm and year
    model_cluster_firmyear <- feols(investment ~ cash_flows + tobins_q | gvkey + year, cluster = c("gvkey", "year"), data = data_investment)

    return(list(model_cluster_firm = model_cluster_firm, model_cluster_firmyear = model_cluster_firmyear))
}

# Example usage of functions
data <- load_data()
investment_data <- prepare_investment_data(data$crsp_monthly, data$compustat)
winsorized_data <- winsorize_data(investment_data)
fixed_effects_models <- fixed_effects_regression(winsorized_data)
clustered_models <- cluster_standard_errors(winsorized_data)

# Print regression results
print(etable(fixed_effects_models$model_ols, fixed_effects_models$model_fe_firm, fixed_effects_models$model_fe_firmyear, coefstat = "tstat", digits = 3, digits.stats = 3))
print(etable(clustered_models$model_cluster_firm, clustered_models$model_cluster_firmyear, coefstat = "tstat", digits = 3, digits.stats = 3))
