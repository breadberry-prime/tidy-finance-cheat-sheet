# Load necessary libraries
library(tidyverse)
library(RSQLite)
library(scales)
library(slider)
library(furrr)

# Connect to the SQLite database
tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

# Load CRSP monthly data and Fama-French 3 Factors Monthly data
load_data <- function() {
  crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
    select(permno, month, industry, ret_excess) |>
    collect()

  factors_ff3_monthly <- tbl(tidy_finance, "factors_ff3_monthly") |>
    select(month, mkt_excess) |>
    collect()

  crsp_monthly <- crsp_monthly |>
    left_join(factors_ff3_monthly, by = "month")

  return(list(crsp_monthly = crsp_monthly, factors_ff3_monthly = factors_ff3_monthly))
}

# Function for estimating CAPM Beta
estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    return(NA_real_)
  } else {
    fit <- lm(ret_excess ~ mkt_excess, data = data)
    return(coefficients(fit)["mkt_excess"])
  }
}

# Function for rolling window CAPM Beta estimation
roll_capm_estimation <- function(data, months, min_obs) {
  data <- arrange(data, month)

  betas <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ estimate_capm(., min_obs),
    .before = months - 1,
    .complete = FALSE
  )

  return(tibble(month = unique(data$month), beta = betas))
}

# Example usage: Estimating rolling betas for Apple
example_permno <- 14593  # Apple's PERMNO
data <- load_data()
apple_beta <- roll_capm_estimation(
  filter(data$crsp_monthly, permno == example_permno),
  months = 60,
  min_obs = 48
)

# Plotting beta estimates
plot_beta <- function(beta_data) {
  ggplot(beta_data, aes(x = month, y = beta)) +
    geom_line() +
    labs(title = "Rolling Beta Estimates", x = "Month", y = "Beta")
}

plot_beta(apple_beta)

# Parallelized rolling-window estimation
# Note: The following code requires a suitable computing environment for parallel execution.
# The user should ensure they have the necessary hardware and software resources.

# Nesting data for parallel processing
crsp_monthly_nested <- nest(data$crsp_monthly, data = c(month, ret_excess, mkt_excess))

# Parallel execution plan
n_cores <- availableCores() - 1
plan(multisession, workers = n_cores)

# Parallelized rolling-window estimation
beta_monthly_parallel <- crsp_monthly_nested |>
  mutate(beta = future_map(
    data, ~ roll_capm_estimation(., months = 60, min_obs = 48)
  )) |>
  unnest(c(beta)) |>
  select(permno, month, beta) |>
  drop_na()

# Saving beta estimates to the database
dbWriteTable(tidy_finance, "beta", beta_monthly_parallel, overwrite = TRUE)