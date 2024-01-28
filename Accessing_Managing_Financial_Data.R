# Load necessary libraries
library(tidyverse)
library(scales)
library(frenchdata)
library(readxl)
library(googledrive)
library(RSQLite)
library(dbplyr)
library(tidyquant)

# Define date range for data fetching
start_date <- ymd("1960-01-01")
end_date <- ymd("2022-12-31")

# Function to download and process Fama-French 3 Factors Monthly
download_process_ff3_monthly <- function() {
  factors_ff3_monthly_raw <- download_french_data("Fama/French 3 Factors")
  factors_ff3_monthly <- factors_ff3_monthly_raw$subsets$data[[1]] |>
    mutate(
      month = floor_date(ymd(str_c(date, "01")), "month"),
      across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
      .keep = "none"
    ) |>
    rename_with(str_to_lower) |>
    rename(mkt_excess = `mkt-rf`) |>
    filter(month >= start_date & month <= end_date)
  return(factors_ff3_monthly)
}

# Similar functions can be created for the following:
# - Fama-French 5 Factors Monthly
# - Fama-French 3 Factors Daily
# - 10 Industry Portfolios Monthly
# - q-Factors Monthly
# - Macroeconomic Predictors

# Download and process macroeconomic predictors
process_macro_predictors <- function() {
  macro_predictors_link <- "https://docs.google.com/spreadsheets/d/1g4LOaRj4TvwJr9RIaA_nwrXXWTOy46bP"
  drive_download(macro_predictors_link, path = "macro_predictors.xlsx")
  macro_predictors <- read_xlsx("macro_predictors.xlsx", sheet = "Monthly") |>
    # Further processing steps as described in the chapter
    # ...
  file.remove("macro_predictors.xlsx")
  return(macro_predictors)
}

# Fetch CPI data using tidyquant
fetch_cpi_data <- function() {
  cpi_monthly <- tq_get("CPIAUCNS", get = "economic.data", from = start_date, to = end_date) |>
    mutate(
      month = floor_date(date, "month"),
      cpi = price / price[month == max(month)],
      .keep = "none"
    )
  return(cpi_monthly)
}

# Setting up SQLite database
setup_database <- function() {
  tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)
  return(tidy_finance)
}

# Writing data to the database
write_data_to_db <- function(db_connection, data_name, data) {
  dbWriteTable(db_connection, data_name, value = data, overwrite = TRUE)
}

# Managing SQLite Databases
# Vacuuming database to optimize size
optimize_database <- function(db_connection) {
  res <- dbSendQuery(db_connection, "VACUUM")
  dbClearResult(res)
}

# Listing tables in the database
list_db_tables <- function(db_connection) {
  return(dbListTables(db_connection))
}

# Example usage of the above functions can be added as needed
