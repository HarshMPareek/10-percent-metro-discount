#### Preamble ####
# Purpose: Tests the structure and validity of the groceries dataset.
# Author: Harsh M Pareek
# Date: 28 November 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# - The `dplyr` package must be installed and loaded
# - 00-simulate_data.R must have been run
# Any other information needed? No


# Load necessary libraries
library(dplyr)

# Load the simulated data
input_file <- "data/00-simulated_data/simulated_grocery_prices.csv"
if (!file.exists(input_file)) {
  stop("The simulated data file does not exist. Run the simulation script first.")
}
simulated_data <- read.csv(input_file)

# Check 1: Validate column names
expected_columns <- c("store", "item", "week", "price", "date")
if (!all(expected_columns %in% colnames(simulated_data))) {
  stop("Column names do not match the expected format.")
} else {
  cat("Column names are valid.\n")
}

# Check 2: Validate data types
if (!is.character(simulated_data$store) || !is.character(simulated_data$item)) {
  stop("Store or Item columns are not character type.")
}
if (!is.numeric(simulated_data$price)) {
  stop("Price column is not numeric.")
}
if (!is.integer(simulated_data$week)) {
  cat("Converting week to integer.\n")
  simulated_data$week <- as.integer(simulated_data$week)
}
if (!all(!is.na(as.Date(simulated_data$date)))) {
  stop("Date column has invalid or missing values.")
} else {
  cat("Data types are valid.\n")
}

# Check 3: Detect missing values
missing_values <- simulated_data %>% summarise_all(~ sum(is.na(.)))
if (any(missing_values > 0)) {
  warning("Missing values detected:\n")
  print(missing_values)
} else {
  cat("No missing values detected.\n")
}

# Check 4: Validate price ranges
if (any(simulated_data$price < 1 | simulated_data$price > 10)) {
  warning("Prices outside expected range of $1 to $10 detected.")
} else {
  cat("All prices are within the expected range.\n")
}

# Check 5: Validate unique combinations of store, item, and week
duplicate_rows <- simulated_data %>%
  group_by(store, item, week) %>%
  filter(n() > 1)
if (nrow(duplicate_rows) > 0) {
  warning("Duplicate rows detected:\n")
  print(duplicate_rows)
} else {
  cat("No duplicate rows found.\n")
}

# Summary statistics for validation
cat("\nSummary Statistics:\n")
summary(simulated_data)

# Check 6: Distribution of prices by store and item
cat("\nChecking price distribution...\n")
price_distribution <- simulated_data %>%
  group_by(store, item) %>%
  summarise(
    min_price = min(price),
    max_price = max(price),
    avg_price = mean(price)
  )
print(head(price_distribution))

# Visualize price distributions (uncomment if needed)
library(ggplot2)
ggplot(simulated_data, aes(x = price, fill = store)) +
  geom_histogram(binwidth = 0.5, alpha = 0.7, position = "identity") +
  facet_wrap(~store) +
  theme_minimal() +
  labs(title = "Price Distribution by Store", x = "Price", y = "Frequency")

cat("\nTest completed. If no warnings or errors appeared, the data looks fine.\n")
