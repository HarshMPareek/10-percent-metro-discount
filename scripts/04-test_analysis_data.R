#### Preamble ####
# Purpose: Tests for analysis data
# Author: Harsh M Pareek
# Date: 28 November 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run 03-clean_data.R
# Any other information needed? install required libraries if not already


# Load necessary libraries
library(testthat)
library(data.table)
library(here)
library(arrow) # For reading Parquet files

# Set the path to the data files
product_data_path <- here("data/01-raw_data/hammer-5-csv/hammer-4-product.csv")
raw_data_path <- here("data/01-raw_data/hammer-5-csv/hammer-4-raw.csv")
output_data_path <- "data/02-analysis_data/cleaned_products_with_prices.parquet"

# Source the main data processing script
# source("path/to/your_data_processing_script.R")  # Uncomment and set the correct path

# Alternatively, if your data processing script is not in a separate file,
# you can wrap the entire processing code into a function and call it here.

# For the purpose of this testing script, we'll assume that the data processing code
# has already been run and the output dataset is written to 'output_data_path'.

# Read the output dataset
final_dataset <- read_parquet(output_data_path)

# Start testing
test_that("No NA values in key columns", {
  key_columns <- c("product_id", "product_name", "category", "vendor", "current_price")
  expect_true(all(complete.cases(final_dataset[, ..key_columns])))
})

test_that("current_price is numeric and positive", {
  expect_true(is.numeric(final_dataset$current_price))
  expect_true(all(final_dataset$current_price > 0))
})

test_that("No unexpected warnings during data processing", {
  # Capture warnings during data processing
  warnings_list <- withCallingHandlers(
    {
      # Place your data processing code here or call the function that runs it
      # For example:
      # process_data()
    },
    warning = function(w) {
      # Collect warnings
      warnings_list <<- c(warnings_list, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Check that no warnings were collected
  expect_length(warnings_list, 0)
})

test_that("Output dataset contains only expected columns", {
  expected_columns <- c("product_id", "product_name", "category", "vendor", "current_price")
  actual_columns <- names(final_dataset)
  expect_equal(sort(actual_columns), sort(expected_columns))
})

test_that("Categories are correctly assigned", {
  # Define a small sample of known products and their expected categories
  sample_products <- data.table(
    product_name = c("Organic Bananas", "Whole Milk", "Chicken Breast", "Wheat Bread", "Almonds"),
    expected_category = c("fruit", "dairy", "protein", "staple", "snack")
  )

  # Apply the category assignment function from your main script to the sample products
  # You may need to adjust this part to match how categories are assigned in your script

  # Assuming you have the 'matches_category_refined' function and 'category_keywords' available

  # Apply category matching to sample products
  for (cat in names(category_keywords)) {
    keyword_list <- category_keywords[[cat]]
    exclude_list <- exclude_keywords[[cat]]

    sample_products[, paste0("is_", cat) := sapply(
      product_name,
      function(x) matches_category_refined(x, keyword_list, exclude_list)
    )]
  }

  # Assign categories
  sample_products[, category := apply(.SD, 1, assign_category), .SDcols = paste0("is_", names(category_keywords))]

  # Check if the assigned categories match the expected categories
  expect_equal(sample_products$category, sample_products$expected_category)
})

test_that("No duplicate entries in the final dataset", {
  expect_equal(nrow(final_dataset), nrow(unique(final_dataset)))
})

test_that("All vendors are among the vendors_to_keep", {
  vendors_to_keep <- c("Loblaws", "TandT", "Metro", "Galleria", "NoFrills")
  expect_true(all(final_dataset$vendor %in% vendors_to_keep))
})

test_that("Product IDs are unique", {
  # Since prices are the latest per product, each product_id should appear only once
  expect_equal(length(unique(final_dataset$product_id)), nrow(final_dataset))
})

test_that("No empty strings in key columns", {
  key_columns <- c("product_id", "product_name", "category", "vendor")
  for (col in key_columns) {
    expect_false(any(final_dataset[[col]] == ""))
  }
})

# Additional tests can be added as needed based on specific requirements
