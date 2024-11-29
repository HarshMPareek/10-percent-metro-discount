#### Preamble ####
# Purpose: Load downloaded data from Jacob Filipp's groceries dataset:
  # https://jacobfilipp.com/hammer/
# Author: Harsh M Pareek
# Date: 28 November 2024
# Contact: harsh.pareek@mail.utronto.ca
# License: MIT
# Pre-requisites: Download data from the given link in purpose and save it in data//01-raw_data and decompress
# Any other information needed? No


#### Workspace Setup ####
# Load necessary libraries
library(tidyverse)

#### Load Datasets ####
# Read the CSV files
product_data <- read.csv(here("data/01-raw_data/hammer-5-csv/hammer-4-product.csv"))
raw_data <- read.csv(here("data/01-raw_data/hammer-5-csv/hammer-4-raw.csv"))

#### Inspect Datasets ####
# Print a summary of the datasets
cat("Summary of product_data:\n")
print(summary(product_data))

cat("\nSummary of raw_data:\n")
print(summary(raw_data))

#### Completion Message ####
cat("\nDatasets have been successfully loaded.\n")
