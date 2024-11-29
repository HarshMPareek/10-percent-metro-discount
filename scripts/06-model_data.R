#### Preamble ####
# Purpose: Fit a linear regression model to analyze the effect of vendor and category on current prices
# Author: Harsh M Pareek
# Date: 28 November 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run data cleaning scripts to produce cleaned_products_with_prices.parquet
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(here)
library(arrow)

#### Read data ####
final_dataset <- read_parquet(here("data/02-analysis_data/cleaned_products_with_prices.parquet"))

#### Model data ####
# Ensure 'vendor' and 'category' are factors
final_dataset$vendor <- factor(final_dataset$vendor)
final_dataset$category <- factor(final_dataset$category)

# Set reference level of vendor to 'Metro'
final_dataset$vendor <- relevel(final_dataset$vendor, ref = "Metro")

# Fit a linear regression model with current_price as the response variable
price_model <- stan_glm(
  current_price ~ vendor + category,
  data = final_dataset,
  family = gaussian(),
  prior = normal(
    location = 0,
    scale = 10,
    autoscale = TRUE
  ),
  prior_intercept = normal(
    location = 0,
    scale = 10,
    autoscale = TRUE
  ),
  seed = 123
)

#### Save models ####
saveRDS(
  price_model,
  file = "models/price_model_stanarm.rds"
)

#### Summarise models ####
summary(price_model)
