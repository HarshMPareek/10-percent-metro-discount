#### Preamble ####
# Purpose: Simulates a dataset of normal weekly groceries from stores near
# University.
# Author: Harsh M Pareek
# Date: 28 November 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `dplyr` package must be installed
# Any other information needed? No


# Load necessary libraries
library(dplyr)

# Define common grocery items for a student's weekly grocery list
grocery_items <- c(
  "Chicken", "Bread", "Cabbage", "Rice", "Fish", "Tofu",
  "Milk", "Eggs", "Apples", "Bananas", "Carrots", "Potatoes"
)

# Define stores
stores <- c("Galleria", "Metro", "Loblaws", "NoFrills", "T&T")

# Generate simulated prices for each grocery item at each store
set.seed(304)
simulated_data <- expand.grid(
  store = stores,
  item = grocery_items,
  week = 1:4 # Simulate for 4 weeks
) %>%
  mutate(
    price = round(runif(nrow(.), 1, 10), 2), # Generate prices between $1 and $10
    date = as.Date("2024-11-01") + (week - 1) * 7 # Assign dates for each week
  )

# Save the simulated data as a CSV file
output_file <- "data/00-simulated_data/simulated_grocery_prices.csv"
write.csv(simulated_data, output_file, row.names = FALSE)

# Display a summary of the simulated data
print(head(simulated_data))

# Notify the user
cat("Simulated grocery prices saved to", output_file, "\n")
