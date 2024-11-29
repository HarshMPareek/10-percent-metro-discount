#### Preamble ####
# Purpose: Perform exploratory data analysis to compare prices across vendors within each category
#          and create visualizations to identify which stores are cheaper
# Author: Harsh M Pareek
# Date: 28 November 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run data cleaning scripts to produce cleaned_products_with_prices.parquet
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(cowplot) # For combining plots
library(ggthemes) # For additional themes

#### Read data ####
# Read the cleaned dataset
final_dataset <- read_parquet(here("data/02-analysis_data/cleaned_products_with_prices.parquet"))

#### Data preparation ####
# Convert to tibble for easier handling with tidyverse functions
final_dataset <- as_tibble(final_dataset)

# Ensure 'vendor' and 'category' are factors
final_dataset <- final_dataset %>%
  mutate(
    vendor = factor(vendor),
    category = factor(category)
  )

# Optional: View the first few rows
# head(final_dataset)

#### Exploratory Data Analysis ####

# Summary statistics of prices by vendor and category
price_summary <- final_dataset %>%
  group_by(category, vendor) %>%
  summarise(
    count = n(),
    mean_price = mean(current_price, na.rm = TRUE),
    median_price = median(current_price, na.rm = TRUE),
    sd_price = sd(current_price, na.rm = TRUE)
  ) %>%
  arrange(category, mean_price)

# View the summary table
print(price_summary)

# Save summary table to CSV (optional)
# write_csv(price_summary, "data/03-analysis_output/price_summary.csv")

#### Visualizations ####

# Function to create boxplots for each category
plot_category_prices <- function(category_name, data) {
  data_filtered <- data %>%
    filter(category == category_name)

  p <- ggplot(data_filtered, aes(x = vendor, y = current_price, fill = vendor)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
    labs(
      title = paste("Price Distribution in", category_name, "Category"),
      x = "Vendor",
      y = "Price ($)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Set2")

  return(p)
}

# Get a list of unique categories
categories <- unique(final_dataset$category)

# Create a list to store plots
category_plots <- list()

# Generate and store plots for each category
for (cat in categories) {
  category_plots[[cat]] <- plot_category_prices(cat, final_dataset)
}

# Optional: Display plots individually
# print(category_plots[["dairy"]])

# Combine all category plots into a grid for comparison
# Adjust the number of columns and rows based on the number of categories
plot_grid(plotlist = category_plots, ncol = 2)

# Save the combined plot to a file (optional)
# ggsave("figures/category_price_distributions.png", width = 12, height = 8)

#### Additional Analysis ####

# Compare vendors within each category by setting different vendors as baseline
# and calculating the difference in mean prices

# Function to calculate price differences with different baselines
calculate_price_differences <- function(category_name, data) {
  data_filtered <- data %>%
    filter(category == category_name)

  vendors <- unique(data_filtered$vendor)

  price_diff_list <- list()

  for (baseline_vendor in vendors) {
    baseline_mean <- data_filtered %>%
      filter(vendor == baseline_vendor) %>%
      summarise(mean_price = mean(current_price, na.rm = TRUE)) %>%
      pull(mean_price)

    price_diff <- data_filtered %>%
      group_by(vendor) %>%
      summarise(
        mean_price = mean(current_price, na.rm = TRUE),
        price_difference = mean_price - baseline_mean
      ) %>%
      mutate(baseline_vendor = baseline_vendor)

    price_diff_list[[baseline_vendor]] <- price_diff
  }

  # Combine all comparisons into one data frame
  price_differences <- bind_rows(price_diff_list)

  return(price_differences)
}

# Calculate price differences for all categories
price_differences_all <- lapply(categories, calculate_price_differences, data = final_dataset)

# Combine into one data frame
price_differences_all <- bind_rows(price_differences_all, .id = "category")

# View the price differences
print(price_differences_all)

#### Visualize Price Differences ####

# Function to calculate price differences with different baselines
calculate_price_differences <- function(category_name, data) {
  data_filtered <- data %>%
    filter(category == category_name)

  vendors <- unique(data_filtered$vendor)

  price_diff_list <- list()

  for (baseline_vendor in vendors) {
    baseline_mean <- data_filtered %>%
      filter(vendor == baseline_vendor) %>%
      summarise(mean_price = mean(current_price, na.rm = TRUE)) %>%
      pull(mean_price)

    price_diff <- data_filtered %>%
      group_by(vendor) %>%
      summarise(
        mean_price = mean(current_price, na.rm = TRUE)
      ) %>%
      mutate(
        price_difference = mean_price - baseline_mean,
        baseline_vendor = baseline_vendor,
        category = category_name
      )

    price_diff_list[[baseline_vendor]] <- price_diff
  }

  # Combine all comparisons into one data frame
  price_differences <- bind_rows(price_diff_list)

  return(price_differences)
}

# Calculate price differences for all categories
library(purrr)
price_differences_all <- map_dfr(categories, function(cat) {
  df <- calculate_price_differences(cat, data = final_dataset)
  return(df)
})

# Function to plot price differences for a category
plot_price_differences <- function(category_name, data) {
  data_filtered <- data %>%
    filter(category == category_name)

  if (nrow(data_filtered) == 0) {
    message(paste("No data available for category:", category_name))
    return(NULL)
  }

  p <- ggplot(data_filtered, aes(x = vendor, y = price_difference, fill = vendor)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~baseline_vendor, ncol = 2) +
    labs(
      title = paste("Price Differences in", category_name, "Category"),
      x = "Vendor",
      y = "Price Difference ($) compared to Baseline"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "none",
      strip.text = element_text(size = 12)
    ) +
    scale_fill_brewer(palette = "Set2")

  return(p)
}

# Generate and display price difference plots for each category
for (cat in categories) {
  p_diff <- plot_price_differences(cat, price_differences_all)

  if (!is.null(p_diff)) {
    print(p_diff)

    # Save the plot (optional)
    # ggsave(filename = paste0("figures/price_differences_", cat, ".png"), plot = p_diff, width = 10, height = 6)
  }
}


