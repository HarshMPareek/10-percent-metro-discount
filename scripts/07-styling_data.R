#### Preamble ####
# Purpose: Styling code
# Author: Harsh M Pareek
# Date: 28 November 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: Finish writing all scripts.
# Any other information needed? None

# Load required libraries
library(lintr)
library(styler)

# Set the directory where the R scripts are located
scripts_dir <- "scripts/"

# Get a list of all .R files in the directory
r_files <- list.files(path = scripts_dir, pattern = "\\.R$", full.names = TRUE)

# Apply lintr, and styler to each R file
for (file in r_files) {
  # Read file contents
  file_contents <- readLines(file, warn = FALSE)

  # Apply styler
  styler::style_file(file)

  # Apply lintr and print results
  lint_results <- lintr::lint(file)
  print(lint_results)
}

cat("All files have been processed with styling, and linting.\n")
