#### Preamble ####
# Purpose: Cleans the raw product data, categorizes products, and exports the cleaned dataset as a Parquet file.
# Author: Harsh M Pareek
# Date: 28 November 2023
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: Data is downloaded and stored as mentioned in 02-download_data.R
# Any other information needed? If not installed already, install required libraries.

# Load required libraries
library(data.table)
library(stringr)
library(here)
library(arrow) # For reading and writing Parquet files

# Step 1: Load the Data
# Replace with your actual file paths if different
product_data <- fread(here("data/01-raw_data/hammer-5-csv/hammer-4-product.csv"))
raw_data <- fread(here("data/01-raw_data/hammer-5-csv/hammer-4-raw.csv"))

# Step 2: Filter by Vendor
vendors_to_keep <- c("Loblaws", "TandT", "Metro", "Galleria", "NoFrills")
products_filtered <- product_data[vendor %in% vendors_to_keep]

# Step 3: Define Keywords for Each Category
category_keywords <- list(
  # Staples
  staple = c("rice", "quinoa", "pasta", "bread", "tortilla", "oats", "potato", "sweet potato"),

  # Proteins
  protein = c(
    "salmon", "halibut", "tilapia", "tuna", "cod", "chicken breast", "chicken thigh",
    "chicken drumstick", "chicken leg", "ground chicken", "ribeye steak", "sirloin steak",
    "ground beef", "chuck roast", "strip steak", "pork chop", "pork tenderloin", "ground pork",
    "pork shoulder", "lamb chop", "lamb shank", "ground lamb", "lamb shoulder", "egg", "tofu",
    "tempeh", "lentil", "chickpea", "black bean", "kidney bean"
  ),

  # Dairy or Dairy Alternatives
  dairy = c("milk", "almond milk", "oat milk", "soy milk", "greek yogurt", "cheese", "butter", "ghee"),

  # Vegetables
  vegetable = c(
    "spinach", "kale", "broccoli", "cauliflower", "carrot", "zucchini", "bell pepper",
    "tomato", "onion", "garlic", "mushroom", "cucumber", "avocado", "green bean", "cabbage",
    "asparagus", "celery"
  ),

  # Fruits
  fruit = c(
    "banana", "apple", "orange", "blueberry", "strawberry", "grape", "lemon", "lime",
    "watermelon", "peach", "nectarine", "pear"
  ),

  # Snacks & Emergency Foods
  snack = c(
    "instant ramen", "microwavable rice", "granola bar", "protein bar", "nuts", "almonds",
    "walnuts", "peanuts", "seeds", "chia", "sunflower seed", "pumpkin seed", "popcorn",
    "dark chocolate"
  ),

  # Condiments/Seasonings
  condiment = c(
    "salt", "black pepper", "olive oil", "vegetable oil", "soy sauce", "hot sauce",
    "vinegar", "honey", "maple syrup", "ketchup", "mustard", "mayonnaise"
  ),

  # Spices
  spice = c(
    "garlic powder", "onion powder", "paprika", "cumin", "turmeric", "chili powder",
    "italian seasoning", "red pepper flakes", "cinnamon", "nutmeg"
  ),

  # Frozen Essentials
  frozen = c(
    "mixed vegetables", "frozen spinach", "frozen kale", "frozen berries", "frozen fish",
    "pre-cooked frozen chicken"
  ),

  # Baking Basics
  baking = c("flour", "sugar", "baking powder", "baking soda")
)

# Step 4: Handle Ambiguities with Exclude Keywords
exclude_keywords <- list(
  # Exclusions for 'staple' category
  staple = c(
    "cracker", "cake", "chips", "pudding", "wine", "syrup", "vinegar",
    "seasoning", "starch", "noodle", "powder", "snack", "paste", "sauce", "oil",
    "candy", "cookies", "drink", "dessert", "pie", "tortilla chips", "bar",
    "popped", "marinated", "frozen", "instant", "pancake", "dumpling", "batter",
    "rice cake", "fish", "seaweed", "squid", "fishcake", "porridge", "pudding", "sushi",
    "noodles", "cereal", "syrup", "pudding", "beverage", "soft drink", "soda",
    "bread crumbs", "breadcrumbs", "seasoned", "syrup", "sweet", "sweetened", "gravy",
    "broth", "stock", "bouillon", "pasta sauce"
  ),

  # Exclusions for 'protein' category
  protein = c(
    "sauce", "flavour", "snack", "broth", "powder", "noodle", "drink", "milk",
    "oil", "cake", "rinds", "jerky", "sausage", "bacon", "ham", "tofu dessert",
    "burger", "nuggets", "balls", "chips", "patty", "vegan", "protein bar", "protein powder",
    "marinated", "smoked", "paste", "soup", "curry", "seasoning", "pouch", "stew", "hotpot",
    "flavored", "instant", "spread", "dip", "dressing", "pie", "dumpling", "pancake",
    "dessert", "ice cream", "sweet", "beverage", "soft drink", "soda", "powdered", "porridge"
  ),

  # Exclusions for 'dairy' category
  dairy = c(
    "chocolate", "tea", "coffee", "cookie", "cracker", "milkis", "drink", "soda",
    "ice cream", "milkshake", "creamer", "magnesia", "pudding", "bar",
    "bread", "yogurt drink", "cake", "dessert", "shake", "smoothie", "soup",
    "almond milk", "soy milk", "oat milk", "coconut milk", "evaporated milk", "condensed milk",
    "grilled", "squid", "fish", "seafood", "snack", "chips", "sauce", "dip", "nacho", "instant",
    "noodle", "porridge", "pasta", "seasoning", "spread", "marinated", "butter cookies",
    "butter chicken", "butterscotch", "caramel", "lactose-free", "vegan", "soup", "broth"
  ),

  # Exclusions for 'vegetable' category
  vegetable = c(
    "chips", "rings", "snack", "soup", "powder", "ketchup", "oil", "burger",
    "sauce", "seasoning", "paste", "dip", "drink", "juice", "candy", "cake",
    "pickle", "frozen", "canned", "fries", "powdered", "extract",
    "noodle", "instant", "bread", "pasta", "flavored", "spread", "dressing",
    "marinated", "salsa", "relish", "sushi", "batter", "powder", "porridge", "pies",
    "dumpling", "rice cake", "seaweed", "seafood", "spring roll"
  ),

  # Exclusions for 'fruit' category
  fruit = c(
    "juice", "jam", "soda", "drink", "lemonade", "snack", "pie", "ice cream",
    "candy", "tart", "cake", "pudding", "yogurt", "bar", "powder", "syrup",
    "jelly", "smoothie", "popsicle", "sauce", "wine", "dried", "canned", "frozen",
    "tea", "extract", "flavour", "flavored", "sweet", "punch", "cocktail", "soft drink",
    "beverage", "porridge", "soup", "milk", "dessert", "pudding", "cider", "gelatin",
    "marmalade", "sherbet", "sorbet", "milkshake", "infusion", "syrup", "compote"
  ),

  # Exclusions for 'snack' category
  snack = c(
    "oil", "powder", "milk", "hardware", "tool", "rice", "pasta", "noodle",
    "soup", "drink", "supplement", "vitamin", "capsule", "butter", "cake",
    "bread", "sugar", "meal", "dish", "baking", "mix", "dough", "frozen meals",
    "marinated", "seasoning", "sauce", "paste", "instant", "fish", "seafood",
    "sushi", "rice cake", "porridge", "nuts", "seeds", "trail mix", "granola", "bar",
    "cereal", "popcorn", "dessert", "ice cream", "beverage", "soft drink", "smoothie"
  ),

  # Exclusions for 'condiment' category
  condiment = c(
    "salted", "roasted", "chips", "bread", "milk", "snack", "candy", "ham",
    "sausage", "jerky", "pepperoni", "fish", "croaker", "salmon", "cod",
    "shrimp", "tuna", "oil", "wine", "soup", "drink", "juice", "powder", "cake",
    "burger", "nuggets", "dressing", "dip", "spread", "hummus", "pesto", "butter",
    "marinated", "noodle", "instant", "steak", "gravy", "seasoning", "paste", "broth",
    "stock", "bouillon", "mayonnaise", "mustard", "ketchup", "barbecue", "bbq",
    "marinade", "hot sauce", "relish", "salsa", "syrup", "jam", "jelly", "honey",
    "vinegar", "extract", "essence"
  ),

  # Exclusions for 'spice' category
  spice = c(
    "sauce", "bread", "ice cream", "con carne", "candy", "drink", "juice",
    "oil", "cake", "cookie", "powdered", "capsule", "supplement", "bar",
    "dessert", "pudding", "paste", "seasoning mix", "extract", "tea", "latte",
    "syrup", "cereal", "fragrance", "aroma", "punch", "soup", "noodle", "instant",
    "spread", "soda", "beverage", "soft drink", "curry", "chai", "syrup", "powder",
    "chocolate", "cocoa", "gum", "mints", "lozenge"
  ),

  # Exclusions for 'frozen' category
  frozen = c(
    "pizza", "dessert", "meal", "yogurt", "fries", "rice", "juice", "cake",
    "ice cream", "pasta", "prepared", "entree", "dinner", "breakfast", "pie",
    "pastry", "dumpling", "noodle", "bread", "burger", "snack", "nuggets",
    "waffles", "pancakes", "burrito", "lasagna", "sausage", "fish sticks",
    "seafood", "marinated", "soup", "stew", "spring roll", "pastry", "puff",
    "roll", "popsicle", "dessert", "fish fillet", "shrimp", "lobster", "calamari"
  ),

  # Exclusions for 'baking' category
  baking = c(
    "candy", "cookie", "cake", "tortilla", "cereal", "toothpaste", "drink",
    "gum", "capsule", "supplement", "bar", "syrup", "powdered sugar", "icing",
    "frosting", "dessert", "mix", "snack", "beverage", "pudding", "doughnut",
    "muffin", "bread", "pie crust", "coffee", "tea", "extract", "seasoning",
    "soup", "noodle", "instant", "chocolate", "brownie", "pastry", "pancake",
    "waffle", "syrup", "honey", "molasses", "custard", "gelatin", "jello"
  )
)

# Step 5: Create a Matching Function (Capitalization-Proof)
matches_category_refined <- function(product_name, keywords, exclude_keywords = NULL) {
  # Exclude if any exclude keyword is present
  if (!is.null(exclude_keywords)) {
    if (any(str_detect(product_name, fixed(exclude_keywords, ignore_case = TRUE)))) {
      return(FALSE)
    }
  }

  # Include if any keyword is present
  any(str_detect(product_name, fixed(keywords, ignore_case = TRUE)))
}

# Step 6: Apply the Matching Function to Each Category
for (cat in names(category_keywords)) {
  keyword_list <- category_keywords[[cat]]
  exclude_list <- exclude_keywords[[cat]]

  products_filtered[, paste0("is_", cat) := sapply(
    product_name,
    function(x) matches_category_refined(x, keyword_list, exclude_list)
  )]
}

# Step 7: Assign Categories to Products
category_order <- names(category_keywords)

assign_category <- function(row) {
  for (cat in category_order) {
    if (row[[paste0("is_", cat)]]) {
      return(cat)
    }
  }
  return(NA)
}

# Apply the category assignment
products_filtered[, category := apply(.SD, 1, assign_category), .SDcols = paste0("is_", category_order)]

# Step 8: Filter Products to Keep Only Those with Assigned Categories
products_final <- products_filtered[!is.na(category), ]

# Step 9: Rename 'id' to 'product_id' and Ensure Data Types Match
# Rename 'id' to 'product_id' in 'products_final'
setnames(products_final, "id", "product_id")

# Convert 'product_id' in both datasets to character
products_final[, product_id := as.character(product_id)]
raw_data[, product_id := as.character(product_id)]

# Step 10: Process the Prices Data to Get Latest Current Price per Product
# Convert 'nowtime' to POSIXct
raw_data[, nowtime := as.POSIXct(nowtime, format = "%Y-%m-%d %H:%M:%S")]

# Order by 'product_id' and 'nowtime' descending to get latest price
setorder(raw_data, product_id, -nowtime)

# Get the latest price for each product_id
latest_prices <- raw_data[, .SD[1], by = product_id]

# Step 11: Merge Products with Latest Prices
merged_data <- merge(
  products_final[, .(product_id, product_name, category, vendor)],
  latest_prices[, .(product_id, current_price)],
  by = "product_id",
  all.x = TRUE
)

# Step 11a: Additional Cleaning - Clean the 'current_price' Field

# Define a function to clean the 'current_price' field
clean_price <- function(price_str) {
  # Remove any whitespace and currency symbols
  price_str <- gsub("\\s|\\$", "", price_str)

  # Check if the cleaned price_str matches a numeric pattern
  if (grepl("^\\d+(\\.\\d{1,2})?$", price_str)) {
    # Convert to numeric
    numeric_value <- as.numeric(price_str)
    return(numeric_value)
  } else {
    # If it doesn't match, exclude it by returning NA
    return(NA_real_)
  }
}

# Apply the 'clean_price' function to the 'current_price' field
merged_data[, current_price := sapply(current_price, clean_price)]

# Remove any rows where 'current_price' is NA
merged_data <- merged_data[!is.na(current_price), ]

# Remove any rows where key columns have NA or empty values
merged_data <- merged_data[!is.na(product_id) & product_id != "" &
  !is.na(product_name) & product_name != "" &
  !is.na(category) & category != "" &
  !is.na(vendor) & vendor != "", ]

# Step 12: Keep Only the Desired Columns
final_dataset <- merged_data[, .(product_id, product_name, category, vendor, current_price)]

# Remove any duplicates if present
final_dataset <- unique(final_dataset)

# Step 13: Write the Final Cleaned Dataset to Parquet
# Replace 'cleaned_products_with_prices.parquet' with your desired output file path
write_parquet(final_dataset, "data/02-analysis_data/cleaned_products_with_prices.parquet")

# Script Complete
