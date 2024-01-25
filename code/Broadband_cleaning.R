library(tidyverse)
library(dplyr)


setwd("C:/Users/EGT/Desktop/datasci")

broad_data1 = read_csv("Zip datas/broadband/201809_fixed_pc_r03/201805_fixed_pc_performance_r03.csv")
broad_data2 = read_csv("Zip datas/broadband/201809_fixed_pc_r03/201809_fixed_pc_coverage_r01.csv")
View(broad_data1)
View(broad_data2)

# Replace 'common_column' with the actual column name
merged_data <- inner_join(broad_data1, broad_data2, by = "postcode")
View(merged_data)

# Check if every value in each column is NA
all_na_columns <- apply(merged_data, 2, function(col) all(is.na(col)))

# Print the results
for (col_name in names(merged_data)) {
  if (all_na_columns[col_name]) {
    cat(paste("Every value in the '", col_name, "' column is NA.\n", sep = ""))
  } else {
    cat(paste("Not every value in the '", col_name, "' column is NA.\n", sep = ""))
  }
}
# Cleaning the merged dataset
# Remove duplicate rows
merged_data <- distinct(merged_data)


# Find names of numeric columns with missing values
numeric_cols_with_na <- colnames(merged_data)[colSums(is.na(merged_data) & sapply(merged_data, is.numeric)) > 0]
numeric_cols_with_na

# Mutate selected numeric columns by filling NA values with their respective means
merged_data <- merged_data %>%
  mutate(across(all_of(numeric_cols_with_na), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
write_csv(merged_data, "clean-data/Clean Broadband.csv")

