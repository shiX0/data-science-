library(tidyverse)
library(dplyr)

setwd("C:/Users/EGT/Desktop/datasci")

housing=read.csv("clean-data/Cleaned_House_Pricing_2019-2022.csv")
broadband=read.csv("clean-data/Clean_Broadband.csv")
view(bro)


# Subset the data for years 2019 to 2022
housing_subset <- housing %>% filter(Year %in% c(2019, 2020, 2021, 2022))

# Calculate average price by postcode
average_price <- housing_subset %>%
  group_by(PostCode) %>%
  summarize(average_price = mean(Price, na.rm = TRUE))

# Check if there are rows in the data frame before proceeding
if (nrow(average_price) > 0) {
  # Standardize Average Prices
  average_price$standardized_price <- scale(average_price$average_price)
  
  # Check if there are rows in the data frame after standardizing
  if (nrow(average_price) > 0) {
    # Create Overall Score (using lowest average prices)
    housing_score <- average_price %>%
      mutate(rank = min_rank(standardized_price),
             overall_score = rank / n())
    
    # Create a new dataset with postcodes and housing scores
    housingscoredata <- data.frame(PostCode = housing_score$PostCode, HousingScore = housing_score$overall_score)
    
    # View the top 10 places
    top_10 <- head(housing_score, 10)
  } else {
    print("No data available after standardizing average prices.")
  }
} else {
  print("No data available for housing scores.")
}

# View the new dataset
print(head(housingscoredata))










