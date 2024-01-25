library(tidyverse)
library(dplyr)
setwd("C:/Users/EGT/Desktop/datasci")
pop_data = read_csv("Zip datas/population/Population2011_1656567141570.csv")
View(pop_data)

# Cleaning the merged dataset
# Remove duplicate rows
clean_data <- distinct(pop_data)

# Handling missing values
# Removing rows with any missing value
clean_data <- na.omit(pop_data)

write_csv(clean_data, "clean-data/Clean_population.csv")

sort(clean_data$Postcode)
