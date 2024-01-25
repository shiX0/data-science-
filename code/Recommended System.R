library(tidyverse)
library(dplyr)
library(scales)
library(ggrepel)


#-----------House Price Ranking-----------#

#Importing cleaned population data
cleaned_population_data = read_csv("Clean-data/Cleaned Population.csv")
cleaned_population_data<- cleaned_population_data %>% 
  select(`Short Postcode`, Town, District, Country)

#Importing cleaned house price data
cleaned_houseprices = read_csv("Clean-Data/Cleaned House Prices.csv")

ranking_houseprices= cleaned_houseprices %>% 
  filter(`Date of Transfer`=="2020") %>% 
  group_by(`Town/City`) %>% 
  summarise(Price=mean(Price),County=first(County)) %>% #reducing the table by merging multiple same towns that belong to the same county
  arrange(Price) %>% 
  mutate(HouseScore=10-(Price/100000)) %>%  #calculating score. We are subtracting from 10 because lower house prices need to have higher rank
  select(`Town/City`,County, HouseScore)


cleaned_school_data=read.csv("clean-data/Cleaned School Dataset.csv")

# Importing cleaned school data
cleaned_school_data <- read.csv("clean-data/Cleaned School Dataset.csv")

# Assuming the school data has columns like `Town/City` and `AttainmentScore`
ranking_school_attainment <- cleaned_school_data %>% 
  group_by(`Town`) %>% 
  summarise(Attainment.Score = mean(Attainment.Score)) %>% 
  arrange(desc(Attainment.Score)) %>%  # Arrange in descending order of attainment scores
  mutate(SchoolScore = 10 - (Attainment.Score / max(Attainment.Score))) %>% 
  select(`Town`, Attainment.Score, SchoolScore)


cleaned_broadband_data=read.csv("clean-data/Cleaned Broadband Speed Dataset.csv")

colnames(cleaned_broadband_data)

# Assuming the broadband data has columns like `Town/City` and `Average.download.speed..Mbit.s.`
ranking_broadband_speed <- cleaned_broadband_data %>% 
  group_by(`Town.City`) %>% 
  summarise(AvgDownloadSpeed = mean(`Average.download.speed..Mbit.s.`)) %>% 
  arrange(desc(AvgDownloadSpeed)) %>%  # Arrange in descending order of average download speed
  mutate(BroadbandScore = 10 - (AvgDownloadSpeed / max(AvgDownloadSpeed))) %>% 
  select(`Town.City`, AvgDownloadSpeed, BroadbandScore)

crime_data=read.csv("clean-data/Cleaned Crime Dataset.csv")
colnames(crime_data)

# Importing cleaned crime data
crime_data <- read.csv("clean-data/Cleaned Crime Dataset.csv")

# Assuming the crime data has columns like "S_No", "Date.of.crime", "Falls.within", "Crime.type", "LSOA.Code", "Postcode", "Short.Postcode", "Town.City"
combined_crime_data <- crime_data %>% 
  group_by(`Town.City`) %>% 
  summarise(TotalCrimes = n()) %>% 
  arrange(TotalCrimes) %>%  # Arrange in ascending order of total crimes
  mutate(CrimeScore = 10 - (TotalCrimes / max(TotalCrimes))) %>% 
  select(`Town.City`, TotalCrimes, CrimeScore)



# Merge data frames# Merge data frames
combined_data <- merge(ranking_houseprices, ranking_school_attainment, by.x = "Town/City", by.y = "Town", all = TRUE)
print("After merging house prices and school attainment:")
print(head(combined_data))

combined_data <- merge(combined_data, ranking_broadband_speed, by.x = "Town/City", by.y = "Town.City", all = TRUE)
print("After merging with broadband speed:")
print(head(combined_data))

combined_data <- merge(combined_data, combined_crime_data, by.x = "Town/City", by.y = "Town.City", all = TRUE)
print("After merging with crime data:")
print(head(combined_data))

# Fill missing values with 0 (if a city doesn't have data for a specific category)
combined_data[is.na(combined_data)] <- 0

# Calculate total score (you can adjust the weights for each category as needed)
combined_data$TotalScore <- with(combined_data, HouseScore + SchoolScore + BroadbandScore + CrimeScore)

# Recommend city with the highest total score
recommended_city <- combined_data[which.max(combined_data$TotalScore), ]

# Print the recommended city
print("Recommended City:")
print(recommended_city)


