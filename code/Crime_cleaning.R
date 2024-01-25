library(tidyverse)


setwd("C:/Users/EGT/Desktop/datasci")

kent_crime_data = read_csv("Zip datas/police/2023-09/2023-09-kent-street.csv")
surrey_crime_data = read_csv("Zip datas/police/2023-09/2023-09-surrey-street.csv")

#Merge the data
merged_data = rbind(kent_crime_data,surrey_crime_data)
merged_data

#Slicing the values in the columns LSOA and Falls 
merged_data$`LSOA name` = substr(merged_data$`LSOA name`,1,nchar(merged_data$`LSOA name`)-5)
merged_data$`Falls within`= substr(merged_data$`Falls within`,1,nchar(merged_data$`Falls within`)-nchar("Police "))

#Deleting all Null Column
merged_data$Context = NULL


write_csv(merged_data, "clean-data/Clean_crime.csv")

#Rank town on the basis of LSOA and Falls
final_data = merged_data %>%
  select(`LSOA name`,`Falls within`) %>% 
  group_by(`LSOA name`,`Falls within`) %>% 
  summarise(crimeCounts=n()) %>% 
  arrange(crimeCounts) %>% 
  head(10)

# Cleaning the data 
final_data = na.omit(final_data)
final_data
