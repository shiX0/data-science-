library(tidyverse)
library(dplyr)

setwd("C:/Users/EGT/Desktop/datasci")
#importing cleaned house price dataset
cleaned_houseprices <- read_csv("clean-data/Cleaned_House_Pricing_2019-2022.csv") 

#Cleaning and joining data through the use of pipe operator
postcode_to_lsoa <- read_csv("Zip datas/Postcode to LSOA.csv") %>% #importing Postcode to LSOA csv file
  select(pcd7, lsoa11cd) %>% #selecting only required columns
  rename(Postcode= pcd7, `LSOA Code`= lsoa11cd) %>% #renaming columns
  right_join(cleaned_houseprices, by="PostCode") %>% #Joining with the cleaned house price dataset by matching Postcode
  select(`LSOA Code`, PostCode,`Town/City`, District, County, ) %>% #selecting only required columns
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first

file_path <- "clean-data/Cleaned Postcode To LSOA Code.csv"


#saving the cleaned dataset
write.csv(postcode_to_lsoa,file_path, row.names = FALSE) 

