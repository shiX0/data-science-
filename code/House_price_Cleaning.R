library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

setwd("C:/Users/EGT/Desktop/datasci")

hp2019 = read_csv("Zip datas/housing price/pp-2019.csv", show_col_types = FALSE)
hp2020 = read_csv("Zip datas/housing price/pp-2020.csv", show_col_types = FALSE)
hp2021 = read_csv("Zip datas/housing price/pp-2021.csv",show_col_types = FALSE)
hp2022= read_csv("Zip datas/housing price/pp-2022.csv",show_col_types = FALSE)



colnames(hp2019) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2" )
colnames(hp2020) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2")
colnames(hp2021) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County" , "Type1", "Type2")
colnames(hp2022) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County" , "Type1", "Type2")

HousePrices = rbind(hp2019,hp2020,hp2021,hp2022) %>% 
  na.omit() %>% 
  distinct() %>% 
  as_tibble()
View(HousePrices)

write.csv(HousePrices, "clean-data/Combined_House_Pricing_2019-2022.csv")


FilteredHousePrices = filter(HousePrices, County == 'KENT' |  County == 'SURREY' )

view(FilteredHousePrices)

FilteredHousePrices = FilteredHousePrices %>% 
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  select(PostCode,Year,PAON,Price) %>% 
  na.omit() %>% 
  distinct() %>% 
  as_tibble()
View(FilteredHousePrices)


# exporting filteredhouseprices data set to  csv
write.csv(FilteredHousePrices, "clea-data/Cleaned House Pricing.csv",row.names = FALSE)

