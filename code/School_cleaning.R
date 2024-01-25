library(tidyverse)
library(dplyr)

setwd("C:/Users/EGT/Desktop/datasci")

Kent_school_2021_2022 <- read.csv("Zip datas/school/2021-2022_886_ks4provisional.csv", fill = TRUE) %>% 
  mutate(Year = 2021) %>% 
  select(Year, PCODE, SCHNAME, ATT8SCR) %>% 
  distinct()

Kent_school_2022_2023 = read.csv("Zip datas/school/2022-2023_886_ks4provisional.csv",fill=TRUE) %>% 
  mutate(Year = 2022) %>% 
  select( Year,PCODE,SCHNAME, ATT8SCR,) %>% 
  na.omit() %>% 
  distinct()

Surrey_school_2021_2022 = read.csv("Zip datas/school/2021-2022_936_ks4provisional.csv",fill=TRUE) %>% 
  mutate(Year = 2021) %>% 
  select( Year,PCODE,SCHNAME, ATT8SCR,) %>% 
  na.omit() %>% 
  distinct()

Surrey_school_2022_2023 = read.csv("Zip datas/school/2022-2023_936_ks4provisional.csv",fill=TRUE) %>% 
  mutate(Year = 2022) %>% 
  select( Year,PCODE,SCHNAME, ATT8SCR,) %>% 
  na.omit() %>% 
  distinct()

SchoolData = rbind(Surrey_school_2022_2023,Surrey_school_2021_2022,Kent_school_2022_2023,Kent_school_2021_2022)

cleanSchooldata=SchoolData %>%
  mutate_all(~ifelse(. == "", NA, .)) %>%  # Replace empty strings with NA
  filter_all(all_vars(!is.na(.))) %>%    # Remove rows with any NA values
  
  # Remove rows where ATT8SCR contains "NE" or "SUPP"
  filter(!grepl("NE|SUPP", ATT8SCR, ignore.case = TRUE)) %>%
  
  # Convert ATT8SCR to numeric (assuming it's a numeric score column)
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  filter(!is.na(ATT8SCR)) %>%
  select(Year, PCODE, SCHNAME, ATT8SCR) %>%
  distinct()


write.csv(cleanSchooldata,"clean-data/CleanSchooldata.csv",row.names =FALSE)

