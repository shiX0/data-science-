
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library("scales")


setwd("C:/Users/EGT/Desktop/datasci")

#importing the cleaned house prices
cleaned_houseprices= read_csv('clean-data/Cleaned House Prices.csv') 

#-----------2022 House Price Box plot-----------#

#grouping the cleaned house prices by county , towns and DOT and showing the average price for each group
Grouped_houseprice = cleaned_houseprices%>% 
  group_by(`Town/City`,District,County,`Date of Transfer`) %>% 
  summarise(`Average Price`= mean(Price)) %>% 
  ungroup(`Town/City`,District,County,`Date of Transfer`) 

#creating box plot to visualize average house prices in kent and surrey in 2022
library(ggplot2)

Grouped_houseprice %>% 
  filter(`Date of Transfer` == 2022) %>%
  group_by(County) %>%
  ggplot(aes(x = County, y = `Average Price`, fill = County)) +
  geom_boxplot(alpha = 0.8, color = "black", width = 0.8) +  # Adjust boxplot appearance
  scale_y_continuous(limits = c(0, 2000000), breaks = seq(0, 2000000, 300000)) +
  labs(title = "2022 Average House Prices By County",
       x = "County",
       y = "Average Price",
       fill = "County") +
  theme_minimal() + 
  theme(legend.position = "bottom")




#-----------2022 Average House Price Bar Chart-----------#


Grouped_houseprice %>% 
  filter(`Date of Transfer` == 2022) %>%
  group_by(County) %>%
  ggplot(aes(x = County, y = `Average Price`, fill = County)) +
  geom_bar(stat = "identity", alpha = 0.7) +  # Adjust bar transparency
  scale_y_continuous(limits = c(0, 2000000), breaks = seq(0, 2000000, 300000)) +
  labs(title = "2022 Average House Prices Barchart",
       x = "County",
       y = "Average Price",
       fill = "County") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10))  # Adjust legend text size




#-----------2019-2022 Average House Line Graph-----------#

#grouping the cleaned house prices by county and year and showing the average price for each group
Grouped_houseprice2 = cleaned_houseprices%>% 
  group_by(County,`Date of Transfer`) %>% 
  summarise(`Average Price`= mean(Price)) 

Grouped_houseprice2 %>%
  filter(`Date of Transfer` %in% c(2019, 2020, 2021, 2022)) %>%
  group_by(County, `Date of Transfer`) %>%
  ggplot(aes(x = as.factor(`Date of Transfer`), y = `Average Price`, group = County, color = County)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 700000), breaks = seq(0, 700000, 100000), labels = scales::label_number()) +
  labs(title = "2019-2022 Average House Prices Line Graph",
       x = "Year",
       y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0),  # Keep x-axis labels horizontal
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10))  # Adjust legend text size





