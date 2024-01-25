library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library("scales")

cleaned_school_dataset= read_csv('clean-data/Cleaned School Dataset.csv')

#Creating a new dataset consisting district and short postcode
district= read_csv('clean-data/Cleaned Population.csv') %>% 
  select(`Short Postcode`, District) %>% 
  rename(`Short Post Code`= `Short Postcode`) #renaming to match the column name in school dataset

#Joining the district dataset into Schoo Dataset by Short Post Code
cleaned_school_dataset <- cleaned_school_dataset %>% 
  left_join(district, by = "Short Post Code") %>% 
  na.omit() 

#-----------2021 Average Attainment Score Box plot-----------#

#grouping school dataset by town, distrct, county and year and showing avg. price for each group
Grouped_school_dataset = cleaned_school_dataset %>% 
  group_by(`Town`,District,County,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) %>% 
  ungroup(`Town`,District,County,`Year`) 

#creating box plot to visualize average attainment score in kent and surrey in 2021
library(ggplot2)

Grouped_school_dataset %>%
  filter(Year == 2021) %>%
  group_by(County) %>%
  ggplot(aes(x = County, y = `Average Attainment Score`, fill = County)) +
  geom_boxplot(alpha = 0.7, color = "black", width = 0.8) +  # Adjust boxplot appearance
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) +
  labs(title = "2021 Average Attainment Score By County Box",
       x = "County",
       y = "Average Attainment Score") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10))  # Adjust legend text size

#-----------2018-2021 Average Attainment Score Line Graph For Kent's District-----------#

#grouping the cleaned school dataset by county and year and showing the average price for each group
Grouped_school_dataset2 <- cleaned_school_dataset %>% 
  filter(County=="Kent") %>% #filtering to show only rows with county as Kent
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 

#creating line graph of average Attainment score from 2018-2021
library(ggplot2)

Grouped_school_dataset2 %>%
  group_by(District, Year) %>%
  ggplot(aes(x = as.factor(Year), y = `Average Attainment Score`, group = District, color = District)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) +
  labs(title = "2018-2021 Average Attainment Score Line Graph For Kent's District",
       x = "Year",
       y = "Average Attainment Score") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0),  # Keep x-axis labels horizontal
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10))

#-----------2018-2021 Average Attainment Score Line Graph For Surrey's District-----------#

#grouping the cleaned school dataset by county and year and showing the average price for each group
Grouped_school_dataset3 <- cleaned_school_dataset %>% 
  filter(County=="Surrey") %>% #filtering to show only rows with county as Surrey
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 

#creating line graph of average Attainment score from 2018-2021
Grouped_school_dataset3 %>%
  group_by(District, Year) %>%
  ggplot(aes(x = as.factor(Year), y = `Average Attainment Score`, group = District, color = District)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 3)) +
  labs(title = "2018-2021 Average Attainment Score Line Graph For Surrey's District",
       x = "Year",
       y = "Average Attainment Score") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0),  # Keep x-axis labels horizontal
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10))
