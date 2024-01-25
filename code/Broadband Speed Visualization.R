library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

#importing the cleaned house prices
cleaned_broadband_speed= read_csv('Clean-data/Cleaned Broadband Speed Dataset.csv') 


#----------- Broadband Speed Box plot-----------#

#creating box plot to visualize average download speed in Kent and Surrey
cleaned_broadband_speed %>%
  group_by(County) %>%
  ggplot(aes(x = County, y = `Average download speed (Mbit/s)`, fill = County)) +
  geom_boxplot(alpha = 0.7, color = "black", width = 0.8) +  # Adjust boxplot appearance
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 2)) +
  labs(title = "Average Download Speed By County",
       x = "County",
       y = "Average Download Speed (Mbit/s)",
       fill = "County") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10))  # Adjust legend text size #setting label for the chart  


#----------- Broadband Speed Bar Charts-----------#

#creating bar chart to visualize average download speed in Kent
cleaned_broadband_speed %>%
  filter(County == "KENT") %>%
  group_by(`Town/City`) %>%
  summarise(`County Average Download Speed` = mean(`Average download speed (Mbit/s)`)) %>%
  ggplot(aes(x = `Town/City`, y = `County Average Download Speed`, fill = `Town/City`)) +
  geom_bar(stat = "identity", alpha = 0.7) +  # Adjust bar transparency
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 5)) +
  labs(title = "Average Download Speed Within Kent Bar Chart",
       x = "Town/City",
       y = "County Average Download Speed") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10)) +  # Adjust legend text size
  coord_flip()

#creating bar chart to visualize average download speed in Surrey
cleaned_broadband_speed %>% 
  filter(County== "SURREY") %>% 
  group_by(`Town/City`) %>% #grouping by county since we are comparing counties only
  summarise(`County Average Download Speed`= mean(`Average download speed (Mbit/s)`)) %>% 
  ggplot(aes(x = `Town/City`, y = `County Average Download Speed`, fill=`Town/City`)) + #setting x-axis and y-axis values
  scale_y_continuous(limits=c(0,90), breaks = seq(0,90,5))+ #setting limits and breaks
  geom_bar(stat = "identity") + #specifying the type of plot we need 
  labs(title="Average Download Speed Within Surrey Bar Chart") + #setting label for the chart  
  coord_flip()+theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10)) +  # Adjust legend text size
  coord_flip()



