install.packages("fmsb") #installing this package for radar chart

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fmsb)

#importing the cleaned crime dataset
cleaned_crime_dataset= read_csv('Clean-data/Cleaned Crime Dataset.csv')

#importing population dataset
population_dataset<- read_csv('clean-data/Cleaned Population.csv')


#-----------2022 Drug Offence Rate Box plot-----------#

#modifying our crime dataset to show drug offence rate and crime count for 2022
crime_dataset_drugs <-cleaned_crime_dataset %>%
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  na.omit() %>%
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>%
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% #select the required columns
  na.omit() %>%
  filter(`Crime type`== "Drugs" & `Date of crime`==2023) %>% #filtering to show only drug crimes of 2029
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

#creating box plot to visualize drug offence rate in kent and surrey's district in 2022
ggplot(data = crime_dataset_drugs, aes(x = District, y = `Drug Offence Rate`, fill = `Falls within`)) +
  scale_y_continuous(limits = c(0, 0.05), breaks = seq(0, 0.1, 0.005), labels = scales::percent_format(scale = 1)) +
  geom_boxplot(alpha = 0.7, color = "black", width = 0.8) +  # Adjust boxplot appearance
  labs(title = "2023 Drug Offence Rate By District Box Plot",
       x = "District",
       y = "Drug Offence Rate") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),  # Add dashed grid lines
        panel.background = element_rect(fill = "white"),  # Change plot background color
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the title
        legend.text = element_text(size = 10)) 

#-----------2022 June Vehicle Crime Rate Per 10000 people Radar Chart-----------#

#modifying our crime dataset to show vehicle crime rate and crime count
crime_dataset_vehicle <-cleaned_crime_dataset %>%
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  na.omit() %>%
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>%
  ungroup(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% #select the required columns
  na.omit() %>%
  filter(`Crime type`== "Vehicle crime" & `Date of crime`=="2022-06") %>%   #filtering to show only vehicle crimes of 2022 June
  mutate(`Vehicle Crime Rate` = (`Crime Count` / Population)*10000) #calculating vehicle crime rate per 10000 people

crime_dataset_drug <-cleaned_crime_dataset %>%
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  na.omit() %>%
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>%
  ungroup(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% #select the required columns
  na.omit() %>%
  filter(`Crime type`== "Drugs" & `Date of crime`=="2022-06") %>%   #filtering to show only vehicle crimes of 2022 June
  mutate(`Vehicle Crime Rate` = (`Crime Count` / Population)*10000) #calculating vehicle crime rate per 10000 people

crime_dataset_combined <- bind_rows(
  mutate(crime_dataset_drug, CrimeType = "Drug crime"),
  mutate(crime_dataset_vehicle, CrimeType = "Vehicle crime")
)
crime_dataset_combined <- bind_rows(
  mutate(crime_dataset_drug, CrimeType = "Drug crime"),
  mutate(crime_dataset_vehicle, CrimeType = "Vehicle crime")
)

# Filter the data for the specific month and crime types
crime_dataset_combined <- crime_dataset_combined %>%
  filter(`Date of crime` == "2022-06") %>%
  group_by(District, CrimeType) %>%
  summarise(`Crime Count` = sum(`Crime Count`), Population = first(Population)) %>%
  ungroup() %>%
  mutate(`Crime Rate` = (`Crime Count` / Population) * 10000)

# Assuming radar_data_combined has only two districts
radar_data_combined <- crime_dataset_combined %>%
  select(District, `Crime Rate`, CrimeType) %>%
  unique()

# Duplicate the rows to have two sets of data for the radar chart
radar_data_combined <- bind_rows(radar_data_combined, radar_data_combined)

# Create the radar chart
radar_chart_combined <- radarchart(radar_data_combined, axistype = 1,
                                   pcol = c("black", rep("blue", nrow(radar_data_combined) - 1)),
                                   pfcol = c(NA, rep(rgba(0, 0, 1, 0.5), nrow(radar_data_combined) - 1)),
                                   plwd = 2)

#-----------2022 June Robbery Rate Per 10000 people Pie Chart-----------#

#modifying our crime dataset to show robbery crime rate and crime count
crime_dataset_robbery <-cleaned_crime_dataset %>%
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  na.omit() %>%
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>%
  ungroup(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% #select the required columns
  na.omit() %>%
  filter(`Crime type`== "Robbery" & `Date of crime`=="2022-06") %>%   #filtering to show only vehicle crimes of 2022 June
  mutate(`Robbery Crime Rate` = (`Crime Count` / Population)*10000) %>% #calculating vehicle crime rate per 10000 people
  group_by(District) %>% #grouping by district
  summarise(TotalRobberyCrimeRate = sum(`Robbery Crime Rate`)) #aggregating crime rates by District

ggplot(crime_dataset_robbery, aes(x = "", y = TotalRobberyCrimeRate, fill = District)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +  # Apply theme_minimal
  theme(
    axis.text = element_blank(),  # Hide axis labels
    axis.title = element_blank(),  # Hide axis title
    panel.grid = element_blank(),  # Hide gridlines
    panel.border = element_blank(),  # Hide panel border
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10),  # Adjust legend text size
    plot.title = element_text(face = "bold", size = 16),  # Bold and adjust plot title size
    plot.subtitle = element_text(size = 12),  # Adjust plot subtitle size
    plot.caption = element_text(size = 10)  # Adjust plot caption size
  ) +
  labs(
    fill = "District",
    title = "Robbery Crime Rate by District in June 2022",
  )

#-----------2019-2022 Drug Offence Rate In Kent and Surrey Line Chart-----------#

#modifying our crime dataset to show drug offence rate and crime count  
crime_dataset_drugs2 <-cleaned_crime_dataset %>%
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  na.omit() %>%
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>%
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% #select the required columns
  na.omit() %>%
  filter(`Crime type`== "Drugs") %>% #filtering to show only drug crimes of 2022
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

#grouping the drug crime dataset by county and year and showing the rate for each group
Grouped_drug_crime <- crime_dataset_drugs2%>%
  group_by(`Falls within`,`Date of crime`) %>%
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))

#creating line graph of average house prices from 2021-2022
Grouped_drug_crime %>%
  group_by(`Falls within`, `Date of crime`) %>%  #grouping by county and date of crime since we are comparing offence rate in counties year after year
  ggplot( aes(x = `Date of crime`, y = `Drug Offence Rate`, group = `Falls within`, color = `Falls within`)) + #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width
  geom_point(size = 2, color = "black") + #defining point size and color
  scale_y_continuous(limits=c(0,0.2), breaks = seq(0,0.2,0.01)) + #defining limit and breaks
  labs(title = "2020-2023 Drug Offence Rate", #defining labels
       x = "Year",
       y = "Drug Offence Rate")+theme_minimal()
