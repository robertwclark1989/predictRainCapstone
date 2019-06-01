---
title: "Predict Rain Capstone"
author: "Robert Clark"
---
  
 
#### Import Dataset
library(tidyverse)
library(caret)
library(mice)
library(VIM)
library(rpart)
library(randomForest)
library(Rborist)


# import data
weather <- read.csv("~/Documents/R Studio/EDX Courses/weatherAUS.csv")

# change class of date column from factor to date
weather <- weather %>% mutate(Date = as.Date(Date))

# examine data

# How many variables do we have that have missing values (NA's)?
colnames(weather)[colSums(is.na(weather)) > 0]
colnames(weather)[colSums(is.na(weather)) == 0]

# What percent of the days in our dataset were rainy days?
(weather %>% filter(RainTomorrow == "Yes") %>% nrow()) / (weather %>% nrow())


# view how much rainfall was recorded in each year? To make further calculations easier, I will add "year" as a column
weather <- weather %>% 
  mutate(year = format(Date, "%Y"))
weather %>%
  group_by(year) %>%
  filter(!is.na(Rainfall)) %>% 
  summarize(rainfall = sum(Rainfall)) %>%
  ggplot(aes(year, rainfall)) + 
  geom_bar(stat = "identity", fill = "#4542EC") + 
  geom_label(aes(year, rainfall, label = year)) + 
  xlab("Year") +
  ylab("Total Rainfall Amount (mm)") + 
  ggtitle("Rainfall in mm by Year")
  

# how many days of rain did we get per year?
weather %>% filter(Rainfall > 0) %>% group_by(year) %>% summarize(days_with_rain = n()) 


# Like year, I'll add month as a column for easier future calculations. 
# Get the month, the year, and the amount of rainfall
years <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
monthly_rainfall <- weather %>% 
  filter(year %in% years, !is.na(Rainfall)) %>% 
  mutate(month = format(Date, "%m")) %>% 
  group_by(month, year) %>% 
  summarize(rainfall = sum(Rainfall))

# Create plot
monthly_rainfall %>% 
  group_by(month) %>% 
  summarize(rain = mean(rainfall)) %>% 
  ggplot(aes(month, rain, group = 1)) + 
  geom_line(aes(month, rain, color = rain)) + 
  geom_point(aes(month, rain, color = rain)) + 
  geom_label(aes(month, rain, label = month, color = rain)) + 
  xlab("Month") +
  ylab("Total Rainfall Amount (mm)") + 
  ggtitle("Rainfall in mm by Month")

# Look at the month of June for each year
monthly_rainfall %>%
  filter(month == "06") %>% 
  ggplot(aes(year, rainfall)) + 
  geom_bar(stat="identity", color = "black", fill = "#4542EC") + 
  geom_label(aes(year, rainfall, label = year)) + 
  xlab("Year") +
  ylab("Rainfall Amount (mm)") + 
  ggtitle("Rainfall Amount in June")

# Let's look at distribution of rainy days
weather %>% 
  filter(Rainfall >= 1.0) %>% 
  group_by(Rainfall) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(Rainfall, count)) + 
  geom_histogram(stat = "identity") + 
  xlab("Rainfall Amount (mm)") +
  ylab("# of Days") + 
  ggtitle("Days with Rainfall Amount")

# Categorize rainfall amounts as low, medium, and high, and then show the number of days for each category and year
# Store categories into separate dataset
rainfall_categorized <- weather %>% 
  filter(Rainfall >= 1.0, year %in% years) %>% 
  # values for category cutoffs were simply chosen after examination of data distribution
  mutate(amount = ifelse(Rainfall <= 3, "Low", ifelse(Rainfall <= 10, "Medium", "High"))) %>% 
  group_by(year, amount) %>% 
  summarize(count = n())

# I want the graph to sort the categories from low to high, which requires factoring and arranging the categories
rainfall_categorized$amount <- factor(rainfall_categorized$amount, levels = c("High", "Medium", "Low")) 
rainfall_categorized <- arrange(rainfall_categorized, amount) 

# Create horizontal bar chart. Each year gets a bar, each bar is divided up by rain amount category. Each category is labeled with the percentage that category takes up in each year.
rainfall_categorized %>% 
  group_by(year) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(year, count, fill = amount)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip()  + 
  xlab("Year") +
  ylab("Rainfall Amount (mm)") + 
  ggtitle("Rainfall Amount Breakdown by Year")

# Does it tend to rain tomorrow if it rained today?
# First, let's look at the number of days in our dataset where it rained tomorrow compared to not raining tomorrow overall
weather %>% 
  group_by(RainTomorrow) %>% 
  summarize(count = n())

# Now let's look at that same data, but only show days where it rained today
weather %>% 
  filter(RainToday == "Yes") %>% 
  group_by(RainTomorrow) %>% 
  summarize(count = n())



# Group into low, medium, high again with same definition
rainfall_categorized <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(Rainfall)) %>% 
  mutate(Rainfall = ifelse(Rainfall <= 3, "Low", ifelse(Rainfall <= 10, "Medium", "High"))) %>% 
  group_by(Rainfall, RainTomorrow) %>% 
  summarize(count = n())

# Factor again for proper ordering in barchart
rainfall_categorized$Rainfall <- factor(rainfall_categorized$Rainfall, levels = c("High", "Medium", "Low"))
rainfall_categorized <- arrange(rainfall_categorized, Rainfall)

# Create bar chart
rainfall_categorized %>% 
  group_by(Rainfall) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(Rainfall, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip()  + 
  xlab("Rainfall Amount") +
  ylab("# of Days") + 
  ggtitle("Rainfall Amount Breakdown Showing Rain Tomorrow")

# This shows the average max temp for each month over a few years
weather %>% 
  mutate(MaxTemp = ifelse(is.na(MaxTemp), 0, MaxTemp)) %>% 
  filter(year %in% c(2011, 2012, 2013, 2014)) %>% 
  mutate(month = format(Date, "%m")) %>% 
  mutate(full_month = paste(year, month, sep = ":")) %>% 
  group_by(full_month) %>% 
  summarize(mean_max = mean(MaxTemp)) %>% 
  ggplot(aes(full_month, mean_max, group = 1)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  xlab("Year:Month") +
  ylab("Average MaxTemp") + 
  ggtitle("Average MaxTemp Across Years, Months")

# filter data, get count of days, categorize variable
rain_temp <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(MaxTemp)) %>% 
  mutate(MaxTemp = ifelse(MaxTemp <= 20, "Low", ifelse(MaxTemp <= 26, "Medium", "High"))) %>% 
  group_by(MaxTemp, RainTomorrow) %>% 
  summarize(count = n())

# factor and arrange for correct visualization
rain_temp$MaxTemp <- factor(rain_temp$MaxTemp, levels = c("High", "Medium", "Low"))
rain_temp <- arrange(rain_temp, MaxTemp)

# build visualization
rain_temp %>% 
  group_by(MaxTemp) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(MaxTemp, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip() + 
  xlab("MaxTemp Category") +
  ylab("# of Days") + 
  ggtitle("MaxTemp Breakdown Compared to RainTomorrow")

weather %>% 
  filter(year %in% years, !is.na(RainTomorrow), !is.na(MinTemp), !is.na(MaxTemp)) %>%
  mutate(temp_change = MaxTemp - MinTemp) %>% 
  group_by(RainTomorrow) %>% 
  summarize(average_change = mean(temp_change))


# filter out the NA's and get the average pressure change for rain tomorrow and no rain tomorrow
weather %>% 
  filter(year %in% years, !is.na(Pressure9am), !is.na(Pressure3pm), !is.na(RainTomorrow)) %>%
  mutate(pressure_change = Pressure3pm - Pressure9am) %>% 
  group_by(RainTomorrow) %>% 
  summarize(average_pressure_change = mean(pressure_change))

# categorize the pressure
pressure_categorized <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(Pressure3pm)) %>% 
  mutate(Pressure3pm = ifelse(Pressure3pm <= 1003, "Low", ifelse(Pressure3pm <= 1023, "Medium", "High"))) %>%
  group_by(Pressure3pm, RainTomorrow) %>% 
  summarize(count = n())

# factor and arrange for visualization
pressure_categorized$Pressure3pm <- factor(pressure_categorized$Pressure3pm, levels = c("High", "Medium", "Low"))
pressure_categorized <- arrange(pressure_categorized, Pressure3pm)

# build visualization
pressure_categorized %>% 
  group_by(Pressure3pm) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(Pressure3pm, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip() + 
  xlab("Pressure Category") +
  ylab("# of Days") + 
  ggtitle("Pressure Breakdown Compared to RainTomorrow")

# store weather in another variable
wind_weather <- weather

# factor and arrange direction
wind_weather$WindGustDir <- factor(weather$WindGustDir, levels = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")) 
wind_weather <- arrange(wind_weather, WindGustDir) 

# view number of days wind blew from direction with rain tomorrow
wind_weather %>% 
  filter(year %in% years, !is.na(WindGustDir), RainTomorrow == "Yes") %>% 
  group_by(WindGustDir) %>% 
  summarize(days = n()) %>% 
  ggplot(aes(WindGustDir, days)) + geom_bar(stat = "identity") + 
  xlab("Wind Gust Direction") +
  ylab("# of Days with Rain Tomorrow") + 
  ggtitle("Wind Directions Bringing Rain Tomorrow")

# create States variable
states_abbv <- c("NSW", "NI", "NT", "Q", "SA", "T", "V", "WA")
states <- c("New South Wales", "Norfolk Island", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")

# rename "Location" to "City" and assign each city their appropriate state
state_weather <- wind_weather %>% 
  mutate(City = Location) %>% 
  mutate(State = 
           ifelse(Location %in% c("Albury", "BadgerysCreek", "Canberra", "Cobar", "CoffsHarbour", "Moree", "MountGinini", "Newcastle", "NorahHead", "Penrith", "Sydney", "SydneyAirport", "Tuggeranong", "WaggaWagga", "Williamtown", "Wollongong"), states[1], 
                  ifelse(Location  %in% c("NorfolkIsland"), states[2], 
                         ifelse(Location %in% c("AliceSprings", "Darwin", "Katherine", "Uluru"), states[3], ifelse(Location %in% c("Brisbane", "Cairns", "GoldCoast", "Townsville"), states[4], 
                                                                                                                   ifelse(Location %in% c("Adelaide", "MountGambier", "Nuriootpa", "Woomera"), states[5], 
                                                                                                                          ifelse(Location %in% c("Hobart", "Launceston"), states[6], 
                                                                                                                                 ifelse(Location %in% c("Ballarat", "Bendigo", "Dartmoor", "Melbourne", "MelbourneAirport", "Mildura", "Nhil", "Portland", "Richmond", "Sale", "Watsonia"), states[7], 
                                                                                                                                        ifelse(Location %in% c("Albany", "PearceRAAF", "Perth", "PerthAirport", "SalmonGums", "Walpole", "Witchcliffe"), states[8], "*******ERROR********"))))))))) %>% 
  select(-Location) 

# create same wind direction graph, but create individual graphs for each state
# let the y-axis change based on amount of data
state_weather %>% 
  filter(year %in% years, !is.na(WindGustDir), RainTomorrow == "Yes") %>% 
  group_by(State, WindGustDir) %>% 
  summarize(days = n()) %>% 
  ggplot(aes(WindGustDir, days)) + 
  geom_bar(stat = "identity") + 
  facet_grid(State~., scales = "free_y", ) + 
  xlab("Wind Gust Direction") +
  ylab("# of Days with Rain Tomorrow") + 
  ggtitle("Wind Directions Bringing Rain Tomorrow by State")

# create same chart, but for RainTomorrow = No
state_weather %>% 
  filter(year %in% years, !is.na(WindGustDir), RainTomorrow == "No") %>% 
  group_by(State, WindGustDir) %>% 
  summarize(days = n()) %>% 
  ggplot(aes(WindGustDir, days)) + 
  geom_bar(stat = "identity") + 
  facet_grid(State~., scales = "free_y", ) + 
  xlab("Wind Gust Direction") +
  ylab("# of Days without Rain Tomorrow") + 
  ggtitle("Wind Directions with No Rain Tomorrow by State")

states_abbv <- c("NSW", "NI", "NT", "Q", "SA", "T", "V", "WA")
states <- c("New South Wales", "Norfolk Island", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")
state_weather <- weather %>% 
  mutate(City = Location) %>% 
  mutate(State = ifelse(Location %in% c("Albury", "BadgerysCreek", "Canberra", "Cobar", "CoffsHarbour", "Moree", "MountGinini", "Newcastle", "NorahHead", "Penrith", "Sydney", "SydneyAirport", "Tuggeranong", "WaggaWagga", "Williamtown", "Wollongong"), states[1], ifelse(Location  %in% c("NorfolkIsland"), states[2], ifelse(Location %in% c("AliceSprings", "Darwin", "Katherine", "Uluru"), states[3], ifelse(Location %in% c("Brisbane", "Cairns", "GoldCoast", "Townsville"), states[4], ifelse(Location %in% c("Adelaide", "MountGambier", "Nuriootpa", "Woomera"), states[5], ifelse(Location %in% c("Hobart", "Launceston"), states[6], ifelse(Location %in% c("Ballarat", "Bendigo", "Dartmoor", "Melbourne", "MelbourneAirport", "Mildura", "Nhil", "Portland", "Richmond", "Sale", "Watsonia"), states[7], ifelse(Location %in% c("Albany", "PearceRAAF", "Perth", "PerthAirport", "SalmonGums", "Walpole", "Witchcliffe"), states[8], "*******ERROR********"))))))))) %>% 
  select(-Location) 
state_weather %>% 
  filter(!is.na(Rainfall)) %>% 
  group_by(State) %>% 
  summarize(rainfall = sum(Rainfall)) %>% 
  ggplot(aes(State, rainfall)) + 
  geom_bar(stat = "identity", fill = "#252580") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# put in number of cities for each state
city_count <- data.frame(State = states, count = ifelse(states == "New South Wales", 16, ifelse(states == "Norfolk Island", 1, ifelse(states == "Northern Territory", 4, ifelse(states == "Queensland", 4, ifelse(states == "South Australia", 4, ifelse(states == "Tasmania", 2, ifelse(states == "Victoria", 11, ifelse(states == "Western Australia", 7, 1)))))))))

# recreate the graph with rainfall normalized
state_weather %>% 
  filter(!is.na(Rainfall)) %>% 
  group_by(State) %>% 
  summarize(rainfall = sum(Rainfall)) %>% 
  left_join(city_count, by="State") %>% 
  mutate(rainfall = rainfall/count) %>% 
  ggplot(aes(State, rainfall)) + 
  geom_bar(stat = "identity", fill = "#252580") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# find average change in humidity for days that rained tomorrow and didn't rain tomorrow
weather %>% 
  filter(year %in% years, !is.na(RainTomorrow), !is.na(Humidity9am), !is.na(Humidity3pm)) %>% 
  mutate(humidity_change = Humidity3pm - Humidity9am) %>% 
  group_by(RainTomorrow) %>% 
  summarize(average_change = mean(humidity_change))

# save data into variable for boxplot chart
humidity_change <- weather %>% 
  filter(year %in% years, !is.na(RainTomorrow), !is.na(Humidity9am), !is.na(Humidity3pm)) %>% 
  mutate(change = Humidity3pm - Humidity9am) %>% 
  select(RainTomorrow, change)

# create boxplot
boxplot(change ~ RainTomorrow, data=humidity_change)


# Does high humidity mean rain tomorrow?
humidity_categorized <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(Humidity3pm)) %>% 
  mutate(Humidity3pm = ifelse(Humidity3pm <= 25, "Low", ifelse(Humidity3pm <= 74, "Medium", "High"))) %>% 
  group_by(Humidity3pm, RainTomorrow) %>% 
  summarize(count = n())

# factor and arrange
humidity_categorized$Humidity3pm <- factor(humidity_categorized$Humidity3pm, levels = c("High", "Medium", "Low"))
humidity_categorized <- arrange(humidity_categorized, Humidity3pm)

# create visualization
humidity_categorized %>% 
  group_by(Humidity3pm) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(Humidity3pm, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip() + 
  xlab("Humidity Levels") +
  ylab("# of Days with Rain Tomorrow") + 
  ggtitle("Humidity Levels for Days with Rain Tomorrow")

# there are a lot of NA's in the cloud data. How many rows do we have with complete 9am and 3pm data?
(weather %>% filter(!is.na(Cloud9am), !is.na(Cloud3pm)) %>% summarize(count = n())) / (weather %>% summarize(count = n()))

# only just over half of all days in this dataset have data for the 9am and 3pm cloud data. I would assume that the data is missing at random, I have no reason to suggest otherwise. For the purpose of data exploration, I will just remove the missing values
# let's look at how cloud coverage changes with regards to whether it rains tomorrow or not
weather %>% 
  filter(!is.na(Cloud9am), !is.na(Cloud3pm)) %>% 
  mutate(cloud_change = Cloud3pm - Cloud9am) %>% 
  group_by(RainTomorrow) %>% 
  summarize(average_change = mean(cloud_change))

# Does very cloudy mean rain tomorrow?
cloud_categorized <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(Cloud3pm)) %>% 
  mutate(Cloud3pm = ifelse(Cloud3pm <= 2, "Low", ifelse(Cloud3pm <= 7, "Medium", "High"))) %>% 
  group_by(Cloud3pm, RainTomorrow) %>% 
  summarize(count = n())

# factor and arrange
cloud_categorized$Cloud3pm <- factor(cloud_categorized$Cloud3pm, levels = c("High", "Medium", "Low"))
cloud_categorized <- arrange(cloud_categorized, Cloud3pm)

# build visualization
cloud_categorized %>% 
  group_by(Cloud3pm) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(Cloud3pm, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip() + 
  xlab("Cloud Coverage") +
  ylab("# of Days with Rain Tomorrow") + 
  ggtitle("Cloud Coverage for Days with Rain Tomorrow")

# Temp for RainTomorrow
weather %>% 
  filter(!is.na(Temp9am), !is.na(Temp3pm), !is.na(RainTomorrow)) %>% 
  mutate(temp_change = Temp3pm - Temp9am) %>% 
  group_by(RainTomorrow) %>% 
  summarize(average_change = mean(temp_change)) 


# check boxplot distributions
temp_change <- weather %>% 
  filter(!is.na(Temp9am), !is.na(Temp3pm), !is.na(RainTomorrow)) %>% 
  mutate(change = Temp3pm - Temp9am) %>%
  select(RainTomorrow, change)
boxplot(change ~ RainTomorrow, data=temp_change)


# Does wind speed change have a relationship whether it rained tomorrow or not?
weather %>% 
  filter(!is.na(WindSpeed9am), !is.na(WindSpeed3pm), !is.na(RainTomorrow)) %>% 
  mutate(wind_change = WindSpeed3pm - WindSpeed9am) %>% 
  group_by(RainTomorrow) %>% 
  summarize(average_change = mean(wind_change))

# Do very windy days indicate rain tomorrow?
wind_categorized <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(WindSpeed3pm)) %>% 
  mutate(WindSpeed3pm = ifelse(WindSpeed3pm <= 7, "Low", ifelse(WindSpeed3pm <= 33, "Medium", "High"))) %>% 
  group_by(WindSpeed3pm, RainTomorrow) %>% 
  summarize(count = n())

# factor and arrange
wind_categorized$WindSpeed3pm <- factor(wind_categorized$WindSpeed3pm, levels = c("High", "Medium", "Low"))
wind_categorized <- arrange(wind_categorized, WindSpeed3pm)

# build visualization
wind_categorized %>% 
  group_by(WindSpeed3pm) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(WindSpeed3pm, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip() + 
  xlab("Wind Speed") +
  ylab("# of Days with Rain Tomorrow") + 
  ggtitle("Wind Speed for Days with Rain Tomorrow")

# How much evaporation data do we have?
(weather %>% filter(!is.na(Evaporation)) %>% summarize(count = n())) / (weather %>% summarize(count = n()))

# let's categorize it 
evaporation_categorized <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(Evaporation)) %>% 
  mutate(Evaporation = ifelse(Evaporation <= 3, "Low", ifelse(Evaporation <= 10, "Medium", "High"))) %>% 
  group_by(Evaporation, RainTomorrow) %>% 
  summarize(count = n())

# factor and arrange
evaporation_categorized$Evaporation <- factor(evaporation_categorized$Evaporation, levels = c("High", "Medium", "Low"))
evaporation_categorized <- arrange(evaporation_categorized, Evaporation)

# build visualization
evaporation_categorized %>% 
  group_by(Evaporation) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(Evaporation, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip() + 
  xlab("Evaporation Level") +
  ylab("# of Days with Rain Tomorrow") + 
  ggtitle("Evaporation Level for Days with Rain Tomorrow")

# categorize data
sunshine_evaporation <- weather %>% 
  filter(!is.na(RainTomorrow), !is.na(Sunshine)) %>% 
  mutate(Sunshine = ifelse(Sunshine <= 3, "Low", ifelse(Sunshine <= 8, "Medium", "High"))) %>% 
  group_by(Sunshine, RainTomorrow) %>% 
  summarize(count = n())

# factor and arrange
sunshine_evaporation$Sunshine <- factor(sunshine_evaporation$Sunshine, levels = c("High", "Medium", "Low"))
sunshine_evaporation <- arrange(sunshine_evaporation, Sunshine)

# build visualization
sunshine_evaporation %>% 
  group_by(Sunshine) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  mutate(label = paste(round(percent, digits = 1), "%", sep = "", collapse = NULL)) %>% 
  ggplot(aes(Sunshine, count, fill = RainTomorrow)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) + 
  coord_flip() + 
  xlab("Hours of Sunshine") +
  ylab("# of Days with Rain Tomorrow") + 
  ggtitle("Hours of Sunshine for Days with Rain Tomorrow")

# create modeling dataset. I will start with all variables that might add value
# we need to put the month in there, combine city and windgustdir
weather_model <- weather %>% 
  mutate(month = format(Date, "%m"), wind_location = apply(.,1 ,function(x) paste(toString(x[2]), toString(x[8]))), humidity_change = Humidity3pm - Humidity9am, temp_change = Temp3pm - Temp9am) %>%
  select(month, Rainfall, Pressure3pm, wind_location, Humidity3pm, humidity_change, Sunshine, MaxTemp, Evaporation, Cloud3pm, temp_change, RainTomorrow)

# check how many records are missing for which variables.
md.pattern(weather_model)

# Let's look at a more helpful visualization from the VIM package:
  aggr(weather_model, col=c('darkblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(weather), cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

# This next visualization will help us understand if Evaporation is missing at random. I will plot it against pressure
marginplot(weather_model[c(9,3)])

# This next visualization will help us understand if Evaporation is missing at random. I will plot it against pressure
marginplot(weather_model[c(7,3)])
marginplot(weather_model[c(9,5)])
marginplot(weather_model[c(5,2)])
marginplot(weather_model[c(7,5)])

# run imputation. This will run 3 iterations with 5 imputations each. The method "pmm" means "predictive mean matching"
tempData <- mice(weather_model, m=5, maxit=1, meth='pmm', seed=500)

# summary for looking at which variables were imputed with which method
summary(tempData)

# to access the imputed data for a single variable, you would run this code:
# tempData$imp$Evaporation

# run density plots to check for plausibility
densityplot(tempData)

# add the imputed values back into the original dataset, using values from the first iteration.
completed_weather <- complete(tempData, 1)

# check to see if mice did its work 
md.pattern(completed_weather)


# due to complexities I was getting from data classified as characters, I have dropped the month and wind_location variables.
completed_weather <- completed_weather %>% select(-month, -wind_location)

# change RainTomorrow to be binary 1 and 0, not "Yes" and "No"
completed_weather <- completed_weather %>% mutate(RainTomorrow = as.factor(ifelse(RainTomorrow == "Yes", 1, 0)))
summary(completed_weather)

# find how many rows that 90% of the data is, 10% of the data
set.seed(1234)
n <- nrow(completed_weather)
ntrain <- round((n*.90),0)
ntest <-  round((n*.10),0)

# random sample the data to get the train and test data.
i_train <- sample(1:n, ntrain, replace=FALSE)
train_set <- completed_weather[i_train, ]
test_set <- completed_weather[-i_train, ]

# run knn in sapply to see which k-value maximizes the accuracy
accuracies <- sapply(seq(5, 15, 1), function(k){
  knn_fit <- knn3(RainTomorrow ~ ., data = train_set, k = k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class")
  confusionMatrix(data = y_hat_knn, reference = test_set$RainTomorrow)$overall["Accuracy"]
})

# get value of k that produced highest accuracy and re-run knn to produce full confusion matrix
k <- which.max(accuracies)+4

# re-run knn to produce full confusion matrix
knn_fit <- knn3(RainTomorrow ~ ., data = train_set, k = k)
y_hat_knn <- predict(knn_fit, test_set, type = "class")
confusionMatrix(data = y_hat_knn, reference = test_set$RainTomorrow)
knn_conf <- confusionMatrix(data = y_hat_knn, reference = test_set$RainTomorrow)

# create results model comparison dataframe
model_results <- data_frame(method = "K-Nearest Neighbors", Accuracy = knn_conf$overall["Accuracy"], Specificity = knn_conf$byClass["Specificity"])

# add predictions to the test set
weather_predicted <- test_set %>% mutate(predictions = y_hat_knn)

# calculate whether we had a true positive, false positive, false negative, or true negative
weather_predicted <- weather_predicted %>% mutate(precision = ifelse(RainTomorrow == 0 & predictions == 0, "True Negative", ifelse(RainTomorrow == 0 & predictions == 1, "False Positive", ifelse(RainTomorrow == 1 & predictions == 0, "False Negative", ifelse(RainTomorrow == 1 & predictions == 1, "True Positive", "Error")))))

# save number of each outcome to variable to later put in matrix
trueNeg <- weather_predicted %>% filter(precision == "True Negative") %>% nrow()
truePos <- weather_predicted %>% filter(precision == "True Positive") %>% nrow()
falseNeg <- weather_predicted %>% filter(precision == "False Negative") %>% nrow()
falsePos <- weather_predicted %>% filter(precision == "False Positive") %>% nrow()

# create matrix
confMatrix <- matrix(c(truePos, falseNeg, falsePos, trueNeg), ncol=2, byrow=TRUE)
rownames(confMatrix) <- c("Actually Rained", "Actually Didn't Rain")
colnames(confMatrix) <- c("Predicted Rain", "Predicted No Rain")
confMatrix <- as.table(confMatrix)
confMatrix

# run CART
train_rpart <- train(RainTomorrow ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = train_set)

# we can see from this plot what complexity parameter provides the highest accuracy, found through cross-validation
ggplot(train_rpart)

# plot decision tree visualization
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)


# check confusion matrix for accuracy
confusionMatrix(predict(train_rpart, test_set), test_set$RainTomorrow)
cart_conf <- confusionMatrix(predict(train_rpart, test_set), test_set$RainTomorrow)


# add CART results to results dataframe
model_results <- bind_rows(model_results,
                           data_frame(method="CART",  
                                      Accuracy = cart_conf$overall["Accuracy"],
                                      Specificity = cart_conf$byClass["Specificity"]))

# try raindom forest on model 2
rf_fit <- randomForest(RainTomorrow ~ ., data = train_set)

# this plot shows the error versus the number of trees. After about 50 trees, the error does not change much.
plot(rf_fit)

# check accuracy
confusionMatrix(predict(rf_fit, test_set), test_set$RainTomorrow)
rf_conf <- confusionMatrix(predict(rf_fit, test_set), test_set$RainTomorrow)

model_results <- bind_rows(model_results,
                          data_frame(method="Random Forest",  
                                     Accuracy = rf_conf$overall["Accuracy"],
                                     Specificity = rf_conf$byClass["Specificity"]))

model_results


