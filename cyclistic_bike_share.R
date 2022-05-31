install.packages('tidyverse')
install.packages('janitor')

# Load Libraries
library(tidyverse)
library(janitor)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(scales)

# Load datasets
df1 <- read_csv("data/tripdata_202105.csv")
df2 <- read_csv("data/tripdata_202106.csv")
df3 <- read_csv("data/tripdata_202107.csv")
df4 <- read_csv("data/tripdata_202108.csv")
df5 <- read_csv("data/tripdata_202109.csv")
df6 <- read_csv("data/tripdata_202110.csv")
df7 <- read_csv("data/tripdata_202111.csv")
df8 <- read_csv("data/tripdata_202112.csv")
df9 <- read_csv("data/tripdata_202201.csv")
df10 <- read_csv("data/tripdata_202202.csv")
df11 <- read_csv("data/tripdata_202203.csv")
df12 <- read_csv("data/tripdata_202204.csv")

# Combine all datasets into a single dataframe
tripdata <- bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# Check data structures and types

str(tripdata)

# Data cleaning and transformation

## Remove NA in rows
tripdata <- drop_na(tripdata)

## Remove duplicates on ride_id
tripdata <- distinct(tripdata, ride_id, .keep_all = TRUE)

## Make sure the rows that the started time is not larger than ended time
tripdata <- subset(tripdata, started_at <= ended_at)

## Set languages to en_us for showing day of week
Sys.setlocale("LC_TIME", "en_us")

## Calculate the duration of ride in Minutes and determine the day of week of ride started

tripdata <- tripdata %>%
  mutate(ride_length = round(difftime(ended_at, started_at, units = "mins"), 2)) %>%
  mutate(day_of_week = weekdays(started_at))

## Sort the dataframe with started_at in descending order
tripdata <- tripdata %>%
  arrange(desc(started_at))

## Add the column for month and year
tripdata <- tripdata %>%
  dplyr::mutate(month = lubridate::month(started_at), year = lubridate::year(started_at)) %>%
  unite(col = "month_year", c("month", "year"), sep = "-")
  
## Remove columns of start_lat, start_lng, end_lat, end_lng that not used
tripdata <- tripdata %>%
  select(-c(start_lat:end_lng))
str(tripdata)

## Convert ride_length to numeric
tripdata$ride_length <- as.numeric(tripdata$ride_length)
is.numeric(tripdata$ride_length)

## Check counts of customer types
table(tripdata$member_casual)

## Aggregate total ride duration by customer types
setNames(aggregate(tripdata$ride_length ~ tripdata$member_casual, tripdata, sum), c("Customer Type", "Total Ride Duration"))

# Data Analysis

## Statistical summary of ride_length for all trips
summary(tripdata$ride_length)

## Summarize ride_length by customer types
tripdata %>%
  group_by(member_casual) %>%
  summarise(min_ride_length = min(ride_length), max_ride_length = max(ride_length),
            median_ride_length = median(ride_length), average_ride_length = mean(ride_length))

## Total number of rides and average duration by customer type and day of week

### Fix the order for day of week variable to show same sequence in output table and visualization
tripdata$day_of_week <- ordered(tripdata$day_of_week, levels = c("Sunday", "Monday", "Tuesday",
                                "Wednesday", "Thursday", "Friday", "Saturday"))

### Calculate the average duration and total number of rides by day of week
tripdata_weekly <- tripdata %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)
head(tripdata_weekly)

## Total number of rides and average duration by customer type and month
### Fix the order for month_year variable to show same sequence in output table and visualization
tripdata$month_year <- ordered(tripdata$month_year, levels = c("5-2021", "6-2021", "7-2021", "8-2021",
                                                               "9-2021", "10-2021", "11-2021", "12-2021",
                                                               "1-2022", "2-2022", "3-2022", "4-2022"))

### Calculate the average duration and total number of rides by month
tripdata_monthly <- tripdata %>%
  group_by(member_casual, month_year) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, month_year)
head(tripdata_monthly)

# Visualization

## Rides by customer type

tripdata %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>%
  mutate(portion = number_of_rides / sum(number_of_rides)) %>%
  ggplot(aes(x ="", y=number_of_rides, fill=member_casual, label = scales::percent(portion))) +
  geom_bar(stat = "identity", size = 1) +
  coord_polar("y", start = 0)+
  theme_void() +
  labs(title = "Rides by customer type", fill="Customer Type")+
  geom_text(position = position_stack(vjust = 0.5), size = 7.5) 

## Most popular rideable type by customer type

tripdata %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill=member_casual)) +
  geom_col(width = 0.5, position = position_dodge(0.5)) +
  labs(title = "Most popular rideable type by customer type", fill = "Customer Type") +
  xlab("Customer Type") + ylab("Total number of rides (Thousands)") +
  scale_y_continuous(labels = unit_format(unit ="", scale = 1e-3)) +
  facet_wrap(~rideable_type)+
  theme(strip.text = element_text(size = 10))

## Total number of rides by customer type for previous 12 months
tripdata_monthly %>%
  ggplot(aes(x = month_year, y = number_of_rides, fill = member_casual)) +
           geom_col(width = 0.5, position = position_dodge(0.6))+
  labs(title = "Total rides by customer type for previous 12 months", fill = "Customer Type")+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Month") + ylab("Total Number of rides (Thousands)") +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3))

## Total number of rides by customer type on each day of week
tripdata_weekly %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(width = 0.5, position = position_dodge(0.6))+
  labs(title = "Total rides by customer type on each day of week", fill = "Customer Type")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
  xlab("Day of week") + ylab("Total Number of rides (Thousands)") +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3))

## The average duration by customer type for previous 12 months
tripdata_monthly %>%
  ggplot(aes(x = month_year, y = average_duration, color = member_casual, group = member_casual)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Average duration by customer type for previous 12 months", color = "Customer Type")+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Month") + ylab("Average duration (Mins)")

## The average duration by customer type on each day of week
tripdata_weekly %>%
  ggplot(aes(x = day_of_week, y = average_duration, color = member_casual, group = member_casual)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Average duration by customer type on each day of week", color = "Customer Type")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
  xlab("Day of week") + ylab("Average duration (Mins)")

## Save the datasets for futher uses
write_csv(tripdata, file = "data\tripdata.csv")
write_csv(tripdata_monthly, file = "data\tripdata_monthly.csv")
write_csv(tripdata_weekly, file = "data\tripdata_weekly.csv")