# Importing libraries

library(tidyverse)
library(ggplot2)
library(dplyr)
library(skimr)


# Importing data
dec_2021 <- read.csv(file.choose())
jan_2022 <- read.csv(file.choose())
feb_2022 <- read.csv(file.choose())
march_2022 <- read.csv(file.choose())
april_2022 <- read.csv(file.choose())
may_2022 <- read.csv(file.choose())

#binding data to a data frame

df_bike_share <- rbind(dec_2021,jan_2022,feb_2022,march_2022,april_2022,may_2022)

#cleaning data
skim_without_charts(df_bike_share)
#Eliminating null values
df_bike_share <- na.omit(df_bike_share)

#Changing col names.

colnames(df_bike_share)[colnames(df_bike_share) == "rideable_type"] <- "bike_type"
colnames(df_bike_share)[colnames(df_bike_share) == "member_casual"] <- "user_type"


# Casting started_st and ended_at to time.
df_bike_share$started_at <- strptime(df_bike_share$started_at, format = "%Y-%m-%d %H:%M:%S")
df_bike_share$ended_at <- strptime(df_bike_share$ended_at, format = "%Y-%m-%d %H:%M:%S") 

# Calculating duration of trip
# Calculating Weekday,month and Year of the trip
df_bike_share$trip_duration <- with(df_bike_share,ended_at - started_at) 
df_bike_share$weekday <- with(df_bike_share,weekdays(started_at))
df_bike_share$ride_month <- format(df_bike_share$started_at,format = "%b")
df_bike_share$ride_year <- format(df_bike_share$started_at,format = "%Y")

# no:of rides of user type in week days
df_bike_share %>% 
  group_by(user_type,weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user_type,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=user_type))+ geom_col(position = "dodge")
