# Importing libraries

library(tidyverse)
library(ggplot2)
library(dplyr)
library(skimr)


# Importing data
dec_2021 <- read.csv("/home/asker/Desktop/DataAnalyst/Project/bike_share/202112-divvy-tripdata.csv")
jan_2022 <- read.csv('/home/asker/Desktop/DataAnalyst/Project/bike_share/202201-divvy-tripdata.csv')
feb_2022 <- read.csv('/home/asker/Desktop/DataAnalyst/Project/bike_share/202202-divvy-tripdata.csv')
march_2022 <- read.csv('/home/asker/Desktop/DataAnalyst/Project/bike_share/202203-divvy-tripdata.csv')
april_2022 <- read.csv('/home/asker/Desktop/DataAnalyst/Project/bike_share/202204-divvy-tripdata.csv')
may_2022 <- read.csv('/home/asker/Desktop/DataAnalyst/Project/bike_share/202205-divvy-tripdata.csv')

#binding data to a data frame

df_bike_share <- rbind(dec_2021,jan_2022,feb_2022,march_2022,april_2022,may_2022)

#Changing col names.

colnames(df_bike_share)[colnames(df_bike_share) == "rideable_type"] <- "bike_type"
colnames(df_bike_share)[colnames(df_bike_share) == "member_casual"] <- "user_type"

# Casting started_st and ended_at to time.
df_bike_share$started_at <- strptime(df_bike_share$started_at, format = "%Y-%m-%d %H:%M:%S")
df_bike_share$ended_at <- strptime(df_bike_share$ended_at, format = "%Y-%m-%d %H:%M:%S") 

# Calculating duration of trip
# Calculating Weekday,month and Year of the trip
df_bike_share$trip_duration <- with(df_bike_share,ended_at - started_at)
df_bike_share$trip_duration <- as.numeric(df_bike_share$trip_duration)
df_bike_share$weekday <- with(df_bike_share,weekdays(started_at))
df_bike_share$ride_month <- format(df_bike_share$started_at,format = "%m")
trip_month <- format(df_bike_share$ride_month,format="%b")
df_bike_share$ride_month <- ordered(df_bike_share$ride_month,month.name)
df_bike_share$ride_year <- format(df_bike_share$started_at,format = "%Y")

#cleaning data
skim_without_charts(df_bike_share)
head(df_bike_share)
colnames(df_bike_share)
#Eliminating null values
df_list_clean <- na.omit(df_bike_share)
# excluding negative values from trip_duration
neg_trip <- nrow(df_bike_share[df_bike_share$trip <0,]) 
fals_trip <- df_bike_share[df_bike_share$trip <0,]


# no:of rides of user type in week days
df_list_clean %>% 
  group_by(user_type,weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user_type) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=user_type))+ geom_col(position = "dodge") +
  labs(title = "User type and weekdays with total rides")

# Visualizing no:of user using different bike type.
 df_bike_share %>% 
   group_by(bike_type,user_type) %>% 
   summarise(number_of_rides = n())%>%
   arrange(bike_type,user_type) %>% 
   ggplot(aes(x=bike_type,y=number_of_rides,fill=user_type)) + geom_col(position = "dodge")+
   labs(title = "User type and bike type with total rides")

# members with longest trip duration and bike type
df_list_clean %>% 
  group_by(user_type,bike_type) %>% 
  arrange(bike_type,user_type) %>% 
  summarise(Avg_trip_duration= mean(trip_duration)) %>% 
  ggplot(aes(x=bike_type,y=Avg_trip_duration,fill=user_type)) + geom_col(position = "dodge")+
  labs(title = "Bike type and Average trip duartion")

#members with trips in each month with bike type
df_list_clean %>% 
  group_by(user_type,bike_type) %>% 
  arrange(user_type,bike_type) %>% 
  ggplot(aes(x="bike_type",y=ride_month,fill=user_type)) + geom_col(position='dodge') + facet_grid(~bike_type)+
  labs(title = "Month vise trip numbers for each bike type ")

#number of trips in each month
df_bike_share %>% 
  group_by(ride_month,user_type) %>% 
  summarise(no_of_trips=mean(n())) %>% 
  arrange(ride_month) %>% 
  ggplot(aes(x=ride_month,y=no_of_trips,group=user_type,colour=user_type)) + geom_line() + 
  labs(title = "Trips in each month")

#no:of user type with average trips duration
df_bike_share %>% 
  group_by(user_type) %>% 
  summarise(Avg_trip_duration= mean(trip_duration)) %>% 
  ggplot(aes(x=Avg_trip_duration,y=user_type,fill= user_type)) + geom_col(position= "dodge") + 
  theme(aspect.ratio = .35) + labs(title = "Average trip duartion and user type")


