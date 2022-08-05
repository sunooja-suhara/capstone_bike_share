

library(ggplot2)
library(skimr)
library(tidyverse)
library(dplyr)
library(lubridate)


aug_2021 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202108-divvy-tripdata.csv")
sep_2021 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202109-divvy-tripdata.csv")
oct_2021 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202110-divvy-tripdata.csv")
nov_2021 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202111-divvy-tripdata.csv")
dec_2021 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202112-divvy-tripdata.csv")
jan_2022 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202201-divvy-tripdata.csv")
feb_2022 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202202-divvy-tripdata.csv")
mar_2022 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202203-divvy-tripdata.csv")
aprl_2022 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202204-divvy-tripdata.csv")
may_2022 <- read.csv("/Users/askerakbar/Downloads/Suhara/Data/202205-divvy-tripdata.csv")

#Binding data into a data frame
data_bike <- rbind(aug_2021,sep_2021,oct_2021,nov_2021,dec_2021,jan_2022,feb_2022,mar_2022,aprl_2022,may_2022)
glimpse(data_bike)
data_bike_share <- rbind(aug_2021,sep_2021,oct_2021,nov_2021,dec_2021,jan_2022,feb_2022,mar_2022,aprl_2022,may_2022)

#renaming existing columns
colnames(data_bike_share)[colnames(data_bike_share) == "rideable_type"] <- "bike_type"
colnames(data_bike_share)[colnames(data_bike_share) == "member_casual"] <- "user_type"

#casting started_at and ended_at to time data type
data_bike_share$started_at <- strptime(data_bike_share$started_at, format = "%Y-%m-%d %H:%M:%S")
data_bike_share$ended_at <- strptime(data_bike_share$ended_at, format = "%Y-%m-%d %H:%M:%S")



## Calculating margin of error if calculated with na values and total negative values in trip duration.
neg_trip <- nrow(data_bike_share[data_bike_share$trip_duration <=0, ])
count_start_na <- nrow(data_bike_share[data_bike_share$start_station_name == "", ])
count_endt_na <- nrow(data_bike_share[data_bike_share$end_station_name == "", ])
total_row <- nrow(data_bike_share)
margin_of_error <- ((count_start_na + count_endt_na) / total_row)* 100


#Adding columns for analysis
data_bike_share$trip_duration <-as.numeric(with(data_bike_share,ended_at - started_at))  
data_bike_share$weekday <- with(data_bike_share,weekdays(started_at))
data_bike_share$ride_month <- format(data_bike_share$started_at, format = "%b")
data_bike_share$ride_year <- format(data_bike_share$started_at, format = "%Y")
#Calculating the time of travel(morning,Afternoon,Eve,night)
data_bike_share$start_hour <- hour(data_bike_share$started_at)
data_bike_share$start_hour <- ifelse(data_bike_share$start_hour >=5 & data_bike_share$start_hour<11,"morning",
                                   if_else(data_bike_share$start_hour>=11 & data_bike_share$start_hour <17,"Noon",
                                           ifelse(data_bike_share$start_hour>=17 & data_bike_share$start_hour<22,"Evening","Night") ))


no_of_rides <- nrow(data_bike_share)
no_of_rides<-mean(no_of_rides)

#####Data Cleaning
head(data_bike_share)
glimpse(data_bike_share)
skim_without_charts(data_bike_share)

  
######################
#Avg trip duration and bike type
data_bike_share %>% 
  filter(trip_duration >0) %>% 
  group_by(bike_type,user_type) %>% 
  arrange(bike_type) %>% 
  summarise(avg_trip_duration=mean(trip_duration),.groups = 'drop') %>% 
  ggplot(aes(x=bike_type,y=avg_trip_duration,fill=user_type))+ 
  geom_col(position = "dodge",,width =0.4)+
  labs(title = "Average trip duration of each user in different bike type")

# comparing users
data_bike_share %>% 
  group_by(user_type) %>% 
  summarise(no_of_rides=n()) %>% 
  ggplot(aes(x="",y=no_of_rides,fill=user_type)) + geom_col(color="black") +
  geom_text(aes(label=no_of_rides),position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") + theme_void()+
  scale_fill_brewer() +  labs(title = "Casual members Vs User with Membership")

#comparing bike types
data_bike_share %>% 
  group_by(bike_type) %>% 
  summarise(no_of_rides=n(),.groups = 'drop') %>% 
  ggplot(aes(x="",y=no_of_rides,fill=bike_type)) + geom_col(color="black") +
  geom_text(aes(label=no_of_rides),position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") + theme_void()+
  scale_fill_brewer()+ labs(title = "Comparing each bike types")

#Comparing the rides in each hours(morning,Noon,Eve and night)
data_bike_share %>% 
  group_by(start_hour) %>% 
  summarise(no_of_rides=n(),.groups = 'drop') %>% 
  ggplot(aes(x="",y=no_of_rides,fill=start_hour)) + geom_col(color="black") +
  geom_text(aes(label=no_of_rides),position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") + theme_void()+
  scale_fill_brewer()+
  labs(title = "Rides in each time of a day")


#Avg Trip for each day for different user type
data_bike_share %>% 
  filter(trip_duration >0) %>% 
  group_by(weekday,user_type) %>%
  summarise(avg_trip_duration= mean(trip_duration),.groups = 'drop') %>% 
  ggplot(aes(x=weekday,y=avg_trip_duration,fill=user_type)) +
  geom_col(position = "dodge") + labs(title = "Avg trip duration on each day for different user")

#Avg Trip for each month for different user
data_bike_share %>% 
  filter(trip_duration >0) %>% 
  group_by(ride_month,user_type) %>%
  summarise(avg_trip_duration= mean(trip_duration),.groups = 'drop') %>% 
  ggplot(aes(x=ride_month,y=avg_trip_duration,fill=user_type)) +
  geom_col(position = "dodge")+labs(title = "Average trip duartion on each month for diffent user")

#No:of trips in each year with user type
data_bike_share %>% 
  group_by(ride_year,user_type) %>% 
  summarise(no_of_rides=n(),.groups = 'drop') %>% 
  ggplot(aes(x=ride_year,y=no_of_rides,fill=user_type)) +
  geom_col(position =position_dodge(),width =0.35) +
  labs(title = "Number of trips in year for each user")

#Avg of trips in each year with user type
data_bike_share %>% 
  group_by(ride_year,user_type) %>% 
  summarise(avg_trip_duration= mean(trip_duration),.groups = 'drop') %>% 
  ggplot(aes(x=ride_year,y=avg_trip_duration,fill=user_type)) +
  geom_col(position = "dodge",width =0.35) +
  labs(title = "Number of trips in year for each user")

#Comparing the rides in each hours(morning,Noon,Eve and night)
data_bike_share %>% 
  group_by(start_hour) %>% 
  summarise(no_of_rides=n(),.groups = 'drop') %>% 
  ggplot(aes(x="",y=no_of_rides,fill=start_hour)) + geom_col(color="black") +
  geom_text(aes(label=no_of_rides),position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") + theme_void()+
  scale_fill_brewer()+
  labs(title = "Rides in each time of a day")

#No of rides in each timing for different user type
data_bike_share %>% 
  group_by(start_hour,user_type,bike_type) %>% 
  summarise(no_of_rides=n(),.groups = 'drop') %>% 
  ggplot(aes(x=start_hour,y=no_of_rides,fill=user_type)) +
  geom_col(position = "dodge") +
  labs(title = "No of rides in each timing for differnt user type")

  





