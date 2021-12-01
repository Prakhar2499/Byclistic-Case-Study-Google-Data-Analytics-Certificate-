library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(writexl)

d1 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202011-divvy-tripdata.csv')
d2 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202012-divvy-tripdata.csv')
d3 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202101-divvy-tripdata.csv')
d4 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202102-divvy-tripdata.csv')
d5 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202103-divvy-tripdata.csv')
d6 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202104-divvy-tripdata.csv')
d7 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202105-divvy-tripdata.csv')
d8 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202106-divvy-tripdata.csv')
d9 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202107-divvy-tripdata.csv')
d10 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202108-divvy-tripdata.csv')
d11 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202109-divvy-tripdata.csv')
d12 <- read.csv('C:/Users/Prakhar Shukla/Desktop/Case Study/Data/CSV/202110-divvy-tripdata.csv')

colnames(d1)
str(d1)

#Here on checking the structure of all the data frames, we find that the data type of start_station_id and end_station_id in d1 i.e in the file for November 2010 is 'int' while the data type in all the other files is 'chr'.
#So now I will change the data type of start_statino_id and end_station_id in d1 to 'chr'.

d1 <- d1 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

#Now that all the files have similar data types and number and name of columns. I will merge all the data frames into a new one, using the 'rbind' function.

new_d <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)

colnames(new_d)
str(new_d)

#Now for the analysis we don't need the start_lat, start_lng, end_lat, and end_lng columns so let's remove them.

new_d_2 <- new_d %>%
  select(-(start_lat:end_lng))


#Now we need to create a new column 'ride_length', the data for this column will be calculated by taking the difference of the data from 'ended_at' and 'started_at' column.

# Step - 1 :- Let's convert the data in the started_at and ended_at columns into formats suitable for subtraction.

new_d_2[['started_at']] = ymd_hms(new_d_2[['started_at']])
new_d_2[['ended_at']] = ymd_hms(new_d_2[['ended_at']])

#We can inspect the data by using new_d_2[['started_at']] and new_d_2[['ended_at']].

new_d_2[['started_at']]
new_d_2[['ended_at']]

# Step - 2 :- Now lets store the difference between the values of the 2 columns in a new column.

new_d_2 <- new_d_2 %>% mutate(ride_length = signif(difftime(ended_at,started_at, units = 'mins'),2))

#Now we will create the 'day_of_week' column to find out the days on which day the ride started.

new_d_2 = new_d_2 %>% mutate(day_of_week = weekdays(started_at))

#Now lets take the month column
new_d_2$month = months(new_d_2$started_at)

#Now let's take the year
new_d_2$year = year(new_d_2$started_at)

#Now let's remove the data where the ride length <= 0

new_d_3 <- new_d_2[!(new_d_2$ride_length <= 0),]

#Lets calculate the min, max and mean of ride_length

new_d_3 %>%
  summarise(min_rl = min(ride_length), max_rl = max(ride_length), mean_rl = mean(ride_length))
           
#Let's calculate the min, max, mean and median ride_length for members and casual riders.

aggregate(new_d_3$ride_length ~ new_d_3$member_casual, FUN=min)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual, FUN=max)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual, FUN=mean)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual, FUN=median)

#Let's calculate the min, max, mean and median ride_length for members and casual riders for each day_of_week.

#Step - 1 :- Let's correct the order of day_of_week

new_d_3$day_of_week <- ordered(new_d_3$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

#Step - 2 :- Let's use the aggregate function now.

aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$day_of_week, FUN=min)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$day_of_week, FUN=max)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$day_of_week, FUN=mean)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$day_of_week, FUN=median)

#Number of rides of casual and member riders for each day of week

new_d_3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(count_of_rides = n(), .groups = 'drop') %>%
  arrange(day_of_week)

#Let's calculate the min, max, mean and median ride_length for members and casual riders for each month.

#Step - 1 :- Now lets arrange the month column in order

new_d_3$month <- ordered(new_d_3$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#Let's apply the aggregate function

aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$month, FUN = min)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$month, FUN = max)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$month, FUN = mean)
aggregate(new_d_3$ride_length ~ new_d_3$member_casual + new_d_3$month, FUN = median)

#Number of rides of casual and member riders for each month

new_d_3 %>%
  group_by(member_casual,month) %>%
  summarise(count_of_rides = n(), .groups = 'drop') %>%
  arrange(month)

#Most popular stations among riders

new_d_3 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(count_of_rides = n(), .groups = 'drop') %>%
  filter(start_station_name != "") %>%
  arrange(-count_of_rides)

#Now let's start visualizing the results that we have obtained...
# 1.Average ride duration of casual riders is more than the riders who are members.

new_d_3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_time = mean(ride_length), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = average_ride_time, fill = member_casual)) + geom_bar(position = 'dodge', stat = 'identity', color = 'black') + scale_fill_manual(values=c("#999999", "#E69F00")) + theme_minimal()
  
new_d_3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = member_casual, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#2. Member riders like to ride more on Wednesday while the Casual riders prefer weekends.

new_d_3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(count_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = count_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_minimal()

#3. The peak time for casual riders is from July to August 

new_d_3 %>%
  group_by(member_casual, month) %>%
  summarise(count_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = month, y = count_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  scale_fill_manual(values=c("#999999", "#E69F00")) + 
  theme_minimal()

new_d_3 %>%
  group_by(member_casual, month) %>%
  summarise(duration_of_rides = mean(ride_length), .groups = 'drop') %>%
  ggplot(aes(x = month, y = duration_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  scale_fill_manual(values=c("#999999", "#E69F00")) + 
  theme_minimal()

#4.Classic bike is preferred by both the casual and member riders.

new_d_3 %>%
  group_by(rideable_type,member_casual) %>%
  summarise(count_of_bikes = n(), .groups = 'drop') %>%
  ggplot(aes(x=member_casual, y = count_of_bikes, fill = rideable_type)) + geom_bar(position = 'dodge', stat = 'identity')

#5. Both the number of casual and member riders peak from 4pm to 7pm.
a <- a[(a%%2)==0 && a<26]
new_d_3 %>%
  mutate(hour = hour(round_date(started_at, unit = "hour"))) %>%
  group_by(hour, member_casual) %>%
  summarise(count_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = hour, y = count_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  scale_x_continuous(breaks=a)

#6. List of top 20 stations with most casual members

new_d_3 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(count_of_ride = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual == 'casual') %>%
  arrange(-count_of_ride) %>%
  head(n=20) 


new_d_3 %>%
  group_by(member_casual) %>%
  summarise(count_of = n(), .groups = 'drop') %>%
  ggplot(aes(x = member_casual, y = count_of)) + geom_bar(position = 'dodge', stat = 'identity')



# Now let's export files for further analysis in Excel

# Weekly Stats
weekly_report <- new_d_3 %>% 
  group_by(member_casual, day_of_week) %>%  
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%    
  arrange(member_casual, day_of_week)
write_csv(weekly_report, "weekly_report.csv")

# Monthly Stats

monthly_report <- new_d_3 %>% 
  group_by(member_casual, month) %>%  
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%    
  arrange(member_casual, month)
write_csv(monthly_report, "monthly_report.csv")

# Stations Report
station_report <- new_d_3 %>% 
  mutate(station = start_station_name) %>%
  filter(start_station_name != "") %>% 
  group_by(start_station_name, member_casual) %>%  
  summarise(number_of_rides = n()) %>%    
  arrange(number_of_rides)
write_csv(station_report, "stations_report.csv")

write_csv(new_d_3, "final_data.csv")


