library(tidyverse) # data wragling 
library(janitor) # data cleaning
library(lubridate) # wragle data attributies 

library(ggplot2) # data vizz
library(dplyr) # count distinct
install.packages("ggmap")
install.packages("geosphere")
library(ggmap)
library(geosphere)


# data preprocessing

getwd()

file_list <- list.files(path = "/Users/nasraddingozalli/Desktop/Case Study 1/Bike_share/", pattern = "\\.csv$", full.names = TRUE)
all_rides <- lapply(file_list, read.csv)
all_rides <- bind_rows(all_rides)

colnames(all_rides)
head(all_rides)
str(all_rides)
summary(all_rides)

# Convert date/time columns to datetime objects
all_rides$started_at <- ymd_hms(all_rides$started_at)
all_rides$ended_at <- ymd_hms(all_rides$ended_at)


# adding date, month, year and day of week columns
all_rides_v1 <- all_rides %>% 
  mutate(year = format(as.Date(started_at), "%Y")) %>% # extreact year
  mutate(month = format(as.Date(started_at), "%B")) %>% # Extract month
  mutate(date = format(as.Date(started_at), "%d")) %>%  # extract date
  mutate(day_of_week = format(as.Date(started_at), "%A")) %>%  # extract days of week
  mutate(ride_length = as.numeric(difftime(ended_at, started_at))) %>% 
  mutate(start_time = strftime(started_at,format = "%H"))


str(all_rides_v1)
is.numeric(all_rides_v1$ride_length)

#adding ride distance in km
all_rides_v1$ride_distance <- distGeo(matrix(c(all_rides_v1$start_lng, all_rides_v1$start_lat), ncol = 2),
                                   matrix(c(all_rides_v1$end_lng, all_rides_v1$end_lat), ncol = 2))

all_rides_v1$ride_distance <- all_rides_v1$ride_distance/1000 # distance in km
View(all_rides_v1)

# remove bad data
# the dataframe includes a few hundreds of entries when bikes were taken out of docks
# Clean the entries where the `ride_length` is smaller or equal to zero
all_rides_v1 <- na.omit(all_rides_v1)
all_rides_v1 <- all_rides_v1[!(all_rides_v1$ride_length<= 0),]

# Convert member_casual to a factor
all_rides_v1$member_casual <- as.factor(all_rides_v1$member_casual)


# Check for and remove duplicate rows (if any)
all_rides_v1 <- distinct(all_rides_v1)


# Analyse. Now data is ready to be explored
str(all_rides_v1)

summary(all_rides_v1)

# Conduct a descriptive analytics

all_rides_v1 %>%
  group_by(member_casual) %>% 
  summarise(
    average_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    std_ride_length = sd(ride_length),
    max_ride_length = max(ride_length),
    min_ride_length = min(ride_length)
  )

all_rides_v1 %>% 
  group_by(member_casual) %>% 
  summarise(
    avg_ride_dist = mean(ride_distance),
    median_ride_dist = median(ride_distance),
    std_ride_dist = sd(ride_distance),
    max_ride_dist = max(ride_distance),
    min_rode_dist = min(ride_distance)
  )
library(stats)

# Assuming you've already segmented the data into two groups: annual_members and casual_riders

# Perform a t-test for ride_distance
t_test_result <- t.test(
  all_rides_v1$ride_distance ~ all_rides_v1$member_casual,
  alternative = "two.sided"  # Use "two.sided" for a two-tailed test
)

# Print the t-test results
print(t_test_result)


# comparing members and casual riders


all_rides_v1 %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id), ride_percentage = (length(ride_id)/
                                                               nrow(all_rides_v1)*100))

ggplot(all_rides_v1, aes(x = member_casual, fill= member_casual))+ geom_bar() + labs(x = "Casual vs Members", y = "Number of rides", title = "Casuals vs Members distributions")


# comparison between Members and Casuals depending on ride length

all_rides_v1 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_length = mean(ride_length), median_ride_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))

# see total ride and average ride times by each day for members and casuals
all_rides_v1$day_of_week <- ordered(all_rides_v1$day_of_week, 
                            levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
"Thursday", "Friday", "Saturday"))

all_rides_v1 %>% 
  group_by(member_casual, day_of_week) %>% # groups by member_casuals
  summarise(number_of_rides = n(), average_ride_length = mean(ride_length), .groups = "drop") %>% 
  arrange(member_casual, day_of_week)


all_rides_v1 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(
    title = "Total rides by Members and casual rides Vs. Day of week",
    y = "number of rides"
    )+
  scale_y_continuous(labels = scales:: comma)


all_rides_v1 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(avg_ride_length = mean(ride_length), .groups = "drop") %>% 
  ggplot(aes(x = day_of_week, y = avg_ride_length, fill = member_casual))+
  geom_col(width = 0.5, position = position_dodge(width = 0.5))+
  labs(title = "Average ride time by Memebers and casual rides Vs Day of week")


all_rides_v1 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(avg_ride_dist = mean(ride_distance), .groups = "drop") %>% 
  ggplot(aes(x = day_of_week, y = avg_ride_dist, fill = member_casual))+
  geom_col(width = 0.5, position = position_dodge(width = 0.5))+
  labs(title = "Average ride distance by Memebers and Casual rides vs Day of week")


all_rides_v1$month <- ordered(all_rides_v1$month, 
                              levels = c("January", "February", "March", "April",
               "May", "June", "July", "August", "September","October", "November", "December"))

ride_lenght_month<- all_rides_v1 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), avg_ride_length = mean(ride_length), .groups = "drop") %>% 
  arrange(member_casual, month)
view(ride_lenght_month)

all_rides_v1 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual))+
  labs(title = "Number of rides by Members and Casual riders Vs. Month", x = "Month", y= "Number of month")+
  theme(axis.text.x = element_text(angle = 45))+
  geom_col(width = 0.5, position = position_dodge(width = 0.5))+
  scale_y_continuous(labels = scales :: comma)

all_rides_v1 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), avg_ride_length = mean(ride_length), .groups = "drop") %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = avg_ride_length, fill = member_casual))+
  labs(title = "Average ride length by Members and Casual rides Vs. Month", x = "Month", y = "Average ride length")+
  theme(axis.text.x = element_text(angle = 45))+
  geom_col(width = 0.5, position = position_dodge(width = 0.5))+
  scale_y_continuous(labels = scales :: comma)

all_rides_v1 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_dist = mean(ride_distance)) %>% 
  ggplot()+
  geom_col(mapping = aes(x = member_casual, y = avg_ride_dist, fill = member_casual), 
           show.legend = FALSE)+
  labs(title = "Mean travel distance by Memebers and Casual rides", x = "Member and Casual rides", y = "Average distance in km")

all_rides_v1 %>% 
  ggplot(aes(start_time, fill = member_casual))+
  labs(x= "Hour of the day", title = "Cyclistic's bike demand by hour in a day")+
  geom_bar(width = 0.7, position = position_dodge(width = 0.7))

all_rides_v1 %>% 
  ggplot(aes(start_time, fill= member_casual))+
  geom_bar()+
  labs(x = " Hour of the day", title = "Cyclictic's bike demand per hour by day of the 
       week")+
  facet_wrap(~day_of_week)

all_rides_v1 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(count = length(ride_id))

ggplot(all_rides_v1, aes(x = rideable_type, fill = member_casual))+
  labs(x = "Rideable type", title = "Rudable typpe Vs. total rides by memebr and casual")+
  geom_bar(width = 0.5, position = position_dodge(width = 0.5))

coordinates_df <- all_rides_v1 %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>% 
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>% 
  summarise(total_rides = n(), .groups = "drop") %>% 
  filter(total_rides > 200)
view(coordinates_df)

casual_riders <- coordinates_df %>%  
  filter(member_casual == "casual")
member_riders <- coordinates_df %>% 
  filter(member_casual == "member")

#lets setup ggmap and story map of chicago 

chicago <- c(left = -87.700424, buttom = 41.790769, right = -87.554855, top = 41.990119)
chicago_map <- get_stamenmap(bbox = c(left = -87.700424, bottom = 41.790769, right = -87.554855, top = 41.990119), zoom = 12, maptype = "terrain")

#vizualization on the map

#casual
ggmap(chicago_map, darken = c(0.1, "white")) +
geom_point(data = casual_riders, aes(x = start_lng, y = start_lat, color = rideable_type), 
           size = 2) +
coord_fixed(0.8) +
labs(title = "Most used routes by Casual riders", x = NULL, y = NULL) +
theme(legend.position = "none")


ggmap(chicago_map, darken = c(0.1, "white"))+
  geom_point(data = member_riders, aes(x = start_lng, y = start_lat, color = rideable_type), 
             size = 2)+
coord_fixed(0.8)+
labs(title = "Most used routes by Member riders", x = NULL, y = NULL) +
theme(legend.position = "none")
  
  