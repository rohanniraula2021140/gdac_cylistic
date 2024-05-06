df1 <- read.csv('gca/202304-divvy-tripdata.csv')
df2 <- read.csv('gca/202305-6divvy-tripdata.csv')
df3 <- read.csv('gca/202306-divvy-tripdata.csv')
df4 <- read.csv('gca/202307-divvy-tripdata.csv')
df5 <- read.csv('gca/202308-divvy-tripdata.csv')
df6 <- read.csv('gca/202309-divvy-tripdata.csv')
df7 <- read.csv('gca/202310-divvy-tripdata.csv')
df8 <- read.csv('gca/202311-divvy-tripdata.csv')
df9 <- read.csv('gca/202312-divvy-tripdata.csv')
df10 <- read.csv('gca/202401-divvy-tripdata.csv')
df11 <- read.csv('gca/202402-divvy-tripdata.csv')
df12 <- read.csv('gca/202402-divvy-tripdata.csv')


# Checking for consistent column Names
check_consistent_column_name <- function(file_paths) {
  # Read the first csv file to get its column names
  first_df <- read.csv(file_paths[1])
  colnames_first <- colnames(first_df)
  
  # Initialize a vector to store file names with inconsistent column names
  inconsistent_files <- character(0)
  
  # Loop through the remaining csv files and compare column names
  for (i in 2:length(file_paths)) {
    df <- read.csv(file_paths[i])
    colnames_df <- colnames(df)
    if (!identical(colnames_first, colnames_df)) {
      inconsistent_files <- c(inconsistent_files, file_paths[i])
    }
  }
  
  # Print Result
  if (length(inconsistent_files) == 0) {
    print("All CSV files have consistent column names")
  } else {
    print("CSV files with inconsistent column names")
    print(inconsistent_files)
  }
}


file_paths <- c('gca/202304-divvy-tripdata.csv' ,
                'gca/202305-divvy-tripdata.csv' ,
                'gca/202306-divvy-tripdata.csv' ,
                'gca/202307-divvy-tripdata.csv' ,
                'gca/202308-divvy-tripdata.csv' ,
                'gca/202309-divvy-tripdata.csv' ,
                'gca/202310-divvy-tripdata.csv' ,
                'gca/202311-divvy-tripdata.csv' ,
                'gca/202312-divvy-tripdata.csv' ,
                'gca/202401-divvy-tripdata.csv' ,
                'gca/202402-divvy-tripdata.csv' ,
                'gca/202402-divvy-tripdata.csv' )
check_consistent_column_name(file_paths)

############################################################################################
# Merging all the the twelve files into one. 

# List of the file paths already given

# Initialize an empty list to store the data frames
dfs <- list()

# Read each csv files and store the data frames in the list 
for(file_path in file_paths) {
  df <- read.csv(file_path)
  dfs <- c(dfs, list(df))
}

# Merge the data frames by adding row
merged_df <- do.call(rbind, dfs)

# Viewing the final result
View(merged_df)

## Removingthe dfs
rm(dfs)

############################################################################################

# Clean up data and Add some column for data analysis

# Inspecting the newly formed df
colnames(merged_df)
nrow(merged_df)
dim(merged_df)
head(merged_df)

#Loading the library tidy verse
library(tidyverse)

# dupplicating the original data source as we will be mutating it.
df <- merged_df

# Convert timestamps to POSIXct objects
df <- df %>%
  mutate(
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at)
  )

# Calculate ride length in seconds
df <- df %>%
  mutate(
    ride_length_seconds = as.numeric(difftime(ended_at, started_at, units = "secs"))
  )

# Calculate ride length in minutes
df <- df %>%
  mutate(
    ride_length = ifelse(ride_length_seconds < 60, 1, round(ride_length_seconds / 60, 2))
  )


View(df)


# Extract hour of the day
df <- df %>%
  mutate(
    hour = format(started_at, "%H")
  )

# Extract day of the week
df <- df %>%
  mutate(
    day_of_week = weekdays(started_at)
  )

# Derive season based on month
df <- df %>%
  mutate(
    season = ifelse(month(started_at) %in% c(12, 1, 2), "Winter",
                    ifelse(month(started_at) %in% c(3, 4, 5), "Spring",
                           ifelse(month(started_at) %in% c(6, 7, 8), "Summer", "Fall")))
  )

# Extract day of the month
df <- df %>%
  mutate(
    day_of_month = format(started_at, "%d")
  )

# Weekend vs. Weekday
df <- df %>%
  mutate(
    weekend = ifelse(weekdays(started_at) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  )

# Morning vs. Afternoon vs. Evening vs. Night
df <- df %>%
  mutate(
    time_of_day = cut(as.numeric(format(started_at, "%H")), breaks = c(0, 6, 12, 18, 24),
                      labels = c("Night", "Morning", "Afternoon", "Evening"), include.lowest = TRUE)
  )

# Month
df <- df %>%
  mutate(
    month = format(started_at, "%m")
  )

# Quarter
df <- df %>%
  mutate(
    quarter = as.character(quarters(started_at))
  )

# Day-Night Indicator
df <- df %>%
  mutate(
    day_night_indicator = ifelse(hour(started_at) >= 6 & hour(started_at) < 18, "Day", "Night")
  )

# Print the resulting data frame
print(df)


# Exporting the data to the csv so that it would be easy to analyze later on
# Assuming your data frame is named 'df' and you want to save it as 'data.csv'
write.csv(df, file = "gca/2023-04-da_v1.csv", row.names = FALSE)


###########################################################################################
# Creating Visualizations
colnames(df)

# Importing the library
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the CSV file into a data frame
df <- read.csv("gca/2023-24-da_v1.csv")


# Ride Patterns by The time of day
ggplot(df, aes(x=hour, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(title="Ride Patterns by the Time of day for Whole Year",
       x = "Hour of the day",
       y = "Number of Rides",
       fill= "User Type") +
  theme_minimal() 

# Ride Patterns by the time of day in a season
ggplot(df, aes(x=hour, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(title="Ride Patterns by the Time of day in Seasons",
       x = "Hour of the day",
       y = "Number of Rides",
       fill= "User Type") +
  theme_minimal() +
  facet_wrap(~season)

# Ride Patterns by the time of the day in the day of week
ggplot(df, aes(x=hour, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(title="Ride Patterns by the Time of day in the day of week",
       x = "Hour of the day",
       y = "Number of Rides",
       fill= "User Type") +
  theme_minimal() +
  facet_wrap(~day_of_week)

# Ride patterns by time of the day in  {particular day} in all season
ggplot(filter(df, day_of_week == 'Sunday'), aes(x=hour, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(title="Ride Patterns by the Time of day",
       x = "Hour of the day",
       y = "Number of Rides",
       fill= "User Type") +
  theme_minimal() +
  facet_wrap(day_of_week~season)

# Ride patterns by time of the day in months
ggplot(df, aes(x=hour, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(title="Ride Patterns by the Month",
       x = "Hour of the day",
       y = "Number of Rides",
       fill= "User Type") +
  theme_minimal() +
  facet_wrap(~df$month)


# Ride patterns by time of the day in  {particular month}
ggplot(filter(df, month == "01"), aes(x=hour, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(title="Ride Patterns by the Time of day",
       x = "Hour of the day",
       y = "Number of Rides",
       fill= "User Type") +
  theme_minimal() +
  facet_wrap(~month)


  # Ride Patterns by Day of the Week
ggplot(df, aes(x = reorder(day_of_week, -factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Ride Patterns by Day of the Week",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

  # Ride Patterns by Day of the Week in season
ggplot(df, aes(x = reorder(day_of_week, -factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Ride Patterns by Day of the Week in All Season",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~season)

  # Ride Patterns by Day of the Week in all Months
ggplot(df, aes(x = reorder(day_of_week, -factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Ride Patterns by Day of the Week in All Month",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~month)


# Seasonal Ride Trends
ggplot(df, aes(x = season, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Seasonal Ride Trends",
       x = "Season",
       y= "Number of Rides" ,
       fill = "User Type") +
  theme_minimal()

# Average Ride Length = Not good
ggplot(df, aes(x="", y=ride_length, fill = member_casual)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +  # Add jittered points
  labs(title="Average Ride Length",
       x="", 
       y="Ride Length (minutes)",
       fill="User Type") + 
  theme_minimal()


# Ride Patterns by Month/Quarter / Replace the month with name
ggplot(df, aes(x=month, fill = member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Ride Patterns by Month",
       x="Month", 
       y="Number of Rides",
       fill="User Type") + 
  theme_minimal()

ggplot(df, aes(x=quarter, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Ride Patterns by Quarter",
       x="Quarter",
       y="Number of Rides",
       fill="User Type") + 
  theme_minimal()

# Day Night Ride Patterns
ggplot(df, aes(x=day_night_indicator, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Day-Night Ride Patterns",
       x = "Time of Day",
       y= "Number of Day", 
       fill = "User Message") +
  theme_minimal()

# Day Night Ride Patterns in day of week in whole Year
ggplot(df, aes(x=day_night_indicator, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Day-Night Ride Patterns in different day of week in Whole Year",
       x = "Time of Day",
       y= "Number of Day", 
       fill = "User Message") +
  theme_minimal() + 
  facet_wrap(~day_of_week)

# Day Night Ride Patterns in different month of year
ggplot(df, aes(x=day_night_indicator, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Day-Night Ride Patterns in different month of year",
       x = "Time of Day",
       y= "Number of Day", 
       fill = "User Message") +
  theme_minimal() + 
  facet_wrap(~month)

# Day Night Ride Patterns in different seasons
ggplot(df, aes(x=day_night_indicator, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Day-Night Ride Patterns in different seasons",
       x = "Time of Day",
       y= "Number of Day", 
       fill = "User Message") +
  theme_minimal() + 
  facet_wrap(~season)

# Day Night Ride Patterns in day_in_week for particular month or season
ggplot(filter(df, month == '01'), aes(x=day_night_indicator, fill=member_casual)) +
  geom_bar(position="dodge") +
  labs(title="Day-Night Ride Patterns for all days in Jan",
       x = "Time of Day",
       y= "Number of Day", 
       fill = "User Message") +
  theme_minimal() +
  facet_wrap(month~day_of_week)


# Create a grouped bar plot to visualize the mean ride length for each day/night category and user type
df %>%
  group_by(member_casual, day_night_indicator) %>%
  summarise(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = day_night_indicator)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Ride Length by Day/Night and User Type",
       x = "Member Casual",
       y = "Mean Ride Length (minutes)",
       fill = "Day/Night") +
  theme_minimal()

# Create a grouped bar plot to visualize the mean ride length for each sesaon category and user type
df %>%
  group_by(member_casual, season) %>%
  summarise(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = season)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Ride Length by Season and User Type",
       x = "User Type",
       y = "Mean Ride Length (minutes)",
       fill = "Seasons") +
  theme_minimal()

# Create a grouped bar plot to visualize the mean ride length for each month category and user type
df %>%
  group_by(member_casual, month) %>%
  summarise(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = month)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Ride Length by Month and User Type",
       x = "User Type",
       y = "Mean Ride Length (minutes)",
       fill = "Month") +
  theme_minimal()

# Create a grouped bar plot to visualize the mean ride length for each quartile category and user type
df %>%
  group_by(member_casual, quarter) %>%
  summarise(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = quarter)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Ride Length by Quartile and User Type",
       x = "User Type",
       y = "Mean Ride Length (minutes)",
       fill = "Quarters") +
  theme_minimal()

# Create a grouped bar plot to visualize the mean ride length for each hour category and user type
df %>%
  group_by(member_casual, hour) %>%
  summarise(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = hour)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Ride Length by Quartile and User Type",
       x = "User Type",
       y = "Mean Ride Length (minutes)",
       fill = "hour") +
  theme_minimal()

# Create a grouped bar plot to visualize the mean ride length for each day_of_week category and user type
df %>%
  group_by(member_casual, day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  summarise(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = day_of_week)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Ride Length by Quartile and User Type",
       x = "User Type",
       y = "Mean Ride Length (minutes)",
       fill = "day of week") +
  theme_minimal()

# Create a grouped bar plot to visualize the mean ride length for each weekends vs weekday category and user type
df %>%
  group_by(member_casual, weekend) %>%
  summarise(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = weekend)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Ride Length by Quartile and User Type",
       x = "User Type",
       y = "Mean Ride Length (minutes)",
       fill = "Weekends") +
  theme_minimal()


##################################################################################
# Some Calculations
summary(df$ride_length)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.00     5.50     9.63    18.47    17.07 98489.07 

# Compare members and casual members
aggregate(df$ride_length ~ df$member_casual, FUN = mean)
# df$member_casual df$ride_length
#  1           casual       28.45655
#  2           member       12.89464
aggregate(df$ride_length ~ df$member_casual, FUN = median)
#   df$member_casual df$ride_length
# 1           casual          11.97
# 2           member           8.62
aggregate(df$ride_length ~ df$member_casual, FUN = max)
# df$member_casual df$ride_length
# 1           casual       98489.07
# 2           member        1500.52
aggregate(df$ride_length ~ df$member_casual, FUN=min)
# df$member_casual df$ride_length
# 1           casual              1
# 2           member              1

# See the average ride time by each day for members vs casual users
aggregate(df$ride_length ~ df$member_casual + df$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Lets fix that
df$day_of_week <- ordered(df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, lets run the average ride time by each day for members vs casual users
aggregate(df$ride_length ~ df$member_casual + df$day_of_week, FUN = mean)



rm(dfs)
rm(merged_df)
