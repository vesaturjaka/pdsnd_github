# Reading the datasets
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Comment
head(ny)
head(wash)
head(chi)

# Load required libraries
library(dplyr)
library(ggplot2)

# Read the three datasets
ny <- read.csv('new_york_city.csv', stringsAsFactors = FALSE)
wash <- read.csv('washington.csv', stringsAsFactors = FALSE)
chi <- read.csv('chicago.csv', stringsAsFactors = FALSE)

# Add city column to each dataset
ny$City <- "New York"
wash$City <- "Washington"
chi$City <- "Chicago"

# Combine datasets
combined_data <- bind_rows(ny, wash, chi)

# Convert Start Time column to datetime with a specified format
combined_data$Start_Time <- as.POSIXct(combined_data$Start.Time, format = "%Y-%m-%d %H:%M:%S")

# Extract month from Start Time
combined_data$Month <- format(combined_data$Start_Time, "%m")

# Filter data for Chicago
chicago_data <- filter(combined_data, City == "Chicago")

# Count occurrences of each month
chicago_month_counts <- table(chicago_data$Month)

# Find the most common month
most_common_chicago_month <- names(chicago_month_counts)[which.max(chicago_month_counts)]
most_common_chicago_month


# Create a bar plot for month counts in Chicago
ggplot(data = data.frame(chicago_month_counts), aes(x = factor(names(chicago_month_counts)), y = chicago_month_counts)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Most Common Month in Chicago",
       x = "Month",
       y = "Frequency") +
  theme_minimal()


# The analysis reveals that the month of June has the highest frequency of bike trips among the 
# first six months of 2017 in Chicago.

# Filter data for New York
new_york_data <- filter(combined_data, City == "New York")

# Count occurrences of each start station
new_york_start_station_counts <- table(new_york_data$Start.Station)

# Find the most common start station
most_common_new_york_start_station <- names(new_york_start_station_counts)[which.max(new_york_start_station_counts)]
most_common_new_york_start_station

# Convert start station counts to data frame
new_york_start_station_df <- data.frame(Start_Station = names(new_york_start_station_counts),
                                        Frequency = as.vector(new_york_start_station_counts))

# Order start stations by frequency
new_york_start_station_df <- new_york_start_station_df[order(-new_york_start_station_df$Frequency), ]

# Limit to top 10 start stations
top_10_start_stations <- head(new_york_start_station_df, 10)

# Create a pie chart for top 10 start stations in New York
ggplot(data = top_10_start_stations, aes(x = "", y = Frequency, fill = Start_Station)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Top 10 Start Stations in New York",
       fill = "Start Station") +
  theme_void() +
  theme(legend.position = "right")


# The data indicates that the "Pershing Square North" station is the most frequently used starting point 
# for bike trips in New York City.

# Extract hour from Start Time
combined_data$Hour <- format(combined_data$Start_Time, "%H")

# Count occurrences of each hour
hour_counts <- table(combined_data$Hour)

# Find the most common hour
most_common_hour <- names(hour_counts)[which.max(hour_counts)]
most_common_hour

# Create a bar plot for hour counts
ggplot(data = data.frame(hour_counts), aes(x = factor(names(hour_counts)), y = hour_counts)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Most Common Hour of the Day",
       x = "Hour",
       y = "Frequency") +
  theme_minimal()


# The findings show that 8 AM is the most common hour of the day for bike trips, 
# suggesting a peak in bike usage during morning commuting hours.

system('python -m nbconvert Explore_bikeshare_data.ipynb')
