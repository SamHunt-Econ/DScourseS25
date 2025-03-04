#Part 1: Calling the API and getting the data
#Calling Packages
library(httr)
library(xml2)
library(jsonlite)

#You must enter your own API key from holidayapi.com
api_key <- "API KEY"
url <- "https://holidayapi.com/v1/holidays"

# Set query parameters (adjust as necessary)
params <- list(
  country = "US",  # Country parameter
  year = 2024,     # Year parameter
  pretty = TRUE,   # Optional pretty print parameter
  key = api_key    # Your API key
)

# Make the GET request
response <- GET(url, query = params)
data <- content(response, as = "text")

#Get the data into dataframe format
holiday_data <- fromJSON(data)
holiday_df <- as.data.frame(holiday_data$holidays)

#Part 2: Now that we have the data we will create a fun graph using that data
#Calling packages
library(lubridate)
library(ggplot2)
library(tidyverse)

#Seperating a months column out of the date column
holiday_df$Date <- ymd(holiday_df$date)
holiday_df$Month <- month(holiday_df$Date, label = TRUE)

#Counting the number of public and non-public holidays by month
holiday_public_month_count <- holiday_df %>%
  filter(public == TRUE) %>%  # Filter for public holidays
  group_by(Month) %>%
  summarise(Number_of_Public_Holidays = n())
holiday_public_month_count
holiday_nonpublic_month_count <- holiday_df %>%
  filter(public == FALSE) %>%  # Filter for nonpublic holidays
  group_by(Month) %>%
  summarise(Number_of_NonPublic_Holidays = n())
holiday_nonpublic_month_count

# Adding a column to distinguish between public and non-public holidays
holiday_public_month_count$Holiday_Type <- "Public"
holiday_nonpublic_month_count$Holiday_Type <- "Non-Public"
colnames(holiday_public_month_count)[2] <- "Number_of_Holidays"
colnames(holiday_nonpublic_month_count)[2] <- "Number_of_Holidays"

# Combine the two datasets
combined_holiday_data <- bind_rows(holiday_public_month_count, holiday_nonpublic_month_count)

# Create the ggplot with stacked bars
ggplot(combined_holiday_data, aes(x = Month, y = Number_of_Holidays, fill = Holiday_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  scale_fill_manual(values = c("skyblue", "purple")) +  # Set colors for public and non-public holidays
  labs(title = "Number of Public and Non-Public Holidays in the U.S. by Month",
       x = "Month",
       y = "Number of Holidays",
       fill = "Holiday Type") +  # Label for the legend
  theme_minimal()









