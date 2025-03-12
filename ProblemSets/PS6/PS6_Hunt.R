#Calling Packages for data cleaning and visualization
library(tibble)
library(dplyr)
library(ggplot2)
library(maps)
library(scales)

#Loading our dataset, This requires you to download the latest overdose dataset from the CDC Website
df <- as_tibble(VSRR_Provisional_Drug_Overdose_Death_Counts)

#Selecting only useful columns for our analysis
df <- df %>%
  select(Year, State, Month, Indicator, Data.Value)

#viewing the various names in the "Indicator" column
unique_values <- unique(df$Indicator)
print(unique_values)

#removing commas and filtering our overdose # column to be numeric.
df$Data.Value <- as.numeric(gsub(",", "", df$Data.Value))

#we now split this dataset into multiple datasets useful for different purposes
#First, lets create a dataframe that contains the total number of overdose deaths by state
df_by_state <- df %>%
  group_by(Year, State, Indicator) %>%
  summarise(Total_Data_Value = sum(Data.Value, na.rm = TRUE)) %>%
  filter(Indicator == "Number of Drug Overdose Deaths") %>%
  select(Year, State, Total_Data_Value)

#Next, lets create a dataframe that contains the total number of overdose deaths by month
df_by_month <- df %>%
  group_by(Year, Month, Indicator) %>%
  summarise(Total_Data_Value = sum(Data.Value, na.rm = TRUE)) %>%
  filter(Indicator == "Number of Drug Overdose Deaths") %>%
  select(Year, Month, Total_Data_Value)

#finally, lets create a dataframe that contains the total number of overdose deaths by drug type
df_drugtype <- df %>%
  group_by(Year, Indicator) %>%
  summarise(Total_Overdoses = sum(Data.Value, na.rm = TRUE), .groups = "drop") %>%
  filter(!Indicator %in% c("Number of Drug Overdose Deaths", 
                           "Percent with drugs specified", 
                           "Number of Deaths", 
                           "Natural & semi-synthetic opioids, incl. methadone (T40.2, T40.3)",
                           "Natural, semi-synthetic, & synthetic opioids, incl. methadone (T40.2-T40.4)"))

#We have now successfully cleaned our data and created three easy to work with dataframes.
#We will now proceed to create our visualizations

###Making visualization 1
#Our first visualization will be a geographic chart showing total overdoses per state over the years

#We must first create a key in order to merge our state abbreviation data with the region data we use to create our maps
state_abbr_to_name <- c(
  "AK" = "alaska", "AL" = "alabama", "AR" = "arkansas", "AZ" = "arizona", "CA" = "california",
  "CO" = "colorado", "CT" = "connecticut", "DC" = "district of columbia", "DE" = "delaware",
  "FL" = "florida", "GA" = "georgia", "HI" = "hawaii", "ID" = "idaho", "IL" = "illinois",
  "IN" = "indiana", "IA" = "iowa", "KS" = "kansas", "KY" = "kentucky", "LA" = "louisiana",
  "MA" = "massachusetts", "MD" = "maryland", "ME" = "maine", "MI" = "michigan", "MN" = "minnesota",
  "MO" = "missouri", "MS" = "mississippi", "MT" = "montana", "NC" = "north carolina", "ND" = "north dakota",
  "NE" = "nebraska", "NH" = "new hampshire", "NJ" = "new jersey", "NM" = "new mexico", "NY" = "new york",
  "OH" = "ohio", "OK" = "oklahoma", "OR" = "oregon", "PA" = "pennsylvania", "RI" = "rhode island", "NV" = "nevada",
  "SC" = "south carolina", "SD" = "south dakota", "TN" = "tennessee", "TX" = "texas", "UT" = "utah",
  "VA" = "virginia", "VT" = "vermont", "WA" = "washington", "WI" = "wisconsin", "WV" = "west virginia",
  "WY" = "wyoming"
)

#Adding the full state names using the key in order to connect the data to the map
valid_abbr <- names(state_abbr_to_name)
df_by_state <- df_by_state %>%
  filter(toupper(State) %in% valid_abbr)
df_by_state$state_full <- state_abbr_to_name[toupper(df_by_state$State)]

#Getting map data for the U.S. states
us_states <- map_data("state")

#Merging our data with the map data by full state name
merged_data <- us_states %>%
  left_join(df_by_state, by = c("region" = "state_full")) %>%
  filter(!is.na(Year))

#Finally we can create our maps using ggplot
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = Total_Data_Value)) +
  geom_polygon(color = "black") +
  coord_fixed(1.1) +
  theme_void() +
  scale_fill_gradientn(colors = c("white", "yellow", "red")) +
  labs(fill = "# of Drug Overdose Deaths", title = "The Number of Drug Overdose Deaths by State") +
  facet_wrap(~ Year)


###Making visualization 2
#Our second visualization will be a line graph showing total overdose deaths per year by drug type
ggplot(df_drugtype, aes(x = Year, y = Total_Overdoses / 1e6, color = Indicator, group = Indicator)) +
  geom_line(alpha = 0.6, linewidth = 1) + 
  geom_point(alpha = 0.7) +  
  scale_x_continuous(breaks = unique(df_drugtype$Year)) +  
  labs(title = "Total Overdose Deaths by Drug Type", x = "Year", y = "Total Overdose Deaths (in millions)", color = "Drug Type") +
  scale_color_viridis_d(option = "turbo") +  
  theme_minimal() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))


###Making visualization 3
#Our third visualization will be a stacked area plot showing total overdose deaths by year and month
#First we must do some work to reorder the months as well as drop all years without 12 months of data
df_by_month$Month <- factor(df_by_month$Month, levels = month.name) 
df_grouped_month <- df_by_month%>%
  group_by(Year) %>%
  filter(n_distinct(Month) == 12) %>%
  ungroup()

#We also must convert the year to a numeric value in order to plot it easier
df_grouped_month$Year <- as.numeric(df_grouped_month$Year)
years <- unique(df_grouped_month$Year)

#We can now plot the Stacked Area Plot in ggplot
ggplot(df_grouped_month, aes(x = Year, y = Total_Data_Value, fill = Month)) +
  geom_area() +  # Stacked area plot
  scale_fill_viridis_d(option = "turbo") +  
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  
  scale_x_continuous(breaks = years) +
  labs(title = "Stacked Area Plot of Overdose Deaths by Year and Month", 
       x = "Year", 
       y = "Total Overdose Deaths (in millions)",
       fill = "Month") +  
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(size = 10))

