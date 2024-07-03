install.packages("ggplot2")
library(ggplot2)
library(dplyr)
getwd()
setwd("C:/Users/debaj/Desktop/data_population")
getwd()

# Read the CSV file
data <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_23.csv", skip = 4, check.names = FALSE, fill = TRUE)
head(data)
str(data)

# Read the CSV file
metadata <- read.csv("Metadata_Indicator_API_SP.POP.TOTL_DS2_en_csv_v2_23.csv", check.names = FALSE, fill = TRUE)

# Verify the data
str(metadata)
head(metadata)
# Read the CSV file
metadata_country <- read.csv("Metadata_Country_API_SP.POP.TOTL_DS2_en_csv_v2_23.csv", check.names = FALSE, fill = TRUE)

# Verify the data
str(metadata_country)
head(metadata_country)

head(data)
# Get the column names
column_names <- colnames(data)

# Print the column names
print(column_names)
# Remove columns with all NA values
data <- data %>%
  select(where(~!all(is.na(.))))
column_names <- colnames(data)
print(column_names)

# Get the number of rows
num_rows <- nrow(data)
print(paste("Number of rows:", num_rows))

# Get the unique country names
country_names <- unique(data$`Country Name`)
print("Different country names:")
print(country_names)


#------------------------------------------------------------------------

#To create a bar plot of the population in a specific year (e.g., 2020) for all countries
# Bar plot for population in 2020
ggplot(data, aes(x = `Country Name`, y = `2020`)) +
  geom_bar(stat = "identity") +
  labs(title = "Population by Country in 2020", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter out rows with NA values in '2020'
clean_data <- data[complete.cases(data$`2020`), ]

# Sort the data by population in 2020 and select the top 10 countries
top_10_countries <- clean_data %>%
  arrange(desc(`2020`)) %>%
  head(10)

# Bar plot for the top 10 countries by population in 2020
ggplot(top_10_countries, aes(x = reorder(`Country Name`, `2020`), y = `2020`)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Countries by Population in 2020", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Boxplot of Population in 2020

# Boxplot of population in 2020
ggplot(data, aes(y = `2020`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Population in 2020", y = "Population")
#Scatter Plot of Population Growth from 1960 to 2020
#Create a scatter plot to visualize the population growth from 1960 to 2020 for the top 10 countries:
# Scatter plot of population growth from 1960 to 2020 for top 10 countries
ggplot(top_10_countries, aes(x = `Country Name`, y = `2020` - `1960`)) +
  geom_point() +
  labs(title = "Population Growth from 1960 to 2020", x = "Country", y = "Population Growth") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Histogram of population in 2020
ggplot(clean_data, aes(x = `2020`)) +
  geom_histogram(binwidth = 100000000, fill = "blue", color = "black") +
  labs(title = "Histogram of Population in 2020", x = "Population", y = "Frequency")

# Filter for East Asia & Pacific countries (example: assuming specific countries)
east_asia_pacific_countries <- c("China", "Japan", "Korea, Rep.", "Indonesia", "Viet Nam")

# Filter data for these countries
east_asia_pacific_data <- clean_data %>%
  filter(`Country Name` %in% east_asia_pacific_countries)

# Sort the data by population in 2020 and select the top 10 countries
top_10_east_asia_pacific <- east_asia_pacific_data %>%
  arrange(desc(`2020`)) %>%
  head(10)

# Bar plot of top 10 countries in East Asia & Pacific by population in 2020
ggplot(top_10_east_asia_pacific, aes(x = reorder(`Country Name`, `2020`), y = `2020`)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Countries in East Asia & Pacific by Population in 2020", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter data for these countries
east_asia_pacific_data <- clean_data %>%
  filter(`Country Name` %in% east_asia_pacific_countries)

# Sort the data by population in 2010 and select the top 10 countries
top_10_east_asia_pacific_2010 <- east_asia_pacific_data %>%
  arrange(desc(`2010`)) %>%
  head(10)

# Bar plot of top 10 countries in East Asia & Pacific by population in 2010
ggplot(top_10_east_asia_pacific_2010, aes(x = reorder(`Country Name`, `2010`), y = `2010`)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Countries in East Asia & Pacific by Population in 2010", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter data for these countries
east_asia_pacific_data <- clean_data %>%
  filter(`Country Name` %in% east_asia_pacific_countries)

# Sort the data by population in 1980 and select the top 10 countries
top_10_east_asia_pacific_1980 <- east_asia_pacific_data %>%
  arrange(desc(`1980`)) %>%
  head(10)

# Bar plot of top 10 countries in East Asia & Pacific by population in 1980
ggplot(top_10_east_asia_pacific_1980, aes(x = reorder(`Country Name`, `1980`), y = `1980`)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Countries in East Asia & Pacific by Population in 1980", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter out rows with NA values in '2023'
clean_data <- clean_data[complete.cases(clean_data$`2023`), ]


# Sort the data by population in 2023 in ascending order and select the first 10 countries
min_10_countries <- clean_data %>%
  arrange(`2023`) %>%
  head(10)

# Bar plot for the countries with minimum population in 2023
ggplot(min_10_countries, aes(x = reorder(`Country Name`, `2023`), y = `2023`)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Countries with Minimum Population in 2023", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
library(tidyr)

# Assuming your dataset is named 'clean_data'

# Filter data for India
india_data <- clean_data %>%
  filter(`Country Name` == "India") %>%
  select(starts_with("20"))  # Selecting columns from 2020 onwards

# Reshape data to long format
india_data_long <- india_data %>%
  pivot_longer(cols = everything(), 
               names_to = "Year", 
               values_to = "Population") %>%
  mutate(Year = as.numeric(Year))  # Convert Year column to numeric (optional)

# Plot time series for India
ggplot(india_data_long, aes(x = Year, y = Population)) +
  geom_line(color = "blue") +
  labs(title = "Population Time Series of India", x = "Year", y = "Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Filter and arrange data for top 15 countries by population in 2023
top_15_countries_2023 <- clean_data %>%
  filter(!is.na(`2023`)) %>%
  arrange(desc(`2023`)) %>%
  head(15)

# Plotting the pie chart
ggplot(top_15_countries_2023, aes(x = "", y = `2023`, fill = `Country Name`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Top 15 Countries by Population in 2023", fill = "Country") +
  theme_void() +
  theme(legend.position = "right")
data$`Country Name`
# Assuming you have a dataset named 'data' with a column 'Country Name' and population data for different years

# Filter data for countries in Asia
asia_data <- data %>%
  filter(`Country Name` %in% c(
    "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan",
    "Brunei Darussalam", "Cambodia", "China", "Georgia", "India", "Indonesia",
    "Iran, Islamic Rep.", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan",
    "Korea, Rep.", "Kuwait", "Kyrgyz Republic", "Lao PDR", "Lebanon", "Malaysia",
    "Maldives", "Mongolia", "Myanmar", "Nepal", "Oman", "Pakistan", "Palestine",
    "Philippines", "Qatar", "Saudi Arabia", "Singapore", "Sri Lanka", "Syrian Arab Republic",
    "Tajikistan", "Thailand", "Timor-Leste", "Turkmenistan", "United Arab Emirates",
    "Uzbekistan", "Viet Nam", "Yemen, Rep."
  ))

# Summarize data by population in a specific year (e.g., 2023)
asia_population_2023 <- asia_data %>%
  select(`Country Name`, `2023`) %>%
  arrange(desc(`2023`))

# Bar plot for population of Asian countries in 2023
ggplot(asia_population_2023, aes(x = reorder(`Country Name`, `2023`), y = `2023`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Population of Asian Countries in 2023", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Summarize data by population in the year 2022
asia_population_2022 <- asia_data %>%
  select(`Country Name`, `2022`) %>%
  arrange(desc(`2022`))

# Bar plot for population of Asian countries in 2022
ggplot(asia_population_2022, aes(x = reorder(`Country Name`, `2022`), y = `2022`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Population of Asian Countries in 2022", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Summarize data by population in the year 2020
asia_population_2020 <- asia_data %>%
  select(`Country Name`, `2020`) %>%
  arrange(desc(`2020`))

# Bar plot for population of Asian countries in 2020
ggplot(asia_population_2020, aes(x = reorder(`Country Name`, `2020`), y = `2020`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Population of Asian Countries in 2020", x = "Country", y = "Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
