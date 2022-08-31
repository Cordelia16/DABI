
# Install and import libraries
library(tidyverse)
library(lubridate)
library(GGally)

#-------------------------------------------------------------------------------
# Import data
weather_data <- read.csv("weather_data.csv", sep = ",")
View(weather_data)

sales_data <- read.csv("sales_data.csv", sep = ",")
View(sales_data)

#-------------------------------------------------------------------------------
# Explore the data
str(weather_data)
dim(weather_data)
colnames(weather_data)

str(sales_data)
dim(sales_data)
colnames(sales_data)

#-------------------------------------------------------------------------------
# Find NAs and 0s
colSums(is.na(weather_data))
colSums(weather_data == 0)

colSums(is.na(sales_data))
colSums(sales_data == 0)

#------------------------------ Formatting Data --------------------------------
# Format "Date" columns
weather_data$Date <- as.Date(weather_data$Date, format = "%d/%m/%Y")
weather_data <- weather_data[order(as.Date(weather_data$Date, format = "%d/%m/%Y")), ]

names(sales_data)[1] <- "Date"
sales_data$Date <- as.Date(sales_data$Date, format = "%d/%m/%Y")

# Rename "Rainfall" column
weather_data <- subset(weather_data, select = -c(Rainfall))
names(weather_data)[4] <- "Rainfall"

# Create "Month" variable
weather_data$Month <- months(as.POSIXlt(weather_data$Date, format = "%Y-%m-%d"))
weather_data$Year <- year(as.POSIXlt(weather_data$Date, format = "%Y-%m-%d"))
sales_data$Month <- months(as.POSIXlt(sales_data$Date, format = "%Y-%m-%d"))
sales_data$Year <- year(as.POSIXlt(sales_data$Date, format = "%Y-%m-%d"))

# Remove column
sales_data <- subset(sales_data, select = -c(X))

# Remove rows from "weather_data" so number of rows is equivalent 
weather_data <- weather_data[-c(1:8), ]

# Change NAs to 0
sales_data[is.na(sales_data)] <- 0

# Impute missing values
weather_data <- as.data.frame(weather_data %>% group_by(Month, Year) %>%
                                mutate(Rainfall = ifelse(is.na(Rainfall),
                                                         mean(Rainfall, na.rm = TRUE), Rainfall)))

#-------------------------------------------------------------------------------
# Correlation
corr_weather <- subset(weather_data, select = -c(1, 7, 9, 13, 14, 19, 20, 22))
View(corr_weather)

ggcorr(corr_weather, label = TRUE)

#-------------------------------------------------------------------------------
# Initial plots
ggplot(weather_data, aes(x = Rainfall)) +
  geom_histogram() +
  xlab("Rainfall") +
  ylab("Counts") +
  ggtitle("Distribution of Rainfall")

ggplot(sales_data, aes(x = Total)) +
  geom_histogram() +
  xlab("Income") +
  ylab("Counts") +
  ggtitle("Distribution of Income")

ggplot(weather_data, aes(x = Month, y = Rainfall)) +
  geom_path()

ggplot(sales_data, aes(x = Month, y = Total)) +
  geom_point()
