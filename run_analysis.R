# Set working directory and load necessary libraries
setwd("~/run_data")
install.packages("wesanderson")
library(tidyverse)
library(wesanderson)
library(chron)
library(ggplot2)
library(zoo)
library(lubridate)

# Function to read and preprocess data
read_and_preprocess <- function(file_name) {
    data <- read.csv(file_name, sep = ",", quote = "\"", stringsAsFactors = FALSE)
    data$Date <- as.Date(data$Date, format="%m/%d/%Y")
    data$Day.of.week <- as.factor(data$Day.of.week)
    as_tibble(data)
}

# Read and preprocess run data and weather data
run_data <- read_and_preprocess("run_data2022.csv")
weather_data <- read_and_preprocess("weather.csv")

# Joining run_data with weather_data
joined_data <- merge(run_data, weather_data, by.x = "Date", by.y = "DATE", all.x = TRUE, all.y = FALSE)
joined_data$distance <- as.double(joined_data$distance)
joined_data$Pace <- (60*60*24*as.numeric(times(joined_data$Pace)))/60

# Repeat the process for another dataset
run_data2 <- read_and_preprocess("run4_6_data2022.csv")
weather_data2 <- read_and_preprocess("weather.csv")
joined_data2 <- merge(run_data2, weather_data2, by.x = "Date", by.y = "DATE", all.x = TRUE, all.y = FALSE)
joined_data2$distance <- as.double(joined_data2$distance)
joined_data2$Pace <- (60*60*24*as.numeric(times(joined_data2$Pace)))/60

# Function to create plots
create_plot <- function(data, mapping, title, x_breaks, x_labels, y_limits, color_scale, facet = NULL) {
    plot <- ggplot(data, mapping) + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        ggtitle(title) + 
        scale_x_date(breaks = x_breaks, date_labels = x_labels) + 
        geom_point(aes(color = color_scale), size = 1, stroke = 2) + 
        theme_light() + 
        scale_color_gradientn(colors = rainbow(5)) + 
        coord_cartesian(ylim = y_limits)
    if (!is.null(facet)) {
        plot <- plot + facet_wrap(facet)
    }
    plot
}

# Example plot
plot1 <- create_plot(joined_data2, aes(x = Date, y = Pace), "Runs by Year and Pace", "1 year", "%Y", c(8.0, 9.5), Avg.Temp)

# Summarizing data
summarize_data <- function(data, group_by_col, sum_col) {
    tapply(data[[sum_col]], data[[group_by_col]], sum)
}

# Summarize data by day of week and year
total_by_day <- summarize_data(run_data, "Day.of.week", "distance")
total_by_year <- summarize_data(joined_data, "Year", "distance")

# Calculate average pace by day of week
avg_pace_by_day <- function(data, day_col, pace_col) {
    tapply(data[[pace_col]], data[[day_col]], mean)
}

# Average pace by day of week
avg_pace_day <- avg_pace_by_day(joined_data, "Day.of.week", "Pace")

# Additional plots
# Here, you can create more plots by calling the create_plot function with different parameters
