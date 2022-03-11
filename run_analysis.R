setwd("~/run_data")

install.packages("wesanderson")
library(tidyverse)
library(wesanderson)
library(chron)
library (ggplot2)
library(zoo)
library(lubridate)
run_data <- read.csv("run_data2022.csv", sep = ",", quote = "\"", stringsAsFactors = FALSE)
run_data$Date <- as.Date(run_data$Date, format="%m/%d/%Y")
run_data$Day.of.week <- as.factor(run_data$Day.of.week)
print(is.factor(run_data$Day.of.week))
#run_data$Date = as.yearmon(run_data$Date)

run_data <- as_tibble(run_data)
#run_data$Pace <- (60*60*24*as.numeric(times(run_data$Pace)))/60

weather_data <- read.csv("weather.csv", sep = ",", quote = "\"", stringsAsFactors = FALSE)
weather_data$DATE <- as.Date(weather_data$DATE, format="%m/%d/%Y")
#weather_data$DATE <- as.yearmon(weather_data$DATE)
joined_data <- merge(run_data,weather_data, by.x = "Date", by.y = "DATE", all.x = TRUE, all.y = FALSE)

str(joined_data)

joined_data$Date <- as.Date(joined_data$Date, format = "%m/%d/%Y")

joined_data$distance <- as.double(joined_data$distance)
str(joined_data$distance)
joined_data$Pace <- (60*60*24*as.numeric(times(joined_data$Pace)))/60
write.csv(joined_data, file = "joined_data_export.csv")

# pace and distance by month and wrapped by year
pvt <- ggplot(data = joined_data, aes(month(Date, label=TRUE, abbr=TRUE), y = Pace))
pvt + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Pace by Month Faceted by Year") + geom_point(aes(color = distance), shape = 21, fill = "white", 
                 size = 3, stroke = 2) + scale_color_gradientn(colors = rainbow(8)) + labs(x = "Months",
                                                                                           y = "Pace") +
  facet_wrap(~Year)

#total miles by day of week
total_Monday <- sum(run_data[which(run_data$Day.of.week=='Monday'), 3])
total_Tuesday <- sum(run_data[which(run_data$Day.of.week=='Tuesday'), 3])
total_Wednesday <- sum(run_data[which(run_data$Day.of.week=='Wednesday'), 3])
total_Thursday <- sum(run_data[which(run_data$Day.of.week=='Thursday'), 3])
total_Friday <- sum(run_data[which(run_data$Day.of.week=='Friday'), 3])
total_Saturday <- sum(run_data[which(run_data$Day.of.week=='Saturday'), 3])
total_Sunday <- sum(run_data[which(run_data$Day.of.week=='Sunday'), 3])
total_Monday
total_Tuesday
total_Wednesday
total_Thursday
total_Friday
total_Saturday
total_Sunday

total_2009 <- sum(joined_data[which(joined_data$Year=='2009'), 3])
total_2010 <- sum(joined_data[which(joined_data$Year=='2010'), 3])
total_2011 <- sum(joined_data[which(joined_data$Year=='2011'), 3])
total_2012 <- sum(joined_data[which(joined_data$Year=='2012'), 3])
total_2013 <- sum(joined_data[which(joined_data$Year=='2013'), 3])
total_2014 <- sum(joined_data[which(joined_data$Year=='2014'), 3])
total_2015 <- sum(joined_data[which(joined_data$Year=='2015'), 3])
total_2016 <- sum(joined_data[which(joined_data$Year=='2016'), 3])
total_2017 <- sum(joined_data[which(joined_data$Year=='2017'), 3])
total_2018 <- sum(joined_data[which(joined_data$Year=='2018'), 3])
total_2019 <- sum(joined_data[which(joined_data$Year=='2019'), 3])
total_2020 <- sum(joined_data[which(joined_data$Year=='2020'), 3])
total_2021 <- sum(joined_data[which(joined_data$Year=='2021'), 3])
total_2022 <- sum(joined_data[which(joined_data$Year=='2022'), 3])
total_2009
total_2010
total_2011
total_2012
total_2013
total_2014
total_2015
total_2016
total_2017
total_2018
total_2019
total_2020
total_2021
total_2022

# average pace by day of week
avg_Monday <- mean(joined_data[which(joined_data$Day.of.week=='Monday'), 5])
avg_Tuesday <- mean(joined_data[which(joined_data$Day.of.week=='Tuesday'), 5])
avg_Wednesday <- mean(joined_data[which(joined_data$Day.of.week=='Wednesday'), 5])
avg_Thursday <- mean(joined_data[which(joined_data$Day.of.week=='Thursday'), 5])
avg_Friday <- mean(joined_data[which(joined_data$Day.of.week=='Friday'), 5])
avg_Saturday <- mean(joined_data[which(joined_data$Day.of.week=='Saturday'), 5])
avg_Sunday <- mean(joined_data[which(joined_data$Day.of.week=='Sunday'), 5])
avg_Monday
avg_Tuesday
avg_Wednesday
avg_Thursday
avg_Friday
avg_Saturday
avg_Sunday

# pace and distance by month and wrapped by year
pvt <- ggplot(data = joined_data, aes(month(Date, label=TRUE, abbr=TRUE), y = Pace))
pvt + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Pace less than 10.5 \n by Month Faceted by Year") + geom_point(aes(color = distance), shape = 21, fill = "white", 
                 size = 2, stroke = 2) + scale_color_gradient2(midpoint=7.5, low="blue", mid="green",
                                                               high="red", space ="Lab" ) +
  coord_cartesian(ylim = c(7.0,10.5)) +
  theme(legend.position = "right", text = element_text(size=10)) + labs(x = "Months",
                                                                        y = "Pace") +
  facet_wrap(~Year) 

# distance and pace by month and wrapped by year
pvt <- ggplot(data = joined_data, aes(month(Date, label=TRUE, abbr=TRUE), y = distance))
pvt + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Month by Distance Faceted by Year") + geom_point(aes(color = Pace), shape = 21, fill = "white", 
                 size = 3, stroke = 2) + labs(x = "Month",
                                              y = "Distance")+ scale_color_gradientn(colors = rainbow(10)) +
  facet_wrap(~Year) 

pvt <- ggplot(data = joined_data, aes(x = MD, y = Pace, group=Year))
pvt + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Runs") + geom_point(aes(color = Year), shape = 21, fill = "white", 
                                                                                   size = 3, stroke = 2)

# Plot of pace by year
ggplot(joined_data, aes(month(Date, label=TRUE, abbr=TRUE), 
                        Pace, group=factor(year(Date)), colour=factor(year(Date)))) +
  geom_point(shape = 21, fill = "white", 
             size = 3, stroke = 2) +
  labs(x="Month", colour="Year") +
  theme_classic()

# Plot of pace by year
ggplot(joined_data, aes(month(Date, label=TRUE, abbr=TRUE), 
                        Pace, group=factor(year(Date)), colour=factor(year(Date)))) + labs(x = "Month",
                                                                                           y = "Pace") +
  geom_bar(stat = "identity") +
  theme_classic()

pvt <- ggplot(data = joined_data, aes(x = Date, y = Pace))
pvt + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + scale_x_date(breaks = "1 month", date_labels = "%B") + geom_point(aes(color = Year), shape = 21, fill = "white", size = 3, stroke = 2) 

pvt <- ggplot(data = joined_data, aes(x = Month, y = Pace))
pvt + geom_point(aes(color=distance), shape = 21, fill = "white", 
                 size = 3, stroke = 2) + theme_light()  + scale_color_gradientn(colors = rainbow(2)) +
  facet_wrap(~Day.of.week) 

pvt <- ggplot(data = joined_data, aes(x = Date, y = Pace))
pvt + ggtitle("Runs by Year and Pace, Legend is Distance") + scale_x_date(breaks = "1 year", date_labels = "%Y") + geom_point(aes(color = distance), size = 1, stroke = 2) +
  theme_light()  + scale_color_gradientn(colors = rainbow(5)) 



ovt <- ggplot(data = joined_data, aes(x = Date, y = Pace)) 
ovt + ggtitle("Runs by Year and Pace faceted by Day of Week, Legend is Average Day Temp") + scale_x_date(breaks = "1 year", date_labels = "%Y") + geom_point(aes(color = TOBS), size = 1, stroke = 2) + 
  theme_light()  + scale_color_gradientn(colors = rainbow(5)) +
  facet_wrap("Day.of.week")

ovt <- ggplot(data = joined_data, aes(x = distance, y = Pace)) 
ovt + ggtitle("Runs by Distance and Pace faceted by Month, Legend is Average Day Temp") + geom_point(aes(color = TOBS), size = 1, stroke = 2) + 
  theme_light()  + scale_color_gradientn(colors = rainbow(5)) +
  facet_wrap("Month")
