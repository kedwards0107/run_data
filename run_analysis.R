setwd("~/Desktop/Rprojects/Health")

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
run_data$Date = as.yearmon(run_data$Date)

run_data <- as_tibble(run_data)

weather_data <- read.csv("weather.csv", sep = ",", quote = "\"", stringsAsFactors = FALSE)
weather_data$DATE <- as.Date(weather_data$DATE, format="%m/%d/%Y")
weather_data$DATE <- as.yearmon(weather_data$DATE)
joined_data <- merge(run_data,weather_data, by.x = "Date", by.y = "DATE", all.x = TRUE, all.y = FALSE)

str(joined_data)

joined_data$Date <- as.Date(joined_data$Date, format = "%m/%d/%Y")

joined_data$distance <- as.double(joined_data$distance)
str(joined_data$distance)
joined_data$Pace <- (60*60*24*as.numeric(times(joined_data$Pace)))/60
write.csv(joined_data, file = "joined_data_export.csv")

# pace and distance by month and wrapped by year
pvt <- ggplot(data = joined_data, aes(month(Date, label=TRUE, abbr=TRUE), y = Pace))
pvt + geom_point(aes(color = distance), shape = 21, fill = "white", 
                 size = 3, stroke = 2) + scale_color_gradientn(colors = rainbow(8)) +
  facet_wrap(~Year) 

# pace and distance by month and wrapped by year
pvt <- ggplot(data = joined_data, aes(month(Date, label=TRUE, abbr=TRUE), y = Pace))
pvt + geom_point(aes(color = distance), shape = 21, fill = "white", 
                 size = 2, stroke = 2) + scale_color_gradient2(midpoint=7.5, low="blue", mid="green",
                                                               high="red", space ="Lab" ) +
  coord_cartesian(ylim = c(7.5,10.5)) +
  theme(legend.position = "right", text = element_text(size=10)) + labs(x = "Months",
                                                                        y = "Pace") +
  facet_wrap(~Year) 

# distance and pace by month and wrapped by year
pvt <- ggplot(data = joined_data, aes(month(Date, label=TRUE, abbr=TRUE), y = distance))
pvt + geom_point(aes(color = Pace), shape = 21, fill = "white", 
                 size = 3, stroke = 2) + scale_color_gradientn(colors = rainbow(10)) +
  facet_wrap(~Year) 

pvt <- ggplot(data = joined_data, aes(x = MD, y = Pace, group=Year))
pvt + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Runs") + geom_point(aes(color = Year), shape = 21, fill = "white", 
                                                                                   size = 3, stroke = 2)

# Plot of pace by year
ggplot(joined_data, aes(month(Date, label=TRUE, abbr=TRUE), 
                        Pace, group=factor(year(Date)), colour=factor(year(Date)))) +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()

pvt <- ggplot(data = joined_data, aes(x = Date, y = Pace))
pvt + scale_x_date(breaks = "1 month", date_labels = "%B") + geom_point(aes(color = Year), shape = 21, fill = "white", 
                                                                        size = 3, stroke = 2) 

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
