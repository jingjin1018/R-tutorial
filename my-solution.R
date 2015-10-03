library(dplyr)
## setwd() to the correct directory
flights <- read.csv('data/flights.csv', head = T)
flights$date <- as.Date(flights$date)
planes <- read.csv('data/planes.csv', head = T)
weather <- read.csv('data/weather.csv', head = T)
weather$date <- as.Date(weather$date)
airports <- read.csv('data/airports.csv', head = T)

## Exercise 1: Find all flights
# To SFO or OAK
df1 <- filter(flights, dest %in% c('SFO', 'OAK'))
# In January
library(lubridate)
flights <- mutate(flights, year = year(flights$date), month = month(flights$date), day = day(flights$date))
df2 <- filter(flights, month == 1)
# Delayed by more than an hour
df3 <- filter(flights, dep_delay > 60)
# That departed between midnight and five am
df4 <- filter(flights, hour >= 0, hour <= 5)

## Exercise 2: 
# Order the flights by depature date and time
df5 <- arrange(flights, date, hour, minute)
# Which flights were most delayed
df6 <- arrange(flights, desc(arr_delay))
df6$flight[1]

## Exercise 3:
# Compute speed in mph from time and distance. Which flight flew the fastest
df7 <- mutate(flights, mph = dist/(time/60))
df7 <- arrange(df7, desc(mph))
df7$flight[1]
# Compute hour and minute from dep
df8 <- mutate(flights, hr = dep %/% 100, min = dep %% 100)

## Exercise 4:
# Which destinations have the highest average delays?
df9 <- flights %>% 
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(avg_delay = mean(arr_delay), n = n()) %>% 
  filter(n > 10) %>%
  arrange(desc(avg_delay)) %>%
  head(5)
# Which flights (i.e. carrier + flight) happen every day? Where do they fly to?
num_date <- length(unique(flights$date))
df10 <- flights %>%
  group_by(carrier, flight) %>%
  summarise(count = n_distinct(date)) %>%
  filter(count == num_date)
df11 <- transmute(df10, cf = paste(carrier, flight, sep = ''))
df12 <- flights %>%
  mutate(cf = paste(carrier, flight, sep = '')) %>%
  select(cf, dest) %>%
  filter(cf %in% df11$cf) %>%
  select(dest) %>%
  distinct()
# On average, how do delays (of non-cancelled flights) vary over the course of a day?
library(ggplot2)
df13 <- flights %>%
  filter(cancelled == 0) %>%
  mutate(h = hour + minute/60) %>%
  group_by(h) %>%
  summarise(avg_delay = mean(dep_delay), n = n()) %>%
  filter(n > 30) %>%
  arrange(h)
ggplot(df13, aes(h, avg_delay)) + 
  geom_point(aes(size = n), alpha = 0.2) + 
  scale_size_area() + 
  geom_smooth(method = 'loess')
 
## Exercise 5:
# Are older planes more likely to be delayed?
by_year <- planes %>% select(plane, year) %>% filter(!is.na(year))
delay <- flights %>% group_by(plane) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE), count = n()) %>%
  filter(count > 50) %>%
  inner_join(by_year)
ggplot(delay, aes(year, avg_delay)) + geom_point() + 
  xlim(1990, 2011) + geom_smooth()
