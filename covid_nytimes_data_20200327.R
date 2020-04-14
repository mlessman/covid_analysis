library(tidyverse)
library(RCurl)
library(scales)


URL <- getURL('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

nyt_county <- read.csv(text = URL, stringsAsFactors = F)

URL <- getURL('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')

nyt_state <- read.csv(text = URL, stringsAsFactors = F)

max(nyt_county$date)
max(nyt_state$date)

#with New York
nyt_state %>% 
  filter(date >= "2020-03-01") %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = cases), size = 1.2) +
  facet_wrap(~ state)


#without New York
nyt_state %>% 
  filter(date >= "2020-03-01") %>% 
  filter(state != "New York") %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = cases), size = .5, color = "red") +
  geom_point(aes(x = as.Date(date), y = cases), size = .5) +
  facet_wrap(~ state) +
  ggtitle("US States without New York")

nyt_state %>% 
  group_by(state) %>% 
  mutate(New_Cases = cases - lag(cases, 1)) %>% 
  filter(date >= "2020-03-01") %>% 
  filter(!(state %in% c("New York", "New Jersey", "Louisiana", "Michigan", "California"))) %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = New_Cases), size = 1.25) +
  geom_point(aes(x = as.Date(date), y = New_Cases), size = .5) +
  geom_smooth(aes(x = as.Date(date), y = New_Cases), se = F, size = .75) +
  scale_x_date(date_breaks = "2 days", labels = date_format("%m/%d")) +
  theme(axis.text.x=element_text(angle=60, hjust=1, size = 6.5)) +
  facet_wrap(~ state) +
  ggtitle("US States Positive Tests per Day without California, Louisiana, Michigan, New York, New Jersey, ") +
  xlab("Date") +
  ylab("Positive Tests per Day")


nyt_state %>% 
  group_by(state) %>% 
  mutate(New_Cases = cases - lag(cases, 1)) %>% 
  filter(date >= "2020-03-01") %>% 
  filter(state %in% c("New York", "New Jersey", "California", "Michigan", "Louisiana")) %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = New_Cases), size = 1.25) +
  geom_point(aes(x = as.Date(date), y = New_Cases), size = .5) +
  geom_smooth(aes(x = as.Date(date), y = New_Cases), se = F) +
  scale_x_date(date_breaks = "2 days", labels = date_format("%m/%d")) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  facet_wrap(~ state) +
  ggtitle("US States Positive Tests per Day California, Louisiana, Michigan, New York, New Jersey") +
  xlab("Date") +
  ylab("Positive Tests per Day")

# Select States
nyt_state %>% 
  group_by(state) %>% 
  mutate(New_Cases = cases - lag(cases, 1)) %>% 
  filter(date >= "2020-03-01") %>% 
  filter(state %in% c("Ohio", "Virginia", "Michigan")) %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = New_Cases), size = 1.25) +
  geom_point(aes(x = as.Date(date), y = New_Cases), size = .5) +
  geom_smooth(aes(x = as.Date(date), y = New_Cases), se = F) +
  scale_x_date(date_breaks = "2 days", labels = date_format("%m/%d")) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  facet_wrap(~ state) +
  ggtitle("US States Positive Tests per Day New York, New Jersey, California") +
  xlab("Date") +
  ylab("Positive Tests per Day")


nyt_state %>% 
  group_by(state) %>% 
  mutate(New_Deaths = deaths - lag(deaths, 1)) %>% 
  filter(date >= "2020-03-01") %>% 
  filter(state != "New York") %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = New_Deaths), size = .5, color = "red") +
  geom_point(aes(x = as.Date(date), y = New_Deaths), size = .5) +
  facet_wrap(~ state) +
  ggtitle("US States Deaths per Day without New York")


nyt_state %>% 
  filter(state %in% c("Maryland", "Virginia", "Ohio")) %>% 
  group_by(state) %>% 
  mutate(New_Cases = cases - lag(cases, 1)) %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = New_Cases, linetype = state), size = 1) +
  geom_point(aes(x = as.Date(date), y = New_Cases, color = state), size = 2) +
  ggtitle("New Cases per Day")


nyt_state %>% 
  filter(state == "Virginia") %>%
  group_by(state) %>% 
  mutate(New_Cases = cases - lag(cases, 1)) %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = New_Cases, linetype = state), size = .5) +
  geom_point(aes(x = as.Date(date), y = New_Cases, color = state), size = 4) +
  geom_smooth(aes(x = as.Date(date), y = New_Cases), se = F, size = 1.5) +
  ggtitle("Postitive Tests per Day") +
  ylab("Positive Tests per Day") +
  xlab("Date")
  
nyt_county %>% 
  filter(state == "Virginia") %>% 
  filter(county == "Arlington") %>% 
  group_by(county) %>% 
  mutate(New_Cases = cases - lag(cases, 1)) %>% 
  filter(date >= "2020-03-01") %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date), y = New_Cases), size = 1.25) +
  geom_point(aes(x = as.Date(date), y = New_Cases), size = .5) +
  geom_smooth(aes(x = as.Date(date), y = New_Cases), se = F) +
  scale_x_date(date_breaks = "2 days", labels = date_format("%m/%d")) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  facet_wrap(~ county) +
  ggtitle("County Positive Tests per Day") +
  xlab("Date") +
  ylab("Positive Tests per Day")

nyt_county %>% 
  filter(state == "Virginia") %>% 
  filter(county == "Arlington") %>% 
  group_by(county) %>% 
  mutate(New_Cases = cases - lag(cases, 1)) %>% 
  View()
