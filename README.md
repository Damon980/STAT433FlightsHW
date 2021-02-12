---
title: "Flights"
author: "Damon O'Connor"
date: "2/12/2021"
output: html_document
---
#Link to github repo: https://github.com/Damon980/STAT433FlightsHW

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#Part 1

library(tidyverse)
library(nycflights13)
library(lubridate)
df = flights

MissingDep = filter(df, is.na(dep_time))

#Flights that are missing a departure time are also missing dep_delay, arr_time, arr_delay, and air_time. Seems like these flights were canceled.

CanceledFlights = MissingDep

#Part 2

#Checking to see if midnight is 0 or 2400
df %>% filter(dep_time == 0)
df %>% filter(dep_time == 2400)

#There are only observations for dep_time == 2400, so midnight must be stored as 2400.

df = df %>% mutate(
  dep_time_MinsSinceMidnight = (dep_time %/% 100 * 60 + dep_time %% 100) %% (24 * 60), #The first part uses integer division to get the hours, and the second                                                                                            uses modulus division for the minutes, then divide again for the 2400                                                                                            hours cases
  
  sched_dep_time_MinsSinceMidnight = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% (24 * 60) #I will use the same method for sched_dep_time
)

#Part 3

df %>% 
  group_by(day) %>%  
  summarize(meanDelay = mean((dep_time_MinsSinceMidnight - sched_dep_time_MinsSinceMidnight), na.rm = TRUE),
            numCanceled = sum(is.na(dep_time))) %>% 
  ggplot(mapping = aes(x = meanDelay, y = numCanceled))+
    geom_point()+
    geom_smooth()

#It looks like there could be a relationship between the number of flights canceled and the average delay length.




```
