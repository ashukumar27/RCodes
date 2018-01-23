########################################
# Code for practicing dPlyr
# Tutorial from Hadley Wickham's Presentation on UseR
########################################

#Libraries
library(dplyr)
library(ggplot2)


#Load Data
setwd("/Users/ashutosh/Documents/analytics/RPractice/dPlyr")

#Files: airports.csv, flights.csv, planes.csv, weather.csv

airports<-read.csv("airports.csv", stringsAsFactors = FALSE)
flights<-read.csv("flights.csv", stringsAsFactors = FALSE)
planes<-read.csv("planes.csv", stringsAsFactors = FALSE)
weather<-read.csv("weather.csv", stringsAsFactors = FALSE)

##########

df<-data.frame(color=c("blue", "black", "blue", "blue", "black"),
               value = 1:5)

## filter ##
filter(df, color=="blue")
filter(df, value %in% c(1,4))

filter(flights, dep_delay>60)

##Select
select(df, color)
select(flights, c(flight,dest))
select(flights, dep:dest)

head(select(flights, ends_with("delay")))

##arrange/ sort

arrange(df, desc(value))

arrange(flights, desc(dep_delay-arr_delay))


## Mutate : Operation on one variable and store information in other

mutate(df, double = 2*value)

flights<-mutate(flights, speed=dist/(time/60))


###################
## Summarize & Group by

summarise(df, total = sum(value))

by_color<-group_by(df, color)
summarise(by_color, total = sum(value))

by_date<-group_by(flights,date)
summarise(by_date, total=n())
