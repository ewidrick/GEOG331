#load in packages
library(lubridate)
library(tidyverse)
library(ggplot2)

# read csv
datP <- read.csv("Z:\\ewidrick\\GitHub\\GEOG331\\Semester Project\\Data\\4161731.csv")

#### define time for precipetation #####
#convert date and time
datesD <- as.Date(datP$DATE, "%Y-%m-%d")
#get day of year
datP$doy <- yday(datesD)
#calculate year
datP$year <- year(datesD)

#### get decimal formats #####
#convert time from a string to a more usable format
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$doy/366),
                       datP$year + (datP$doy/365))

plot(datP$decYear, datP$PRCP, type="l", xlab="Time", ylab="Precipetation (in)", 
     main = "Eugene-Mahlon Daily Precipitation Over 25 Years" )
