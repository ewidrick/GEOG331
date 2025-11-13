#load in packages
library(lubridate)
library(tidyverse)
library(ggplot2)

# read csv
datP <- read.csv("Z:\\ewidrick\\GitHub\\GEOG331\\Semester Project\\Data\\4161731.csv")

#### define time for precipitation #####
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

#Plot the daily average for the past 25 years
plot(datP$decYear, datP$PRCP, type="l", xlab="Time", ylab="Precipetation (in)", 
     main = "Eugene-Mahlon Average Daily Precipitation Over 25 Years" )


#vector for doy = summer
Summer<- filter(datP$doy=c(173:264))


#Make a Plot with Values during the summer
ggplot(data = datP, mapping = aes(decYear, PRCP)) +
theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) + 
geom_line(data =datP, aes(decYear, PRCP)) +
geom_point(data =datP, aes(decYear[doy == Summer], PRCP))
