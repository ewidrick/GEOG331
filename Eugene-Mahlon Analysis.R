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


#vector for summer
Summer<- c(173:264)

#make factor for seasons 
datP <- datP %>% mutate(season = case_when(
                                           doy %in% Summer ~ "Summer",
                                           TRUE ~ "Not Summer"  # everything else (end & start of year)
))

#Make a Plot with Values during the summer
ggplot(datP, aes(decYear, PRCP, color = season)) + geom_point()+
scale_color_manual(values = c("Summer" = "red", "Not Summer" = "black"))+ labs(
  title = "Daily Average Precipetation Over 25 years at Eugene-Mahlon",
  x = "Time",
  y = "Daily average precipitation (in)"
)
