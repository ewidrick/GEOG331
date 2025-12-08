#load in packages
library(lubridate)
library(tidyverse)
library(ggplot2)

# read csv
datP <- read.csv("Z:\\ewidrick\\Data\\4183171.csv")

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



#make vectors for months
Jan <- 1:31
Feb <- 32:60
Mar <- 61:91
Apr <- 92:121
May <- 122:152
Jun <- 153:182
Jul <- 183:213
Aug <- 214:244
Sep <- 245:274
Oct <- 275:305
Nov <- 306:335
Dec <- 336:366

#make factor for seasons 
datP <- datP %>% mutate(month = case_when(
                                           doy %in% Jan ~ "Jan",
                                           doy %in% Feb ~ "Feb",
                                           doy %in% Mar ~ "Mar", 
                                           doy %in% Apr ~ "Apr",
                                           doy %in% May ~ "May",
                                           doy %in% Jun ~ "Jun",
                                           doy %in% Jul ~ "Jul",
                                           doy %in% Aug ~ "Aug",
                                           doy %in% Sep ~ "Sep",
                                           doy %in% Oct ~ "Oct",
                                           doy %in% Nov ~ "Nov",
                                           doy %in% Dec ~ "Dec"
))

#filter by station
PIA <- datP %>% filter(STATION == "USW00024229") 
B7 <- datP %>% filter(STATION == "USC00350699")
EM <- datP %>% filter(STATION == "USW00024221") 
SAP <- datP %>% filter(STATION == "USW00024232")

#take averages from each station
datP_avg <- datP %>%
  group_by(doy) %>%
  summarise(mean_PRCP = mean(PRCP, na.rm = TRUE),
            mean_AWND = mean(AWND, na.rm = TRUE),
            mean_TMAX = mean(TMAX, na.rm = TRUE),
            mean_TMIN = mean(TMIN, na.rm = TRUE))

datP_avg <- datP_avg %>% mutate(month = case_when(
  doy %in% Jan ~ "Jan",
  doy %in% Feb ~ "Feb",
  doy %in% Mar ~ "Mar", 
  doy %in% Apr ~ "Apr",
  doy %in% May ~ "May",
  doy %in% Jun ~ "Jun",
  doy %in% Jul ~ "Jul",
  doy %in% Aug ~ "Aug",
  doy %in% Sep ~ "Sep",
  doy %in% Oct ~ "Oct",
  doy %in% Nov ~ "Nov",
  doy %in% Dec ~ "Dec" ))

# Make Violin plots for months of the year
ggplot(data=datP_avg, aes(month,mean_PRCP)) + geom_violin () +
  scale_x_discrete(limits =c("Jan","Feb","Mar","Apr","May","Jun",
                             "Jul","Aug","Sep","Oct","Nov","Dec"))

ggplot(data=datP_avg, aes(month,mean_AWND)) + geom_violin () +
  scale_x_discrete(limits =c("Jan","Feb","Mar","Apr","May","Jun",
                             "Jul","Aug","Sep","Oct","Nov","Dec"))

ggplot(data=datP_avg, aes(month,mean_TMAX)) + geom_violin () +
  scale_x_discrete(limits =c("Jan","Feb","Mar","Apr","May","Jun",
                             "Jul","Aug","Sep","Oct","Nov","Dec"))

ggplot(data=datP_avg, aes(month,mean_TMIN)) + geom_violin () +
  scale_x_discrete(limits =c("Jan","Feb","Mar","Apr","May","Jun",
                             "Jul","Aug","Sep","Oct","Nov","Dec"))
