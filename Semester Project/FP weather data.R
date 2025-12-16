#load in packages
library(lubridate)
library(tidyverse)
library(ggplot2)

#read csv
datC <-  read.csv("Z:\\ewidrick\\Data\\4183363.csv")

#### define time #####
#convert date and time
datesD <- as.Date(datC$DATE, "%Y-%m-%d")
#get day of year
datC$doy <- yday(datesD)

#calculate year
datC$year <- year(datesD)

#### get decimal formats #####
#convert time from a string to a more usable format
datC$decYear <- ifelse(leap_year(datC$year),datC$year + (datC$doy/366),
                       datC$year + (datC$doy/365))

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
datC <- datC %>% mutate(month = case_when(
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
PIA <- datC %>% filter(STATION == "USW00024229") 
B7 <- datC %>% filter(STATION == "USC00350699")
EM <- datC %>% filter(STATION == "USW00024221") 
SAP <- datC %>% filter(STATION == "USW00024232")

#2020

Dat2020 <- filter(datC, datC$year == 2020) 

dat20_avg <- Dat2020 %>%
  group_by(doy) %>%
  summarise(mean_PRCP = mean(PRCP, na.rm = TRUE),
            mean_AWND = mean(AWND, na.rm = TRUE),
            mean_TAVG = mean(TAVG, na.rm = TRUE))
          

dat20_avg <- dat20_avg %>% mutate(month = case_when(
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

#make violin plot with TAVG
ggplot(data=dat20_avg, aes(month,mean_TAVG)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "1a.",
    x = "Month",
    y = "Daily Average Temperature (C°)")


#make violin plot with PRCP
ggplot(data=dat20_avg, aes(month,mean_PRCP)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "2a.",
    x = "Month",
    y = "Daily Average Precipitation (mm)")
  
#2013
  
Dat2013 <- filter(datC, datC$year == 2020)

dat13_avg <- Dat2013 %>%
  group_by(doy) %>%
  summarise(mean_PRCP = mean(PRCP, na.rm = TRUE),
            mean_AWND = mean(AWND, na.rm = TRUE),
            mean_TAVG = mean(TAVG, na.rm = TRUE))


dat13_avg <- dat13_avg %>% mutate(month = case_when(
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

#make violin plot with TAVG
ggplot(data=dat13_avg, aes(month,mean_TAVG)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "1b.",
    x = "Month",
    y = "Daily Average Temperature (C°)")


#make violin plot with PRCP
ggplot(data=dat13_avg, aes(month,mean_PRCP)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "2b.",
    x = "Month",
    y = "Daily Average Precipitation (mm)")  
#2002
  
Dat2002 <- filter(datC, datC$year == 2020)

dat02_avg <- Dat2002 %>%
  group_by(doy) %>%
  summarise(mean_PRCP = mean(PRCP, na.rm = TRUE),
            mean_AWND = mean(AWND, na.rm = TRUE),
            mean_TAVG = mean(TAVG, na.rm = TRUE))


dat02_avg <- dat02_avg %>% mutate(month = case_when(
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

#make violin plot with TAVG
ggplot(data=dat02_avg, aes(month,mean_TAVG)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "1c.",
    x = "Month",
    y = "Daily Average Temperature (C°)")


#make violin plot with PRCP
ggplot(data=dat02_avg, aes(month,mean_PRCP)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "2c.",
    x = "Month",
    y = "Daily Average Precipitation (mm)")  
  
#2021
  
Dat2021 <- filter(datC, datC$year == 2020)

dat21_avg <- Dat2021 %>%
  group_by(doy) %>%
  summarise(mean_PRCP = mean(PRCP, na.rm = TRUE),
            mean_AWND = mean(AWND, na.rm = TRUE),
            mean_TAVG = mean(TAVG, na.rm = TRUE))


dat21_avg <- dat21_avg %>% mutate(month = case_when(
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

#make violin plot with TAVG
ggplot(data=dat21_avg, aes(month,mean_TAVG)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "1d.",
    x = "Month",
    y = "Daily Average Temperature (C°)")


how #make violin plot with PRCP
ggplot(data=dat21_avg, aes(month,mean_PRCP)) + geom_violin () +
  scale_x_discrete(limits =c("Jun","Jul","Aug","Sep","Oct")) +
  labs(
    title = "2d.",
    x = "Month",
    y = "Daily Average Precipitation (mm)")  

# Extreme Values
datC_avg$DATE <- round_date(date_decimal(datC_avg$decYear))

# Sum all important variables
datC_avg$EV <- (datC_avg$mean_TEMP + datC_avg$mean_AWND - datC_avg$mean_PRCP)

#filter for dates in the 2000s
datE <- filter(datC_avg, decYear >= 2000) 

# add doy
dates <- as.Date(datE$DATE, "%Y-%m-%d")
datE$doy <- yday(dates)

#filter data with extreme values like SEP 9th 2020
LDFC <- datE %>%
filter(mean_TEMP >= 19.850000, mean_AWND >= 8.4666667, mean_PRCP == 0.0000000)  

#filter just for wind
datWind <- datE %>%
filter(mean_AWND >= 8.4666667, doy >= 152 & doy <= 305)

#filter just for average temp
datTemp <- datE %>%
filter(mean_TEMP >= 19.850000, doy >= 152 & doy <= 305 )

