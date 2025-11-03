#load in lubridate
library(lubridate)

# read in streamflow data
datH <- read.csv("Z:\\ewidrick\\GitHub\\GEOG331\\Data\\hw5_data\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)  

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:\\ewidrick\\GitHub\\GEOG331\\Data\\hw5_data\\2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
#Q2
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))        

#plot discharge
#Q3 and Q4
plot(datD$decYear, datD$discharge, type="l", xlab="Year", 
ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#Q5
#bigger margins
par(mai=c(1,1,1,1))

#Create vector for months
m2017 <- c("2017","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="2017", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,300),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
lines(datD$doy[datD$year == 2017], datD$discharge [datD$year == 2017], col = "red")
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=30), #tick intervals
     lab = m2017)  #tick labels
axis(2, seq(0,175, by=25),
     seq(0,175, by=25),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","2017","1 standard deviation"), #legend items
       lwd=c(2,2,NA),#lines
       col=c("black","red",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,NA,15),#symbols
       bty="n")#no legend border

#Q7 
#load in tidyverse
library(tidyverse)

# make a new data frame
alldayP <- datP %>%

#group the data by the doy and year
group_by(doy,year) %>%
  
# find the days with 24 observations
filter(n() == 24)

mutate(datH, )
# make a plot with all discharge measurements and days with 24 hours of measurements
plot.default(datD$decYear, datD$discharge, type = "l", xlim = NULL, ylim = NULL, xlab= "Time",
ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")))


# add points for precipitation measurements
points.default(alldayP$decYear, alldayP$HPCP, col = "blue", pch = 19)

#make a legend
legend("topright", c("discharge", "precipitation on days with 24-hours of observations"), #legend items
                      lwd=c(2,NA),#lines
                      col=c("black","blue"),#colors
                      pch=c(NA,19),#symbols
                      bty="n")#no legend border

#subset discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)}

#Q8
#subset discharge and precipitation within range of interest
Q8hydroD <- datD[datD$doy >= 356 & datD$doy < 357 & datD$year == 2012,]
Q8hydroP <- datP[datP$doy >= 356 & datP$doy < 357 & datP$year == 2012,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
nyl <- floor(min(Q8hydroD$discharge))-1
#ceiling rounds up to the integer
nyh <- ceiling(max(Q8hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
npl <- 0
npm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
Q8hydroP$pscale <- (((nyh-nyl)/(npm-npl)) * Q8hydroP$HPCP) + nyl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(Q8hydroD$decDay,
     Q8hydroD$discharge, 
     type="l", 
     ylim=c(nyl,nyh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(Q8hydroP)){
  polygon(c(Q8hydroP$decDay[i]-0.017,Q8hydroP$decDay[i]-0.017,
            Q8hydroP$decDay[i]+0.017,Q8hydroP$decDay[i]+0.017),
          c(yl,Q8hydroP$pscale[i],Q8hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)}


#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#Q9 
#load ggplot2
library(ggplot2)

#make factor for seasons 
DatD$season <- as.factor( case_when(doy >= 79  & doy < 172 ~ "Spring",
                         doy >= 172 & doy < 264 ~ "Summer",
                         doy >= 264 & doy < 355 ~ "Fall",
                         TRUE ~ "Winter"  # everything else (end & start of year)
))

#2016 plot 

ggplot(data= datD[datD$year == "2016",], aes(doy,discharge)) + 
  geom_violin()

#2017 plot
