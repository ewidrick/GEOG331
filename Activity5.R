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
     xlab="Year", 
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
     labels = m2017)  #tick labels
axis(2, seq(0,175, by=25),
     seq(0,175, by=25),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","2017","1 standard deviation"), #legend items
       lwd=c(2,2,NA),#lines
       col=c("black","red",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,NA,15),#symbols
       bty="n")#no legend border

#Q7 
# create a vector from 0 to 24
hours <- 0:24

V1<- datP$doy
# make a data frame for days with 24 hours of precipitation
alldayP_df <- data.frame(datP$doy[datP$hour == hours ], datP$year[datP$hour == hours])

                         