#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm
#look at the first tree height
heights[1]
#look at the 2nd and 3rd tree heights
heights[2:3]

#get more info on the matrix function
help(matrix)
#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol
#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from your data folder

# Here is my PC file path - note my data folder is on Google Drive, so I can access it from multiple computers
datW <- read.csv("Z:\\ewidrick\\GitHub\\GEOG331\\Data\\noaa_weather\\2011124.csv", 
                 stringsAsFactors = T)
#get more information about the dataframe
str(datW)
#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more
#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#Q2:
# create 4 types of vectors
Character_Vector <- c("GEOG","331","Environmental","data","science")
Numeric_Vector <- c(1.23,4.56,7.89,100,1064)
Integer_Vector <- c(1L,2L,3L,4L,5L)
Factor_Vector <- factor(c( "red", "orange","yellow", "blue", "green", "indego", "violet"))
# check vector types
print(class(Character_Vector))
print(class(Numeric_Vector))
print(class(Integer_Vector))
print(class(Factor_Vector))