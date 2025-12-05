#load in packages
library(terra)
library(tidyterra)
library(FedData)

#April 26th 2020
#load in files
first <- list.files("Z:/ewidrick/Data/045030/LC08_L2SP_045030_20200426_20200822_02_T1", full.names = T)

first

# read in files 3-10 as a single multi-band raster
lc1 <- rast(first[5:13])

# calculate ndvi
ndvi1 <- (lc[[5]]-lc[[4]])/(lc[[5]]+lc[[4]])

names(ndvi) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi)

#September 11th 2020
#load in files
Sec <- list.files("Z:/ewidrick/Data/045030/LC08_L2SP_045030_20200715_20200911_02_T1", full.names = T)

Sec

# read in files 3-10 as a single multi-band raster
lc2 <- rast(Sec[5:13])

# calculate ndvi
ndvi2 <- (lc[[5]]-lc[[4]])/(lc[[5]]+lc[[4]])

names(ndvi) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi)
