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
ndvi1 <- (lc1[[5]]-lc1[[4]])/(lc1[[5]]+lc1[[4]])

names(ndvi1) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi1)

#September 11th 2020
#load in files
Sec <- list.files("Z:/ewidrick/Data/045030/LC08_L2SP_045030_20200715_20200911_02_T1", full.names = T)

Sec

# read in files 3-10 as a single multi-band raster
lc2 <- rast(Sec[5:13])

# calculate ndvi
ndvi2 <- (lc2[[5]]-lc2[[4]])/(lc2[[5]]+lc2[[4]])

names(ndvi2) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi2)

#October 3rd 2020
#load in files
third <- list.files("Z:/ewidrick/Data/045030/LC08_L2SP_045030_20201003_20201015_02_T1", full.names = T)

third

# read in files 3-10 as a single multi-band raster
lc3 <- rast(third[5:13])

# calculate ndvi
ndvi3 <- (lc3[[5]]-lc3[[4]])/(lc3[[5]]+lc3[[4]])

names(ndvi3) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi3)
