#load in packages
library(terra)
library(tidyterra)
library(FedData)

#load in files
f <- list.files("Z:/ewidrick/Data/046030", full.names = T)

f

# read in files 3-10 as a single multi-band raster
lc <- rast(f[5:13])

# calculate ndvi
ndvi <- (lc[[5]]-lc[[4]])/(lc[[5]]+lc[[4]])

names(ndvi) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi)

