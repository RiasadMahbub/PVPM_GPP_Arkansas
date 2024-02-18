### 2020 spatial map
library(sp)
library(raster)
remove.packages("rgdal")
int
library(raster)
library(rgdal)
library(sp)
library(tidyverse)
install.packages("rgdal")
install.packages("raster")
install.packages("dismo")
library(reshape2)
install.packages('dismo', repos='https://rspatial.r-universe.dev')
### Load all the raster files

#first import all files in a single folder as a list 
rastlist <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/PVPMclippedByShapefile/2020", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)
rastlist
#import all raster files in folder using lapply
## set wd
setwd("C:/Users/rbmahbub/Documents/Data/GeospatialData/PVPMclippedByShapefile/2020")
allrasters <- lapply(rastlist, raster)

#to check the index numbers of all imported raster list elements
allrasters

#call single raster element
allrasters[[1]]

#to run a function on an individual raster e.g., plot 
plot(allrasters[[1]])


library(raster)
library(rgdal)
library(dismo)
allrasters <- stack(rastlist)
allrasters


gpp_2020<-stack(rastlist)
gpp_2020


DEM <- raster(( "C:/Users/rbmahbub/Documents/Data/GeospatialData/PVPMclippedByShapefile/2020/RCS2020_07_27_26_26.tif"))
plot(gpp_2020)

gpp.df<- as.data.frame(gpp_2020, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
class(gpp.df$x)
class(gpp.df$y)
gpp.df$variable<-as.numeric(gpp.df$variable)
class(gpp.df$variable)
class(gpp.df$value)

# If there are NA values, replace them by 0 and add a column to keep information
gpp.df$is_na = ifelse(is.na(gpp.df$value), TRUE, FALSE)
index = gpp.df$is_na == TRUE
gpp.df[index, "value"] <- 0
gpp.df[index, "y"] <- 0

View(gpp.df)
sincrs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m"

s <- SpatialPoints(gpp.df, proj4string=CRS(sincrs))

lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
View(s)
x <- spTransform(s, lonlat)
View(x)

gpp.df2
class(x)
gpp.df2 <- gpp.df2 %>% mutate(across(c(x, y), round, digits = 4))
gpp.df2<- as.data.frame(x, xy = TRUE) %>% melt(id.vars = c('x', 'y'))
View(gpp.df2)

ggplot()+
  geom_raster(data = gpp.df2, aes(x = x, y = y, fill = value))


rastlist


library(raster)
stacked <-stack(rastlist)

stacked

# calculate a mean raster
meanR <- calc(stacked, fun = mean)


meanR



library(raster)
rastlist <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/PVPMclippedByShapefile/2020", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)
stacked <-stack(rastlist)
summary(getValues(stacked))

rastlist
stacked[stacked== NA] <- 0
summary(getValues(stacked))

