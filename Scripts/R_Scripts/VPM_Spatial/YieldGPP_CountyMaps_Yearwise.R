#####
### Load the libraries
library(raster)
library(maptools)
library(terra)
library(cowplot)
library(tidyverse)
library(ggsn)
library(ggpubr)
library(sf)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(sp)

### Read the arkansas shpaefile
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/tl_2016_05_cousub/tl_2016_05_cousub.shp")

### Make a county map
county <- aggregate(arkansasshp["COUNTYFP"], by = list(diss = arkansasshp$COUNTYFP), 
                    FUN = function(x)x[1], do_union = TRUE)

##County with one variable
countyonevariable <- county[ ,c(2,3)]
plot(countyonevariable)

### Read the yield data
yield20082020<-read.csv("C:/Users/rbmahbub/Documents/Data/YieldData/ArkansasCounty2008_2020lb_acre_areayield.csv")
yield20082020$COUNTYFP<-yield20082020$County.ANSI

for(i in unique(yield20082020$Year)) {
  nam <- paste("yield", i, sep = ".")
  assign(nam, yield20082020[yield20082020$Year==i,])
}

View(yield.2008)
yield.2008$COUNTYFP<-str_pad(yield.2008$COUNTYFP, 3, pad = "0")
yield.2008$Yield<-as.numeric(gsub(",", "", yield.2008$Value))
yield2008shp <- merge(countyonevariable, yield.2008, by='COUNTYFP', all.x=TRUE)
yield2008shp<-yield2008shp[ ,c("COUNTYFP", "Year", "Yield")] 
plot(yield2008shp)

View(yield2008shp)

### Cumulative GPP
rastercumugpp2008 <- rast("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/arkansasRice2008PVPMcumulative.tif")
rastercumugpp2020 <- raster("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/arkansasRice2020PVPMcumulative.tif")
rasdfcumugpp2008 <- as.data.frame(rastercumugpp2008,xy=TRUE)%>%drop_na()

plot(rastercumugpp2020)

## It seems like the cumulative data are clipped by the rice layer shapefile. So there is no need for clipping again and we can check through the qgis
rastercumugpp2008
mtranformed<-st_transform(yield2008shp, crs(rastercumugpp2008))
elevation = terra::extract(rastercumugpp2008, vect(mtranformed))
view(elevation)

agg_mean = aggregate(elevation,by=list(elevation$ID),FUN=mean, na.rm=TRUE)
View(agg_mean)
agg_mean<-as.data.frame(agg_mean)


agg_mean$ID<-str_pad(agg_mean$ID, 3, pad = "0")
agg_mean$COUNTYFP<-agg_mean$ID
yield2008shp$ID<-1:75
agg_mean$ID<-str_pad(agg_mean$ID, 3, pad = "0")
agg_mean$COUNTYFP<-agg_mean$ID

gppricecounty <- merge(yield2008shp, agg_mean, by='COUNTYFP', all.x=TRUE)
gppricecounty <- data.frame(yield2008shp, agg_mean)

yield2008shp
m
view(gppricecounty)
plot(gppricecounty$gpppvpm_sum, gppricecounty$Yield)
gppricecountydf<- as.data.frame(gppricecounty)


gppricecountydfnaomit<-gppricecountydf %>% drop_na(Yield)
plot(gppricecountydfnaomit$gpppvpm_sum, gppricecountydfnaomit$Yield)
View(gppricecountydfnaomit)


#### Make it in a loop

##make the path list for each file 

## Create the raster list
path = 'C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/arkansasRice2008PVPMcumulative.tif'

path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/"
Imagenameyield <-list.files(path = path, pattern='.tif$', 
                           all.files=TRUE, full.names=FALSE)

pathlistyield <- vector("list", 13)
for (i in 1:13) {
  pathlistyield[i] = paste('C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/', Imagenameyield[i], sep = "")
  year = year+1
} 

rastlistyield<- vector("list", 13)
for (i in 1:13) {
  rastlistyield[[i]] <- raster(pathlistyield[[i]])
}


###Cropping the shapefiles are from pvpm all images file
### Crop the shapefiles

## Transform the shapfiles
shapefilelisttransformedyield<-vector("list", 13)
for (i in 1:13) {
  shapefilelisttransformedyield[[i]] <- spTransform(shapefilelist[[i]], crs(rastlistyield[[i]]))
}


rastlistcroppedyield <- vector("list", 13) 
for (i in 1:13) {
  rastlistcroppedyield[[i]] <- raster::crop(rastlistyield[[i]], shapefilelisttransformedyield[[i]])
}


## Mask the shapefiles
rastlistmaskedyield <- vector("list", 13) 

for (i in 1:13) {
  rastlistmaskedyield[[i]] <- raster::mask(rastlistcroppedyield[[i]], shapefilelisttransformedyield[[i]])
}

raster::mask(rastlistcroppedyield[[1]], shapefilelisttransformedyield[[1]])

## Yield data
yield20082020<-read.csv("C:/Users/rbmahbub/Documents/Data/YieldData/ArkansasCounty2008_2020lb_acre_areayield.csv")
yield20082020$COUNTYFP<-yield20082020$County.ANSI
yield20082020$COUNTYFP<-str_pad(yield20082020$COUNTYFP, 3, pad = "0")
yield20082020$Yield<-as.numeric(gsub(",", "", yield20082020$Value))


for(i in unique(yield20082020$Year)) {
  nam <- paste("yield", i, sep = ".")
  assign(nam, yield20082020[yield20082020$Year==i,])
}

yieldlist<-list(yield.2008, yield.2009, yield.2010, yield.2011, yield.2012, yield.2013, yield.2014, yield.2015, yield.2016, yield.2017, yield.2018, yield.2019, yield.2020)

yieldlist


yieldlistshp<-vector("list", 13)
for (i in 1:13) {
  yieldlistshp[[i]] <- merge(countyonevariable, yieldlist[i], by='COUNTYFP', all.x=TRUE)
  yieldlistshp[[i]] <- yieldlistshp[[i]][ ,c("COUNTYFP", "Year", "Yield")]
  yieldlistshp[[i]]$ID<-1:75
} 

yieldlistshp
## Raster Yield merging
mtranformedlist<-vector("list", 13)
for (i in 1:13) {
  mtranformedlist[[i]] <- st_transform(yieldlistshp[[i]], crs(rastlistyield[[i]]))
} 

View(mtranformedlist[[1]])
##mttranformelist work as list []

elevationlist<-vector("list", 13)
for (i in 1:13) {
  elevationlist[[i]] <- terra::extract(rastlistyield[[i]], vect(mtranformedlist[[i]]))
} 
View(elevationlist[[1]])


agg_meanlist<-vector("list", 13)
for (i in 1:13) {
  agg_meanlist[[i]] <- aggregate(elevation,by=list(elevation$ID),FUN=mean, na.rm=TRUE)
  agg_meanlist[[i]]<-as.data.frame(agg_meanlist[[i]])
  agg_meanlist[[i]]$ID<-str_pad(agg_meanlist[[i]]$ID, 3, pad = "0")
  agg_meanlist[[i]]$COUNTYFP<-agg_meanlist[[i]]$ID
} 
View(yieldlistshp[[1]])

gppricecounty<-vector("list", 13)
gppricecountymerged2<-vector("list", 13)
for (i in 1:13) {
  gppricecounty[[i]] <- data.frame(yieldlistshp[[i]], agg_meanlist[[i]])
  gppricecountymerged2[[i]] <- merge(yieldlistshp[[i]], agg_meanlist[[i]],by="ID")
  gppricecountymerged2[[i]] <- gppricecountymerged2[[i]]  %>% st_set_geometry(NULL)
} 

agg_meanlist


gppricecountydfnaomit<-vector("list", 13)
for (i in 1:13) {
  gppricecountydfnaomit[[i]]<-gppricecountymerged2[[i]] %>% drop_na(Yield)
} 

gppricecountydfnaomit<-as.data.frame(gppricecountydfnaomit)
class(gppricecountydfnaomit[[1]])
gppricecountydfnaomit


### Bind the 
gppricecounty<-bind_rows(gppricecountydfnaomit, .id = "column_label")
View(gppricecounty)

gppricecounty[[2]]

##Converting the yield to g/m2
gppricecounty$yieldgm<-(gppricecounty$Yield)*0.112085

##Converting the modis to 8 day
gppricecounty$gpp_sum_8day<-(gppricecounty$gpp_sum)*8

gppricecounty                     


library(ggplot2)
library(ggpmisc)
library(ggpmisc)
library(ggrepel)
library(broom)

formula1 <- gppricecounty$yieldgm ~ gppricecounty$gpp_sum_8day

ggplot(data = gppricecounty, aes(x = gpp_sum_8day, y = yieldgm, col = Year)) +
  geom_point() +
  
  theme_bw(base_size = 16)+
  xlab("Gross primary producvity")+
  ylab("Yield")

gppricecounty$COUNTYFP

ggplot(gppricecounty, aes(gpp_sum_8day, yieldgm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_poly_eq() +
  facet_wrap(~Year, scales = "free_y")+
  xlab("Mean Cumulative Gross Primary Productivity of the counties (gC/m2/season)")+
  ylab("Mean Yieldof the counties (g/m2/season)")
  
require(ggplot2)
require(plyr)
lm_eqn = function(gppricecounty){
  m = lm(yieldgm ~ gpp_sum_8day, gppricecounty);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}
eq <- ddply(gppricecounty,.(Year),lm_eqn)

p <- ggplot(data = gppricecounty, aes(x = gpp_sum_8day, y = yieldgm)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()+
  xlab("Mean Cumulative Gross Primary Productivity of the counties (gC/m2/season)")+
  ylab("Mean Yieldof the counties (g/m2/season)")
p1 = p + geom_text(data=eq,aes(x = 1200, y = 600,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_wrap(Year~.)
p1 

eq[1]
eq[1,2]
new_str <- gsub('c(','',eq,  fixed=TRUE)
new_str

View(eq)
library(stringr)
new_str <- str_split_fixed(eq$V1, "", 1000)

new_str_2 = new_str[,c(-14,-15, -27, -41, -42, -65)]
new_str_2_3<-paste(new_str_2, collapse="") 
View(new_str_2_3)
library(dplyr)
head(select(new_str_2_3))
new_str_2<-as.data.frame(new_str_2)
new_str_2_3<-tidyr::unite_(new_str_2, paste(colnames(new_str_2)[-1], collapse=""), colnames(new_str_2)[-1])

hist(gppricecounty$gpp_sum_8day)


dplyr::glimpse(eq)
library(tidyverse)

eq2 = eq %>%
  mutate(V1 = str_replace_all(V1, '\\sc\\(|\\)\\s', ''))


eq2 = eq %>%
  separate(V1, into = c('V2', 'V3'), sep = '\\+') %>%
  separate(V3, into = c('V4', 'V5'), sep = '%.%') %>%
  mutate_all(~str_squish(.)) %>%
  mutate(V2 = str_replace(V2, '\\sc\\(', ''),
         V2 = substr(V2, 1, nchar(V2) - 1),
         V2 = paste(V2, '+')) %>%
  mutate(V4 = str_replace(V4, 'c\\(', ''),
         V4 = substr(V4, 1, nchar(V4) - 1),
         V4 = paste(V4, '%.%')) %>%
  mutate(V1 = paste0(V2, ' ', V4, ' ', V5)) %>%
  select(Year, V1)

dplyr::glimpse(eq2)

View(as.data.frame((rastlistyield[[1]])))





library(raster)
# read data    
p <- shapefile("path/file.shp")
d <- read.csv("path/file.csv")

plot(countyonevariable)
yield.2008$COUNTYFP

# merge on common variable, here called 'key'
m <- merge(countyonevariable, yield.2020, by='COUNTYFP')
m <- m[ ,c(1,23)]
plot(m)
# perhaps save as shapefile again
shapefile(m, "path/merged.shp")
View(yield.2020)


#reproject
library(sf)
library(rgdal)
library(spDataLarge)

#check the data type, class(m) if it returns sf "data.frame" you should use st_transform instead of spTransform

m.reprojected <- st_transform(m, crs(rastlistyield[[13]]))

extractedvalues = terra::extract(rastlistyield[[13]], m.reprojected)
class(extractedvalues)

extractedcombine<-c(1:nrow(m.reprojected))
extractedcombine<-as.data.frame(extractedcombine)
extractedcombine$meanGPP<-NA
extractedcombine

for (i in 1: nrow(m.reprojected)){
  extractedcombine$meanGPP[i]<-mean(extractedvalues[[i]], na.rm = TRUE)
}

m$extractedcombine<- c(1:nrow(m.reprojected))

mgppyield <- merge(m, extractedcombine, by='extractedcombine')

extractedcombine$extractedcombine

extractedcombine

plot(mgppyield,  legend = levels(mgppyield$meanGPP))

plot(rastlistyield[[13]])

plot(mgppyield$meanGPP, mgppyield$Yield)

#### New Code
### Plotting everything in a for loop


#rastlistyield = list of the raster images 
#yieldlist = list of the yield dataset

path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/"
Imagenameyield <-list.files(path = path, pattern='.tif$', 
                            all.files=TRUE, full.names=FALSE)

pathlistyield <- vector("list", 13)
for (i in 1:13) {
  pathlistyield[i] = paste('C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/', Imagenameyield[i], sep = "")
  year = year+1
} 

rastlistyield<- vector("list", 13)
for (i in 1:13) {
  rastlistyield[[i]] <- raster(pathlistyield[[i]])
}

## Yield data
yield20082020<-read.csv("C:/Users/rbmahbub/Documents/Data/YieldData/ArkansasCounty2008_2020lb_acre_areayield.csv")
yield20082020$COUNTYFP<-yield20082020$County.ANSI
yield20082020$COUNTYFP<-str_pad(yield20082020$COUNTYFP, 3, pad = "0")
yield20082020$Yield<-as.numeric(gsub(",", "", yield20082020$Value))

for(i in unique(yield20082020$Year)) {
  nam <- paste("yield", i, sep = ".")
  assign(nam, yield20082020[yield20082020$Year==i,])
}

yieldlist<-list(yield.2008, yield.2009, yield.2010, yield.2011, yield.2012, yield.2013, yield.2014, yield.2015, yield.2016, yield.2017, yield.2018, yield.2019, yield.2020)
yieldlist

##Merging Shapefile with yield dataset
## Read the shapefile
### Read the arkansas shpaefile
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/tl_2016_05_cousub/tl_2016_05_cousub.shp")

### Make a county map
county <- aggregate(arkansasshp["COUNTYFP"], by = list(diss = arkansasshp$COUNTYFP), 
                    FUN = function(x)x[1], do_union = TRUE)

##County with one variable
countyonevariable <- county[ ,c(2,3)]

##List of shapefile
yieldlistshp<-vector("list", 13)
for (i in 1:13) {
  yieldlistshp[[i]] <- merge(countyonevariable, yieldlist[i], by='COUNTYFP')
  yieldlistshp[[i]] <- yieldlistshp[[i]][ ,c("COUNTYFP", "Year", "Yield")]
  yieldlistshp[[i]] <- st_transform(yieldlistshp[[i]], crs(rastlistyield[[i]]))
} 

### Extract the rasters or raster values on all the counties
extractedvalueslist<-vector("list", 13)
extractedcombinelist<-vector("list", 13)

for (i in 1:13) {
  extractedvalueslist[[i]] <- terra::extract(rastlistyield[[i]], yieldlistshp[[i]])
  extractedcombinelist[[i]]<-c(1:nrow(yieldlistshp[[i]]))
  extractedcombinelist[[i]]<-as.data.frame(extractedcombinelist[[i]])
  extractedcombinelist[[i]]$meanGPP<-NA
}

for (i in 1: 13){
  for (j in 1: nrow(yieldlistshp[[i]])){
    extractedcombinelist[[i]]$meanGPP[j]<-lapply((extractedvalueslist[[i]][j]), mean, na.rm = TRUE)
  }
}

for (i in 1:13) {
  yieldlistshp[[i]]$extractedcombine<- c(1:nrow(yieldlistshp[[i]]))
  names(yieldlistshp[[i]])[names(yieldlistshp[[i]]) == 'extractedcombine'] <- "extractedcombinelist[[i]]"
}
##rename the column




mgppyieldlist<-vector("list", 13)
for (i in 1:13) {
  mgppyieldlist[[i]]<-merge(yieldlistshp[[i]], extractedcombinelist[[i]], by='extractedcombinelist[[i]]')
}


## Drop the null values
gppricecountydfnaomit<-vector("list", 13)
for (i in 1:13) {
  gppricecountydfnaomit[[i]]<-mgppyieldlist[[i]] %>% drop_na(Yield)
} 

## Convert the shapefile to dataframe
gppricecountydfnaomit_dataframe<-vector("list", 13)
for (i in 1:13) {
  gppricecountydfnaomit_dataframe[[i]]<-gppricecountydfnaomit[[i]] %>% st_drop_geometry()
} 

## Bind the rows convert the list of list to a single dataframe
gppricecounty<-do.call(rbind, Map(data.frame, gppricecountydfnaomit_dataframe))

## COnvert the yield to gram
gppricecounty$yieldgm<-(gppricecounty$Yield)*0.112085

## Convert the gpp to 8 day cumulative (the gpp was giving non numeric argument error so converted to binary operator==)
gppricecounty$meanGPP<-as.numeric(gppricecounty$meanGPP)
gppricecounty$gpp_sum_8day<-(gppricecounty$meanGPP)*8

plot(gppricecounty$gpp_sum_8day, gppricecounty$yieldgm)

## plotting libraries are not working so we are taking the data to a laptop to plot
write.csv(gppricecounty,"gppricecounty.csv", row.names = FALSE)

library(ggppubr)

formula1 <- gppricecounty$yieldgm ~ gppricecounty$gpp_sum_8day
library(ggplot2)
library(ggpmisc)
ggplot(data = gppricecounty, aes(x = gpp_sum_8day, y = yieldgm, col = as.factor(Year), shape)) +
stat_regline_equation
  
  geom_point() +
   theme_bw(base_size = 16)+
  xlab("Gross primary producvity")+
  ylab("Yield")


require(ggplot2)
require(plyr)
lm_eqn = function(gppricecounty){
  m = lm(yieldgm ~ gpp_sum_8day, gppricecounty);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
eq <- ddply(gppricecounty,.(Year),lm_eqn)

p <- ggplot(data = gppricecounty, aes(x = gpp_sum_8day, y = yieldgm)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()+
  xlab("Mean Cumulative Gross Primary Productivity of the counties (gC/m2/season)")+
  ylab("Mean Yieldof the counties (g/m2/season)")
p1 = p + geom_text(data=eq,aes(x = 1200, y = 600,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_wrap(Year~.)
p1 



eq = eq %>%
  separate(V1, into = c('V2', 'V3'), sep = '\\+') %>%
  separate(V3, into = c('V4', 'V5'), sep = '%.%') %>%
  mutate_all(~str_squish(.)) %>%
  mutate(V2 = str_replace(V2, '\\sc\\(', ''),
         V2 = substr(V2, 1, nchar(V2) - 1),
         V2 = paste(V2, '+')) %>%
  mutate(V4 = str_replace(V4, 'c\\(', ''),
         V4 = substr(V4, 1, nchar(V4) - 1),
         V4 = paste(V4, '%.%')) %>%
  mutate(V1 = paste0(V2, ' ', V4, ' ', V5)) %>%
  select(Year, V1)
eq


gppricecounty$yieldbygpp<-gppricecounty$yieldgm/gppricecounty$gpp_sum_8day

hist(gppricecounty$yieldbygpp, xlab = "Yield divided by GPP")

## histogram with probability density function

# Histogram with density plot
ggplot(gppricecounty, aes(x=yieldbygpp)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


library(ggplot2)
library(ggridges)
theme_set(theme_minimal())

ggplot(
  gppricecounty, 
  aes(x = yieldbygpp, y = `Month`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "yieldbygpp", option = "C") +
  labs(title = 'Temperatures in Lincoln NE') 



p <- ggplot(data = gppricecounty, aes(x = yieldbygpp)) + geom_histogram()+
  geom_density(alpha=.2, fill="#FF6666") 

p + facet_wrap(~Year)+theme(strip.background = element_blank(), strip.text = element_blank())
p

gppricecounty$Year

library(viridis)
# Grouped
gppricecounty %>%
  ggplot(aes(fill=Year, y=yieldbygpp, x=Year)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
  scale_fill_viridis(discrete=T, name="") +
  theme_ipsum()  +
  xlab("") +
  ylab("Tip (%)") 

gppricecounty$Yearfactor<-as.factor(gppricecounty$Year)

ggplot(gppricecounty, aes(x = Yearfactor, y = yieldbygpp, fill = Yearfactor)) +
  geom_violin(alpha = 0.5)+
  geom_boxplot(width = 0.07)+
  guides(fill = guide_legend(title = "Title"))





lm_eqn = function(gppricecounty){
  m = lm(yieldgm ~ gpp_sum_8day, gppricecounty);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}

# original eq data
eq <- plyr::ddply(gppricecounty, .(Year),lm_eqn)

# transformation steps resulting in a table with Year and Label
eq = eq %>%
  # split the string apart into new columns
  separate(V1, into = c('V2', 'V3'), sep = '\\+') %>%
  separate(V3, into = c('V4', 'V5'), sep = '%.%') %>%
  separate(V5, into = c('V6', 'V7'), sep = '\\* "," ~ ~') %>%
  separate(V7, into = c('V8', 'V9'), sep = ' ~ "=" ~') %>%
  mutate_all(~str_squish(.)) %>%
  # clean/transform the columns
  mutate(V2 = paste0('y = ', str_extract(V2, '\".*\"'))) %>%
  mutate(V2 = str_replace_all(V2, '\\"', '')) %>%
  mutate(V2 = str_replace(V2, 'y', '<i>y</i>')) %>%
  mutate(V4 = str_extract(V4, '\".*\"')) %>%
  mutate(V4 = str_replace_all(V4, '\\"', '')) %>%
  mutate(V6 = str_extract(V6, '\\(.*\\)')) %>%
  mutate(V6 = str_replace_all(V6, '\\(|\\)', '')) %>%
  mutate(V8a = paste0(str_extract(V8, '\\(.*\\)'))) %>%
  mutate(V8a = str_replace_all(V8a, '\\(|\\)', '')) %>%
  mutate(V8a = paste0(tolower(V8a), '<sup>', substr(V8, nchar(V8) , nchar(V8)), '</sup>')) %>%
  mutate(V9 = str_replace_all(V9, '\\"', '')) %>%
  # assemble final label
  mutate(Label = paste0(V2, ' + ', V4, '&#8729;', V6, ', ', V8a, ' = ', V9)) %>%
  # turn math symbols plus minus (+ -) into a single minus (-)
  mutate(Label = str_replace(Label, ' \\+ -', ' - ')) %>%
  # keep only two columns
  select(Year, Label)

library(ggtext)
library(ggpmisc)

p <- ggplot(data = gppricecounty, aes(x = gpp_sum_8day, y = yieldgm)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size =5)+
  #xlab("Mean Cumulative Gross Primary Productivity of the counties (gC/m2/season)")+
  xlab(expression(Mean~cumulative~gross~primary~productivity~of~the~counties~'in'~Arkansas~(g~C~m^{"-2"}~season^{"-1"})))+
  ylab(expression(Mean~reported~yield~of~the~counties~'in'~Arkansas~(g~m^{"-2"}~season^{"-1"})))+
  #theme_bw()+
  theme(text = element_text(size = 50))

p1 = p + 
  geom_richtext(data=eq, 
                size = 16,
                aes(x = 1400, y = 600, label = Label), 
                inherit.aes=FALSE, 
                fill = NA, 
                label.color = NA) + 
  facet_wrap(Year ~ ., ncol = 3)

p2<-p1 + theme(panel.spacing = unit(5, "lines"))
p2
p1
eq
ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/YieldGPPdifferentyears.png", width = 49, height = 30)

data<-as.data.frame(gppricecounty$gpp_sum_8day)

data$x<-gppricecounty$gpp_sum_8day
data$y<-gppricecounty$yieldgm
data$Year<-gppricecounty$Year


# Create linear model
lm_model <- lm(y ~ x, data = data)
data$Year<-as.character(data$Year)

# Create data frame with predicted values
pred_data <- data %>%
  group_by(Year) %>%
  do(data.frame(x = seq(min(.$x), max(.$x), length.out = 10))) %>%
  ungroup() %>%
  mutate(y = predict(lm_model, newdata = .))
library("plyr")
detach(package:plyr)
library(tidyverse)
data$Year<-as.character(data$Year)
# Calculate RMSE
rmse <- data %>%
  group_by(Year) %>%
  summarize(rmse = sqrt(mean((y - predict(lm_model))^2)))
rmse

stddevcounty <- data %>%
  group_by(Year) %>%
  summarize(sddev = sd(y))
stddevcounty
sd(gppricecounty$gpp_sum_8day)

ggplot(gppricecounty, aes(gpp_sum_8day, yieldgm)) +
  geom_point(size =8) +
  geom_smooth(method = "lm", se = FALSE, size =4) +
  stat_regline_equation(label.x.npc = 0.00,
                        label.y.npc = 0.95, size =13)+
  #stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label..,..rr.label.., sep = "~~~")), 
               #label.x.npc = "right", label.y.npc = 0.15,
                #parse = TRUE, size = 3)+
  stat_poly_eq(label.x.npc = 0.010,
               label.y.npc = 0.80, size =13) +
  facet_wrap(~Year, scales = "fixed", ncol = 3)+
  geom_text(data = rmse, aes(x = max(data$x), y = max(data$y), label = paste0("RMSE = ", round(rmse, 2))),
            x = 950, y = 625, size =13)+
  geom_text(data = stddevcounty, aes(x = max(data$x), y = max(data$y), label = paste0("Std Dev = ", round(sddev, 2))),
            x = 950, y = 600, size =13)+
  
  xlab(expression(Mean~cumulative~gross~primary~productivity~of~the~counties~'in'~Arkansas~(g~C~m^{"-2"}~season^{"-1"})))+
  ylab(expression(Mean~reported~yield~of~the~counties~'in'~Arkansas~(g~m^{"-2"}~season^{"-1"})))+

  theme_bw(base_size = 50)+
  theme(panel.spacing = unit(3, "cm"))

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/YieldGPPdifferentyears.png", width = 49, height = 40)
rmse

plot(rmse)
plot(rmse$rmse, stddevcounty$sddev)
stddevcounty
