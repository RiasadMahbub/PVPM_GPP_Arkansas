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
library(ggpmisc)
library(ggrepel)
library(broom)
require(ggplot2)
require(plyr)
library(Metrics)

#### Make it in a loop
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/"
Imagenameyield <-list.files(path = path, pattern='.tif$', 
                            all.files=TRUE, full.names=FALSE)

pathlistyield <- vector("list", 13)
for (i in 1:13) {
  pathlistyield[i] = paste('C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/', Imagenameyield[i], sep = "")
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
yieldlist##Merging Shapefile with yield dataset
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
(cor(gppricecounty$gpp_sum_8day, gppricecounty$yieldgm))^2

gppricecounty$predictedyield<- 650+ 0.092*gppricecounty$gpp_sum_8day
rmseyield<-rmse(gppricecounty$yieldgm, gppricecounty$predictedyield)
rmseyield

lb1 <- paste("RMSE == rmseyield", round(runif(1),4))

gppcountyyieldplot<-ggplot(data=gppricecounty, aes(x =gpp_sum_8day , y =yieldgm, col = Year))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('Mean Cumulative GPP ('*g~ 'C'~ m^-2~season^-1*')'))+
  ylab(bquote('Reported Yield ('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+
  scale_y_continuous(limits = c(500,1000))+
  scale_color_continuous(high = "#132B43", low = "#56B1F7", breaks = c(2008, 2012, 2016, 2020), labels = c("2008","2012", "2016", "2020"))+
  
  stat_regline_equation(label.x = 1750, label.y = 580, size = 10)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = 1750, label.y = 625, size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 30))+
  annotate(geom = "text", x = 1850, y = 520, label = "RMSE = 61", size = 10)+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(axis.ticks.length = unit(.25, "cm"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

gppcountyyieldplot
ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMyield.png",width = 14, height = 8 )


gppcountyyieldplot$data
